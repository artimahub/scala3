#!/usr/bin/env python3
"""Fill TODO FILL IN Scaladoc placeholders by talking to an OpenAI-compatible
endpoint directly, with no coding-agent client in between.

Why this exists
---------------
Three of four models failed through aider, each differently:

  laguna-s-2.1   could not emit SEARCH/REPLACE at all, and whole-file mode
                 silently deleted a public class
  gpt-oss-120b   was fed bogus lint errors by a grammar that cannot parse
                 Scala 3 capture checking, and asked to "fix" correct code
  zai-glm-4.7    returned empty `content` under aider's prompt in three
                 separate configurations

Meanwhile a plain single-message request to zai-glm-4.7 -- same model, same
file, same endpoint -- returned 42 of 42 SEARCH/REPLACE blocks, every one
matching the file byte-exactly, in five seconds. The obstacle was the client,
not the models.

So: one user message, no system prompt, no few-shot examples, no linter, no
metadata guessing. Parse the blocks and apply them here.

Safety
------
A block is applied only when its SEARCH text occurs EXACTLY ONCE in the file.
Zero matches or several matches are skipped and counted, never guessed at. Since
every edit is an exact-string replacement, the file can only change where a
block matched, which is a stronger guarantee than a client regenerating the
file. The caller's strip-comments integrity check still backstops this.

Usage:
  direct-writer.py --file <path> --prompt <path> --model <id> \
                   --base-url <url> --api-key-env <VAR> [--max-tokens N]

Exits 0 on a completed request (even if nothing applied -- the caller's marker
count is the completion signal, not this exit code), 1 on a request failure.
"""

import argparse
import json
import os
import re
import sys
import time
import urllib.error
import urllib.request

BLOCK = re.compile(
    r"<<<<<<< SEARCH\n(.*?)\n=======\n(.*?)\n>>>>>>> REPLACE",
    re.DOTALL,
)


def _strip_indent(text):
    """Drop each line's leading whitespace, for indentation-blind comparison."""
    return "\n".join(l.lstrip() for l in text.split("\n"))


def find_dedented(src, search):
    """Locate `search` in `src` ignoring leading whitespace on every line.

    Returns (start, end, indent) for exactly one match, else None. `indent` is
    the leading whitespace of the matched region's first line, so the
    replacement can be re-indented to suit the file rather than the model.
    Refuses on zero or multiple matches, exactly like the exact-match path.
    """
    want = _strip_indent(search).strip("\n")
    if not want:
        return None
    want_lines = want.split("\n")
    src_lines = src.split("\n")
    stripped = [l.lstrip() for l in src_lines]

    # byte offset of the start of each line
    offsets, pos = [], 0
    for l in src_lines:
        offsets.append(pos)
        pos += len(l) + 1

    hits = []
    for i in range(len(src_lines) - len(want_lines) + 1):
        if stripped[i:i + len(want_lines)] == want_lines:
            hits.append(i)
    if len(hits) != 1:
        return None
    i = hits[0]
    first = src_lines[i]
    indent = first[: len(first) - len(first.lstrip())]
    start = offsets[i]
    end = offsets[i + len(want_lines) - 1] + len(src_lines[i + len(want_lines) - 1])
    return start, end, indent


def reindent(text, indent):
    """Re-indent `text` so every non-empty line carries `indent`, preserving
    the relative shape the model produced."""
    lines = text.strip("\n").split("\n")
    base = None
    for l in lines:
        if l.strip():
            lead = len(l) - len(l.lstrip())
            base = lead if base is None else min(base, lead)
    base = base or 0
    return "\n".join(indent + l[base:] if l.strip() else "" for l in lines)


def code_only(text):
    """The non-comment, non-blank lines, for verifying an edit touched nothing
    but Scaladoc. Mirrors the caller's strip-comments integrity check."""
    out = []
    for l in text.split("\n"):
        t = l.strip()
        if not t or t.startswith("*") or t.startswith("/**") or t.startswith("*/"):
            continue
        out.append(l)
    return out


def post(base_url, api_key, payload, timeout):
    req = urllib.request.Request(
        base_url.rstrip("/") + "/chat/completions",
        data=json.dumps(payload).encode(),
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
            # Cerebras sits behind Cloudflare, which rejects urllib's default
            # "Python-urllib/3.11" User-Agent with HTTP 403 "error code: 1010"
            # (bot-signature block) before the request ever reaches the API.
            # Measured: default UA -> 403, curl-like UA -> 200. The same request
            # via curl always worked, which is why the probe never hit this.
            "User-Agent": "curl/8.5.0",
        },
    )
    with urllib.request.urlopen(req, timeout=timeout) as r:
        body = r.read().decode()
    # OpenRouter holds the connection open during long generations by emitting
    # keepalive lines -- blank lines and lines of spaces -- BEFORE the JSON
    # body. json.loads rejects the result outright; jq tolerates it, which is
    # why every curl probe worked and the first real run did not. Start at the
    # first brace rather than trusting the payload to be the whole response.
    start = body.find("{")
    if start == -1:
        raise ValueError(f"no JSON object in response: {body[:200]!r}")
    return json.loads(body[start:])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--file", required=True)
    ap.add_argument("--prompt", required=True)
    ap.add_argument("--model", required=True)
    ap.add_argument("--base-url", required=True)
    ap.add_argument("--api-key-env", required=True)
    ap.add_argument("--max-tokens", type=int, default=32000)
    ap.add_argument("--timeout", type=int, default=900)
    ap.add_argument("--reasoning-effort", default="")
    ap.add_argument("--retry-backoff", type=int, default=30)
    ap.add_argument("--dump", default="", help="write the raw reply here")
    args = ap.parse_args()

    api_key = os.environ.get(args.api_key_env, "")
    if not api_key:
        print(f"ERROR: ${args.api_key_env} is empty", file=sys.stderr)
        return 1

    src = open(args.file, encoding="utf-8").read()
    instructions = open(args.prompt, encoding="utf-8").read()

    # One user message: the instructions, then the file. Deliberately no system
    # prompt -- that is what silenced GLM through aider.
    content = (
        f"{instructions}\n\n"
        f"Here is the current content of {args.file}:\n\n"
        f"```scala\n{src}\n```\n"
    )
    payload = {
        "model": args.model,
        "max_tokens": args.max_tokens,
        "messages": [{"role": "user", "content": content}],
    }
    # Reasoning control, which is NOT portable between providers.
    #
    # Cerebras takes  reasoning_effort: "none"|"low"|"medium"|"high"
    # OpenRouter takes reasoning: {"enabled": false} / {"effort": "..."}
    #
    # This matters more than a compatibility nicety. Reasoning models emit into
    # a separate channel before any `content`, and with a large budget they can
    # spend all of it thinking. cohere/north-mini-code:free did exactly that:
    # completion=32000, reasoning=32910, content empty, 1011 seconds for zero
    # blocks. Sending the wrong provider's spelling means the setting is
    # silently ignored and the run burns the full budget again.
    if args.reasoning_effort:
        if "openrouter.ai" in args.base_url:
            if args.reasoning_effort == "none":
                payload["reasoning"] = {"enabled": False}
            else:
                payload["reasoning"] = {"effort": args.reasoning_effort}
        else:
            payload["reasoning_effort"] = args.reasoning_effort

    # Retry on rate limits. Mistral's free tier 429s readily when several roles
    # fire in quick succession; the first pipeline run lost both refine passes
    # to it. Everything else fails fast, since retrying a 400 just repeats it.
    resp = None
    delay = args.retry_backoff
    for attempt in range(1, 5):
        try:
            resp = post(args.base_url, api_key, payload, args.timeout)
            break
        except urllib.error.HTTPError as e:
            body = e.read().decode()[:400]
            if e.code in (429, 503) and attempt < 4:
                print(f"  rate limited (HTTP {e.code}), attempt {attempt}/4; waiting {delay}s")
                sys.stdout.flush()
                time.sleep(delay)
                delay *= 2
                continue
            print(f"ERROR: HTTP {e.code}: {body}", file=sys.stderr)
            return 1
        except urllib.error.URLError as e:
            # DNS and transient network failures. Seen live: "Errno -3 Temporary
            # failure in name resolution" mid-run, with both endpoints healthy
            # seconds later. Worth retrying rather than losing the file.
            if attempt < 4:
                print(f"  network error ({e.reason}), attempt {attempt}/4; waiting {delay}s")
                sys.stdout.flush()
                time.sleep(delay)
                delay *= 2
                continue
            print(f"ERROR: URLError: {e}", file=sys.stderr)
            return 1
        except Exception as e:  # malformed JSON, anything unexpected
            print(f"ERROR: {type(e).__name__}: {e}", file=sys.stderr)
            return 1
    if resp is None:
        print("ERROR: exhausted retries", file=sys.stderr)
        return 1

    choice = (resp.get("choices") or [{}])[0]
    msg = choice.get("message") or {}
    reply = msg.get("content") or ""
    usage = resp.get("usage") or {}
    details = usage.get("completion_tokens_details") or {}

    print(f"  finish_reason:   {choice.get('finish_reason')}")
    print(
        f"  tokens:          prompt={usage.get('prompt_tokens')} "
        f"completion={usage.get('completion_tokens')} "
        f"reasoning={details.get('reasoning_tokens')}"
    )

    if args.dump:
        with open(args.dump, "w", encoding="utf-8") as fh:
            fh.write(reply)

    if not reply.strip():
        # The exact failure aider hit with GLM: everything went to the reasoning
        # channel and `content` came back empty. Say so plainly.
        print("  reply had EMPTY content (check the reasoning channel)")
        print("  blocks parsed:   0")
        return 0

    blocks = BLOCK.findall(reply)
    applied = skipped_nomatch = skipped_ambiguous = unchanged = reindented = 0
    skipped_would_alter_code = 0

    def try_apply(candidate):
        """Accept an edited file ONLY if it changed no code line.

        Defence at the point of application, not after the fact. The re-indent
        fallback below re-indents a replacement to suit the file, and when the
        model's block has inconsistent relative indentation that arithmetic
        shifted a `def` line by a space: it fired on 4 files and corrupted 2.
        Rather than trying to make the arithmetic always right, verify the
        outcome -- which also covers every other way a block could touch code."""
        return code_only(candidate) == code_baseline

    code_baseline = code_only(src)

    for search, replace in blocks:
        n = src.count(search)
        if n == 0:
            # Exact match failed. Before giving up, try again ignoring LEADING
            # WHITESPACE, which is the one thing models reliably get wrong:
            # BlockContext.scala's markers are indented 4 spaces (nested in an
            # object) and the writer emitted a 2-space SEARCH block, so a
            # perfectly good comment could not be applied. 9 of 36 files in the
            # week-4 partition nest deeper than 2 spaces.
            #
            # This stays safe: it still demands EXACTLY ONE match, and it
            # re-indents the replacement to whatever the file actually uses, so
            # the result is indented like its neighbours rather than like the
            # model's guess.
            hit = find_dedented(src, search)
            if hit is None:
                skipped_nomatch += 1
                continue
            start, end, indent = hit
            candidate = src[:start] + reindent(replace, indent) + src[end:]
            if not try_apply(candidate):
                skipped_would_alter_code += 1
                continue
            src = candidate
            applied += 1
            reindented += 1
        elif n > 1:
            # Ambiguous: applying it would edit an arbitrary one of several
            # sites. Refuse rather than guess.
            skipped_ambiguous += 1
        elif search == replace:
            unchanged += 1
        else:
            candidate = src.replace(search, replace, 1)
            if not try_apply(candidate):
                skipped_would_alter_code += 1
                continue
            src = candidate
            applied += 1

    if applied:
        with open(args.file, "w", encoding="utf-8") as fh:
            fh.write(src)

    print(f"  blocks parsed:   {len(blocks)}")
    print(f"  applied:         {applied}")
    print(f"  skipped (no match):  {skipped_nomatch}")
    print(f"  skipped (ambiguous): {skipped_ambiguous}")
    if skipped_would_alter_code:
        print(f"  REFUSED (would alter code): {skipped_would_alter_code}")
    if reindented:
        print(f"  re-indented:     {reindented} (SEARCH whitespace did not match; matched dedented, unique)")
    if unchanged:
        print(f"  no-op blocks:    {unchanged}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
