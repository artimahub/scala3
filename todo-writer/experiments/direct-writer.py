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
A block is applied only when its target site is determined, never guessed at.
A site is determined when the SEARCH text matches exactly one place -- either
verbatim, or ignoring leading whitespace, since mis-indentation is the mistake
models make most.

Text alone cannot always decide. Sibling declarations are sometimes identical
down to the byte, marker block and declaration line alike, so nothing textual
separates them. For those, position decides: when a reply's unambiguous blocks
are confirmed to arrive in file order, a duplicated block takes the next site
not yet consumed. Fewer than two agreeing anchors means no such confirmation,
and the block is skipped as before.

Every edit replaces one located region and nothing else, so the file can only
change where a block matched, which is a stronger guarantee than a client
regenerating the file. Each edit is additionally rejected unless the file's code
lines come out byte-identical, and the caller's strip-comments integrity check
still backstops the whole run.

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


def find_regions(src, search, from_pos=0):
    """Every region of `src` matching `search` ignoring leading whitespace.

    Returns a list of (start, end, indent) in file order, restricted to regions
    starting at or after `from_pos`. `indent` is the leading whitespace of the
    matched region's first line, so a replacement can be re-indented to suit the
    file rather than the model.

    Indentation-blind because that is the one thing models reliably get wrong,
    and they get it wrong *within* a single block: on duration/package.scala the
    writer emitted the comment at 2 spaces and the declaration line under it at
    4, so the block matched nothing at all despite being otherwise perfect.
    """
    want = _strip_indent(search).strip("\n")
    if not want:
        return []
    want_lines = want.split("\n")
    src_lines = src.split("\n")
    stripped = [l.lstrip() for l in src_lines]

    # byte offset of the start of each line
    offsets, pos = [], 0
    for l in src_lines:
        offsets.append(pos)
        pos += len(l) + 1

    out = []
    for i in range(len(src_lines) - len(want_lines) + 1):
        if stripped[i:i + len(want_lines)] != want_lines:
            continue
        start = offsets[i]
        if start < from_pos:
            continue
        first = src_lines[i]
        indent = first[: len(first) - len(first.lstrip())]
        last = i + len(want_lines) - 1
        out.append((start, offsets[last] + len(src_lines[last]), indent))
    return out


def find_dedented(src, search):
    """Locate `search` ignoring leading whitespace, for exactly one match.

    Returns (start, end, indent), else None. Refuses on zero or multiple
    matches, exactly like the exact-match path.
    """
    hits = find_regions(src, search)
    return hits[0] if len(hits) == 1 else None


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


def apply_blocks(src, blocks):
    """Apply SEARCH/REPLACE `blocks` to `src`. Returns (new_src, stats).

    Separate from the HTTP path so the matching rules can be exercised against a
    saved reply without spending a request. See test-apply-blocks.py.
    """
    stats = dict(applied=0, nomatch=0, ambiguous=0, unchanged=0,
                 reindented=0, would_alter_code=0, order_resolved=0)
    code_baseline = code_only(src)

    def try_apply(candidate):
        """Accept an edited file ONLY if it changed no code line.

        Defence at the point of application, not after the fact. The re-indent
        path below re-indents a replacement to suit the file, and when the
        model's block has inconsistent relative indentation that arithmetic
        shifted a `def` line by a space: it fired on 4 files and corrupted 2.
        Rather than trying to make the arithmetic always right, verify the
        outcome -- which also covers every other way a block could touch code."""
        return code_only(candidate) == code_baseline

    # Does this reply list its blocks in file order? Decided from the blocks that
    # match exactly one place, which cannot be mistaken for anything else.
    #
    # Some declarations are indistinguishable by text: IntMult and LongMult in
    # duration/package.scala hold a byte-identical marker block AND a
    # byte-identical declaration line under it, so no amount of surrounding
    # context separates them -- only position does. Assuming file order without
    # checking would be precisely the guess this writer exists to refuse, so
    # require two or more anchors that agree. Absent that evidence, ambiguous
    # blocks are still skipped.
    anchors = []
    for search, _ in blocks:
        hits = find_regions(src, search)
        if len(hits) == 1:
            anchors.append(hits[0][0])
    in_file_order = len(anchors) >= 2 and all(a < b for a, b in zip(anchors, anchors[1:]))

    cursor = 0
    for search, replace in blocks:
        if search == replace:
            stats["unchanged"] += 1
            continue

        if src.count(search) == 1:
            start = src.index(search)
            end = start + len(search)
            new_text = replace
        else:
            # Exact match failed or was ambiguous. Retry ignoring LEADING
            # WHITESPACE, which is the one thing models reliably get wrong:
            # BlockContext.scala's markers are indented 4 spaces (nested in an
            # object) and the writer emitted a 2-space SEARCH block, so a
            # perfectly good comment could not be applied. 9 of 36 files in the
            # week-4 partition nest deeper than 2 spaces.
            hits = find_regions(src, search)
            if not hits:
                stats["nomatch"] += 1
                continue
            if len(hits) > 1:
                forward = [h for h in hits if h[0] >= cursor]
                if not (in_file_order and forward):
                    stats["ambiguous"] += 1
                    continue
                # Blocks arrive in file order, so the next unconsumed site is
                # this block's site. Every earlier one is already filled.
                hits = forward[:1]
                stats["order_resolved"] += 1
            start, end, indent = hits[0]
            new_text = reindent(replace, indent)
            stats["reindented"] += 1

        candidate = src[:start] + new_text + src[end:]
        if not try_apply(candidate):
            stats["would_alter_code"] += 1
            continue
        src = candidate
        cursor = start + len(new_text)
        stats["applied"] += 1

    return src, stats


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
    src, stats = apply_blocks(src, blocks)

    if stats["applied"]:
        with open(args.file, "w", encoding="utf-8") as fh:
            fh.write(src)

    print(f"  blocks parsed:   {len(blocks)}")
    print(f"  applied:         {stats['applied']}")
    print(f"  skipped (no match):  {stats['nomatch']}")
    print(f"  skipped (ambiguous): {stats['ambiguous']}")
    if stats["would_alter_code"]:
        print(f"  REFUSED (would alter code): {stats['would_alter_code']}")
    if stats["order_resolved"]:
        print(f"  order-resolved:  {stats['order_resolved']} (SEARCH text duplicated; matched by position, blocks verified in file order)")
    if stats["reindented"]:
        print(f"  re-indented:     {stats['reindented']} (SEARCH whitespace did not match; matched dedented)")
    if stats["unchanged"]:
        print(f"  no-op blocks:    {stats['unchanged']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
