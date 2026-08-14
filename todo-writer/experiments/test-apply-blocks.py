#!/usr/bin/env python3
"""Exercise direct-writer's block matching without spending an API request.

Replays saved model replies against real files. The interesting cases all come
from the week-4 run:

  duration/package.scala  IntMult and LongMult hold byte-identical marker blocks
                          AND byte-identical declaration lines, so text cannot
                          separate them. Before order-resolution the writer
                          refused all 4 blocks and left 12 markers unfilled.

Usage:  python3 test-apply-blocks.py [--repo /path/to/scala3]
"""

import argparse
import importlib.util
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))


def load_writer():
    path = os.path.join(HERE, "direct-writer.py")
    spec = importlib.util.spec_from_file_location("direct_writer", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def check(name, got, want):
    ok = got == want
    print(f"    {'PASS' if ok else 'FAIL'}  {name}: got {got}, want {want}")
    return ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", default="/workspace/scala3")
    args = ap.parse_args()
    w = load_writer()
    failures = 0

    # ---- 1. Synthetic: duplicated siblings, blocks in file order ------------
    # Two classes whose marker block and declaration line are byte-identical.
    src = (
        "class IntMult {\n"
        "  /** TODO FILL IN */\n"
        "  def x: Int = 1\n"
        "}\n"
        "class LongMult {\n"
        "  /** TODO FILL IN */\n"
        "  def x: Int = 1\n"
        "}\n"
        "/** TODO FILL IN */\n"
        "def unique: Int = 2\n"
        "/** TODO FILL IN */\n"
        "def alsoUnique: Int = 3\n"
    )
    dup = "  /** TODO FILL IN */\n  def x: Int = 1"
    blocks = [
        (dup, "  /** First. */\n  def x: Int = 1"),
        (dup, "  /** Second. */\n  def x: Int = 1"),
        ("/** TODO FILL IN */\ndef unique: Int = 2", "/** U. */\ndef unique: Int = 2"),
        ("/** TODO FILL IN */\ndef alsoUnique: Int = 3", "/** A. */\ndef alsoUnique: Int = 3"),
    ]
    out, st = w.apply_blocks(src, blocks)
    print("  [1] duplicated siblings, two anchors confirm file order")
    failures += not check("applied", st["applied"], 4)
    # Only the FIRST of the pair needs position: once it is filled its text no
    # longer matches, so the second resolves by ordinary exact match.
    failures += not check("order_resolved", st["order_resolved"], 1)
    failures += not check("ambiguous", st["ambiguous"], 0)
    failures += not check("markers left", out.count("TODO FILL IN"), 0)
    # The decisive assertion: each doc landed on its OWN class, in order.
    failures += not check("First lands in IntMult",
                          out.index("First.") < out.index("class LongMult"), True)
    failures += not check("Second lands in LongMult",
                          out.index("Second.") > out.index("class LongMult"), True)
    failures += not check("code unchanged",
                          w.code_only(out) == w.code_only(src), True)

    # ---- 2. Without corroborating anchors, still refuse ---------------------
    # Same duplicated pair, but no unique blocks to establish ordering.
    print("  [2] duplicated siblings, no anchors -> must still refuse")
    out2, st2 = w.apply_blocks(src, blocks[:2])
    failures += not check("applied", st2["applied"], 0)
    failures += not check("ambiguous", st2["ambiguous"], 2)
    failures += not check("order_resolved", st2["order_resolved"], 0)

    # ---- 3. Mixed indentation within one block -----------------------------
    # The duration/package.scala shape: comment at 2 spaces, declaration at 4.
    print("  [3] comment indented 2, declaration indented 4")
    src3 = "object O {\n    /** TODO FILL IN */\n    def y: Int = 1\n}\n"
    blocks3 = [("  /** TODO FILL IN */\n    def y: Int = 1",
                "  /** Y. */\n    def y: Int = 1")]
    out3, st3 = w.apply_blocks(src3, blocks3)
    failures += not check("applied", st3["applied"], 1)
    failures += not check("markers left", out3.count("TODO FILL IN"), 0)
    failures += not check("code unchanged",
                          w.code_only(out3) == w.code_only(src3), True)
    failures += not check("declaration kept 4-space indent",
                          "    def y: Int = 1" in out3, True)

    # ---- 4. A block that would alter code is still refused ------------------
    print("  [4] REPLACE that edits a code line -> refused")
    src4 = "/** TODO FILL IN */\ndef z: Int = 1\n"
    out4, st4 = w.apply_blocks(src4, [("/** TODO FILL IN */\ndef z: Int = 1",
                                       "/** Z. */\ndef z: Int = 2")])
    failures += not check("applied", st4["applied"], 0)
    failures += not check("would_alter_code", st4["would_alter_code"], 1)
    failures += not check("file untouched", out4 == src4, True)

    # ---- 5. Replay the real week-4 failure ----------------------------------
    real = os.path.join(args.repo,
                        "library/src/scala/concurrent/duration/package.scala")
    reply = os.path.join(
        args.repo, "todo-writer/reviews",
        "library_src_scala_concurrent_duration_package.scala.reply.txt")
    # The saved reply is from the LAST writer pass, by which point every block
    # still outstanding is one of the duplicates -- so there are no unique
    # blocks left to establish file order, and resolution correctly declines.
    # The fix earns its keep on pass 1, where the reply is mostly unique blocks
    # (test 1 above is that shape). This case pins the conservative behaviour:
    # a duplicates-only reply must change nothing rather than guess.
    print("  [5] replay: duration/package.scala, duplicates-only reply")
    if not (os.path.exists(real) and os.path.exists(reply)):
        print("    SKIP  (file or saved reply not present)")
    else:
        rsrc = open(real, encoding="utf-8").read()
        rblocks = w.BLOCK.findall(open(reply, encoding="utf-8").read())
        rout, rst = w.apply_blocks(rsrc, rblocks)
        print(f"    blocks in reply: {len(rblocks)}, anchors available: 0")
        failures += not check("code unchanged",
                              w.code_only(rout) == w.code_only(rsrc), True)
        failures += not check("refused rather than guessed",
                              rst["ambiguous"], len(rblocks))
        failures += not check("file untouched", rout == rsrc, True)

    print(f"\n  {'ALL PASS' if not failures else str(failures) + ' FAILURE(S)'}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
