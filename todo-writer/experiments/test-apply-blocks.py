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
    elif "TODO FILL IN" not in open(real, encoding="utf-8").read():
        # Only meaningful on a branch mid-partition. On feature-todo-writer the
        # library file is upstream and marker-free, so the saved blocks match
        # nothing and there is no ambiguity left to refuse.
        print("    SKIP  (file holds no markers on this branch)")
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

    # ---- 6. ID protocol: the case SEARCH/REPLACE cannot do ------------------
    # Identical comments on declarations with DIFFERENT meanings. Text matching
    # must refuse these; by ID there is nothing to be ambiguous about.
    print("  [6] ID protocol on byte-identical comments, different declarations")
    src6 = (
        "trait T {\n"
        "  /** TODO FILL IN */\n"
        "  def foreach(f: A => Unit): Unit\n"
        "\n"
        "  /** TODO FILL IN */\n"
        "  def onComplete(f: A => Unit): Unit\n"
        "}\n"
    )
    mb = w.find_marker_blocks(src6)
    failures += not check("markers found", len(mb), 2)
    failures += not check("ids are 1..n", [b["id"] for b in mb], [1, 2])
    failures += not check("block 1 sees its declaration",
                          mb[0]["decl"], "def foreach(f: A => Unit): Unit")
    failures += not check("block 2 sees its declaration",
                          mb[1]["decl"], "def onComplete(f: A => Unit): Unit")
    out6, st6 = w.apply_by_id(
        src6,
        {1: "/** Applies `f` to each element. */",
         2: "/** Registers a completion callback. */"},
        mb)
    failures += not check("applied", st6["applied"], 2)
    failures += not check("markers left", out6.count("TODO FILL IN"), 0)
    failures += not check("code unchanged",
                          w.code_only(out6) == w.code_only(src6), True)
    failures += not check("foreach got the foreach doc",
                          out6.index("Applies `f`") < out6.index("def foreach"), True)
    failures += not check("onComplete got the onComplete doc",
                          out6.index("Registers a completion") < out6.index("def onComplete")
                          and out6.index("Registers a completion") > out6.index("def foreach"),
                          True)
    failures += not check("indentation preserved",
                          "  /** Applies `f` to each element. */" in out6, True)

    # ---- 7. ID protocol rejects a body that would change code ---------------
    print("  [7] ID block whose text carries a code line -> refused")
    out7, st7 = w.apply_by_id(src6, {1: "/** Doc. */\n  def foreach(f: A => Int): Unit"}, mb)
    failures += not check("applied", st7["applied"], 0)
    failures += not check("would_alter_code", st7["would_alter_code"], 1)
    failures += not check("file untouched", out7 == src6, True)

    # ---- 8. Multi-line comments, nesting, and an unknown ID -----------------
    print("  [8] nested indentation, multi-line comment, unknown ID ignored")
    src8 = (
        "object O {\n"
        "  class C {\n"
        "      /** TODO FILL IN\n"
        "       *\n"
        "       *  @param x TODO FILL IN\n"
        "       */\n"
        "      def m(x: Int): Int = x\n"
        "  }\n"
        "}\n"
    )
    mb8 = w.find_marker_blocks(src8)
    failures += not check("markers found", len(mb8), 1)
    failures += not check("indent captured", mb8[0]["indent"], "      ")
    out8, st8 = w.apply_by_id(
        src8,
        {1: "/** Doubles `x`.\n *\n *  @param x the input\n */", 99: "/** nope */"},
        mb8)
    failures += not check("applied", st8["applied"], 1)
    failures += not check("unknown_id", st8["unknown_id"], 1)
    failures += not check("markers left", out8.count("TODO FILL IN"), 0)
    failures += not check("code unchanged",
                          w.code_only(out8) == w.code_only(src8), True)
    failures += not check("re-indented to 6 spaces",
                          "      /** Doubles `x`." in out8, True)
    failures += not check("continuation lines aligned",
                          "       *  @param x the input" in out8, True)

    # ---- 9. The real leftovers enumerate cleanly ----------------------------
    print("  [9] enumerate the real stranded files")
    for rel, want in (("concurrent/Future.scala", None),
                      ("concurrent/duration/package.scala", None),
                      ("util/matching/Regex.scala", None)):
        p = os.path.join(args.repo, "library/src/scala", rel)
        if not os.path.exists(p):
            print(f"    SKIP  {rel}")
            continue
        s = open(p, encoding="utf-8").read()
        bs = w.find_marker_blocks(s)
        n_markers = s.count("TODO FILL IN")
        every = all(b["decl"] for b in bs)
        print(f"    {rel}: {len(bs)} comments hold {n_markers} markers")
        failures += not check(f"{rel}: every block has a declaration", every, True)
        failures += not check(f"{rel}: ids contiguous",
                              [b["id"] for b in bs] == list(range(1, len(bs) + 1)), True)

    print(f"\n  {'ALL PASS' if not failures else str(failures) + ' FAILURE(S)'}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
