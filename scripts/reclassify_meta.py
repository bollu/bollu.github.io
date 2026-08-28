#!/usr/bin/env python3
"""One-off: recompute the `status:` line of every post's ```meta block.

Rules:
  big-list  title starts with "big list" (living list documents).
  draft     WIP/TODO in the title, or TODO/WIP/FIXME/"unfinished" markers
            in the content.
  done      everything else.

created/last-edited lines are left untouched.

Usage: python3 scripts/reclassify_meta.py [--dry-run]
"""

import re
import sys


def judge(title, content):
    if title.lower().lstrip().startswith("big list"):
        return "big-list"
    if re.search(r"\b(wip|todo)\b", title, re.IGNORECASE):
        return "draft"
    for marker in ("TODO", "WIP", "FIXME", "unfinished"):
        if marker in content:
            return "draft"
    return "done"


def main():
    dry_run = "--dry-run" in sys.argv
    path = "README.txt"
    lines = open(path, encoding="utf-8").read().splitlines(keepends=True)

    # article ranges: (title, start, end) with fence-aware heading detection.
    posts = []
    in_fence = False
    for i, line in enumerate(lines):
        if line.startswith("```"):
            in_fence = not in_fence
        elif not in_fence and line.startswith("# "):
            if posts:
                posts[-1][2] = i
            posts.append([line[2:].strip(), i, len(lines)])

    counts = {"done": 0, "draft": 0, "big-list": 0}
    changed = 0
    for title, start, end in posts:
        # scan only prose for WIP markers: a TODO inside a fenced code
        # listing is the code's business, not the post's status.
        prose = []
        fence = False
        for l in lines[start + 1:end]:
            if l.startswith("```"):
                fence = not fence
                continue
            if not fence:
                prose.append(l)
        content = "".join(prose)
        status = judge(title, content)
        counts[status] += 1
        # the meta block's status line is within the first few lines.
        for j in range(start + 1, min(start + 8, end)):
            if lines[j].startswith("status: "):
                new = f"status: {status}\n"
                if lines[j] != new:
                    lines[j] = new
                    changed += 1
                break
        else:
            print(f"WARNING: no status line for {title!r}")

    print(f"judgment: {counts}; {changed} posts changed")
    if dry_run:
        print("[dry-run] README.txt not modified")
    else:
        open(path, "w", encoding="utf-8").write("".join(lines))
        print("README.txt reclassified")


if __name__ == "__main__":
    main()
