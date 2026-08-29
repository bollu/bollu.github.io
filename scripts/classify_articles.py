#!/usr/bin/env python3
"""One-off: split `status: article` into `technical-note` and `essay`.

Technical notes are the default. A post is an essay when it reads as
first-person reflection: dense first-person prose with no mathematical or
code content. scratch / big-list posts are untouched.

Usage: python3 scripts/classify_articles.py [--dry-run]
"""

import re
import sys

TECH_WORDS = re.compile(
    r"\b(theorem|lemma|proof|corollary|induction|algorithm|semantics|"
    r"compiler|monad|functor|topology|isomorph\w*|homomorph\w*|manifold|"
    r"eigen\w*|polynomial|vector space|category theory|typeclass|"
    r"unification|SAT|UNSAT|clause|derivative|integral|matrix)\b",
    re.IGNORECASE)
FIRST_PERSON = re.compile(r"\b(I|I'm|I've|I'd|my|me|myself)\b")


def classify(prose_lines):
    text = "\n".join(prose_lines)
    nonblank = [l for l in prose_lines if l.strip()]
    if not nonblank:
        return "technical-note"

    inline_math = len(re.findall(r"\$[^$]+\$", text))
    tech_hits = len(TECH_WORDS.findall(text))
    fp = len(FIRST_PERSON.findall(text))
    fp_density = fp / len(nonblank)

    if inline_math >= 2 or tech_hits >= 3:
        return "technical-note"
    if fp >= 6 and fp_density >= 0.35:
        return "essay"
    return "technical-note"


def main():
    dry_run = "--dry-run" in sys.argv
    path = "README.txt"
    lines = open(path, encoding="utf-8").read().splitlines(keepends=True)

    posts = []
    in_fence = False
    for i, line in enumerate(lines):
        if line.startswith("```"):
            in_fence = not in_fence
        elif not in_fence and line.startswith("# "):
            if posts:
                posts[-1][2] = i
            posts.append([line[2:].strip(), i, len(lines)])

    counts = {"technical-note": 0, "essay": 0}
    essays = []
    for title, start, end in posts:
        status_ix = None
        for j in range(start + 1, min(start + 8, end)):
            if lines[j].startswith("status: "):
                status_ix = j
                break
        if status_ix is None or lines[status_ix].strip() != "status: article":
            continue

        # classify on prose only; a post whose body has code fences is
        # technical by construction.
        prose, fence = [], False
        for l in lines[start + 1:end]:
            if l.startswith("```"):
                fence = not fence
                continue
            if not fence:
                prose.append(l.rstrip("\n"))
        # the meta block fence doesn't count as technical content.
        has_real_fence = "".join(lines[start + 1:end]).count("```") > 2

        if has_real_fence:
            kind = "technical-note"
        else:
            kind = classify(prose)
        counts[kind] += 1
        if kind == "essay":
            essays.append(title)
        lines[status_ix] = f"status: {kind}\n"

    print(f"classified: {counts}")
    print("essays:")
    for t in essays:
        print("  -", t)
    if dry_run:
        print("[dry-run] README.txt not modified")
    else:
        open(path, "w", encoding="utf-8").write("".join(lines))
        print("README.txt updated")


if __name__ == "__main__":
    main()
