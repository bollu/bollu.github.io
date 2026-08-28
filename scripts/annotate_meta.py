#!/usr/bin/env python3
"""One-off: annotate every post in README.txt with a ```meta block:

    status: done|draft   (heuristic judgment, see judge_status)
    created: YYYY-MM-DD  (author date of the first commit containing the post)
    last-edited: YYYY-MM-DD (author date of the last commit that changed it)

Dates are mined from the full git history of README.txt (following the
README.md -> README.txt rename). Posts are identified by their `# ` heading
text; a retitled post counts as new on the rename date.

Usage: python3 scripts/annotate_meta.py [--dry-run]
"""

import hashlib
import subprocess
import sys


def run(*args):
    return subprocess.run(args, check=True, capture_output=True).stdout


def history(root):
    """[(sha, date, filename)] for README.txt, oldest first."""
    out = run(
        "git", "-C", root, "log", "--follow",
        "--format=%x01%H%x09%ad", "--date=short", "--name-only",
        "--", "README.txt",
    ).decode("utf-8", errors="replace")
    entries = []
    sha = date = None
    for line in out.splitlines():
        if line.startswith("\x01"):
            sha, date = line[1:].split("\t")
        elif line.strip() and sha is not None:
            entries.append((sha, date, line.strip()))
            sha = date = None
    entries.reverse()  # oldest first; --reverse misbehaves with --follow.
    return entries


def split_articles(text):
    """Fence-aware {heading: content} split; first duplicate heading wins."""
    articles = {}
    heading = None
    content = []
    in_fence = False
    for line in text.splitlines():
        if line.startswith("```"):
            in_fence = not in_fence
        elif not in_fence and line.startswith("# "):
            if heading is not None and heading not in articles:
                articles[heading] = "\n".join(content)
            heading = line[2:].strip()
            content = []
            continue
        if heading is not None:
            content.append(line)
    if heading is not None and heading not in articles:
        articles[heading] = "\n".join(content)
    return articles


def mine_dates(root, entries):
    """{heading: (created, last_edited)} across the whole history."""
    dates = {}   # heading -> [created, last_edited]
    hashes = {}  # heading -> content hash at last sighting
    for i, (sha, date, name) in enumerate(entries):
        try:
            text = run("git", "-C", root, "show", f"{sha}:{name}")
        except subprocess.CalledProcessError:
            print(f"WARNING: unreadable {name} at {sha[:12]}; skipped")
            continue
        text = text.decode("utf-8", errors="replace")
        for heading, content in split_articles(text).items():
            h = hashlib.sha1(content.encode("utf-8")).hexdigest()
            if heading not in dates:
                dates[heading] = [date, date]
            elif hashes.get(heading) != h:
                dates[heading][1] = date
            hashes[heading] = h
        if i % 100 == 0:
            print(f"  scanned {i}/{len(entries)} commits", file=sys.stderr)
    return {k: tuple(v) for k, v in dates.items()}


def judge_status(title, content_lines):
    """draft for living lists, stubs, and posts with explicit WIP markers;
    done otherwise."""
    text = "\n".join(content_lines)
    if title.lower().startswith("big list"):
        return "draft"  # living documents, never finished by design.
    for marker in ("TODO", "WIP", "FIXME", "unfinished", "(wip)"):
        if marker in text:
            return "draft"
    nonblank = [l for l in content_lines if l.strip()]
    if len(nonblank) < 3:
        return "draft"  # a stub.
    return "done"


def annotate(text, dates, fallback_date):
    out = []
    lines = text.splitlines(keepends=True)
    in_fence = False
    counts = {"done": 0, "draft": 0}
    warnings = []
    i = 0
    while i < len(lines):
        line = lines[i]
        out.append(line)
        if line.startswith("```"):
            in_fence = not in_fence
        elif not in_fence and line.startswith("# "):
            title = line[2:].strip()
            # collect the article's content (up to the next H1) for judging.
            content = []
            fence = False
            for l in lines[i + 1:]:
                if l.startswith("```"):
                    fence = not fence
                elif not fence and l.startswith("# "):
                    break
                content.append(l.rstrip("\n"))
            created, last_edited = dates.get(title, (None, None))
            if created is None:
                warnings.append(f"no history for {title!r}; using fallback")
                created = last_edited = fallback_date
            status = judge_status(title, content)
            counts[status] += 1
            out.append(
                f"\n```meta\nstatus: {status}\ncreated: {created}\n"
                f"last-edited: {last_edited}\n```\n"
            )
        i += 1
    return "".join(out), counts, warnings


def main():
    dry_run = "--dry-run" in sys.argv
    root = run("git", "rev-parse", "--show-toplevel").decode().strip()
    readme = f"{root}/README.txt"

    entries = history(root)
    print(f"{len(entries)} commits in the history of README.txt")
    fallback_date = entries[-1][1]

    dates = mine_dates(root, entries)
    print(f"{len(dates)} distinct headings dated")

    text = open(readme, encoding="utf-8").read()
    new_text, counts, warnings = annotate(text, dates, fallback_date)
    for w in warnings:
        print("WARNING:", w)
    print(f"status judgment: {counts['done']} done, {counts['draft']} draft")

    if dry_run:
        print("[dry-run] README.txt not modified")
    else:
        open(readme, "w", encoding="utf-8").write(new_text)
        print("README.txt annotated")


if __name__ == "__main__":
    main()
