#!/usr/bin/env python3
"""Downscale synced photos into the committed web-size set.

Reads every JPEG in the source directory (photos-sync/, fetched by
sync_photos.py), resizes it to fit --max-edge, recompresses at --quality,
and writes it under the destination directory (static/photos/, committed).
Files whose source hasn't changed are skipped; destination files whose
source disappeared are pruned, so the committed set mirrors the album.

Usage:
    python3 scripts/downscale_photos.py            # photos-sync -> static/photos
    python3 scripts/downscale_photos.py --max-edge 1024 --quality 80
"""

import argparse
import os
import sys

from PIL import Image

ROOT = os.path.join(os.path.dirname(__file__), "..")


def downscale(src: str, dst: str, max_edge: int, quality: int) -> None:
    img = Image.open(src)
    img.thumbnail((max_edge, max_edge), Image.Resampling.LANCZOS)
    # re-encoding drops EXIF, which is deliberate: the web copies carry no
    # camera metadata, and the builder only needs pixel dimensions.
    img.convert("RGB").save(dst, "JPEG", quality=quality, optimize=True,
                            progressive=True)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--src", default=os.path.join(ROOT, "photos-sync"))
    ap.add_argument("--dst", default=os.path.join(ROOT, "static", "photos"))
    ap.add_argument("--max-edge", type=int, default=1024,
                    help="longest side of the output, in pixels")
    ap.add_argument("--quality", type=int, default=80,
                    help="JPEG quality of the output")
    args = ap.parse_args()

    if not os.path.isdir(args.src):
        print(f"nothing to do: no source directory {args.src}")
        return 0
    os.makedirs(args.dst, exist_ok=True)

    srcs = {f for f in os.listdir(args.src) if f.endswith(".jpg")}
    for stale in {f for f in os.listdir(args.dst) if f.endswith(".jpg")} - srcs:
        os.remove(os.path.join(args.dst, stale))
        print(f"pruned {stale}")

    written = 0
    for name in sorted(srcs):
        src = os.path.join(args.src, name)
        dst = os.path.join(args.dst, name)
        if os.path.exists(dst) and os.path.getmtime(dst) >= os.path.getmtime(src):
            continue
        try:
            downscale(src, dst, args.max_edge, args.quality)
            written += 1
        except Exception as e:
            print(f"WARNING: {name}: {e}", file=sys.stderr)
    print(f"downscaled {written} photos; {len(srcs)} in the web set")
    return 0


if __name__ == "__main__":
    sys.exit(main())
