#!/usr/bin/env python3
"""Sync the public Nextcloud photo album into static/photos/strip/.

Lists the album via public WebDAV, then downloads a 512px preview of every
photo (skipping ones already present, pruning ones that left the album).
The strip directory is gitignored: run this before deploying to refresh the
homepage photo strip. A build with an empty strip dir simply omits the
strip.
"""

import os
import sys
import urllib.request
import xml.etree.ElementTree as ET

HOST = "https://nx72119.your-storageshare.de"
TOKEN = "3unZrGZ2EsoVZiAJKeWWxtH1SueN0TR5"
OUT = os.path.join(os.path.dirname(__file__), "..", "static", "photos", "strip")


def list_album():
    req = urllib.request.Request(
        f"{HOST}/remote.php/dav/photospublic/{TOKEN}/",
        method="PROPFIND",
        headers={"Depth": "1", "Content-Type": "application/xml"},
        data=b'<?xml version="1.0"?>'
             b'<d:propfind xmlns:d="DAV:" xmlns:oc="http://owncloud.org/ns">'
             b'<d:prop><oc:fileid/><d:getcontenttype/></d:prop></d:propfind>',
    )
    with urllib.request.urlopen(req, timeout=30) as r:
        tree = ET.fromstring(r.read())
    ids = []
    for resp in tree.iter("{DAV:}response"):
        ct = resp.find(".//{DAV:}getcontenttype")
        fid = resp.find(".//{http://owncloud.org/ns}fileid")
        if ct is not None and fid is not None and \
                (ct.text or "").startswith("image/"):
            ids.append(fid.text)
    return ids


def jpeg_size(path):
    """Width/height from a JPEG's SOF marker; None if unparseable."""
    with open(path, "rb") as f:
        data = f.read()
    i = 2  # past SOI
    while i + 9 < len(data):
        if data[i] != 0xFF:
            return None
        while i < len(data) and data[i] == 0xFF:  # fill bytes
            i += 1
        marker = data[i]
        i += 1
        if marker in (0x01,) or 0xD0 <= marker <= 0xD9:  # standalone
            continue
        seglen = (data[i] << 8) | data[i + 1]
        if 0xC0 <= marker <= 0xCF and marker not in (0xC4, 0xC8, 0xCC):
            h = (data[i + 3] << 8) | data[i + 4]
            w = (data[i + 5] << 8) | data[i + 6]
            return w, h
        i += seglen
    return None


def write_manifest(ids):
    """manifest.txt: '<fileid> <width> <height>' per photo, newest first.
    The homepage mosaic reads this to emit width/height attributes so
    masonry can lay out before any image loads."""
    lines = []
    for i in sorted(ids, key=int, reverse=True):
        path = os.path.join(OUT, f"{i}.jpg")
        if not os.path.exists(path):
            continue
        wh = jpeg_size(path)
        if wh is None:
            print(f"WARNING: no dimensions for {i}.jpg", file=sys.stderr)
            continue
        lines.append(f"{i} {wh[0]} {wh[1]}\n")
    with open(os.path.join(OUT, "manifest.txt"), "w") as f:
        f.writelines(lines)
    print(f"manifest lists {len(lines)} photos")


def main():
    os.makedirs(OUT, exist_ok=True)
    ids = list_album()
    print(f"album lists {len(ids)} photos")

    want = {f"{i}.jpg" for i in ids} | {"manifest.txt"}
    for stale in set(os.listdir(OUT)) - want:
        os.remove(os.path.join(OUT, stale))
        print(f"pruned {stale}")

    fetched = 0
    for i in ids:
        dst = os.path.join(OUT, f"{i}.jpg")
        if os.path.exists(dst) and os.path.getsize(dst) > 0:
            continue
        url = (f"{HOST}/apps/photos/api/v1/publicPreview/{i}"
               f"?x=512&y=512&token={TOKEN}")
        try:
            with urllib.request.urlopen(url, timeout=60) as r:
                data = r.read()
            with open(dst, "wb") as f:
                f.write(data)
            fetched += 1
        except Exception as e:
            print(f"WARNING: preview {i} failed: {e}", file=sys.stderr)
    njpg = len([f for f in os.listdir(OUT) if f.endswith(".jpg")])
    print(f"fetched {fetched} new previews; strip has {njpg} photos")
    write_manifest(ids)


if __name__ == "__main__":
    main()
