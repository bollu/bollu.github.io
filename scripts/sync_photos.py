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


def main():
    os.makedirs(OUT, exist_ok=True)
    ids = list_album()
    print(f"album lists {len(ids)} photos")

    want = {f"{i}.jpg" for i in ids}
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
    print(f"fetched {fetched} new previews; strip has "
          f"{len(os.listdir(OUT))} photos")


if __name__ == "__main__":
    main()
