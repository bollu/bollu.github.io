#!/usr/bin/env bash
set -o xtrace
set -e

make build

git checkout github-pages
git add -f out/
git commit -m "github page upload"

ssh root@pixel-druid.com "rm -rf /var/blog/"
rsync -r --progress -avz ./ root@www.pixel-druid.com:/var/blog
