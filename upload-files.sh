#!/usr/bin/env bash
set -o xtrace
set -e


rm -rf docs/
hugo -d docs

ssh root@pixel-druid.com "rm -rf /var/www/"
rsync -r --progress -avz ./docs/* root@www.pixel-druid.com:/var/www

git add docs/
git commit -m "docs build."
