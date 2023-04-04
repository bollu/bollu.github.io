#!/usr/bin/env bash
set -o xtrace
set -e

ssh root@pixel-druid.com "rm -rf /var/blog/"
rsync -r --progress -avz ./ root@www.pixel-druid.com:/var/blog
