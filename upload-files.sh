#!/usr/bin/env bash
set -o xtrace
set -e


rm -rf public/
hugo

ssh root@pixel-druid.com "rm -rf /var/www/"
rsync -r --progress -avz ./public/* root@www.pixel-druid.com:/var/www
