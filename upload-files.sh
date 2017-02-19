#!/usr/bin/env bash


hugo
scp -r ./public/* root@www.pixel-druid.com:/var/www/
# rsync -r --progress ./public/* ubuntu@www.pixel-druid.com:/www/
