#!/usr/bin/env bash


rm -rf public/
hugo

ssh root@pixel-druid.com "rm -rf /var/www/"
scp -r ./public/* root@www.pixel-druid.com:/var/www/
# rsync -r --progress ./public/* ubuntu@www.pixel-druid.com:/www/
