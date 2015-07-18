#!/usr/bin/env bash


hugo
# scp -r -i ~/Downloads/ssh-pixel-druid.pem /home/bollu/webdev/pixel-druid.com/public/* ubuntu@www.pixel-druid.com:/www/
rsync -r --progress ./public/* ubuntu@www.pixel-druid.com:/www/
