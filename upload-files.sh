#!/usr/bin/env bash


hugo
scp -r ./public/* ubuntu@www.pixel-druid.com:/www/
# rsync -r --progress ./public/* ubuntu@www.pixel-druid.com:/www/
