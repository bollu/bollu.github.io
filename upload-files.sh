#!/usr/bin/env bash


scp -r -i ~/Downloads/ssh-pixel-druid.pem /home/bollu/webdev/new-bollu.github.io/public/* ubuntu@www.pixel-druid.com:/www/
