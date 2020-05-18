#!/bin/bash
# https://graphicdesign.stackexchange.com/questions/77359/how-to-convert-a-square-svg-to-all-size-ico

set -ex

svg=$1

size=(16 32 24 48 72 96 144 152 192 196)

echo Making bitmaps from your svg...

for i in ${size[@]}; do
  inkscape $svg --export-png="favicon-$i.png" -w$i -h$i --without-gui
done

echo Compressing...

## Replace with your favorite (e.g. pngquant)
# optipng -o7 favicon-*.png
pngquant -f --ext .png favicon-*.png --posterize 4 --speed 1

echo Converting to favicon.ico...

convert $(ls -v favicon-*.png) favicon.ico

## Clean-up maybe?
# rm favicon-*.png

echo Done

cp favicon-196.png banner.png
