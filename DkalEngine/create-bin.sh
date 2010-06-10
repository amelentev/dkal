#!/bin/sh

set -x -e
rm -rf pack
mkdir pack
cd pack
cp ../fto/*.dkal .
cp ../DkalController/bin/Debug/DkalController.exe .
cp ../DkalController/bin/Debug/DkalEngine.dll .
rm -f ../dkal.zip
zip ../dkal.zip *
cd ..
rm -rf pack
