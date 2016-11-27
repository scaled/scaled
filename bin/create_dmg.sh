#!/bin/sh

BINDIR=`dirname $0`
ROOTDIR=`cd $BINDIR/.. ; pwd`

BUILDDIR=$ROOTDIR/target/macsrc
mkdir -p $BUILDDIR/package/macosx
cd $BUILDDIR/package

ICON=$ROOTDIR/etc/icon.png
ICONSET=Scaled.iconset
rm -rf $ICONSET
mkdir $ICONSET
convert -resize 16x16 $ICON $ICONSET/icon_16x16.png
convert -resize 32x32 $ICON $ICONSET/icon_16x16@2x.png
convert -resize 32x32 $ICON $ICONSET/icon_32x32.png
convert -resize 64x64 $ICON $ICONSET/icon_32x32@2x.png
convert -resize 128x128 $ICON $ICONSET/icon_128x128.png
convert -resize 256x256 $ICON $ICONSET/icon_128x128@2x.png
convert -resize 256x256 $ICON $ICONSET/icon_256x256.png
convert -resize 512x512 $ICON $ICONSET/icon_256x256@2x.png
convert -resize 512x512 $ICON $ICONSET/icon_512x512.png
iconutil -c icns $ICONSET

cp Scaled.icns macosx/Scaled-volume.icns
cp Scaled.icns macosx/Scaled.icns
cp $ROOTDIR/etc/Info.plist.xml macosx/Info.plist

cd $BUILDDIR
javapackager \
    -deploy -v \
    -title Scaled \
    -name Scaled \
    -appclass scaled.pacman.Bootstrap \
    -outdir $ROOTDIR/target/macapp \
    -outfile Scaled \
    -native dmg \
    -BmainJar=scaled-pacman.jar \
    -Bruntime= \
    -srcfiles ~/bin/scaled-pacman.jar \
    -argument run \
    -argument "scaled#editor" \
    -argument scaled.impl.Scaled
