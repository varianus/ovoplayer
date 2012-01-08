#!/bin/bash
# set -x
if [ "$BASE" = "" ]; then
  BASE=$(pwd)
fi 

#bash $BASE/buildlinux.sh

PROG_VER=$(sed -e "s/[^0-9.]//g" $BASE/src/version.inc)
BIN_DIR=$BASE/bin/linux
DEBSRCDIR=$BASE/packages/debian/
PACKAGES_DIR=$BASE/packages

MKDIR="install -m 755 -d "
INSTALLFILE="install -c -m 644"
INSTALLEXE="install -c -m 755"

rm -Rf $DEBSRCDIR
$MKDIR $DEBSRCDIR
$MKDIR $DEBSRCDIR/usr
$MKDIR $DEBSRCDIR/usr/bin
$MKDIR $DEBSRCDIR/usr/share
$MKDIR $DEBSRCDIR/usr/share/doc
$MKDIR $DEBSRCDIR/usr/share/doc/ovoplayer
$MKDIR $DEBSRCDIR/usr/share/ovoplayer
$MKDIR $DEBSRCDIR/usr/share/applications
$MKDIR $DEBSRCDIR/usr/share/pixmaps
$MKDIR $DEBSRCDIR/usr/share/man
$MKDIR $DEBSRCDIR/usr/share/man/man1
$MKDIR $DEBSRCDIR/DEBIAN

$INSTALLEXE  -s $BIN_DIR/ovoplayer $DEBSRCDIR/usr/bin
#cp   $BASE/language/*.po $DEBSRCDIR/locale
$INSTALLFILE  $BASE/images/logo.png $DEBSRCDIR/usr/share/pixmaps/ovoplayer.png
$INSTALLFILE  $BASE/images/nocover.png $DEBSRCDIR/usr/share/ovoplayer
$INSTALLFILE  $BASE/images/volume-slider.png $DEBSRCDIR/usr/share/ovoplayer
$INSTALLFILE  $BASE/images/volume-slider-mask.png $DEBSRCDIR/usr/share/ovoplayer
$INSTALLFILE  $BASE/packages/ovoplayer.desktop $DEBSRCDIR/usr/share/applications

$INSTALLFILE  $BASE/packages/copyright $DEBSRCDIR/usr/share/doc/ovoplayer

gzip -c --best $BASE/DOC/manpages/ovoplayer.1 > $DEBSRCDIR/usr/share/man/man1/ovoplayer.1.gz
chmod 0644 $DEBSRCDIR/usr/share/man/man1/ovoplayer.1.gz



INSTALLEDSIZE=$(du -xs --apparent-size --block-size=1024 $DEBSRCDIR/usr | sed -e '$s/[^0-9]//g')
sed -e 's/:INSTALLEDSIZE/'$INSTALLEDSIZE'/;s/:VERSION/'$PROG_VER'/' $BASE/packages/control > $DEBSRCDIR/DEBIAN/control
chmod 0644 $DEBSRCDIR/DEBIAN/control

cd $DEBSRCDIR
find usr -type f | xargs md5sum >> $DEBSRCDIR/DEBIAN/md5sums
chmod 0644 $DEBSRCDIR/DEBIAN/md5sums

fakeroot dpkg --build $DEBSRCDIR $PACKAGES_DIR/ovoplayer-$PROG_VER-x86_64.deb



