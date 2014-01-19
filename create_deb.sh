#!/bin/bash
# set -x
if [ "$BASE" = "" ]; then
  BASE=$(pwd)
fi 

ARCH=$(./get_architecture.sh )

bash $BASE/buildlinux.sh

PROG_VER=$(sed -e "s/[^0-9.a-zA-Z]//g" $BASE/src/version.inc)
BIN_DIR=$BASE/bin/linux
DEBSRCDIR=$BASE/packages/debian/
PACKAGES_DIR=$BASE/packages

MKDIR="install -m 755 -d "
INSTALLFILE="install -c -m 644"
INSTALLEXE="install -c -m 755"


##
copylanguage()
{
$MKDIR $DEBSRCDIR/usr/share/locale/$1
$MKDIR $DEBSRCDIR/usr/share/locale/$1/LC_MESSAGES
$INSTALLFILE  $BASE/language/ovoplayer.$1.po $DEBSRCDIR/usr/share/locale/$1/LC_MESSAGES/ovoplayer.po
}


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
$MKDIR $DEBSRCDIR/usr/share/locale

$MKDIR $DEBSRCDIR/DEBIAN

$INSTALLEXE  -s $BIN_DIR/ovoplayer $DEBSRCDIR/usr/bin
$INSTALLEXE  -s $BIN_DIR/ovoplayerctrl $DEBSRCDIR/usr/bin
$INSTALLFILE  $BASE/images/logo.png $DEBSRCDIR/usr/share/pixmaps/ovoplayer.png
$INSTALLFILE  $BASE/images/logo.png $DEBSRCDIR/usr/share/ovoplayer
$INSTALLFILE  $BASE/images/nocover.png $DEBSRCDIR/usr/share/ovoplayer
$INSTALLFILE  $BASE/images/volume-slider.png $DEBSRCDIR/usr/share/ovoplayer
$INSTALLFILE  $BASE/images/volume-slider-mask.png $DEBSRCDIR/usr/share/ovoplayer
$INSTALLFILE  $PACKAGES_DIR/ovoplayer.desktop $DEBSRCDIR/usr/share/applications

##  Add language files
copylanguage it
##

$INSTALLFILE  $PACKAGES_DIR/copyright $DEBSRCDIR/usr/share/doc/ovoplayer

gzip -c --best $BASE/DOC/manpages/ovoplayerctrl.1 > $DEBSRCDIR/usr/share/man/man1/ovoplayerctrl.1.gz
chmod 0644 $DEBSRCDIR/usr/share/man/man1/ovoplayerctrl.1.gz

gzip -c --best $BASE/DOC/manpages/ovoplayer.1 > $DEBSRCDIR/usr/share/man/man1/ovoplayer.1.gz
chmod 0644 $DEBSRCDIR/usr/share/man/man1/ovoplayer.1.gz

gzip -c --best $PACKAGES_DIR/changelog > $DEBSRCDIR/usr/share/doc/ovoplayer/changelog.gz
chmod 0644 $DEBSRCDIR/usr/share/doc/ovoplayer/changelog.gz

INSTALLEDSIZE=$(du -0 -xs --apparent-size --block-size=1024 $DEBSRCDIR/usr | cut -f 1)
sed -e 's/:INSTALLEDSIZE/'$INSTALLEDSIZE'/;s/:VERSION/'$PROG_VER'/;s/:ARCHITECTURE/'$ARCH'/' $BASE/packages/control > $DEBSRCDIR/DEBIAN/control
chmod 0644 $DEBSRCDIR/DEBIAN/control

cd $DEBSRCDIR
find usr -type f | xargs md5sum >> $DEBSRCDIR/DEBIAN/md5sums
chmod 0644 $DEBSRCDIR/DEBIAN/md5sums

fakeroot dpkg --build $DEBSRCDIR $PACKAGES_DIR/ovoplayer-$PROG_VER-gtk2-$ARCH.deb



