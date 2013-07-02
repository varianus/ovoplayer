#!/bin/sh
# Force Bourne shell in case tcsh is default.
#
 
#
# Reads the bundle type
#
 
echo "========================================================"
echo "    Bundle creation script"
echo "========================================================"
echo ""
echo " Please select which kind of bundle you would like to build:"
echo ""
echo " 1 > Debug bundle"
echo " 2 > Release bundle"
echo " 0 > Exit"
 
read command
 
case $command in
 
  1) ;;
 
  2) ;;
 
  0) exit 0;;
 
  *) echo "Invalid command"
     exit 0;;
 
esac
 
#
# Creates the bundle
#
BASE=$(pwd) 
BIN_DIR=$BASE/bin/darwin
PACKAGES_DIR=$BASE/packages

appname=OvoPlayer
appfolder=$PACKAGES_DIR/$appname.app
macosfolder=$appfolder/Contents/MacOS
plistfile=$appfolder/Contents/Info.plist
appfile=ovoplayer
 
PkgInfoContents="APPLMAG#"
 
#
if ! [ -e $appfile ]
then
  echo "$appfile does not exist"
elif [ -e $appfolder ]
then
  echo "$appfolder already exists"
else
  echo "Creating $appfolder..."
  mkdir $appfolder
  mkdir $appfolder/Contents
  mkdir $appfolder/Contents/MacOS
  mkdir $appfolder/Contents/Resources
 
#
# For a debug bundle,
# Instead of copying executable into .app folder after each compile,
# simply create a symbolic link to executable.
#
if [ $command = 1 ]; then
  ln -s ../../../$appname $macosfolder/$appname
else
  cp $appname $macosfolder/$appname
fi  
 cp  $BASE/images/logo.png $appfolder/Contents/Resources
 cp  $BASE/images/nocover.png $appfolder/Contents/Resources
 cp  $BASE/images/volume-slider.png $appfolder/Contents/Resources
 cp  $BASE/images/volume-slider-mask.png $appfolder/Contents/Resources

# Copy the resource files to the correct place
 cp o$BASE/images/ovoplayer.icns $appfolder/Contents/Resources
  
# Create PkgInfo file.
  echo $PkgInfoContents >$appfolder/Contents/PkgInfo
#
# Create information property list file (Info.plist).
  echo '<?xml version="1.0" encoding="UTF-8"?>' >$plistfile
  echo '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' >>$plistfile
  echo '<plist version="1.0">' >>$plistfile
  echo '<dict>' >>$plistfile
  echo '  <key>CFBundleDevelopmentRegion</key>' >>$plistfile
  echo '  <string>English</string>' >>$plistfile
  echo '  <key>CFBundleExecutable</key>' >>$plistfile
  echo '  <string>'$appname'</string>' >>$plistfile
  echo '  <key>CFBundleIconFile</key>' >>$plistfile
  echo '  <string>ovoplayer.icns</string>' >>$plistfile
  echo '  <key>CFBundleIdentifier</key>' >>$plistfile
  echo '  <string>org.ovoplayer.ovoplayer</string>' >>$plistfile
  echo '  <key>CFBundleInfoDictionaryVersion</key>' >>$plistfile
  echo '  <string>6.0</string>' >>$plistfile
  echo '  <key>CFBundlePackageType</key>' >>$plistfile
  echo '  <string>APPL</string>' >>$plistfile
  echo '  <key>CFBundleSignature</key>' >>$plistfile
  echo '  <string>MAG#</string>' >>$plistfile
  echo '  <key>CFBundleVersion</key>' >>$plistfile
  echo '  <string>1.0</string>' >>$plistfile
  echo '</dict>' >>$plistfile
  echo '</plist>' >>$plistfile
fi

hdiutil create -srcfolder "$appfolder" -volname "$appname" -fs HFS+ \
      -fsargs "-c c=64,a=16,e=16" -format UDRW -size ${size}k pack.temp.dmg
	  
device=$(hdiutil attach -readwrite -noverify -noautoopen "pack.temp.dmg" | \
         egrep '^/dev/' | sed 1q | awk '{print $1}')	  
		 
chmod -Rf go-w /Volumes/"$appname"
sync
sync
hdiutil detach $device
hdiutil convert "/pack.temp.dmg" -format UDZO -imagekey zlib-level=9 -o "${finalDMGName}"
rm -f /pack.temp.dmg 	


	OUTPUT=$1
	TITLE=$2
	FILESIZE=$3
	CONTENTDIR=$4
	USER=`whoami`
	TMPDIR="/tmp/dmgdir"

	if [ ${USER} != "root" ]; then
		echo "makedmg must be run as root!"
	else
		echo "Creating DMG File..."
		dd if=/dev/zero of=${OUTPUT} bs=1M count=$FILESIZE
		mkfs.hfsplus -v "${TITLE}" ${OUTPUT}

		echo "Mounting DMG File..."
		mkdir -p ${TMPDIR}
		mount -t hfsplus -o loop ${OUTPUT} ${TMPDIR}

		echo "Copying content to DMG File..."
		cp -R ${CONTENTDIR}/* ${TMPDIR}

		echo "Unmounting DMG File..."
		umount ${TMPDIR}
		rm -rf ${TMPDIR}

		echo "All Done!"
	fi
fi	 