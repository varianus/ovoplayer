#!/bin/bash
# set -x
#Set path where lazarus is installed
LAZARUS_DIR=~/lazarus/lazarus

# 
if [ "$BASE" = "" ]; then
  BASE=c:/source/ovoplayer 
  #$(pwd)
fi   
   
if [ ! "$OS_TARGET" = "" ]; then 
  DC_ARCH="$DC_ARCH --os=$OS_TARGET"
fi
   
if [ ! "$CPU_TARGET" = "" ]; then
 DC_ARCH="$DC_ARCH% --cpu=$CPU_TARGET"
fi 

if [ ! "$WIDGETSET_TARGET" = "" ]; then
 DC_ARCH="$DC_ARCH% --cpu=$WIDGETSET_TARGET"
fi 

NONE=-l
# clean build files
rm -Rf $BASE/src/lib/*
rm -Rf $BASE/src/components/lib/*.*

rm -Rf $BASE/bin/linux
#
cp $BASE/release.cfg  $BASE/extrafpc.cfg
$LAZARUS_DIR/lazbuild -B -r $BASE/src/ovoplayer.lpi $DC_ARCH
strip --strip-all $BASE/bin/linux/ovoplayer

echo $none > $BASE/extrafpc.cfg
