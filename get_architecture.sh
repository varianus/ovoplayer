#!/bin/bash

ARCH=$(uname -m)

case "$ARCH" in

 "i686") ARCH="i386";;

 "i586") ARCH="i386";;

 "i486") ARCH="i386";;

 "x86_64") ARCH="amd64";;
esac

echo "$ARCH"

