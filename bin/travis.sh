#!/bin/sh
#
# Builds and tests (for travis-ci.org)

set -e
GITURL=`git remote -v | grep origin | grep fetch | awk '{ print $2 }'`
wget https://raw.githubusercontent.com/scaled/pacman/master/bin/build-test.sh
sh build-test.sh git:$GITURL
rm build-test.sh
