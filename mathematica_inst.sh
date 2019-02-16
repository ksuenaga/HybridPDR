#!/bin/sh
set -e

chmod a+x installer.sh

# To deal with a bug in xdg, make /usr/share/desktop-directories
# For more detail, see https://askubuntu.com/questions/405800/installation-problem-xdg-desktop-menu-no-writable-system-menu-directory-found
# and also, https://github.com/dnschneid/crouton/issues/632
mkdir -p /usr/share/desktop-directories /usr/share/icons/hicolor

./installer.sh  -- -auto -execdir=/usr/local/bin -targetdir=/usr/local/Wolfram/Mathematica

rm -rf /usr/local/Wolfram/Mathematica/Documentation
