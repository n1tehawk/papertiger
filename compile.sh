#!/bin/bash
# Convenience script to get updates and compile programs
# Please adjust to your own path
hg pull
hg update

chmod ug+x hocrwrap.sh
chmod ug+x scanwrap.sh
chmod ug+x test.sh
chmod ug+x textdetect.sh

~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigercgi.lpr
~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigerserver.lpr
~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigerclient.lpr


