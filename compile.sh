#!/bin/bash
# Convenience script to get updates and compile programs
# Please adjust to your own path
hg pull
hg update
~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigercgi.lpr
~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigerclient.lpr
~/trunk/lazarus/lazbuild --pcp="~/trunk/config_lazarus" --build-mode=Default tigerserver.lpr

