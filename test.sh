#!/bin/bash
hg pull && hg update
~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus tigerserver.lpi && ./tigerserver --image=bla.tif

