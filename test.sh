#!/bin/bash
delp .
rm tigercgi
rm tigerserver
hg pull && hg update
#~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus tigerserver.lpi && ./tigerserver --image=bla.tif
~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus tigerserver.lpi
~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus tigercgi.lpi
cp tigercgi /usr/lib/cgi-bin/
# for easier troubleshooting:
cp tigerserver /usr/lib/cgi-bin/
cp tigerserver.ini /usr/lib/cgi-bin
chmod ug+x /usr/lib/cgi-bin/tigercgi
chmod ug+x /usr/lib/cgi-bin/tigerserver
chmod ug+r /usr/lib/cgi-bin/tigerserver.ini
ls -al /usr/lib/cgi-bin/
