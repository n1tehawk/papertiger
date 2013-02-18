#!/bin/bash
delp .
rm tigercgi
rm tigerserver
hg pull && hg update
#~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus tigerserver.lpi && ./tigerserver --image=bla.tif
echo
echo Compiling tigerserver
echo "**************************************************"
~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus --build-all tigerserver.lpi
echo
echo
echo Compiling tigercgi
echo "**************************************************"
~/trunk/lazarus/lazbuild --primary-config-path=~/trunk/config_lazarus --build-all tigercgi.lpi
echo

cp tigercgi        /usr/lib/cgi-bin/
cp hocrwrap.sh     /usr/lib/cgi-bin/
cp scanwrap.sh     /usr/lib/cgi-bin/
cp tigerserver     /usr/lib/cgi-bin/
cp tigerserver.ini /usr/lib/cgi-bin

delp .

chmod ugo+rx /usr/lib/cgi-bin/hocrwrap.sh
chmod ugo+rx /usr/lib/cgi-bin/scanwrap.sh
chmod ugo+rx /usr/lib/cgi-bin/tigercgi
chmod ugo+rx /usr/lib/cgi-bin/tigerserver
chmod ugo+r  /usr/lib/cgi-bin/tigerserver.ini
echo "Directory /usr/lib/cgi-bin"
ls -al /usr/lib/cgi-bin/
