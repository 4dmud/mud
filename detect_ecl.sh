#!/bin/sh
if /sbin/ldconfig -p|grep -q libecl && ldd `which ecl`|grep -q libstdc++;then
    echo 1
else
    echo 0
fi
