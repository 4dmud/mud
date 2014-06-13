#!/bin/sh
if /sbin/ldconfig -p|grep -q libecl;then
    echo 1
else
    echo 0
fi
