/*
4d versioning:
a.b.c.d

d: minor bug fix (typos, changes that wont be seen)
c: Bug fixes, or commands, or changes to a feature that are minor but will be seen.
b: Changes in functionality, a major game feature, a section rewritten.
a: TODO list completed.
*/
/* added this file because it will mean easier changes to it via cvs */
#ifndef VERSIONLIST_H
#define VERSIONLIST_H

cpp_extern const char *fourdimensions_version;
cpp_extern const char *circlemud_version;
cpp_extern const char *oasisolc_version;

const char *fourdimensions_version = "4Dimensions, Version 3.9.8.706";
const char *circlemud_version = "CircleMUD, version 3.1 patchlevel 21";
const char *oasisolc_version = "OasisOLC 2.0.6";

#endif
