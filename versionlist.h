/*
 * $Log: versionlist.h,v $
 * Revision 1.2  2004/11/17 05:13:05  w4dimenscor
 * updated pets so that they don't have weight problems, updated award points
 *
 * Revision 1.1.1.1  2004/11/12 02:16:19  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.2  2004/08/15 01:22:42  molly
 * added newline to end of versionlist, and logging
 *
 */
 /*
4d versioning:
a.b.c.d

d: minor bug fix (typos, changes that wont be seen)
c: Bug fixes, or commands, or changes to a feature that are minor but will be seen.
b: Changes in functionality, a major game feature, a section rewritten.
a: TODO list completed.
*/
/* added this file because it will mean easier changes to it via cvs */

cpp_extern const char *fourdimensions_version;
cpp_extern const char *circlemud_version;
cpp_extern const char *oasisolc_version;

const char *fourdimensions_version = "4Dimensions, Version 3.9.8.706";
const char *circlemud_version = "CircleMUD, version 3.1 patchlevel 21";
const char *oasisolc_version = "OasisOLC 2.0.6";

