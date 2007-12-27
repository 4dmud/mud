/*
Mordecai's XML help file system.
$Log: xmlhelp.c,v $
Revision 1.3  2005/06/21 08:53:40  w4dimenscor
added in better help finder and help editor, a la mordecai

Revision 1.2  2004/11/20 02:33:25  w4dimenscor
updated and cleaned up the script system

Revision 1.1.1.1  2004/11/12 02:17:12  w4dimenscor
Initial clean submission of 4Dimensions src code

Revision 1.1  2004/08/22 00:50:49  molly
removed all the origional help code, added the start of the xml reader.

22 aug 04
*/
/*
new help load and unload in XML
save the help files with a:
---
category
title
keywords
date created
date modified
revision
approved
---



*/


#include <libxml2/libxml/parser.h>
#include <libxml2/libxml/xinclude.h>
#include <libxml2/libxml/tree.h>
#include <libxml2/libxml/xmlIO.h>

#include "conf.h"
#include "sysdep.h"

#include <dirent.h>


#include "structs.h"
#include "utils.h"
#include "db.h"
#include "comm.h"
#include "handler.h"
#include "interpreter.h"
#include "constants.h"
#include "xmlhelp.h"

void load_xml_help(const char *root)
{
}

void save_xml_help(const char *root)
{
char filebuf[MAX_STRING_LENGTH];

}

void writeXmlHelp(char *filename, char *category, struct help_index_element *he)
{
  int rc;
  FILE *fp;
  struct help_entry_data *el;
  
  if ((fp = fopen(filename, "w")) != NULL) {
  
  for (
  
  
  }
  /** edit end */

}
