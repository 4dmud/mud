/*
Mordecai's XML help file system.
$Log: xmlhelp.c,v $
Revision 1.1  2004/11/12 02:17:12  w4dimenscor
Initial revision

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


#include <libxml/parser.h>
#include <libxml/xinclude.h>
#include <libxml/tree.h>
#include <libxml/xmlIO.h>

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
struct help_category_data *cat;
for (cat = help_categories; cat; cat = cat->next) {
if (!cat->uri)
continue;
snprintf(filebuf, sizeof(filebuf), "%s%s", root, cat->uri);
writeXmlHelp(filebuf, cat->
}
}

void writeXmlHelp(char *uri, char *category, struct help_entry_data *he)
{
  int rc;
  xmlTextWriterPtr writer;
  xmlChar *tmp;
  struct help_entry_data *el;
  char timeb[MAX_INPUT_LENGTH];

  /*TODO: Validate uri, category and help_entries */

  /* Create a new XmlWriter for uri, with no compression. */
  writer = xmlNewTextWriterFilename(uri, 0);
  if (writer == NULL)
  {
    /*TODO: change to mudlog */
    log("writeXmlHelp: Error creating the xml writer\n");
    return;
  }
  /* Start the document with the xml default for the version,
       * encoding ISO 8859-1 and the default for the standalone
       * declaration. */
  rc = xmlTextWriterStartDocument(writer, NULL, MY_ENCODING, NULL);
  if (rc < 0)
  {
    log("writeXmlHelp: Error at xmlTextWriterStartDocument\n");
    return;
  }
  /* Start an element named "EXAMPLE". Since thist is the first
  * element, this will be the root element of the document. */
  rc = xmlTextWriterStartElement(writer, BAD_CAST category);
  if (rc < 0)
  {
    log("writeXmlHelp: Error at xmlTextWriterStartElement\n");
    return;
  }
  /** edit */
  for (el = he; el; el= el->next)
  {

    /* Start an element named "ORDER" as child of EXAMPLE. */
    rc = xmlTextWriterStartElement(writer, BAD_CAST "entry");
    if (rc < 0)
    {
      /* TODO:change to mudlog */
      log("writeXmlHelp: Error at xmlTextWriterStartElement\n");
      return;
    }
    sprintf(timeb, "%d", el->id);
    tmp = ConvertInput(timeb, MY_ENCODING);
    rc = xmlTextWriterWriteAttribute(writer, BAD_CAST "id_num", BAD_CAST tmp);
    if (rc < 0)
    {
      /* TODO:change to mudlog */
      log("writeXmlHelp: Error at xmlTextWriterWriteAttribute : id_num\n");
      return;
    }
    if (tmp != NULL) xmlFree(tmp);

    sprintf(timeb, "%ld", el->created);
    tmp = ConvertInput(timeb, MY_ENCODING);
    /* Add an attribute with name "version" and value "1.0" to ORDER. */
    rc = xmlTextWriterWriteAttribute(writer, BAD_CAST "created", BAD_CAST tmp);
    if (rc < 0)
    {
      /* TODO:change to mudlog */
      log("writeXmlHelp: Error at xmlTextWriterWriteAttribute\n");
      return;
    }
    if (tmp != NULL) xmlFree(tmp);

    sprintf(timeb, "%ld", el->modified);
    tmp = ConvertInput(timeb, MY_ENCODING);
    rc = xmlTextWriterWriteAttribute(writer, BAD_CAST "modified", BAD_CAST tmp);
    if (rc < 0)
    {
      /* TODO:change to mudlog */
      log("writeXmlHelp: Error at xmlTextWriterWriteAttribute : modified\n");
      return;
    }
    if (tmp != NULL) xmlFree(tmp);

    sprintf(timeb, "%d", el->revision);
    tmp = ConvertInput(timeb, MY_ENCODING);
    rc = xmlTextWriterWriteAttribute(writer, BAD_CAST "revision", BAD_CAST tmp);
    if (rc < 0)
    {
      /* TODO:change to mudlog */
      log("writeXmlHelp: Error at xmlTextWriterWriteAttribute : revision\n");
      return;
    }
    if (tmp != NULL) xmlFree(tmp);

    /* Write an element named "X_ORDER_ID" as child of HEADER. */
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST "header", "%s", el->header);
    if (rc < 0)
    {
      /* TODO:change to mudlog */
      log("writeXmlHelp: Error at xmlTextWriterWriteFormatElement : header\n");
      return;
    }
    /* Write an element named "X_ORDER_ID" as child of HEADER. */
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST "keywords", "%s", el->keywords);
    if (rc < 0)
    {
      /* TODO:change to mudlog */
      log("writeXmlHelp: Error at xmlTextWriterWriteFormatElement : keywords\n");
      return;
    }
    /* Write an element named "X_ORDER_ID" as child of HEADER. */
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST "body", "%s", el->body);
    if (rc < 0)
    {
      /* TODO:change to mudlog */
      log("writeXmlHelp: Error at xmlTextWriterWriteFormatElement : body\n");
      return;
    }

    /* Close the element named HEADER. */
    rc = xmlTextWriterEndElement(writer);
    if (rc < 0)
    {
      /* TODO:change to mudlog */ 
      log("writeXmlHelp: Error at xmlTextWriterEndElement : end entry\n");
      return;
    }
  }
  /* Close the element named HEADER. */
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0)
  {
    /* TODO:change to mudlog */ log
    ("writeXmlHelp: Error at xmlTextWriterEndElement : end category\n");
    return;
  }

  /* Here we could close the elements ORDER and EXAMPLE using the
   * function xmlTextWriterEndElement, but since we do not want to
   * write any other elements, we simply call xmlTextWriterEndDocument,
   * which will do all the work. */
  rc = xmlTextWriterEndDocument(writer);
  if (rc < 0)
  {
    /* TODO:change to mudlog */ log
    ("writeXmlHelp: Error at xmlTextWriterEndDocument : end page\n");
    return;
  }

  xmlFreeTextWriter(writer);

  /** edit end */

}
