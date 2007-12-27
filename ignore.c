/**************************************************************************
*  File: ignore.c                                                         *
*  Usage: Ignoring Players                                                *
*                                                                         *
*  Written by Josh Brittenham                                             *
*                                                                         *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "interpreter.h"
#include "comm.h"
#include "handler.h"

struct ignore *find_ignore(struct ignore *ignore_list, char *str)
{
  while (ignore_list != NULL)
  {
    if (*str == *ignore_list->ignore)
      if (!strcmp(str, ignore_list->ignore))
        return ignore_list;
    ignore_list = ignore_list->next;
  }
  return NULL;
}
/**
If ch is ignoring vict then this function returns 1
otherwise it returns 0
*/
int is_ignoring(Character *ch, Character *vict)
{
  struct ignore *temp;
//  char buf[127];

  if (!ch || !vict)
    return (0);

  if (IS_NPC(ch) || IS_NPC(vict))
    return (0);
  
  temp = GET_IGNORELIST(ch);
  while (temp != NULL)
  {
    if (!str_cmp(GET_NAME(vict), temp->ignore))
      return (1);
    temp = temp->next;
  }
  return (0);
}
void print_ignorelist(Character *ch, Character *vict)
{
  struct ignore *temp;

  if (!ch || !vict)
    return;

  if (IS_NPC(ch) || IS_NPC(vict))
    return;

  temp = GET_IGNORELIST(ch);
  while (temp != NULL)
  {
    vict->Send( "%s\r\n", temp->ignore);
    temp = temp->next;
  }
  return;
}

void write_ignorelist(Character *ch)
{
  FILE *file;
  char ignoref[127];
  struct ignore *ignoretemp;
int valid_to_save(char *name);

  get_filename(GET_NAME(ch), ignoref, IGNORE_FILE);
  unlink(ignoref);
  if (!GET_IGNORELIST(ch))
    return;
  if ((file = fopen(ignoref, "wt")) == NULL)
  {
    log("Can't open ignore file %s", ignoref);
    return;
  }
  ignoretemp = GET_IGNORELIST(ch);
  while (ignoretemp)
  {
if (valid_to_save(ignoretemp->ignore))
    fprintf(file, "%d\n%s\n", strlen(ignoretemp->ignore), ignoretemp->ignore);
    ignoretemp = ignoretemp->next;
  }
  fclose(file);
}

void read_ignorelist(Character *ch)
{
  FILE *file;
  char ignoref[127];
  struct ignore *ignoretemp;
  char buf[127];
  size_t ignorelength;
GET_IGNORELIST(ch) = NULL;
  get_filename(GET_NAME(ch), ignoref, IGNORE_FILE);
  file = fopen(ignoref, "r");
  if (!file)
    return;
    
  
  do
  {
    if (!fscanf(file, "%d\n", &ignorelength)) {
    fclose(file);
    return;
    }
    if (!fgets(buf, ignorelength + 1, file)) {
    fclose(file);
    return;
    }
    CREATE(ignoretemp, struct ignore, 1);
    ignoretemp->ignore = strdup(buf);
    ignoretemp->next = GET_IGNORELIST(ch);
    GET_IGNORELIST(ch) = ignoretemp;
  }
  while (!feof(file));
  fclose(file);
}

void free_ignore(struct ignore *i) {
if (!i)
return;
if (i->next)
free_ignore(i->next);

free_string(&i->ignore);
free(i);
}
void free_ignorelist(Character *ch) {
free_ignore(GET_IGNORELIST(ch));
GET_IGNORELIST(ch) = NULL;
}
