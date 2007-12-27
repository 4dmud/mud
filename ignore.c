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

int is_ignoring(struct char_data *ch, struct char_data *vict)
{
  struct ignore *temp;
  char buf[127];

  if (!ch || !vict)
    return (0);

  if (IS_NPC(ch) || IS_NPC(vict))
    return (0);
  if (GET_LEVEL(ch) > LVL_SEN || GET_LEVEL(vict) > LVL_SEN)
  sprintf(buf, "%s", GET_NAME(ch));
  temp = GET_IGNORELIST(vict);
  while (temp != NULL)
  {
    if (!str_cmp(buf, temp->ignore))
      return (1);
    temp = temp->next;
  }
  return (0);
}
void print_ignorelist(struct char_data *ch, struct char_data *vict)
{
  struct ignore *temp;

  if (!ch || !vict)
    return;

  if (IS_NPC(ch) || IS_NPC(vict))
    return;

  temp = GET_IGNORELIST(ch);
  while (temp != NULL)
  {
    new_send_to_char(vict, "%s\r\n", temp->ignore);
    temp = temp->next;
  }
  return;
}

void write_ignorelist(struct char_data *ch)
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

void read_ignorelist(struct char_data *ch)
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
free_ignore(i);

free_string(&i->ignore);
free(i);
}
void free_ignorelist(struct char_data *ch) {
free_ignore(GET_IGNORELIST(ch));
GET_IGNORELIST(ch) = NULL;
}
