/* ************************************************************************
*  file:  autowiz.c                                     Part of CircleMUD *
*  Usage: self-updating wizlists                                          *
*  Written by Jeremy Elson                                                *
*  All Rights Reserved                                                    *
*  Copyright (C) 1993 The Trustees of The Johns Hopkins University        *
************************************************************************* */


/* 
   WARNING:  THIS CODE IS A HACK.  WE CAN NOT AND WILL NOT BE RESPONSIBLE
   FOR ANY NASUEA, DIZZINESS, VOMITING, OR SHORTNESS OF BREATH RESULTING
   FROM READING THIS CODE.  PREGNANT WOMEN AND INDIVIDUALS WITH BACK
   INJURIES, HEART CONDITIONS, OR ARE UNDER THE CARE OF A PHYSICIAN SHOULD
   NOT READ THIS CODE.

   -- The Management
 */

#include "conf.h"
#include "sysdep.h"

#include <signal.h>

#include "structs.h"
#include "utils.h"
#include "db.h"
#include "clan.h"

#define IMM_LMARG "   "
#define IMM_NSIZE  16
#define LINE_LEN   64
#define MIN_LEVEL LVL_HERO

/* max level that should be in columns instead of centered */
#define COL_LEVEL LVL_HERO

struct name_rec {
  char name[25];
  struct name_rec *next;
};

struct control_rec {
  int level;
  const char *level_name;
};

struct level_rec {
  struct control_rec *params;
  struct level_rec *next;
  struct name_rec *names;
};

struct control_rec level_params[] =
{
  {LVL_HERO, "Heros"},
  {LVL_GOD, "Gods"},
  {LVL_GRGOD, "Greater Gods"},
  {LVL_IMPL, "Implementors"},
  {0, ""}
};


struct level_rec *levels = 0;

void initialize(void)
{
  struct level_rec *tmp;
  int i = 0;

  while (level_params[i].level > 0) {
    tmp = (struct level_rec *) malloc(sizeof(struct level_rec));
    tmp->names = 0;
    tmp->params = &(level_params[i++]);
    tmp->next = levels;
    levels = tmp;
  }
}


void read_file(void)
{
  void add_name(byte level, char *name);

  struct char_file_u player;
  char clan_name[256];
  FILE *fl;

  if (!(fl = fopen(PLAYER_FILE, "rb"))) {
    perror("Error opening playerfile");
    exit(1);
  }
  while (!feof(fl)) {
    fread(&player, sizeof(struct char_file_u), 1, fl);
    if (!feof(fl) && player.player_specials_saved.clan &&
	!(IS_SET_AR(player.char_specials_saved.act, PLR_FROZEN)) &&
	!(IS_SET_AR(player.char_specials_saved.act, PLR_NOWIZLIST)) &&
        !(IS_SET_AR(player.char_specials_saved.act, PLR_DELETED))) {
        clan_name = clan[find_clan_by_id(player.player_specials_save.clan)].name;
        add_name(player.level, player.name, clan_name);
    }
  }

  fclose(fl);
}


void add_name(byte level, char *name)
{
  struct name_rec *tmp;
  struct level_rec *curr_level;
  char *ptr;

  if (!*name)
    return;

  for (ptr = name; *ptr; ptr++)
    if (!isalpha(*ptr))
      return;

  tmp = (struct name_rec *) malloc(sizeof(struct name_rec));
  strcpy(tmp->name, name);
  tmp->next = 0;

  curr_level = levels;
  while (curr_level->params->level > level)
    curr_level = curr_level->next;

  tmp->next = curr_level->names;
  curr_level->names = tmp;
}


void sort_names(void)
{
  struct level_rec *curr_level;
  struct name_rec *a, *b;
  char temp[100];

  for (curr_level = levels; curr_level; curr_level = curr_level->next) {
    for (a = curr_level->names; a && a->next; a = a->next) {
      for (b = a->next; b; b = b->next) {
	if (strcmp(a->name, b->name) > 0) {
	  strcpy(temp, a->name);
	  strcpy(a->name, b->name);
	  strcpy(b->name, temp);
	}
      }
    }
  }
}


void write_wizlist(FILE * out, int minlev, int maxlev)
{
  char buf[100];
  struct level_rec *curr_level;
  struct name_rec *curr_name;
  int i, j;

  fprintf(out,
"****************************************************************************\n"
"* The following people have reached immortality on CircleMUD.  They are to *\n"
"* treated with respect and awe.  Occasional prayers to them are advisable. *\n"
"* Annoying them is not recommended.  Stealing from them is punishable by   *\n"
"* immediate death.                                                         *\n"
"****************************************************************************\n\n");

  for (curr_level = levels; curr_level; curr_level = curr_level->next) {
    if (curr_level->params->level < minlev ||
	curr_level->params->level > maxlev)
      continue;
    i = 39 - (strlen(curr_level->params->level_name) >> 1);
    for (j = 1; j <= i; j++)
      fputc(' ', out);
    fprintf(out, "%s\n", curr_level->params->level_name);
    for (j = 1; j <= i; j++)
      fputc(' ', out);
    for (j = 1; j <= strlen(curr_level->params->level_name); j++)
      fputc('~', out);
    fprintf(out, "\n");

    strcpy(buf, "");
    curr_name = curr_level->names;
    while (curr_name) {
      strcat(buf, curr_name->name);
      if (strlen(buf) > LINE_LEN) {
	if (curr_level->params->level <= COL_LEVEL)
	  fprintf(out, IMM_LMARG);
	else {
	  i = 40 - (strlen(buf) >> 1);
	  for (j = 1; j <= i; j++)
	    fputc(' ', out);
	}
	fprintf(out, "%s\n", buf);
	strcpy(buf, "");
      } else {
	if (curr_level->params->level <= COL_LEVEL) {
	  for (j = 1; j <= (IMM_NSIZE - strlen(curr_name->name)); j++)
	    strcat(buf, " ");
	}
	if (curr_level->params->level > COL_LEVEL)
	  strcat(buf, "   ");
      }
      curr_name = curr_name->next;
    }

    if (*buf) {
      if (curr_level->params->level <= COL_LEVEL)
	fprintf(out, "%s%s\n", IMM_LMARG, buf);
      else {
	i = 40 - (strlen(buf) >> 1);
	for (j = 1; j <= i; j++)
	  fputc(' ', out);
	fprintf(out, "%s\n", buf);
      }
    }
    fprintf(out, "\n");
  }
}




int main(int argc, char **argv)
{
    // int wizlevel, immlevel, pid = 0;
    int pid = 0;
    FILE *fl;

//    if (argc != 5 && argc != 6) {
//        printf("Format: %s wizlev wizlistfile immlev immlistfile [pid to signal]\n",
//               argv[0]);
//        exit(0);
//    }
//    wizlevel = atoi(argv[1]);
//   immlevel = atoi(argv[3]);
//    if (argc == 6)
//        pid = atoi(argv[5]);
    initialize();
    read_file();
    sort_names();
    fl = fopen(argv[2], "w");
    write_wizlist(fl, wizlevel, LVL_IMPL);
    fclose(fl);
    fl = fopen(argv[4], "w");
    write_wizlist(fl, immlevel, wizlevel - 1);
    fclose(fl);
    if (pid)
        kill(pid, SIGUSR1);
    return 0;
}
