/* ************************************************************************
*  file: purgeplay.c                                    Part of CircleMUD * 
*  Usage: purge useless chars from playerfile                             *
*  All Rights Reserved                                                    *
*  Copyright (C) 1992, 1993 The Trustees of The Johns Hopkins University  *
************************************************************************* */

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"

struct char_file_u_new {
   /* char_player_data */
   char name[MAX_NAME_LENGTH+1];
   char description[EXDSCR_LENGTH];
   char title[MAX_TITLE_LENGTH+1];
   byte sex;
   byte chclass;
   byte race;
   byte level;
   sh_int hometown;
   time_t birth;   /* Time of birth of character     */
   int  played;    /* Number of secs played in total */
   ubyte weight;
   ubyte height;
   byte was_class;
   byte was_class1;
   byte was_class2;
   byte was_class3;

   char pwd[MAX_PWD_LENGTH+1];    /* character's password */

   struct char_special_data_saved char_specials_saved;
   struct player_special_data_saved player_specials_saved;
   struct char_ability_data abilities;
   struct char_point_data points;
   struct affected_type affected[MAX_AFFECT];

   time_t last_logon;           /* Time (in secs) of last logon */
   char host[HOST_LENGTH+1];    /* host of last logon */
   int romance;
   char partner[MAX_NAME_LENGTH+1];
   int ticks_left;
};

void convert(char *filename)
{
  FILE *fl;
  FILE *outfile;
  struct char_file_u player;
  struct char_file_u_new new_player;
  long size;

  if (!(fl = fopen(filename, "r+"))) {
    printf("Can't open %s.", filename);
    exit(1);
  }
  fseek(fl, 0L, SEEK_END);
  size = ftell(fl);
  rewind(fl);
  if (size % sizeof(struct char_file_u)) {
    fprintf(stderr, "\aWARNING:  File size does not match structure, recompile purgeplay.\n");
    fclose(fl);
    exit(1);
  }

  outfile = fopen("players.new", "w");
  printf("Converting: \n");

  for (;;) {
    fread(&player, sizeof(struct char_file_u), 1, fl);
    if (feof(fl)) {
      fclose(fl);
      fclose(outfile);
      printf("Done.\n");
      exit(0);
    }

    strcpy(new_player.name, player.name);
    strcpy(new_player.description, player.description);
    strcpy(new_player.title, player.title);
    new_player.sex = player.sex;
    new_player.chclass = player.chclass;
    new_player.race = player.race;
    new_player.level = player.level;
    new_player.hometown = player.hometown;
    new_player.birth = player.birth;
    new_player.played = player.played;
    new_player.weight = player.weight;
    new_player.height = player.height;
    new_player.was_class = player.was_class;
    new_player.was_class1 = player.was_class1;
    new_player.was_class2 = player.was_class2;
    new_player.was_class3 = player.was_class3;
    strcpy(new_player.pwd, player.pwd);
    new_player.char_specials_saved = player.char_specials_saved;
    new_player.player_specials_saved = player.player_specials_saved;
    new_player.abilities = player.abilities;
    new_player.points = player.points;
    // new_player.affected, player.affected);
    new_player.last_logon = player.last_logon;
    strcpy(new_player.host, player.host);
    new_player.romance = 0;
    *new_player.partner = '\0';
    new_player.ticks_left = NOT_PREG;
    
    fwrite(&new_player, sizeof(struct char_file_u_new), 1, outfile);
    printf("%-20s\n", player.name);

  }
}



int main(int argc, char *argv[])
{
  if (argc != 2)
    printf("Usage: %s playerfile-name\n", argv[0]);
  else
    convert(argv[1]);

  return 0;
}
