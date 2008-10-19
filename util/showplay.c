/* ************************************************************************
*  file:  showplay.c                                  Part of CircleMud   *
*  Usage: list a diku playerfile                                          *
*  Copyright (C) 1990, 1991 - see 'license.doc' for complete information. *
*  All Rights Reserved                                                    *
************************************************************************* */

#include "config.h"
#include "sysdep.h"

#include "structs.h"


void show(char *filename)
{
  char sexname;
  char classname[10];
  char racename[15];
  FILE *fl;
  struct char_file_u player;
  int num = 0;
  long size;

  if (!(fl = fopen(filename, "r+"))) {
    perror("error opening playerfile");
    exit(1);
  }
  fseek(fl, 0L, SEEK_END);
  size = ftell(fl);
  rewind(fl);
  if (size % sizeof(struct char_file_u)) {
    fprintf(stderr, "\aWARNING:  File size does not match structure, recompile showplay.\n");
    fclose(fl);
    exit(1);
  }

  for (;;) {
    fread(&player, sizeof(struct char_file_u), 1, fl);
    if (feof(fl)) {
      fclose(fl);
      exit(0);
    }
    switch (player.chclass) {
    case CLASS_THIEF:
        strcpy(classname, "Th");
        break;
    case CLASS_WARRIOR:
        strcpy(classname, "Wa");
        break;
    case CLASS_MAGE:
        strcpy(classname, "Ma");
        break;
    case CLASS_PRIEST:
        strcpy(classname, "Pr");
        break;
    case CLASS_HUNTER:
        strcpy(classname, "Hu");
        break;
    case CLASS_RANGER:
        strcpy(classname, "Ra");
        break;
    case CLASS_GYPSY:
        strcpy(classname, "Gy");
        break;
    case CLASS_ESPER:
        strcpy(classname, "Es");
        break;

    default:
      strcpy(classname, "--");
      break;
    }
    switch (player.race) {
    case RACE_FAUN:
      strcpy(racename, "Fau");
      break;
    case RACE_CENTAUR:
      strcpy(racename, "Cen");
      break;
    case RACE_ELF:
      strcpy(racename, "Elf");
      break;
    case RACE_DWARF:
      strcpy(racename, "Dwa");
      break;
    case RACE_INDIAN:
      strcpy(racename, "Ind");
      break;
    case RACE_GRINGO:
      strcpy(racename, "Gri");
      break;
    case RACE_MARTIAN:
      strcpy(racename, "Mar");
      break;
    case RACE_SPACE_WOLF:
      strcpy(racename, "Wol");
      break;
    default:
      strcpy(racename, "---");
      break;
    }
    switch (player.sex) {
    case SEX_FEMALE:
      sexname = 'F';
      break;
    case SEX_MALE:
      sexname = 'M';
      break;
    case SEX_NEUTRAL:
      sexname = 'N';
      break;
    default:
      sexname = '-';
      break;
    }

    printf("%5d. ID: %5ld (%c) [%2d %s %s] %-16s %9ldg %9ldb\n", ++num,
	   player.char_specials_saved.idnum, sexname, player.level,
	   racename, classname, player.name, (long)player.points.gold,
	   (long)player.points.bank_gold);
  }
}


int main(int argc, char **argv)
{
  if (argc != 2)
    printf("Usage: %s playerfile-name\n", argv[0]);
  else
    show(argv[1]);

  return 0;
}
