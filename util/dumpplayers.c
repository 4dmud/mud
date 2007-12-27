/* ************************************************************************
*  file: purgeplay.c                                    Part of CircleMUD * 
*  Usage: purge useless chars from playerfile                             *
*  All Rights Reserved                                                    *
*  Copyright (C) 1992, 1993 The Trustees of The Johns Hopkins University  *
************************************************************************* */

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"


void purge(char *filename)
{
  FILE *fl;
  FILE *outfile;
  struct char_file_u player;
  int num = 0;
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
  fprintf(stderr, "Dump of Player Database:\r\n");

  for (;;) {
    fread(&player, sizeof(struct char_file_u), 1, fl);
    if (feof(fl)) {
      fclose(fl);
      fprintf(stderr, "Done.\n");
      exit(0);
    }
    
    fprintf(stderr, "%4d. %-20s Gold:%12d, Bank:%12d\n", ++num, player.name,
	    player.points.gold, player.points.bank_gold );

  }
}

int main(int argc, char *argv[])
{
  if (argc != 2)
    printf("Usage: %s playerfile-name\n", argv[0]);
  else
    purge(argv[1]);

  return 0;
}
