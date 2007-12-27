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


void list_qic(char *filename)
{
  FILE *qic_fl;
  FILE *outfile;
  struct qic_data q;
  int size, i, nr = 0, j;

  if (!(qic_fl = fopen(filename, "r+"))) {
    printf("Can't open %s.", filename);
    exit(1);
  }
  fseek(qic_fl, 0L, SEEK_END);
  size = ftell(qic_fl);
  rewind(qic_fl);
  if (size % sizeof(struct qic_data)) {
    fprintf(stderr, "\aWARNING:  File size does not match structure, recompile listqic.\n");
    fclose(qic_fl);
    exit(1);
  }

  outfile = fopen("qic_list.out", "w");

  for (;;) {
    fread(&q, sizeof(struct qic_data), 1, qic_fl);
    if (feof(qic_fl)) {
      fclose(qic_fl);
      fclose(outfile);
      printf("Done.\n");
      exit(0);
    }

    fprintf(stderr, "Obj Vnum: %d, Limit: %d, Current: %d.\r\n", q.vnum, q.limit, q.items);
    
    fprintf(stderr, "Owners:\r\n");
    for (i=0;i<QIC_OWNERS;i++) 
      fprintf(stderr, "%ld.\r\n", q.owners[i]);
  }
}



int main(int argc, char *argv[])
{
  if (argc != 2)
    printf("Usage: %s qic_db-name\n", argv[0]);
  else
    list_qic(argv[1]);

  return 0;
}
