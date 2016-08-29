/************************************************************************
 * Generic OLC Library - Mobiles / genmob.h			v1.0	*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#ifndef GENMOB_H
#define GENMOB_H

int delete_mobile(mob_rnum);
int copy_mobile(Character *to, Character *from);
int add_mobile(Character *, mob_vnum);
//int copy_mob_strings(Character *to, Character *from);
int free_mobile_strings(Character *mob);
int free_mobile(Character *mob);
int save_mobiles(zone_rnum rznum);
void extract_mobile_all(mob_vnum vnum);

#define MAX_MOB_LEVELS 151
extern struct mob_stat_table mob_stats[MAX_MOB_LEVELS];

/* Handy macros. */
#define GET_NDD(mob)	((mob)->mob_specials.damnodice)
#define GET_SDD(mob)	((mob)->mob_specials.damsizedice)
#define GET_ALIAS(mob)	((mob)->player.name)
#define GET_SDESC(mob)	((mob)->player.short_descr)
#define GET_LDESC(mob)	((mob)->player.long_descr)
#define GET_DDESC(mob)	((mob)->player.description)
#define GET_ATTACK(mob)	((mob)->mob_specials.attack_type)

#endif
