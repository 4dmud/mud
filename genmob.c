/************************************************************************
 * Generic OLC Library - Mobiles / genmob.c			v1.0	*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "db.h"
#include "shop.h"
#include "handler.h"
#include "genolc.h"
#include "genmob.h"
#include "genzon.h"
#include "dg_olc.h"
#include "htree.h"


int update_mobile_strings(struct char_data *t, struct char_data *f);
void check_mobile_strings(struct char_data *mob);
void check_mobile_string(mob_vnum i, char **string, const char *dscr);
int write_mobile_record(mob_vnum mvnum, struct char_data *mob, FILE *fd);
int write_mobile_espec(mob_vnum mvnum, struct char_data *mob, FILE *fd);
int write_mobile_links(mob_vnum mvnum, struct char_data *mob, FILE *fd);
int free_mobile_strings(struct char_data *mob);
int copy_mobile_strings(struct char_data *t, struct char_data *f);
#if CONFIG_GENOLC_MOBPROG
int write_mobile_mobprog(mob_vnum mvnum, struct char_data *mob, FILE *fd);
#endif

float m_powf(float x, float y);
struct mob_stat_table mob_stats[MAX_MOB_LEVELS];

void assign_mob_stats()
{
  int s[11] =     {0, 100, 1, 8, 0, 1, 1, 0, 100, 0, 0};

  int i;

  for (i = 1; i < MAX_MOB_LEVELS; i++ )
  {

    s[0] = i;
    /* ac needs to drop from 100 at level <1 to -100 at level > 50  */
    s[1] = ((i > 50) ? -100 : (100 - ((i< 1?1:i)*4)));
    //Hp
    s[2] = i;
    s[3] = i;
    s[4] = MAX(40, (i * i * i)/35) + 150;
    //Damroll
    s[5] = MAX(1, (i*0.3) + (i*0.3));
    s[6] = MAX(2, (i*0.25) + (i*0.3));
    s[7] = m_powf(i, (i*0.004)+1.31);//(i >= 10) ? (i * i * i * (0.0036 + (i>=60?0.01:0.0))) : (i);

    s[8] =  (i * i * i * 15) + 200; /* exp */
    s[9] = (i * i * i * 1.4); /* gold */
    s[10] = i * (1.0 + (i * 0.01)); /* hitroll */

    /*-------put it back together--------*/
    mob_stats[i].level 		= s[0];
    mob_stats[i].ac 		= s[1];
    mob_stats[i].hp_dice 	= s[2];
    mob_stats[i].hp_sides 	= s[3];
    mob_stats[i].hp_bonus 	= s[4];
    mob_stats[i].dam_dice 	= s[5];
    mob_stats[i].dam_sides 	= s[6];
    mob_stats[i].dam_bonus 	= s[7];
    mob_stats[i].exp 		= s[8] + (s[8] * 0.05); /*added 5%*/
    mob_stats[i].gold 		= s[9];
    mob_stats[i].hitroll 	= s[10];
    log("LEV:%-2d -- HP:%-6d Avg Dam:%-4d X:%-7d G:%-d",
        s[0],(int)((s[2] * s[3]) * 0.5) + s[3] + s[4],(int)((s[5] * s[6]) * 0.5) + s[6] + s[7],s[8], s[9]);


  }
}

float dam_avg(int from, int to, int lev)
{
  float ret_val = 0.0;
  return ret_val;
}

void smash_tilde(char *str);

int add_mobile(struct char_data *mob, mob_vnum vnum)
{
  int rnum, i, found = FALSE, shop, cmd_no;
  zone_rnum zone;
  struct char_data *live_mob;

  if ((rnum = real_mobile(vnum)) != NOBODY)
  {
    /* Copy over the mobile and free() the old strings. */
    copy_mobile(&mob_proto[rnum], mob);

    /* Now re-point all existing mobile strings to here. */
    for (live_mob = character_list; live_mob; live_mob = live_mob->next)
      if (rnum == live_mob->nr)
        update_mobile_strings(live_mob, &mob_proto[rnum]);

    add_to_save_list(zone_table[real_zone_by_thing(vnum)].number, SL_MOB);
    log("GenOLC: add_mobile: Updated existing mobile #%d.", vnum);
    return rnum;
  }

  RECREATE(mob_proto, struct char_data, top_of_mobt + 2);
  RECREATE(mob_index, struct index_data, top_of_mobt + 2);
  top_of_mobt++;

  for (i = top_of_mobt; i > 0; i--)
  {
    if (vnum > mob_index[i - 1].vnum)
    {
      mob_proto[i] = *mob;
      mob_proto[i].nr = i;
      copy_mobile_strings(mob_proto + i, mob);
      mob_index[i].vnum = vnum;
      mob_index[i].number = 0;
      mob_index[i].func = 0;
      found = i;
      break;
    }
    mob_index[i] = mob_index[i - 1];
    mob_proto[i] = mob_proto[i - 1];
    mob_proto[i].nr++;
htree_add(mob_htree, mob_index[i].vnum, i);
  }
  if (!found)
  {
    mob_proto[0] = *mob;
    mob_proto[0].nr = 0;
    copy_mobile_strings(&mob_proto[0], mob);
    mob_index[0].vnum = vnum;
    mob_index[0].number = 0;
    mob_index[0].func = 0;
htree_add(mob_htree, mob_index[0].vnum, 0);
  }

  log("GenOLC: add_mobile: Added mobile %d at index #%d.", vnum, found);

#if CONFIG_GENOLC_MOBPROG
  GET_MPROG(OLC_MOB(d)) = OLC_MPROGL(d);
  GET_MPROG_TYPE(OLC_MOB(d)) = (OLC_MPROGL(d) ? OLC_MPROGL(d)->type : 0);
  while (OLC_MPROGL(d))
  {
    GET_MPROG_TYPE(OLC_MOB(d)) |= OLC_MPROGL(d)->type;
    OLC_MPROGL(d) = OLC_MPROGL(d)->next;
  }
#endif

  /*
   * Update live mobile rnums.
   */
  for (live_mob = character_list; live_mob; live_mob = live_mob->next)
    GET_MOB_RNUM(live_mob) += (GET_MOB_RNUM(live_mob) >= found);

  /*
   * Update zone table.
   */
  for (zone = 0; zone <= top_of_zone_table; zone++)
    for (cmd_no = 0; ZCMD(zone, cmd_no).command != 'S'; cmd_no++)
      if (ZCMD(zone, cmd_no).command == 'M')
        ZCMD(zone, cmd_no).arg1 += (ZCMD(zone, cmd_no).arg1 >= found);

  /*
   * Update shop keepers.
   */
  if (shop_index)
    for (shop = 0; shop <= top_shop - top_shop_offset; shop++)
      SHOP_KEEPER(shop) += (SHOP_KEEPER(shop) >= found);

  add_to_save_list(zone_table[real_zone_by_thing(vnum)].number, SL_MOB);
  return found;
}

int copy_mobile(struct char_data *to, struct char_data *from)
{
  free_mobile_strings(to);
  *to = *from;
  check_mobile_strings(from);
  copy_mobile_strings(to, from);
  return TRUE;
}

void extract_mobile_all(mob_vnum vnum)
{
  struct char_data *next, *ch;

  for (ch = character_list; ch; ch = next)
  {
    next = ch->next;
    if (GET_MOB_VNUM(ch) == vnum)
      extract_char(ch);
  }
}

int delete_mobile(mob_rnum refpt)
{
  struct char_data *live_mob;
  int counter, cmd_no;
  mob_vnum vnum;
  zone_rnum zone;

#if CIRCLE_UNSIGNED_INDEX
  if (refpt == NOBODY || refpt > top_of_mobt)
  {
#else
  if (refpt < 0 || refpt > top_of_mobt)
  {
#endif
    log("SYSERR: GenOLC: delete_mobile: Invalid rnum %d.", refpt);
    return NOBODY;
  }

  vnum = mob_index[refpt].vnum;
  add_to_save_list(zone_table[real_zone_by_thing(vnum)].number, SL_MOB);
  extract_mobile_all(vnum);
  free_mobile_strings(&mob_proto[refpt]);

  for (counter = refpt; counter < top_of_mobt; counter++)
  {
    mob_index[counter] = mob_index[counter + 1];
    mob_proto[counter] = mob_proto[counter + 1];
    mob_proto[counter].nr--;
  }

  top_of_mobt--;
  RECREATE(mob_index, struct index_data, top_of_mobt + 1);
  RECREATE(mob_proto, struct char_data, top_of_mobt + 1);

  /*
   * Update live mobile rnums.
   */
  for (live_mob = character_list; live_mob; live_mob = live_mob->next)
    GET_MOB_RNUM(live_mob) -= (GET_MOB_RNUM(live_mob) >= refpt);

  /*
   * Update zone table.
   */
  for (zone = 0; zone <= top_of_zone_table; zone++)
    for (cmd_no = 0; ZCMD(zone, cmd_no).command != 'S'; cmd_no++)
      if (ZCMD(zone, cmd_no).command == 'M')
        ZCMD(zone, cmd_no).arg1 -= (ZCMD(zone, cmd_no).arg1 >= refpt);

  /*
   * Update shop keepers.
   */
  if (shop_index)
    for (counter = 0; counter <= top_shop - top_shop_offset; counter++)
      SHOP_KEEPER(counter) -= (SHOP_KEEPER(counter) >= refpt);

  return refpt;
}

int copy_mobile_strings(struct char_data *t, struct char_data *f)
{
  if (f->player.name)
    t->player.name = strdup(f->player.name);
  if (f->player.title)
    t->player.title = strdup(f->player.title);
  if (f->player.short_descr)
    t->player.short_descr = strdup(f->player.short_descr);
  if (f->player.long_descr)
    t->player.long_descr = strdup(f->player.long_descr);
  if (f->player.description)
    t->player.description = strdup(f->player.description);
  return TRUE;
}

int update_mobile_strings(struct char_data *t, struct char_data *f)
{
  if (f->player.name)
    t->player.name = f->player.name;
  if (f->player.title)
    t->player.title = f->player.title;
  if (f->player.short_descr)
    t->player.short_descr = f->player.short_descr;
  if (f->player.long_descr)
    t->player.long_descr = f->player.long_descr;
  if (f->player.description)
    t->player.description = f->player.description;
  return TRUE;
}

int free_mobile_strings(struct char_data *mob)
{
  if (mob->player.name)
    free(mob->player.name);
  if (mob->player.title)
    free(mob->player.title);
  if (mob->player.short_descr)
    free(mob->player.short_descr);
  if (mob->player.long_descr)
    free(mob->player.long_descr);
  if (mob->player.description)
    free(mob->player.description);
  return TRUE;
}

/*
 * Free a mobile structure that has been edited.
 * Take care of existing mobiles and their mob_proto!
 */
int free_mobile(struct char_data *mob)
{
  mob_rnum i;

  if (mob == NULL)
    return FALSE;

  /*
   * Non-prototyped mobile.  Also known as new mobiles.
   */
  if ((i = GET_MOB_RNUM(mob)) == NOBODY)
  {
    free_mobile_strings(mob);
    /* free script proto list */
    free_proto_script(mob, MOB_TRIGGER);
  }
  else
  {	/* Prototyped mobile. */
    if (mob->player.name && mob->player.name != mob_proto[i].player.name)
      free(mob->player.name);
    if (mob->player.title && mob->player.title != mob_proto[i].player.title)
      free(mob->player.title);
    if (mob->player.short_descr &&
        mob->player.short_descr != mob_proto[i].player.short_descr)
      free(mob->player.short_descr);
    if (mob->player.long_descr &&
        mob->player.long_descr != mob_proto[i].player.long_descr)
      free(mob->player.long_descr);
    if (mob->player.description &&
        mob->player.description != mob_proto[i].player.description)
      free(mob->player.description);
    /* free script proto list if it's not the prototype */
    if (mob->proto_script && mob->proto_script != mob_proto[i].proto_script)
      free_proto_script(mob, MOB_TRIGGER);
  }
  while (mob->affected)
    affect_remove(mob, mob->affected);
  /* free any assigned scripts */
  if (SCRIPT(mob))
    extract_script(mob, MOB_TRIGGER);

  free(mob);
  return TRUE;
}

int save_mobiles(zone_rnum rznum)
{
  zone_vnum vznum;
  FILE *mobfd;
  room_vnum i;
  mob_rnum rmob;
  int written;
  char mobfname[64], usedfname[64];

#if CIRCLE_UNSIGNED_INDEX
  if (rznum == NOWHERE || rznum > top_of_zone_table)
  {
#else
  if (rznum < 0 || rznum > top_of_zone_table)
  {
#endif
    log("SYSERR: GenOLC: save_mobiles: Invalid real zone number %d. (0-%d)",
        rznum, top_of_zone_table);
    return FALSE;
  }

  vznum = zone_table[rznum].number;
  snprintf(mobfname, sizeof(mobfname), "%s/%d/%d.new", LIB_WORLD, vznum, vznum);
  if ((mobfd = fopen(mobfname, "w")) == NULL)
  {
    new_mudlog(BRF, LVL_GOD, TRUE,
               "SYSERR: GenOLC: Cannot open mob file for writing.");
    return FALSE;
  }

  for (i = genolc_zone_bottom(rznum); i <= zone_table[rznum].top; i++)
  {
    if ((rmob = real_mobile(i)) == NOBODY)
      continue;
    check_mobile_strings(&mob_proto[rmob]);
    if (write_mobile_record(i, &mob_proto[rmob], mobfd) < 0)
      log("SYSERR: GenOLC: Error writing mobile #%d.", i);
  }
  fputs("$\n", mobfd);
  written = ftell(mobfd);
  fclose(mobfd);
  snprintf(usedfname, sizeof(usedfname), "%s/%d/%d.mob", LIB_WORLD, vznum, vznum);
  remove(usedfname);
  rename(mobfname, usedfname);

  if (in_save_list(vznum, SL_MOB))
    remove_from_save_list(vznum, SL_MOB);
  log("GenOLC: '%s' saved, %d bytes written.", usedfname, written);
  return written;
}

#if CONFIG_GENOLC_MOBPROG
int write_mobile_mobprog(mob_vnum mvnum, struct char_data *mob, FILE *fd)
{
  char wmmarg[MAX_STRING_LENGTH], wmmcom[MAX_STRING_LENGTH];
  MPROG_DATA *mob_prog;

  for (mob_prog = GET_MPROG(mob); mob_prog; mob_prog = mob_prog->next)
  {
    wmmarg[MAX_STRING_LENGTH - 1] = '\0';
    wmmcom[MAX_STRING_LENGTH - 1] = '\0';
    strip_cr(strncpy(wmmarg, mob_prog->arglist, MAX_STRING_LENGTH - 1));
    strip_cr(strncpy(wmmcom, mob_prog->comlist, MAX_STRING_LENGTH - 1));
    fprintf(fd,	"%s %s~\n"
            "%s%c\n",
            medit_get_mprog_type(mob_prog), wmmarg,
            wmmcom, STRING_TERMINATOR
           );
    if (mob_prog->next == NULL)
      fputs("|\n", fd);
  }
  return TRUE;
}
#endif
int write_mobile_links(mob_vnum mvnum, struct char_data *mob, FILE *fd)
{
  struct combine_data *temp = NULL;
  if ((temp = mob->mob_specials.join_list) != NULL)
  {

    /* linked mob */
    while (temp)
    {
      fprintf(fd, "J %d\n", temp->vnum);
      temp = temp->next;
    }
  }
  return TRUE;
}

int write_mobile_espec(mob_vnum mvnum, struct char_data *mob, FILE *fd)
{
  if (GET_ATTACK(mob) != 0)
    fprintf(fd, "BareHandAttack: %d\n", GET_ATTACK(mob));
  if (GET_STR(mob) != 11)
    fprintf(fd, "Str: %d\n", GET_STR(mob));
  if (GET_ADD(mob) != 0)
    fprintf(fd, "StrAdd: %d\n", GET_ADD(mob));
  if (GET_DEX(mob) != 11)
    fprintf(fd, "Dex: %d\n", GET_DEX(mob));
  if (GET_INT(mob) != 11)
    fprintf(fd, "Int: %d\n", GET_INT(mob));
  if (GET_WIS(mob) != 11)
    fprintf(fd, "Wis: %d\n", GET_WIS(mob));
  if (GET_CON(mob) != 11)
    fprintf(fd, "Con: %d\n", GET_CON(mob));
  if (GET_CHA(mob) != 11)
    fprintf(fd, "Cha: %d\n", GET_CHA(mob));
  if (GET_CLASS(mob) != 0)
    fprintf(fd, "Class: %d\n", GET_CLASS(mob));
  if (MOB_TIER(mob) != 0)
    fprintf(fd, "Tier: %d\n", MOB_TIER(mob));
  if (MOB_SUBSKILL(mob) != SUB_UNDEFINED)
    fprintf(fd, "Subskill: %d\n", MOB_SUBSKILL(mob));
 if (MOB_SKIN(mob) != -1)
    fprintf(fd, "Skin: %d\n", MOB_SKIN(mob));
  fputs("E\n", fd);
  return TRUE;
}


int write_mobile_record(mob_vnum mvnum, struct char_data *mob, FILE *fd)
{


  char ldesc[MAX_STRING_LENGTH];
  char ddesc[MAX_STRING_LENGTH];

  ldesc[MAX_STRING_LENGTH - 1] = '\0';
  ddesc[MAX_STRING_LENGTH - 1] = '\0';
  strip_cr(strncpy(ldesc, GET_LDESC(mob), MAX_STRING_LENGTH - 1));
  strip_cr(strncpy(ddesc, GET_DDESC(mob), MAX_STRING_LENGTH - 1));

  fprintf(fd,	"#%d\n"
          "%s%c\n"
          "%s%c\n"
          "%s%c\n"
          "%s%c\n",
          mvnum,
          GET_ALIAS(mob), STRING_TERMINATOR,
          GET_SDESC(mob), STRING_TERMINATOR,
          ldesc, STRING_TERMINATOR,
          ddesc, STRING_TERMINATOR
         );



  fprintf(fd,	"%d %d %d %d %d %d %d %d %d E\n"
          "%d %d %d %dd%d+%d %dd%d+%d\n",
          MOB_FLAGS(mob)[0], MOB_FLAGS(mob)[1],
          MOB_FLAGS(mob)[2], MOB_FLAGS(mob)[3],
          AFF_FLAGS(mob)[0], AFF_FLAGS(mob)[1],
          AFF_FLAGS(mob)[2], AFF_FLAGS(mob)[3], GET_ALIGNMENT(mob),

          GET_LEVEL(mob), 20 - GET_HITROLL(mob), GET_AC(mob) / 10, GET_HIT(mob),
          GET_MANA(mob), GET_MOVE(mob), GET_NDD(mob), GET_SDD(mob),
          GET_DAMROLL(mob)
         );
  fprintf(fd, 	"%lld %lld\n"
          "%d %d %d %d\n",
          GET_GOLD(mob), GET_EXP(mob),
          GET_POS(mob), GET_DEFAULT_POS(mob), GET_SEX(mob), GET_MRACE(mob)
         );



  if (write_mobile_espec(mvnum, mob, fd) < 0)
    log("SYSERR: GenOLC: Error writing E-specs for mobile #%d.", mvnum);

  if (write_mobile_links(mvnum, mob, fd) < 0)
    log("SYSERR: GenOLC: Error writing links for mobile #%d.", mvnum);

  script_save_to_disk(fd, mob, MOB_TRIGGER);

#if CONFIG_GENOLC_MOBPROG
  if (write_mobile_mobprog(mvnum, mob, fd) < 0)
    log("SYSERR: GenOLC: Error writing MobProgs for mobile #%d.", mvnum);
#endif

  return TRUE;
}

void check_mobile_strings(struct char_data *mob)
{
  mob_vnum mvnum = mob_index[mob->nr].vnum;
  check_mobile_string(mvnum, &GET_LDESC(mob), "long description");
  check_mobile_string(mvnum, &GET_DDESC(mob), "detailed description");
  check_mobile_string(mvnum, &GET_ALIAS(mob), "alias list");
  check_mobile_string(mvnum, &GET_SDESC(mob), "short description");
}

void check_mobile_string(mob_vnum i, char **string, const char *dscr)
{
  if (*string == NULL || **string == '\0')
  {
    char smbuf[128];
    sprintf(smbuf, "GenOLC: Mob #%d has an invalid %s.", i, dscr);
    new_mudlog(BRF, LVL_GOD, TRUE, smbuf);
    if (*string)
      free(*string);
    *string = strdup("An undefined string.");
  }
}



