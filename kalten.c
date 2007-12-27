#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "house.h"
#include "screen.h"
#include "constants.h"
#include "oasis.h"
#include "dg_scripts.h"
#include "clan.h"
#include "damage.h"
#include "event.h"
#include "fight.h"
#include "kalten.h"

/* Romance Module Defines */
#define SAME_SEX_ALLOWED FALSE	/* True/False */

/* MatingMod Defines */
#define OUT_OF_WEDLOCK TRUE	/* True/False */
#define REWARD_ALLOWED TRUE	/* T/F */
#define HP_REWARD      10	/* All rewards are NUMERICAL */
#define MP_REWARD      10
#define STR_REWARD     0
#define CON_REWARD     1
#define INT_REWARD     0
#define WIS_REWARD     0
#define DEX_REWARD     0
#define ABORT_ALLOWED  TRUE	/* T/F */
#define NINE_MONTHS    6000	/* 6000 realtime minutes TO GO */
#define MONTHS_8        5333
#define MONTHS_7        4666	/* Note: These are MONTHS REMAINING */
#define MONTHS_6        4000
#define MONTHS_5        3333
#define MONTHS_4        2666
#define MONTHS_3        2000
#define MONTHS_2        1333
#define MONTH_1         666

/*   external vars  */
extern struct attack_hit_type attack_hit_text[];
extern char *class_abbrevs[];
extern char *race_abbrevs[];
extern struct time_info_data time_info;
extern int circle_shutdown, circle_reboot;

/* for chars */
extern const char *pc_class_types[];

/* external functions */

void hit_death_trap(CHAR_DATA *ch);
void do_objstat(struct char_data *ch, struct obj_data *j);
void do_sstat_objdump(char_data * ch, obj_data * j);
void script_stat_dump(char_data * ch, struct script_data *sc);
void affect_modify_ar(struct char_data *ch, byte loc, sbyte mod,
                      int bitv[], bool add);
void improve_skill(struct char_data *ch, int skill);
void dismount_char(struct char_data *ch);
int can_breathe_underwater(struct char_data *ch);
int has_space_suit(struct char_data *ch);
int has_sun_protection(struct char_data *ch);
int invalid_align(struct char_data *ch, struct obj_data *obj);
int zdelete_check(int zone);
void perform_remove(struct char_data *ch, int pos);

/* local functions */
int genpreg();
int crashcheck_alpha(struct char_data *ch, struct char_data *vict);



EVENT2(delayed_command)
{
  command_interpreter(CAUSER_CH, (char *) info);
  free((char *) info);
}



#define DUMP_FILE LIB_MISC"objects.dmp"

ACMD(do_objdump)
{
  struct obj_data *obj;
  int nr, item_type;
  char buf[MAX_INPUT_LENGTH];

  one_argument(argument, buf);

  if (!*buf)
  {
    send_to_char("Usage: objdump <item_type>\r\n"
                 "Where item_type is one of the following:\r\n"
                 "light          scroll          wand            staff           weapon\r\n"
                 "fireweapon     missile         treasure        armor           potion\r\n"
                 "worn           other           trash           trap            container\r\n"
                 "note           drinkcon        key             food            money\r\n"
                 "pen            boat            fountain        throw           grenade\r\n"
                 "bow            sling           crossbow        bolt            arrow\r\n"
                 "rock           vehicle         v_controls      v_hatch         v_window\r\n"
                 "portal         gun             ammo            wings           spacesuit\r\n"
                 "aqualung       climbable       poison_1        poison_2        poison_3\r\n"
                 "poison_4       antidote_1      antidote_2      antidote_3      descendable\r\n"
                 "portal_bush    portal_water    portal_hole\r\n"
                 "all = every object in game\r\n", ch);
    return;
  }


  if (is_abbrev(buf, "light"))
    item_type = ITEM_LIGHT;
  else if (is_abbrev(buf, "scroll"))
    item_type = ITEM_SCROLL;
  else if (is_abbrev(buf, "wand"))
    item_type = ITEM_WAND;
  else if (is_abbrev(buf, "staff"))
    item_type = ITEM_STAFF;
  else if (is_abbrev(buf, "weapon"))
    item_type = ITEM_WEAPON;
  else if (is_abbrev(buf, "fireweapon"))
    item_type = ITEM_FIREWEAPON;
  else if (is_abbrev(buf, "missile"))
    item_type = ITEM_MISSILE;
  else if (is_abbrev(buf, "treasure"))
    item_type = ITEM_TREASURE;
  else if (is_abbrev(buf, "armor"))
    item_type = ITEM_ARMOR;
  else if (is_abbrev(buf, "potion"))
    item_type = ITEM_POTION;
  else if (is_abbrev(buf, "worn"))
    item_type = ITEM_WORN;
  else if (is_abbrev(buf, "other"))
    item_type = ITEM_OTHER;
  else if (is_abbrev(buf, "trash"))
    item_type = ITEM_TRASH;
  else if (is_abbrev(buf, "trap"))
    item_type = ITEM_TRAP;
  else if (is_abbrev(buf, "container"))
    item_type = ITEM_CONTAINER;
  else if (is_abbrev(buf, "note"))
    item_type = ITEM_NOTE;
  else if (is_abbrev(buf, "drinkcon"))
    item_type = ITEM_DRINKCON;
  else if (is_abbrev(buf, "key"))
    item_type = ITEM_KEY;
  else if (is_abbrev(buf, "food"))
    item_type = ITEM_FOOD;
  else if (is_abbrev(buf, "money"))
    item_type = ITEM_MONEY;
  else if (is_abbrev(buf, "pen"))
    item_type = ITEM_PEN;
  else if (is_abbrev(buf, "boat"))
    item_type = ITEM_BOAT;
  else if (is_abbrev(buf, "fountain"))
    item_type = ITEM_FOUNTAIN;
  else if (is_abbrev(buf, "throw"))
    item_type = ITEM_THROW;
  else if (is_abbrev(buf, "grenade"))
    item_type = ITEM_GRENADE;
  else if (is_abbrev(buf, "bow"))
    item_type = ITEM_BOW;
  else if (is_abbrev(buf, "sling"))
    item_type = ITEM_SLING;
  else if (is_abbrev(buf, "crossbow"))
    item_type = ITEM_CROSSBOW;
  else if (is_abbrev(buf, "bolt"))
    item_type = ITEM_BOLT;
  else if (is_abbrev(buf, "arrow"))
    item_type = ITEM_ARROW;
  else if (is_abbrev(buf, "rock"))
    item_type = ITEM_ROCK;
  else if (is_abbrev(buf, "vehicle"))
    item_type = ITEM_VEHICLE;
  else if (is_abbrev(buf, "v_controls"))
    item_type = ITEM_V_CONTROLS;
  else if (is_abbrev(buf, "v_hatch"))
    item_type = ITEM_V_HATCH;
  else if (is_abbrev(buf, "v_window"))
    item_type = ITEM_V_WINDOW;
  else if (is_abbrev(buf, "portal"))
    item_type = ITEM_PORTAL;
  else if (is_abbrev(buf, "gun"))
    item_type = ITEM_GUN;
  else if (is_abbrev(buf, "ammo"))
    item_type = ITEM_AMMO;
  else if (is_abbrev(buf, "wings"))
    item_type = ITEM_WINGS;
  else if (is_abbrev(buf, "spacesuit"))
    item_type = ITEM_SPACESUIT;
  else if (is_abbrev(buf, "aqualung"))
    item_type = ITEM_AQUALUNG;
  else if (is_abbrev(buf, "climbable"))
    item_type = ITEM_CLIMBABLE;
  else if (is_abbrev(buf, "poison_1"))
    item_type = ITEM_POISON_1;
  else if (is_abbrev(buf, "poison_2"))
    item_type = ITEM_POISON_2;
  else if (is_abbrev(buf, "poison_3"))
    item_type = ITEM_POISON_3;
  else if (is_abbrev(buf, "poison_4"))
    item_type = ITEM_POISON_4;
  else if (is_abbrev(buf, "antidote_1"))
    item_type = ITEM_ANTIDOTE_1;
  else if (is_abbrev(buf, "antidote_2"))
    item_type = ITEM_ANTIDOTE_2;
  else if (is_abbrev(buf, "antidote_3"))
    item_type = ITEM_ANTIDOTE_3;
  else if (is_abbrev(buf, "descendable"))
    item_type = ITEM_DESCENDABLE;
  else if (is_abbrev(buf, "portal_bush"))
    item_type = ITEM_PORTAL_BUSH;
  else if (is_abbrev(buf, "portal_water"))
    item_type = ITEM_PORTAL_WATER;
  else if (is_abbrev(buf, "portal_hole"))
    item_type = ITEM_PORTAL_HOLE;
  else			// list all objects
    item_type = 0;

  for (nr = 0; nr <= top_of_objt; nr++)
  {
    if (item_type >= 1)
    {
      if ((obj = read_object(nr, REAL)) &&
          (GET_OBJ_TYPE(obj) == item_type))
      {
        do_objstat(ch, obj);
        extract_obj(obj);
      }
    }
    else
    {
      if ((obj = read_object(nr, REAL)))
      {
        do_objstat(ch, obj);
        extract_obj(obj);
      }
    }
  }

  send_to_char("Done.\r\n", ch);
}


void do_objstat(struct char_data *ch, struct obj_data *j)
{
  int i, found;
  obj_vnum vnum;
  FILE *fp;
  char buf[MAX_STRING_LENGTH];
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];

  fp = fopen(DUMP_FILE, "a+");

  if (!fp)
  {
    send_to_char("Can't open the file for writing.\r\n", ch);
    return;
  }

  vnum = GET_OBJ_VNUM(j);
  /*    if (GET_OBJ_VAL(j,1) == 15 || GET_OBJ_VAL(j,1) == 16 ||
  	GET_OBJ_VAL(j,1) == 28 || GET_OBJ_VAL(j,2) == 15 ||
  	GET_OBJ_VAL(j,2) == 16 || GET_OBJ_VAL(j,2) == 28 ||
  	GET_OBJ_VAL(j,3) == 15 || GET_OBJ_VAL(j,3) == 16 ||
  	GET_OBJ_VAL(j,3) == 28) { -- uncomment to pick heal potions */
  sprintf(buf, "\r\nName: '%s'\n",
          ((j->short_description) ? j->short_description : "<None>"));
  fprintf(fp, buf);
  sprinttype(GET_OBJ_TYPE(j), item_types, buf1, sizeof(buf1));
  sprintf(buf, "VNum: [%5d], Type: %s\n", vnum, buf1);
  fprintf(fp, buf);

  fprintf(fp, "Can be worn on: ");
  sprintbitarray(j->obj_flags.wear_flags, wear_bits, TW_ARRAY_MAX, buf, sizeof(buf1));
  strcat(buf, "\n");
  fprintf(fp, buf);

  fprintf(fp, "Set char bits : ");
  sprintbitarray(j->obj_flags.bitvector, affected_bits, AF_ARRAY_MAX,
                 buf, sizeof(buf1));
  strcat(buf, "\n");
  fprintf(fp, buf);

  fprintf(fp, "Extra flags   : ");
  sprintbitarray(GET_OBJ_EXTRA(j), extra_bits, EF_ARRAY_MAX, buf, sizeof(buf1));
  strcat(buf, "\n");
  fprintf(fp, buf);


  switch (GET_OBJ_TYPE(j))
  {
  case ITEM_LIGHT:
    if (GET_OBJ_VAL(j, 2) == -1)
      strlcpy(buf, "Hours left: Infinite", sizeof(buf));
    else
      snprintf(buf, sizeof(buf), "Hours left: [%d]", GET_OBJ_VAL(j, 2));
    break;
  case ITEM_SCROLL:
  case ITEM_POTION:
    sprintf(buf, "Spells: (Level %d) %s, %s, %s", GET_OBJ_VAL(j, 0),
            skill_name(GET_OBJ_VAL(j, 1)),
            skill_name(GET_OBJ_VAL(j, 2)),
            skill_name(GET_OBJ_VAL(j, 3)));
    break;
  case ITEM_WAND:
  case ITEM_STAFF:
    sprintf(buf, "Spell: %s at level %d, %d (of %d) charges remaining",
            skill_name(GET_OBJ_VAL(j, 3)), GET_OBJ_VAL(j, 0),
            GET_OBJ_VAL(j, 2), GET_OBJ_VAL(j, 1));
    break;
  case ITEM_WEAPON:
    sprintf(buf, "Todam: %dd%d, Message type: %d",
            GET_OBJ_VAL(j, 1), GET_OBJ_VAL(j, 2), GET_OBJ_VAL(j, 3));
    break;
  case ITEM_ARMOR:
    sprintf(buf, "AC-apply: [%d]", GET_OBJ_VAL(j, 0));
    break;
  case ITEM_TRAP:
    sprintf(buf, "Spell: %d, - Hitpoints: %d",
            GET_OBJ_VAL(j, 0), GET_OBJ_VAL(j, 1));
    break;
  case ITEM_CONTAINER:
    sprintbit(GET_OBJ_VAL(j, 1), container_bits, buf2, sizeof(buf2));
    sprintf(buf,
            "Weight capacity: %d, Lock Type: %s, Key Num: %d, Corpse: %s",
            GET_OBJ_VAL(j, 0), buf2, GET_OBJ_VAL(j, 2),
            YESNO(GET_OBJ_VAL(j, 3)));
    break;
  case ITEM_DRINKCON:
  case ITEM_FOUNTAIN:
    sprinttype(GET_OBJ_VAL(j, 2), drinks, buf2, sizeof(buf2));
    sprintf(buf,
            "Capacity: %d, Contains: %d, Poisoned: %s, Liquid: %s",
            GET_OBJ_VAL(j, 0), GET_OBJ_VAL(j, 1),
            YESNO(GET_OBJ_VAL(j, 3)), buf2);
    break;
  case ITEM_NOTE:
    sprintf(buf, "Tongue: %d", GET_OBJ_VAL(j, 0));
    break;
  case ITEM_KEY:
    strcpy(buf, "");
    break;
  case ITEM_FOOD:
    sprintf(buf, "Makes full: %d, Poisoned: %s", GET_OBJ_VAL(j, 0),
            YESNO(GET_OBJ_VAL(j, 3)));
    break;
  case ITEM_MONEY:
    sprintf(buf, "Coins: %d", GET_OBJ_VAL(j, 0));
    break;
  default:
    sprintf(buf, "Values 0-3: [%d] [%d] [%d] [%d]",
            GET_OBJ_VAL(j, 0), GET_OBJ_VAL(j, 1),
            GET_OBJ_VAL(j, 2), GET_OBJ_VAL(j, 3));
    break;
  }
  fprintf(fp, (strcat(buf, "\n")));

  found = 0;
  fprintf(fp, "Affections:");
  for (i = 0; i < MAX_OBJ_AFFECT; i++)
    if (j->affected[i].modifier)
    {
      sprinttype(j->affected[i].location, apply_types, buf2, sizeof(buf2));
      sprintf(buf, "%s %+d to %s", found++ ? "," : "",
              j->affected[i].modifier, buf2);
      fprintf(fp, buf);
    }
  if (!found)
    fprintf(fp, " None");

  fprintf(fp, "\r\n");

  fclose(fp);

  /* check the object for a script */
  //    do_sstat_objdump(ch, j);
  //  }
}

void do_sstat_objdump(char_data * ch, obj_data * j)
{
  FILE *fp;

  fp = fopen(DUMP_FILE, "a+");

  fprintf(fp, "Script information:\n");
  if (!SCRIPT(j))
  {
    fprintf(fp, "  None.\n");
    return;
  }

  fclose(fp);

  script_stat_dump(ch, SCRIPT(j));
}


/* general function to display stats on script sc */
void script_stat_dump(char_data * ch, struct script_data *sc)
{
  struct trig_var_data *tv;
  trig_data *t;
  char name[MAX_INPUT_LENGTH];
  char buf[MAX_STRING_LENGTH];
  char buf1[MAX_STRING_LENGTH];
  char namebuf[512];
  FILE *fp;

  fp = fopen(DUMP_FILE, "a+");

  sprintf(buf, "Global Variables: %s\n", sc->global_vars ? "" : "None");
  fprintf(fp, buf);
  sprintf(buf, "Global context: %ld\n", sc->context);
  fprintf(fp, buf);

  for (tv = sc->global_vars; tv; tv = tv->next)
  {
    sprintf(namebuf, "%s:%ld", tv->name, tv->context);
    if (*(tv->value) == UID_CHAR)
    {
      find_uid_name(tv->value, name, sizeof(name));
      sprintf(buf, "    %20s:  %20s\n",
              tv->context ? namebuf : tv->name, name);
    }
    else
      sprintf(buf, "    %20s:  %20s\n",
              tv->context ? namebuf : tv->name, tv->value);
    fprintf(fp, buf);
  }

  for (t = TRIGGERS(sc); t; t = t->next)
  {
    sprintf(buf, "\n  Trigger: %s, VNum: [%5d], RNum: [%5d]\n",
            GET_TRIG_NAME(t), GET_TRIG_VNUM(t), GET_TRIG_RNUM(t));
    fprintf(fp, buf);

    if (t->attach_type == OBJ_TRIGGER)
    {
      fprintf(fp, "  Trigger Intended Assignment: Objects\n");
      sprintbit(GET_TRIG_TYPE(t), otrig_types, buf1, sizeof(buf1));
    }
    else if (t->attach_type == WLD_TRIGGER)
    {
      fprintf(fp, "  Trigger Intended Assignment: Rooms\n");
      sprintbit(GET_TRIG_TYPE(t), wtrig_types, buf1, sizeof(buf1));
    }
    else
    {
      fprintf(fp, "  Trigger Intended Assignment: Mobiles\n");
      sprintbit(GET_TRIG_TYPE(t), trig_types, buf1, sizeof(buf1));
    }

    sprintf(buf, "  Trigger Type: %s, Numeric Arg: %d, Arg list: %s\n",
            buf1, GET_TRIG_NARG(t),
            ((GET_TRIG_ARG(t) && *GET_TRIG_ARG(t)) ? GET_TRIG_ARG(t) :
             "None"));
    fprintf(fp, buf);
  }
  fclose(fp);
}

ACMD(do_linkload)
{
  struct char_data *victim = 0;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);

  if (!*arg)
  {
    send_to_char("Linkload who?\r\n", ch);
    return;
  }

  if (get_player_vis(ch, arg, NULL, 0))
  {
    send_to_char("They are already connected!\r\n", ch);
    return;
  }

  CREATE(victim, struct char_data, 1);
  clear_char(victim);
  CREATE(victim->player_specials, struct player_special_data, 1);
  if (GET_IDNUM(ch))
    victim->loader = GET_IDNUM(ch);
  if (load_char(arg, victim) > -1)
  {
    if (GET_LEVEL(victim) < GET_LEVEL(ch))
    {
      new_mudlog( BRF, GET_LEVEL(ch) + 1, TRUE, "(GC) %s has link-loaded %s.", GET_NAME(ch), GET_NAME(victim));
      add_char_to_list(victim);
      victim->desc = NULL;
      char_to_room(victim, IN_ROOM(ch));
      Crash_load(victim);
      act("You linkload $N.", FALSE, ch, 0, victim, TO_CHAR);
      act("$n linkloads $N.", FALSE, ch, 0, victim, TO_NOTVICT);
      victim->loader = NOBODY;
    }
    else
    {
      send_to_char
      ("Sorry, you aren't high enough to link-load that char.\r\n",
       ch);
      free_char(victim);
    }

  }
  else
  {
    send_to_char("No such player exists.\r\n", ch);
    free_char(victim);

  }

  return;
}

ACMD(do_find)
{
  struct obj_data *obj;
  int nr;

  for (nr = 0; nr <= top_of_objt; nr++)
  {
    if ((obj = read_object(nr, REAL)) &&
        (IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_ARTIFACT)))
    {
      new_send_to_char(ch, "QIC Flag on %s, vnum: %d.\r\n",
                       obj->short_description, GET_OBJ_VNUM(obj));
      
    }
    if (obj) free_obj(obj, FALSE);
  }
}

ACMD(do_reload)
{
  struct obj_data *weapon;
  struct obj_data *ammo;
  struct obj_data *ammo_next;

  if (IS_NPC(ch))
    return;

  if (!(weapon = GET_EQ(ch, WEAR_WIELD)))
  {
    send_to_char("You aren't even wielding a weapon.\r\n", ch);
    return;
  }

  if (GET_OBJ_TYPE(weapon) != ITEM_GUN)
  {
    send_to_char("You aren't wielding a reloadable weapon.\r\n", ch);
    return;
  }

  for (ammo = ch->carrying; ammo; ammo = ammo_next)
  {
    ammo_next = ammo->next_content;

    if (GET_OBJ_TYPE(ammo) == ITEM_AMMO
        && (GET_OBJ_VNUM(ammo) == GET_OBJ_VAL(weapon, 3)))
      break;
  }

  if (!ammo)
  {
    send_to_char("You do not have ammo for this weapon.\r\n", ch);
    return;
  }

  if (GET_OBJ_VAL(weapon, 2) >= GET_OBJ_VAL(weapon, 1))
  {
    new_send_to_char(ch, "%s is fully loaded.\r\n", weapon->short_description);
    return;
  }
  else
    GET_OBJ_VAL(weapon, 2) = GET_OBJ_VAL(weapon, 2) + 1;

  act("You get $p.", FALSE, ch, ammo, weapon, TO_CHAR);
  act("$n gets $p.", FALSE, ch, ammo, weapon, TO_ROOM);

  act("You load $P with $p.", FALSE, ch, ammo, weapon, TO_CHAR);
  act("$n loads $P with $p.", FALSE, ch, ammo, weapon, TO_ROOM);

  extract_obj(ammo);
  return;
}

/* -^- *** YORU'S ROMANCE MODULE     *** -^-
		** Romance For CircleMUD 3.0 **
		 *  Special Thanks to:       *
	 Rawther and Liran of AugMUD for their help! */
/* Extra Thanks to Philip Ames for help debugging V 0.92 */
/* Extra Thanks to Brian Finley for help debugging V 0.9/0.92.2b */

void namesave(struct char_data *ch, struct char_data *vict)
{
  /* Saves the names in PARTNER() */
  /* Now we're set! Save it! */

  if (!IS_NPC(ch)) SET_BIT_AR(PLR_FLAGS(ch), PLR_CRASH);
  if (!IS_NPC(vict)) SET_BIT_AR(PLR_FLAGS(vict), PLR_CRASH);
}

void changesave(struct char_data *ch, struct char_data *vict)
{
  /* Saves changes after rejection */
  namesave(ch, vict);
}

int check_samesex(struct char_data *ch, struct char_data *victim)
{
  /*Checks if it's a same-sex proposition/marriage */
  /* Then checks if SAME_SEX_ALLOWED is TRUE. */
  /* If the proposition is same-sex, and SAME_SEX_ALLOWED */
  /* is FALSE, then it returns 1, which will halt the procedure. */
  /* Otherwise, if SAME_SEX_ALLOWED is TRUE or the proposition is */
  /* NOT same-sex, it returns 0, and things continue as normal */
  if ((GET_SEX(ch) == GET_SEX(victim)) && (SAME_SEX_ALLOWED == FALSE))
  {
    return (1);
  }
  else
  {
    return (0);
  }
}

/* Romance Module -- Ask Someone Out */
ACMD(do_askout)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];

  one_argument(argument, arg);
  if (!*arg)
  {		/* What, ask no one out? */
    send_to_char("Uh, whom do you wish to ask out?\r\n", ch);
    return;
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    /* Is that person here? Nope! */
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return;
  }

  if (ROMANCE(ch) != 0)
  {	/* Are you already involved? */
    send_to_char("Sorry, you're already romantically involved!\r\n",
                 ch);
    return;
  }
  else if (crashcheck_alpha(victim, ch) == 1)
  {
    return;
  }
  else if (ROMANCE(victim) != 0)
  {	/* Is the person you're propositioning to involved? */
    send_to_char
    ("Try propositioning someone who ISN'T already romantically involved!\r\n",
     ch);
    return;
  }
  else if (victim == ch)
  {
    /* Ask yourself out?!? */
    send_to_char("Sorry, but you cannot ask yourself out!\r\n", ch);
    return;
    //    } //else if (check_samesex(ch, victim) == 1) {
    /* Check if it's same-sex, and if same-sex is allowed. */
    //send_to_char("Sorry, same-sex relations are not allowed here.\r\n", ch);
    //return;
  }
  else if (ROMANCE(ch) == -3)
  {
    send_to_char
    ("Sorry, you have to turn romance on before asking someone out.\r\n",
     ch);
    return;
  }
  else if (ROMANCE(victim) == -3)
  {
    send_to_char
    ("Sorry, they've got romance turned off for the moment..\r\n",
     ch);
    return;
  }
  else
  {			/* Okay, now we do that actual asking out.. */
    act("You ask $N out...\r\n", TRUE, ch, 0, victim, TO_CHAR);
    act("$n is asking you out!\r\n", TRUE, ch, 0, victim, TO_VICT);
    act("$n asks $N out on a date!\r\n", TRUE, ch, 0, victim,
        TO_NOTVICT);
    ROMANCE(victim) = ASKED_OUT;	/* Now they're being asked out. */
    ROMANCE(ch) = ASKING;	/* Temporarily declare the person as being asked out.. */
    /* NOTE: PARTNER(ch) must be reset in DO_REJECT */
    /* At this point, the Victim can either ACCEPT or REJECT the proposition. */
  }
}

/* Romance Module -- Accept a Proposition */

ACMD(do_accept)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];
  if (ROMANCE(ch) < ASKED_OUT)
  {	/* You're not being asked out.. */
    send_to_char("You're not being romantically propositioned..\r\n",
                 ch);
    return;
  }
  else
  {			/* Okay, you've been asked out or proposed to.. */
    one_argument(argument, arg);
    if (!*arg)
    {		/* Accept no one? */
      send_to_char("Who do you want to accept?\r\n", ch);
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {	/* Accept yourself? */
      send_to_char("You can't accept yourself as a partner!\r\n",
                   ch);
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) < ASKING)
    {	/* Are they propositioning as well? */
      send_to_char("But they're not asking you out!\r\n", ch);
      return;
      //        } //else if(check_samesex(ch, victim) == 1) {
      /* Check for same-sex relations.. */
      //send_to_char("Sorry, same-sex relationships are not allowed here.\r\n", ch);
      /* Okay, they've been corrected. The tick-timer will expire the
         ROMANCE factor on it's own. */
      //return;
    }
    else
    {		/* Okay, all the tests pass.. */
      if ((ROMANCE(ch) == ASKED_OUT) && (ROMANCE(victim) == ASKING))
      {
        /* For dating.. */
        act("You agree to date $N!\r\n", TRUE, ch, 0, victim,
            TO_CHAR);
        act("$n has agreed to date you!\r\n", TRUE, ch, 0, victim,
            TO_VICT);
        act("$n agrees to date $N!\r\n", TRUE, ch, 0, victim,
            TO_NOTVICT);
        /* We've notified them, now change the variables. */

        PARTNER(ch) = GET_IDNUM(victim);
        PARTNER(victim) = GET_IDNUM(ch);
        ROMANCE(ch) = 1;
        ROMANCE(victim) = 1;
        /* Now they're set as partners, and they've been set at Dating. */
        namesave(ch, victim);	// Reorient PARTNER()s
        return;
      }
      else if ((ROMANCE(ch) == PROPOSED_TO)
               && (ROMANCE(victim) == ASKING))
      {
        /* For engagement.. */
        act("You agree to marry $N!\r\n", TRUE, ch, 0, victim,
            TO_CHAR);
        act("$n has agreed to marry you!\r\n", TRUE, ch, 0, victim,
            TO_VICT);
        act("$n agrees to marry $N!\r\n", TRUE, ch, 0, victim,
            TO_NOTVICT);
        /* We've done the notification, now change the variables. */

        PARTNER(ch) = GET_IDNUM(victim);
        PARTNER(victim) = GET_IDNUM(ch);
        ROMANCE(ch) = 2;
        ROMANCE(victim) = 2;
        /* Now they're engaged and partnered.. */
        namesave(ch, victim);	// Reorient PARTNER()s
        return;		/* Get the hell outta this command. */
      }
      else
      {		/* They're not propositioning you after all. */
        send_to_char("That person isn't propositioning you..\r\n",
                     ch);
        return;
      }
    }
  }
}

/* Romance Module -- Reject A Proposition */
ACMD(do_reject)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH+1];
  if (ROMANCE(ch) < ASKED_OUT)
  {	/* You're not being asked out.. */
    send_to_char("You're not being romantically propositioned..\r\n",
                 ch);
    return;
  }
  else
  {			/* Okay, you've been asked out or proposed to.. */
    one_argument(argument, arg);
    if (!*arg)
    {		/* Reject no one? */
      send_to_char("Whose heart do you wish to break?\r\n", ch);
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {	/* Reject yourself? */
      send_to_char("You can't reject yourself!\r\n", ch);
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) < ASKING)
    {	/* Are they propositioning as well? */
      send_to_char("But they're not asking you out!\r\n", ch);
      return;
      //       } //else if(check_samesex(ch, victim) == 1) {
      /* Uh, they couldn't have asked you out. */
      //send_to_char("Sorry, but that person isn't propositioning you.\r\n", ch);
      //return;
    }
    else
    {		/* Okay, all the tests pass.. */
      if ((ROMANCE(ch) == ASKED_OUT) && (ROMANCE(victim) == ASKING))
      {
        /* For dating.. */
        act("You tell $N that you'd rather just be friends.\r\n",
            TRUE, ch, 0, victim, TO_CHAR);
        act("$n would rather just be 'friends', but you feel your heart shatter.\r\n", TRUE, ch, 0, victim, TO_VICT);
        act("$n rejects $N! How cold!\r\n", TRUE, ch, 0, victim,
            TO_NOTVICT);
        PARTNER(victim) = 0;
        PARTNER(ch) = 0;
        ROMANCE(victim) = 0;
        ROMANCE(ch) = 0;
        changesave(ch, victim);
        return;
      }
      else if ((ROMANCE(ch) == PROPOSED_TO)
               && (ROMANCE(victim) == ASKING))
      {
        /* For marriage proposals... */
        send_to_char
        ("You decide you'd rather not get married just yet.\r\n",
         ch);
        act("$n doesn't want to get married just yet..\r\n", TRUE,
            ch, 0, victim, TO_VICT);
        act("$n hands $N's ring back to $S.\r\n", TRUE, ch, 0,
            victim, TO_NOTVICT);
        PARTNER(ch) = GET_IDNUM(victim);
        PARTNER(victim) = GET_IDNUM(ch);
        /* Just in case. */
        ROMANCE(victim) = 1;
        ROMANCE(ch) = 1;	/* Back to "Dating" status. */
        namesave(ch, victim);
        return;
      }
      else
      {		/* Oops! They're not asking you after all! */
        send_to_char("But they're not propositioning you!\r\n",
                     ch);
        return;
      }
    }
  }
}

/* Romance Module -- Propose Marriage */
ACMD(do_propose)
{
  /* To propose to someone. */
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];

  switch (ROMANCE(ch))
  {
  default:
    new_send_to_char(ch, "Error!!\r\n");
    return;
  case 1:
    //success:
    break;
  case 2:
    new_send_to_char(ch, "But you're already engaged!\r\n");
    return;
  case 3:
    new_send_to_char(ch,
                     "But you're married already! %s wouldn't approve!\r\n", get_name_by_id(PARTNER(ch)));
    return;
  case 4:
    new_send_to_char(ch, "But you're being asked out! That would be rude!\r\n");
    return;
  case 5:
    new_send_to_char(ch, "But you're already proposing!\r\n");
    return;
  }

  if (ROMANCE(ch) == ASKING)
  {	/*Asking someone out? */
    new_send_to_char(ch, "But you're asking someone else!\r\n");
    return;

  }
  else
  {			/* Okay, YOU pass... */
    one_argument(argument, arg);
    if (!*arg)
    {		/* Propose to no one? */
      new_send_to_char(ch,"Whom do you want to propose to?\r\n");
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {	/* Propose to yourself? */
      new_send_to_char(ch,"You can't propose to yourself!\r\n");
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) < 1)
    {	/* Are they already dating? */
      new_send_to_char(ch, "But they're not dating anyone!\r\n");
      return;
    }
    else if (ROMANCE(victim) == 2)
    {	/* Are they already engaged? */
      new_send_to_char(ch, "But they're already engaged to %s!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if (ROMANCE(victim) == 3)
    {	/* Are they already married? */
      new_send_to_char(ch,
                       "But they're married already! %s wouldn't approve!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if (ROMANCE(victim) == 4)
    {	/* Are they being asked out? */
      new_send_to_char(ch, "But they're being asked out! That would be rude!\r\n");
      return;
    }
    else if (ROMANCE(victim) == 5)
    {	/* Are they already proposing? */
      new_send_to_char(ch,"But they're already proposing!\r\n");
      return;
    }
    else if (ROMANCE(victim) == 6)
    {	/*Asking someone? */
      new_send_to_char(ch,"But they're already asking someone else!\r\n");
      return;
    }
    else if (ROMANCE(victim) > 6)
    {	/* Any errors in the module? */
      new_send_to_char(ch, "ERROR IN ROMANCE MODULE: Romance Factor > 5!\r\n");
      return;
    }
    /* Okay, we've established you're both dating someone.. */
    else if (PARTNER(ch) != GET_IDNUM(victim))
    {
      /* But are you dating them? */
      new_send_to_char(ch, "But you're not dating %s!\r\n",   GET_NAME(victim));
      return;
    }
    else if (PARTNER(victim) != GET_IDNUM(ch))
    {
      /* Are they dating you? We shouldn't need this, though.. */
      new_send_to_char(ch, "But %s isn't dating you!\r\n", GET_NAME(victim));
      return;
    }
    /* Okay, you're dating each other, now we can get on with it! */
    else
    {
      act("You kneel in front of $N and ask for $S hand in marriage!\r\n", TRUE, ch, 0, victim, TO_CHAR);
      act("$n kneels before you and asks for your hand in marriage!\r\n", TRUE, ch, 0, victim, TO_VICT);
      act("$n kneels before $N, asking for $S hand in marriage!\r\n",
          TRUE, ch, 0, victim, TO_NOTVICT);
      /* We've informed the world, now change the vars.. */
      ROMANCE(ch) = ASKING;
      ROMANCE(victim) = PROPOSED_TO;
      /* Now all the partner has to do is accept or reject. */
      return;
    }
  }
}

/* Romance Module: Break up */

ACMD(do_breakup)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];
  /* First, standard checks: Are you in a relationship? */
  if (ROMANCE(ch) == 0)
  {
    send_to_char("But you're not romantically involved!\r\n", ch);
    return;
  }
  else if (ROMANCE(ch) == 3)
  {
    new_send_to_char(ch,
                     "But you're already married! You have to DIVORCE %s!\r\n",
                     get_name_by_id(PARTNER(ch)));
    return;
  }
  else
  {
    /* Okay, you're involved.. */
    one_argument(argument, arg);
    if (!*arg)
    {		/* Break up with noone? */
      send_to_char("Whom do you want to break up with?\r\n", ch);
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {	/* Break up with yourself? */
      send_to_char("You can't break up with yourself!\r\n", ch);
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) == 0)
    {
      send_to_char("But they're not romantically involved!\r\n", ch);
      return;
    }
    else if ((ROMANCE(victim) == 1)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      new_send_to_char(ch, "But they're dating %s, not you!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 2)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      new_send_to_char(ch, "But they're engaged to %s, not you!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 3)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      new_send_to_char(ch, "But they're married to %s, not you!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 3)
             && (PARTNER(victim) == GET_IDNUM(ch)))
    {
      new_send_to_char(ch,
                       "They're already married to you! You have to DIVORCE %s!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if (PARTNER(victim) != GET_IDNUM(ch))
    {
      new_send_to_char(ch, "But %s isn't involved with you!\r\n",
                       GET_NAME(victim));
      return;
    }
    else
    {
      /* Okay, they're involved and with you... */
      /* Now we break them up! How FUN! */
      if ((ROMANCE(ch) == 1) && (ROMANCE(victim) == 1))
      {
        /* For dating */
        act("You inform $N that you will no longer date $M!\r\n",
            TRUE, ch, 0, victim, TO_CHAR);
        act("$n dumps you, tearing your heart out in the process!\r\n", TRUE, ch, 0, victim, TO_VICT);
        act("$n sends $s relationship with $N to Splitsville!\r\n",
            TRUE, ch, 0, victim, TO_NOTVICT);
        /* Now set the variables to 0 */
        PARTNER(ch) = 0;
        PARTNER(victim) = 0;
        ROMANCE(ch) = 0;
        ROMANCE(victim) = 0;
        namesave(ch, victim);
        /* Done. You've dumped them. */
        return;
      }
      else if ((ROMANCE(ch) == 2) && (ROMANCE(victim) == 2))
      {
        /* For engagements */
        act("You call off your wedding with $N.\r\n", TRUE, ch, 0,
            victim, TO_CHAR);
        act("$n doesn't want to marry you anymore! The wedding's off!\r\n", TRUE, ch, 0, victim, TO_VICT);
        act("$n nullifies $s engagement with $N! The wedding's off!\r\n", TRUE, ch, 0, victim, TO_NOTVICT);
        /* Now set the variables to 0 */
        PARTNER(ch) = 0;
        PARTNER(victim) = 0;
        ROMANCE(ch) = 0;
        ROMANCE(victim) = 0;
        namesave(ch, victim);
        /* Done. You've dumped them. */
        return;
      }
      else if ((ROMANCE(ch) == 4) && (ROMANCE(victim) == 4))
      {
        /* For dating/askouts */
        act("You inform $N that you no longer wish to date $S!\r\n", TRUE, ch, 0, victim, TO_CHAR);
        act("$n doesn't feel like dating you anymore!\r\n", TRUE,
            ch, 0, victim, TO_VICT);
        act("$n decides not to date $N.\r\n", TRUE, ch, 0, victim,
            TO_NOTVICT);
        /* Now set the variables to 0 */
        PARTNER(ch) = 0;
        PARTNER(victim) = 0;
        ROMANCE(ch) = 0;
        ROMANCE(victim) = 0;
        namesave(ch, victim);
        /* Done. You've dumped them. */
        return;
      }
      else if ((ROMANCE(ch) == 5) && (ROMANCE(victim) == 5))
      {
        /* For engagements/proposals */
        act("You cancel your wedding proposal to $N.\r\n", TRUE,
            ch, 0, victim, TO_CHAR);
        act("$n doesn't want to marry you anymore!\r\n", TRUE, ch,
            0, victim, TO_VICT);
        act("$n nullifies $s engagement proposal with $N!\r\n",
            TRUE, ch, 0, victim, TO_NOTVICT);
        /* Now set the variables to back to Dating */
        /* This is the only exception. You can change it to cancel
           Dating status as well. */
        ROMANCE(ch) = 1;
        ROMANCE(victim) = 1;
        /* Done. You've cancelled your proposal them. */
        return;
      }
      else
      {		/* Guess you're not involved after all. */
        new_send_to_char(ch, "But you're not involved with %s!",
                         GET_NAME(victim));
        return;
      }
    }
  }
}

/* Function for the actual marriage */
void marry_them(struct char_data *ch, struct char_data *victim,
                struct char_data *imm)
{
  char buf[MAX_STRING_LENGTH];
  /* Do standard checks.. */
  if (crashcheck_alpha(victim, imm) == 1)
  {
    return;
  }
  else if (crashcheck_alpha(ch, imm) == 1)
  {
    return;
  }
  if (ROMANCE(ch) != 2)
  {
    /* Groom isn't engaged */
    new_send_to_char(imm,"But %s isn't engaged!\r\n", GET_NAME(ch));
    return;
  }
  else if (ROMANCE(victim) != 2)
  {
    /* Bride isn't engaged */
    new_send_to_char(imm, "But %s isn't engaged!\r\n", GET_NAME(victim));
    return;
  }
  else if (PARTNER(ch) != GET_IDNUM(victim))
  {
    /* Not engaged to each other */
    send_to_char("But they're not engaged to each other!\r\n", imm);
    return;
  }
  else if (PARTNER(victim) != GET_IDNUM(ch))
  {
    /* Not engaged to each other */
    send_to_char("But they're not engaged to each other!\r\n", imm);
    return;
  }
  else if (check_samesex(ch, victim) == 1)
  {
    /* Same Sex Marriages? */
    send_to_char("Same-sex marriages are not allowed.\r\n", imm);
    return;
  }
  else
  {
    if (GET_SEX(ch) != GET_SEX(victim))
    {
      /* Regular Marriage */
      /* They're engaged to each other, now perform the marriage. */
      new_send_to_char(ch, "%s declares you married to %s!\r\n",   GET_NAME(imm), get_name_by_id(PARTNER(ch)));
      new_send_to_char(victim, "%s declares you married to %s!\r\n",   GET_NAME(imm), get_name_by_id(PARTNER(victim)));
      new_send_to_char(imm, "You declare %s and %s man and wife!\r\n",   GET_NAME(ch),GET_NAME(victim));
      snprintf(buf, sizeof(buf), "%s declares %s and %s man and wife!\r\n",
               GET_NAME(imm), GET_NAME(ch), GET_NAME(victim));
      act(buf, TRUE, 0, 0, 0, TO_ROOM);
    }
    else
    {		/* Same-sex Marriage */
      new_send_to_char(ch, "%s declares you married to %s!\r\n",
                       GET_NAME(imm), get_name_by_id(PARTNER(ch)));
      new_send_to_char(victim, "%s declares you married to %s!\r\n",
                       GET_NAME(imm), get_name_by_id(PARTNER(victim)));
      new_send_to_char(imm, "You declare %s and %s married!\r\n",
                       GET_NAME(ch), GET_NAME(victim));
      snprintf(buf, sizeof(buf), "%s declares %s and %s married!\r\n",
               GET_NAME(imm), GET_NAME(ch), GET_NAME(victim));
      act(buf, TRUE, 0, 0, 0, TO_NOTVICT);
    }
    PARTNER(ch) = GET_IDNUM(victim);
    PARTNER(victim) = GET_IDNUM(ch);
    /* Just in case! */
    ROMANCE(ch) = 3;
    ROMANCE(victim) = 3;
    /* Now we're set! Save it! */
    namesave(ch, victim);
    return;
  }
}

/* Romance Module -- Marry Two People */
/* THIS SHOULD BE AN IMM-LEVEL COMMAND! */

ACMD(do_marry)
{
  struct char_data *groom;
  struct char_data *bride;
  char groom_name[MAX_INPUT_LENGTH];
  char bride_name[MAX_INPUT_LENGTH];
  argument = one_argument(argument, groom_name);
  one_argument(argument, bride_name);
  if (!(groom = get_char_room_vis(ch, groom_name, NULL)))
  {
    /* Are they here? No! */
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return;
  }
  else if (!(bride = get_char_room_vis(ch, bride_name, NULL)))
  {
    /* Are they here? No! */
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return;
  }
  if (groom_name == bride_name)
  {
    /* Groom is the Bride? */
    send_to_char("You can't marry someone to themself!\r\n", ch);
    return;
  }
  else if (groom == ch)
  {
    /* Can't perform a ceremony on yourself. */
    send_to_char
    ("Sorry, you have to get someone ELSE to perform the ceremony!\r\n",
     ch);
    return;
  }
  else if (bride == ch)
  {
    /* Can't perform a ceremony on yourself. */
    send_to_char
    ("Sorry, you have to get someone ELSE to perform the ceremony!\r\n",
     ch);
    return;
  }
  else if ((!groom) || (!bride))
  {
    send_to_char("Which couple do you wish to marry?\r\n", ch);
    return;
  }
  else
  {			/* Let the marry function check the rest. */
    marry_them(groom, bride, ch);
  }
}

/* Romance Module - Divorce */

ACMD(do_divorce)
{
  struct char_data *victim;

  char arg[MAX_INPUT_LENGTH];
  /* First, standard checks: Are you in a relationship? */
  if (ROMANCE(ch) != 3)
  {
    send_to_char("But you're not married!\r\n", ch);
    return;
  }
  else
  {
    /* Okay, you're involved.. */
    one_argument(argument, arg);
    if (!*arg)
    {		/* Break up with noone? */
      send_to_char("Whom do you want to divorce?\r\n", ch);
      return;
    }
    else if (!(victim = get_char_room_vis(ch, arg, NULL)))
    {
      /* Are they here? No! */
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {	/* Break up with yourself? */
      send_to_char("You can't divorce yourself!\r\n", ch);
      return;
    }
    else if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    else if (ROMANCE(victim) == 0)
    {
      send_to_char("But they're not romantically involved!\r\n", ch);
      return;
    }
    else if ((ROMANCE(victim) == 1)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      new_send_to_char(ch, "But they're dating %s, not you!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 2)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      new_send_to_char(ch,"But they're engaged to %s, not you!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if ((ROMANCE(victim) == 3)
             && (PARTNER(victim) != GET_IDNUM(ch)))
    {
      new_send_to_char(ch, "But they're married to %s, not you!\r\n",
                       get_name_by_id(PARTNER(victim)));
      return;
    }
    else if (PARTNER(victim) != GET_IDNUM(ch))
    {
      new_send_to_char(ch, "But %s isn't involved with you!\r\n",
                       GET_NAME(victim));
      return;
    }
    else
    {
      /* Okay, they're involved and with you... */
      /* Now we break them up! How FUN! */
      act("You yank $N's wedding ring off and throw it on the floor!\r\n", TRUE, ch, 0, victim, TO_CHAR);
      act("$n yanks $s wedding ring off and throws it on the floor!\r\n", TRUE, ch, 0, victim, TO_VICT);
      act("$n yanks $s wedding ring from $N off and throws it on the floor! DIVORCE!\r\n", TRUE, ch, 0, victim, TO_NOTVICT);
      PARTNER(ch) = 0;
      PARTNER(victim) = 0;
      ROMANCE(ch) = 0;
      ROMANCE(victim) = 0;
      /* It's all reset, save changes. */
      changesave(ch, victim);
    }
  }
}

int crashcheck_alpha(struct char_data *ch, struct char_data *vict)
{
  /* Just some crash-preventing checks. */
  /* Returns 1 to indicate a crash will occur. */
  /* Return 0 if everything's ok */
  if (IN_ROOM(ch) == NULL)
  {
    return 1;
  }
  else if (IS_NPC(ch))
  {
    send_to_char("You have to do that to a player!\r\n", vict);
    return 1;
  }
  else if (IS_NPC(vict))
  {
    send_to_char("You can't do that!\r\n", vict);
    return 1;
  }
  else if (GET_POS(ch) < POS_RESTING)
  {
    send_to_char("You can't do that now!\r\n", vict);
    return 1;
  }
  else
  {
    return 0;
  }
}

/* MatingMod ACMDs and functions */
/* MATINGMOD v0.93 by Yoru-Hikage */
/* Special Thanks to Rawther (formerly) of AugMUD for his help */

/* First command, to conceive */
ACMD(do_seduce)
{
  struct char_data *victim;

  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);
  victim = get_char_room_vis(ch, arg, NULL);
  if (PREG(ch) < 0)
  {

    if (!*arg)
    {		/* Mate with noone? */
      send_to_char("Whom do you want to mate with?\r\n", ch);
      return;
    }
    else if (!(victim))
    {
      /* Are they here? No! */
      new_send_to_char(ch, "%s", CONFIG_NOPERSON);
      return;
    }
    else if (victim == ch)
    {	/* Mate with yourself? */
      send_to_char("You can't mate with yourself!\r\n", ch);
      return;
    }
    if (GET_SEX(ch) == SEX_MALE)
    {
      send_to_char("Hey, only women can have kids..\r\n", ch);
      return;
    }
    else if (GET_SEX(victim) == SEX_FEMALE)
    {
      send_to_char("You need a male partner to conceive.\r\n", ch);
      return;
    }
    if ((ROMANCE(ch) != 3) && (OUT_OF_WEDLOCK == FALSE))
    {
      send_to_char
      ("Sorry, children may not be conceived out of wedlock here.\r\n",
       ch);
      return;
    }
    if (crashcheck_alpha(victim, ch) == 1)
    {
      return;
    }
    if (GET_POS(ch) == POS_FIGHTING)
    {
      send_to_char
      ("You can't do that while fighting! It ruins the mood!\r\n",
       ch);
      return;
    }
    if (GET_POS(victim) == POS_FIGHTING)
    {
      send_to_char
      ("You have to wait until your mate finishes fighting!\r\n",
       ch);
      return;
    }
    if (GET_POS(ch) < POS_RESTING)
    {
      send_to_char("You can't do that now!\r\n", ch);
      return;
    }
    if (PREG(ch) == CANT)
    {
      send_to_char
      ("Sorry, you have to turn mating on to be able to mate.\r\n",
       ch);
      return;
    }
    if (PREG(victim) == CANT)
    {
      send_to_char("Sorry, they have turned mating off.\r\n", ch);
      return;
    }
    /* Routine Checks Performed */
    act("You ask $N if $E wishes to have a child with you.\r\n", TRUE,
        ch, 0, victim, TO_CHAR);
    act("$n asks if you want to have a child with $m.\r\n", TRUE, ch,
        0, victim, TO_VICT);
    PREG(ch) = ASKING_P;
    /* Players involved are informed, female is set to ASKING */
  }
  else
  {
    send_to_char("You're already pregnant!!\r\n", ch);
    return;
  }
}

/* For Men - Agree */
ACMD(do_consent)
{
  struct char_data *victim;

  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);

  if (!*arg)
  {		/* Break up with noone? */
    send_to_char("Whom do you want to mate with?\r\n", ch);
    return;
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    /* Are they here? No! */
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return;
  }
  else if (victim == ch)
  {	/* Break up with yourself? */
    send_to_char("You can't agree to mate with yourself!\r\n", ch);
    return;
  }
  if (crashcheck_alpha(victim, ch) == 1)
  {
    return;
  }
  if (GET_POS(ch) == POS_FIGHTING)
  {
    send_to_char
    ("You can't do that while fighting! It ruins the mood!\r\n",
     ch);
    return;
  }
  if (GET_POS(victim) == POS_FIGHTING)
  {
    send_to_char
    ("You have to wait until your mate finishes fighting!\r\n",
     ch);
    return;
  }

  if (GET_SEX(victim) == SEX_MALE)
  {
    send_to_char("You need a female partner to mate with.\r\n", ch);
    return;
  }
  else if (GET_SEX(ch) == SEX_FEMALE)
  {
    send_to_char
    ("Perhaps you should be asking to mate instead of agreeing?\r\n",
     ch);
    return;
  }
  if ((ROMANCE(ch) != 3) && (OUT_OF_WEDLOCK == FALSE))
  {
    send_to_char
    ("Sorry, children may not be conceived out of wedlock here.\r\n",
     ch);
    return;
  }
  if (GET_POS(ch) < POS_RESTING)
  {
    send_to_char("You can't do that now!\r\n", ch);
    return;
  }
  if (PREG(victim) != -3)
  {
    send_to_char("But she's not asking to mate with you!\r\n", ch);
    return;
  }
  /* Routine Checks Performed */
  act("You agree to have a child with $N!\r\n", TRUE, ch, 0, victim,
      TO_CHAR);
  act("$n agrees to have a kid with you!\r\n", TRUE, ch, 0, victim,
      TO_VICT);
  act("$N announces that she's having $n's baby!\r\n", TRUE, ch, 0,
      victim, TO_NOTVICT);
  PREG(victim) = genpreg();	/* Generate a pregnancy time */
  /* All set. */

}

/* Deny fatherhood */

ACMD(do_deny)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);
  if (!*arg)
  {		/* Break up with noone? */
    send_to_char("Whom do you want to deny to mate with?\r\n", ch);
    return;
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    /* Are they here? No! */
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return;
  }
  else if (victim == ch)
  {	/* Break up with yourself? */
    send_to_char("You can't mate with yourself anyway!\r\n", ch);
    return;
  }
  if (crashcheck_alpha(victim, ch) == 1)
  {
    return;
  }
  if (GET_POS(ch) == POS_FIGHTING)
  {
    send_to_char("You can't do that while fighting!\r\n", ch);
    return;
  }
  if (GET_POS(victim) == POS_FIGHTING)
  {
    send_to_char
    ("You have to wait until your mate finishes fighting!\r\n",
     ch);
    return;
  }

  else if (GET_SEX(victim) == SEX_MALE)
  {
    send_to_char("You can only mate with females...\r\n", ch);
    return;
  }
  else if (GET_SEX(ch) == SEX_FEMALE)
  {
    send_to_char
    ("Perhaps you should be asking to mate instead of denying?\r\n",
     ch);
    return;
  }
  if (PREG(victim) != -3)
  {
    send_to_char("But she's not asking to mate with you!\r\n", ch);
    return;
  }
  act("You inform $N that you don't wish to mate with her.\r\n", TRUE,
      ch, 0, victim, TO_CHAR);
  act("$n doesn't feel like mating just yet.\r\n", TRUE, ch, 0, victim,
      TO_VICT);
  PREG(victim) = -1;		/* Inform & Reset */
}

/* Generates a pregnancy time */
int genpreg()
{
  int time;
  int rand_time;
  time = (NINE_MONTHS);
  rand_time = dice(6, 6);	/* Max = 36 (One more mudmonth) Min = 6 (Less than one MudWeek) */
  time = time + rand_time + 2;
  return time;
}

/* Certain late-pregnancy symptoms.. */
void symptoms(struct char_data *ch)
{
  struct obj_data *obj;

  int contract = 0;
  if ((PREG(ch) < (MONTHS_5)) && (PREG(ch) > 0))
  {
    /* She's past month 5 */
    int check = 0;
    check = dice(1, 20);	/* Random Number, 1 - 20) */
    if (check > 10)
    {
      return;
    }			/* No symptoms */
    if (check >= 10 && check < 13)
    {
      act("You feel the baby kick! OW!\r\n", TRUE, ch, 0, 0,
          TO_CHAR);
      act("$n flinches in pain. Baby is kicking again!\r\n", TRUE,
          ch, 0, 0, TO_ROOM);
      damage(ch,ch,2, TYPE_UNDEFINED);
      return;
    }
    if (check >= 13 && check < 16)
    {
      act("You feel the baby kick! OW!\r\n", TRUE, ch, 0, 0,
          TO_CHAR);
      act("$n flinches in pain. Baby is kicking again!\r\n", TRUE,
          ch, 0, 0, TO_ROOM);
      damage(ch,ch, 1, TYPE_UNDEFINED);
      return;
    }
    if (check >= 15)
    {
      act("A wave of nausea washes over you, and you vomit.\r\n",
          TRUE, ch, 0, 0, TO_CHAR);
      act("$n suddenly turns very green, then stumbles into a corner and vomits up her breakfast.\r\n", TRUE, ch, 0, 0, TO_CHAR);
      gain_condition(ch, FULL, -2);
      gain_condition(ch, THIRST, -3);	/* Stomach's slightly emptier */
      return;
    }
  }
  if ((PREG(ch) <= (MONTH_1) && (PREG(ch) > 6)))
  {
    /* Near Birth  */
    contract = dice(2, 3);
    if (contract >= 4)
    {
      act("You double over as a contraction causes a wave of sheer pain to wash over your body!\r\n", TRUE, ch, 0, 0, TO_CHAR);
      act("$n groans and doubles over as a contraction shoots through her.\r\n", TRUE, ch, 0, 0, TO_ROOM);
      damage(ch,ch, contract, TYPE_UNDEFINED);
    }
  }
  if ((PREG(ch) <= 6) && (PREG(ch) > 0))
  {
    act("You scream as the pain from birthing burns in your blood...\r\n", TRUE, ch, 0, 0, TO_CHAR);
    act("$n screams loudly and howls in pain!\r\n", TRUE, ch, 0, 0,
        TO_ROOM);
  }
  if (PREG(ch) == 0)
  {
    /* Birthing */
    PREG(ch) = -1;
    send_to_all("A large, white stork flies high above you!\r\n");
    act("You are whisked away to a nearby hospital, and return moments later, carrying your new child!\r\n", TRUE, ch, 0, 0, TO_CHAR);
    act("$n is whisked away in an ambulance, and quickly returns, baby in hand.\r\n", TRUE, ch, 0, 0, TO_ROOM);

    //        act("A cart labelled 'Adventurer's Academy' arrives, and two uniformed men take your child from you.\r\n", TRUE, ch, 0, 0, TO_CHAR);
    //        act("A cart labelled 'Adventurer's Academy' arrives, and two uniformed men take $n's child.\r\n", TRUE, ch, 0, 0, TO_ROOM);
    obj = read_object(number(5, 6), VIRTUAL);
    obj_to_char(obj, ch);
    if (REWARD_ALLOWED == TRUE)
    {	/* Rewards? */
      GET_MAX_HIT(ch) += HP_REWARD;
      GET_MAX_MANA(ch) += MP_REWARD;
      GET_STR(ch) += STR_REWARD;
      GET_CON(ch) += CON_REWARD;
      GET_DEX(ch) += DEX_REWARD;
      GET_WIS(ch) += WIS_REWARD;
      GET_INT(ch) += INT_REWARD;
      affect_total(ch);
    }
    damage(ch,ch, 100, TYPE_UNDEFINED);
  }
}

/* ABORT Command - MatingMod -- IMMORTAL LEVEL */
ACMD(do_abort)
{
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];
  if (GET_LEVEL(ch) <= LVL_HERO)
  {
    send_to_char("Only Immortals can use this command!\r\n", ch);
    return;
  }
  if (ABORT_ALLOWED != TRUE)
  {
    send_to_char("Sorry, abortions are not allowed here!\r\n", ch);
    return;
  }
  one_argument(argument, arg);
  if (!*arg)
  {		/* Mate with noone? */
    send_to_char("Whose pregnancy do you want to abort?\r\n", ch);
    return;
  }
  else if (!(victim = get_char_room_vis(ch, arg, NULL)))
  {
    /* Are they here? No! */
    new_send_to_char(ch, "%s", CONFIG_NOPERSON);
    return;
  }
  if (PREG(victim) < 0)
  {
    new_send_to_char(ch, "But %s isn't pregnant!\r\n", GET_NAME(victim));
    return;
  }

  if (crashcheck_alpha(victim, ch) == 1)
  {
    return;
  }
  /* All checks pass.. */
  if (SELF(victim, ch))
  {
    act("You makes a mystical gesture towards your abdomen.\r\nAfter a moment of pain, it shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_CHAR);
    act("$n makes a mystical gesture towards $N's abdomen.\r\nIt shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_NOTVICT);
    PREG(ch) = NOT_PREG;
  }
  else
  {
    act("You make a mystical gesture towards $N's abdomen.\r\nIt shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_CHAR);
    act("$n makes a mystical gesture towards your abdomen.\r\nAfter a moment of pain, it shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_VICT);
    act("$n makes a mystical gesture towards $N's abdomen.\r\nIt shrinks back to normal.\r\n", TRUE, ch, 0, victim, TO_NOTVICT);
    PREG(victim) = NOT_PREG;
    /* Done. */

  }
}

ACMD(mate_toggle)
{
  /* Romance/Mating Toggle On/Off
   * Precondition: Person must be single + not pregnant
   * Postcondition: If the person wants to not be bothered with
   * M/R-mod thingies, then this toggle will prevent that.
   * If they did not and now want to be involved in the
   * social scene, then now they can.
   */
  if (ROMANCE(ch) > 0)
  {
    send_to_char("Sorry, you have to be single to turn mating off.\n",
                 ch);
  }
  if (PREG(ch) > -1)
  {
    send_to_char("Sorry, you cannot turn Mating off while pregnant.\n",
                 ch);
  }
  /* Both Conditions pass.. Check + Set! */
  if (ROMANCE(ch) == 0)
  {
    ROMANCE(ch) = -1;
  }				// OFF
  else if (ROMANCE(ch) == -1)
  {
    ROMANCE(ch) = 0;
  }				// Single
  if ((PREG(ch) == -1) || (PREG(ch) == -2))
  {
    PREG(ch) = CANT;	// OFF
  }
  else if (PREG(ch) == CANT)
  {
    if (GET_SEX(ch) == MALE)
    {
      PREG(ch) = -2;
    }
    else
    {
      PREG(ch) = -1;
    }
  }
}				/* EO mate_toggle */

bool check_time(void)
{
  if (time_info.hours > 6 && time_info.hours < 19)
    return TRUE;
  else
    return FALSE;
}

const char *wing_color(int align) {
if (align > 950)
return "shimmering silky white";
else if (align > 350)
return "silky white";
else if (align > 0)
return "pale grey";
else if (align >= -350)
return "dark grey";
else if (align >= 950)
return "sooty black";
else
return "crackling hazy black";
}


#define TEST(x) new_send_to_char(ch, "%d\r\n", (x));

int check_dam_affects(struct char_data *ch)
{
  struct obj_data *obj = NULL;
  char buf[MAX_STRING_LENGTH];


  if (AFF_FLAGGED(ch, AFF_SUFFOCATING))
  {
    if (damage(ch, ch, number(25, 50) + (GET_MAX_HIT(ch) * 0.01), SPELL_SUFFOCATE) == -1)
      return -1;
  }
  if (!MOB_FLAGGED(ch, MOB_NOPOISON))
  {
    if (AFF_FLAGGED(ch, AFF_POISON_1))
    {
      if (damage(ch, ch, 20 + (GET_MAX_HIT(ch) * 0.0001), SPELL_POISON) == -1)
      {
        return -1;
      }
      alter_mana(ch, GET_MANA(ch)/25.0);
      alter_move(ch, GET_MOVE(ch)/25.0);

    }


    if (AFF_FLAGGED(ch, AFF_POISON_2))
    {
      if (damage(ch, ch, 75 + (GET_MAX_HIT(ch) * 0.0002), SPELL_POISON) == -1)
      {
        return -1;
      }
      alter_mana(ch, GET_MANA(ch)/16.0);
      alter_move(ch, GET_MOVE(ch)/16.0);

    }



    if (AFF_FLAGGED(ch, AFF_POISON_3))
    {
      if (damage(ch, ch, 150 + (GET_MAX_HIT(ch) * 0.0003), SPELL_POISON) == -1)
        return -1;

      alter_mana(ch, GET_MANA(ch)/8.0);
      alter_move(ch, GET_MOVE(ch)/8.0);

    }


    if (AFF_FLAGGED(ch, AFF_POISON_4))
    {
      if (damage(ch, ch, 200 + (GET_MAX_HIT(ch) * 0.0004), SPELL_POISON) == -1)
        return -1;

      alter_mana(ch, (GET_MANA(ch)/6.0)+2);
      alter_move(ch, (GET_MOVE(ch)/6.0)+2);

    }
  }

  if ((obj = GET_EQ(ch, WEAR_WIELD_2))!=NULL)
  {
    if (GET_OBJ_WEIGHT(obj) > str_app[STRENGTH_APPLY_INDEX(ch)].wield_w)
    {
      new_send_to_char(ch,"Your weapon becomes too heavy for you.\r\n");
      perform_remove(ch, WEAR_WIELD_2);
    }
  }
  if ((obj = GET_EQ(ch, WEAR_WIELD))!=NULL)
  {
    if (GET_OBJ_WEIGHT(obj) > str_app[STRENGTH_APPLY_INDEX(ch)].wield_w)
    {
      new_send_to_char(ch, "Your weapon becomes too heavy for you.\r\n");
      perform_remove(ch, WEAR_WIELD);
    }
  }



  if (AFF_FLAGGED(ch, AFF_ACIDED))
    if (damage(ch, ch, 100, SPELL_ACID) == -1)
      return -1;

  if (AFF_FLAGGED(ch, AFF_FREEZING))
    if (damage(ch, ch, 100, SPELL_FREEZE) == -1)
      return -1;

  if (AFF_FLAGGED(ch, AFF_BURNING))
    if (damage(ch, ch, 100, SPELL_BURN) == -1)
      return -1;



  if ( AFF_FLAGGED(ch, AFF_FLY) && GET_POS(ch) == POS_STANDING)
  {
    if (number(1, 200) >195)
    {
      if (number(0, 1))
      {
        if (!IS_NPC(ch))
        {
          snprintf(buf,sizeof(buf), "You flap your %s wings and lift yourself a little higher.", wing_color(GET_ALIGNMENT(ch)));
          act(buf, FALSE, ch, 0, 0, TO_CHAR);
          snprintf(buf,sizeof(buf), "$n flaps $s %s wings and lifts $mself a little higher.",  wing_color(GET_ALIGNMENT(ch)));
          act(buf, FALSE, ch, 0, 0, TO_ROOM);
        }
        else
        {
          act("$n flaps $s wings and lifts $mself a little higher.", FALSE, ch, 0, 0, TO_ROOM);
        }
      }
      else
      {
        if (!IS_NPC(ch))
        {
          snprintf(buf, sizeof(buf),"You still your %s wings and swoop a little closer to the ground.",  wing_color(GET_ALIGNMENT(ch)));
          act(buf, FALSE, ch, 0, 0, TO_CHAR);
          snprintf(buf,sizeof(buf), "$n stills $s %s wings and swoops a little closer to the ground.",  wing_color(GET_ALIGNMENT(ch)));
          act(buf, FALSE, ch, 0, 0, TO_ROOM);
        }
        else
        {
          act("$n stills $s wings and swoops a little closer to the ground.", FALSE, ch, 0, 0, TO_ROOM);
        }
      }
    }

  }
  return 0;

}

void sector_update(void)
{
  struct char_data *i, *next_char;

  for (i = character_list; i != NULL; i = next_char)
  {
    next_char = i->next;

    if (check_dam_affects(i) == -1)
      continue;
    if ((IS_NPC(i) && !i->master))
      continue;
    if (PLR_FLAGGED(i, PLR_DYING) || DEAD(i))
      continue;
    if (!IS_NPC(i) && GET_LEVEL(i) > LVL_IMMORT)
      continue;
    if (GET_POS(i) >= POS_STUNNED)
    {
      if (SECT(i->in_room) == SECT_UNDERWATER &&
          !can_breathe_underwater(i))
        if (damage(i, i, number(10, 25), TYPE_UNDERWATER) == -1)
          continue;
      if (SECT(i->in_room) == SECT_SPACE && !has_space_suit(i))
        if (damage(i, i, number(25, 500), TYPE_SUFFERING) == -1)
          continue;
      if (SECT(i->in_room) == SECT_DESERT &&
          check_time() && !has_sun_protection(i))
        if (damage(i, i, number(1, 10), TYPE_DESERT) == -1)
          continue;
      if (GET_POS(i) <= POS_STUNNED)
        update_pos(i);
    }
    else if (GET_POS(i) == POS_INCAP)
    {
      if (damage(i, i, 1, TYPE_SUFFERING) == -1)
        continue;
    }
    else if (GET_POS(i) == POS_MORTALLYW)
    {
      if (damage(i, i, 2, TYPE_SUFFERING) == -1)
        continue;
    }
    if (GET_POS(i) == POS_DEAD)
      if (damage(i, i, 1, TYPE_SUFFERING) == -1)
        continue;



  }
}


int can_mate_with(struct char_data *ch, struct char_data *mate)
{
  if (!MOB_FLAGGED(mate, MOB_CAN_MATE))
    return (0);

  if (GET_SEX(ch) == GET_SEX(mate))
    return (0);

  if (GET_SEX(mate) != SEX_FEMALE)
    return (0);

  if (GET_MTYPE(ch) != GET_MTYPE(mate))
    return (0);

  if (GET_MOB_PREG(mate) == TRUE)
    return (0);

  return (1);
}


void mobile_mating(void)
{
  struct char_data *i, *k = NULL, *next_char;

  for (i = character_list; i != NULL; i = next_char)
  {
    log("MATING: checking a char");
    next_char = i->next;
    if (!IS_MOB(i))
      continue;
    log("MATING:   char is a mob");
    if (GET_SEX(i) == SEX_MALE)
      continue;
    log("MATING:   mob is a male");
    if (MOB_FLAGGED(i, MOB_CAN_MATE))
      continue;
    log("MATING:   the mob can mate");
    log("MATING:     mob is %s.", GET_NAME(i));
    if (GET_POS(i) >= POS_STANDING)
    {
      for (k = IN_ROOM(i)->people; k; k = k->next_in_room)
      {
        log("MATING:     finding mating mobs in room");
        if ((k != i) && (IS_NPC(k)) && can_mate_with(k, i))
        {
          log("MATING:     FOUND mating mobs");
          act("A bull makes his way to a cow and mounts it as you watch.", FALSE, k, 0, 0, TO_ROOM);
          GET_MOB_PREG(k) = TRUE;
        }
      }
    }
  }
}

void log_push_to_death(struct char_data *ch, struct char_data *attacker)
{
  new_mudlog(BRF, LVL_GOD, TRUE, "%s pushed to Death Trap #%d (%s)", GET_NAME(ch),
             IN_ROOM(ch)->number, IN_ROOM(ch)->name);
  new_mudlog(BRF, LVL_GOD, TRUE, "Pushed by %s", GET_NAME(attacker));
}

int perform_push(struct char_data *ch, int dir, int need_specials_check,
                 struct char_data *attacker)
{
  room_rnum was_in;
  int House_can_enter(struct char_data *ch, room_vnum house);
  void death_cry(struct char_data *ch);
  int special(struct char_data *ch, int cmd, char *arg);
  char buf2[MAX_INPUT_LENGTH];

  if (need_specials_check && special(ch, dir + 1, ""))
    return 0;

  /* charmed? */
  if (IS_AFFECTED(ch, AFF_CHARM) && ch->master &&
      IN_ROOM(ch) == ch->master->in_room)
  {
    new_send_to_char(ch,
                     "The thought of leaving your master makes you weep.\r\n");
    new_send_to_char(attacker, "You cant push charmies!\r\n");
    return 0;
  }
  if (!enter_wtrigger(EXIT(ch, dir)->to_room, ch, dir))
  {
    new_send_to_char(ch, "You slam into an invisible barrier!\r\n");
    new_send_to_char(attacker, "You push, but can't get them into that room.\r\n");
    return 0;
  }

  if (IS_SET_AR(ROOM_FLAGS(IN_ROOM(ch)), ROOM_ATRIUM))
  {
    if (!House_can_enter(ch, EXIT(ch, dir)->to_room->number))
    {
      new_send_to_char(ch,
                       "You are pushed, but you can't tresspass!\r\n");
      return 0;
    }
  }
  if (IS_SET_AR(ROOM_FLAGS(EXIT(ch, dir)->to_room), ROOM_TUNNEL) &&
      EXIT(ch, dir)->to_room->people != NULL)
  {
    send_to_char("You are pushed, but there isn't enough room\r\n",
                 ch);
    return 0;
  }
  sprintf(buf2, "$n is pushed to the %s by $N.", dirs[dir]);
  act(buf2, TRUE, ch, 0, attacker, TO_NOTVICT);
  was_in = IN_ROOM(ch);
  char_from_room(ch);
  char_to_room(ch, was_in->dir_option[dir]->to_room);

  if (!IS_AFFECTED(ch, AFF_SNEAK))
    act("$n tumbles into the room.", TRUE, ch, 0, 0, TO_ROOM);

  if (ch->desc != NULL)
    look_at_room(ch, 0);

  if (IS_SET_AR(ROOM_FLAGS(IN_ROOM(ch)), ROOM_DEATH) &&
      GET_LEVEL(ch) <= LVL_HERO)
  {
  hit_death_trap(ch);
    log_push_to_death(ch, attacker);
    return 0;
  }
  return 1;
}



char * romance_status(struct char_data *ch)
{
  switch (ROMANCE(ch))
  {
  case 2:
    return "Engaged";
    break;
  case 3:
    return "Married";
    break;
  case 1:
    return "Dating";
    break;
  case 0:
    return "Single";
    break;
  default:
    return "Broken!";
    break;
  }
}
char * baby_status(struct char_data *i, char * buf, size_t len)
{
  /* MatingMod Addition - Essential! */
  if ((PREG(i) > NOT_PREG) && !IS_NPC(i))
  {

    if (PREG(i) < 7)
      snprintf(buf, len, "%s %s giving birth!\r\n", GET_NAME(i), "is");
    else if (PREG(i) < (MONTHS_2))	// Customize these messages, if you want.
      snprintf(buf, len, "%s look%s as if %s may give birth to a healthy, baby sometime soon.\r\n",
               GET_NAME(i), "s","she");	// They're pretty cheesy.
    else if (PREG(i) < (MONTHS_3))	// Month 8
      snprintf(buf, len,   "%s %s a large, cumbersome bulge in %s waist.\r\n", GET_NAME(i),  "has", "her");
    else if (PREG(i) < (MONTHS_4))	// Month 7
      snprintf(buf, len, "%s %s already had to let out %s armor.\r\n", GET_NAME(i),  "has",  "her");
    else if (PREG(i) < (MONTHS_5))	// Month 6
      snprintf(buf, len, "%s %s a small bulge in %s midsection.\r\n",  GET_NAME(i),  "has",  "her");
    else if (PREG(i) < (MONTHS_6))	// Month 5
      snprintf(buf, len, "%s %s just beginning to show.\r\n",  GET_NAME(i),  "is");
    else
    {
      snprintf(buf, len, "%s %s a motherly glow...\r\n", GET_NAME(i), "has");
    }
    return buf;
  }
  else
  {
    return "\r\n";
  }
}


ACMD(do_finger)
{
  struct char_data *vict;
  char arg[MAX_INPUT_LENGTH];
  char buf[MAX_INPUT_LENGTH];
  char state[MAX_INPUT_LENGTH];
  DESCRIPTOR_DATA *d;
  int id = 0;

  one_argument(argument, arg);
  if (!*arg)
  {
    send_to_char("For whom do you wish to gather information on?\r\n",
                 ch);
    return;
  }
  if (!(id = get_id_by_name(arg)))
  {
    send_to_char("There is no such player.\r\n", ch);
    return;
  }
  for (d = descriptor_list; d; d = d->next)
  {
    if (d->character && GET_ID(d->character) == id)
    {
      vict = d->character;
      break;
    }
  }
  if (!d)
  {
    CREATE(vict, struct char_data, 1);
    clear_char(vict);
    TEMP_LOAD_CHAR = TRUE;
    if (store_to_char(arg, vict) == -1)
    {
      new_send_to_char(ch, "Player doesn't exist.\r\n");
      TEMP_LOAD_CHAR = FALSE;
      free(vict);
      return;
    }
  }
  TEMP_LOAD_CHAR = FALSE;
  if (GET_LEVEL(ch) <= LVL_HERO && GET_LEVEL(vict) > LVL_HERO)
  {
    send_to_char("You don't seem to be able to gather info for that player.\r\n", ch);
    if (!d)
      free_char(vict);
    return;
  }

  if (d)
  {
    if (STATE(d) == CON_PLAYING && d->original)
      strcpy(state, "Switched");
    else
    {
      switch (STATE(d))
      {
      case CON_CREATE_NEW:
        strcpy(state, creation_state_types[SUB_STATE(d)]);
        break;
      default:
        strcpy(state, connected_types[STATE(d)]);
        break;
      }
    }
  }
  else
  {
    strcpy(state, "Logged Out");
  }
  new_send_to_char(ch,
                   "\r\n {cyO-----------------------------------------------------------O{c0\r\n"
                   "     {ccName: {cg%-15s{ccRace: {cg%-8s        {ccClass: {cg%-8s\r\n"
                   "  {ccRemorts: {cg%-3d           {ccLevel: {cg%-2d       {ccCurrent Tier: {cg%d\r\n"
                   "    {ccKills: {cg%-8d     {ccDeaths: {cg%-5d        {ccPK Kills: {cg%-3d\r\n"
                   "      {ccAge: {cg%-3d          {ccHeight: {cg%-3d            {ccWeight: {cg%-3d\r\n"
                   "     {ccClan: {cg%-15s\r\n"
                   "  {ccPartner:{cg %s - %-15s \r\n"
                   "  {cg%s"
                   " {cyO-----------------------------------------------------------O{c0\r\n\r\n"
                   " {cW%s\r\n"
                   " {cyO-----------------------------------------------------------O{c0\r\n"
                   " {ccLast Logged In: {cg%s "
                   " {ccStatus: {cg%s{c0\r\n"
                   " {cc%s%s%s{c0"
                   "\r\n",
                   GET_NAME(vict), race_name(vict), simple_class_name(vict),
                   REMORTS(vict), GET_LEVEL(vict), current_class_is_tier_num(vict),
                   GET_KILL_CNT(vict), GET_RIP_CNT(vict), GET_PK_CNT(vict),
                   GET_AGE(vict), GET_HEIGHT(vict), GET_WEIGHT(vict),
                   ((!GET_CLAN(vict)) ? "<none>" : clan[find_clan_by_id(GET_CLAN(vict))].name),
                   romance_status(vict), PARTNER(vict) <= 0 ? "<none>" : get_name_by_id(PARTNER(vict)),
                   baby_status(vict, buf, sizeof(buf)),
                   vict->player.description ? vict->player.description : "They are unremarkable",
                   ctime(&vict->player.time.logon),
                   state,
                   AFK_MSG(vict) ? "AFK: " : "", AFK_MSG(vict) ? AFK_MSG(vict) : "", AFK_MSG(vict) ? "\r\n" : ""
                  );

  if (!d)
    free_char(vict);
  return;

}


ACMD(do_zlist)
{
  extern struct zone_data *zone_table;
  extern int top_of_zone_table;
  char buf[MAX_INPUT_LENGTH];
  char buf2[MAX_INPUT_LENGTH];

  new_send_to_char(ch, "Please use: show zones\r\n");
  return;

  int first, last, nr, found = 0;
  two_arguments(argument, buf, buf2);

  if (!*buf || !*buf2)
  {
    send_to_char("Usage: zlist <begining number> <ending number>\r\n",
                 ch);
    return;
  }

  first = atoi(buf);
  last = atoi(buf2);

  if ((first < 0) || (first > 99999) || (last < 0) || (last > 99999))
  {
    send_to_char("Values must be between 0 and 99999.\r\n", ch);
    return;
  }

  if (first >= last)
  {
    send_to_char("Second value must be greater than first.\r\n", ch);
    return;
  }

  for (nr = 0; nr <= top_of_zone_table; nr++)
  {
    new_send_to_char(ch, "%5d. [%5d] %-60s %ld\r\n", ++found,
                     zone_table[nr].number,
                     zone_table[nr].name, zone_table[nr].zone_flags);
  }

  if (!found)
    send_to_char("No mobiles were found in those parameters.\r\n", ch);
}

void zap_char(struct char_data *victim)
{
  int i;
  struct obj_data *obj = NULL;

  for (i = 0; i < NUM_WEARS; i++)
  {
    if (GET_EQ(victim, i)
        && (invalid_align(victim, GET_EQ(victim, i))))
    {
//    char objname[MAX_INPUT_LENGTH];
    int i = 0;
      obj = unequip_char(victim, i);
      act("You are zapped by $p and instantly let go of it.",
          FALSE, victim, obj, 0, TO_CHAR);
	  /*if (strlen(obj->short_description) < MAX_INPUT_LENGTH) {
	  strcpy(objname, obj->short_description);
	  i = strlen(objname);
	  
	  while (objname[i] == ' ') {
	  objname[i] = '\0';
	  i--;	  
	  }
	  if (objname[i] == 's') {
	  //check to see if it is a plural and apostrophy appropriately
	  act("$n is zapped by $p' and instantly lets go of it.",
          FALSE, victim, obj, 0, TO_ROOM);
	  }
	  } else */{	  
      act("$n is zapped by $p and instantly lets go of it.",
          FALSE, victim, obj, 0, TO_ROOM);
	  }
      obj_to_char(obj, victim);
    }
  }
}

ACMD(do_smite)
{				/* by Garion */
  struct obj_data *obj;
  struct char_data *victim;
  char arg[MAX_INPUT_LENGTH];
  char arg2[MAX_INPUT_LENGTH];
  char buf[MAX_STRING_LENGTH];
  int i;

  two_arguments(argument, arg, arg2);

  if (arg[0] == '\0')
  {
    send_to_char("Smite whom?\r\n", ch);
    return;
  }

  if ((victim = get_char_room_vis(ch, arg, NULL)) == NULL)
  {
    send_to_char("That person is not here.\r\n", ch);
    return;
  }

  if (victim == ch)
  {
    send_to_char("Take it somewhere else, Jack.\r\n", ch);
    return;
  }

  if (!IS_NPC(victim) && GET_LEVEL(victim) > GET_LEVEL(ch))
  {
    send_to_char("You failed.\r\n", ch);
    act("$n tried to smite you.", FALSE, ch, NULL, victim, TO_VICT);
    return;
  }

  if (FIGHTING(victim) != NULL)
    stop_fighting(victim);

  sprintf(buf,
          "       *     W     W   H  H   AA    M   M   !!     *       \r\n");
  strcat(buf,
         "     *****    W W W    HHHH  AAAA   M M M   !!   *****     \r\n");
  strcat(buf,
         "       *      W   W    H  H  A  A  M     M  !!     *       \r\n");
  strcat(buf, "\r\n");
  send_to_char(buf, victim);

  act("$n raises $s hand and smites you!", FALSE, ch, NULL, victim,
      TO_VICT);
  act("$n raises $s hand and smites $N!", FALSE, ch, NULL, victim,
      TO_NOTVICT);
  act("You raise your hand and smite $N!", FALSE, ch, NULL, victim,
      TO_CHAR);



  if (!strcmp(arg2, "hard"))
  {
    for (i = 0; i < NUM_WEARS; i++)
    {
      if ((obj = GET_EQ(victim, i)) != NULL)
      {
        obj_to_char(unequip_char(victim, i), victim);
        obj_from_char(obj);
        obj_to_room(obj, victim->in_room);
      }
    }
    send_to_char
    ("You are blown out of your stuff and right onto your ass!\r\n",
     victim);
    act("$N is blown out of $S stuff and right onto $S ass!", FALSE,
        ch, NULL, victim, TO_NOTVICT);
    act("$N is blown out of $S stuff and right onto $S ass!", FALSE,
        ch, NULL, victim, TO_CHAR);
  }
  else if ((obj = GET_EQ(victim, WEAR_FEET)) != NULL)
  {
    obj_to_char(unequip_char(victim, WEAR_FEET), victim);
    obj_from_char(obj);
    send_to_char
    ("You are blown out of your shoes and right onto your ass!\r\n",
     victim);
    act("$N is blown out of $S shoes and right onto $S ass!", FALSE,
        ch, NULL, victim, TO_NOTVICT);
    act("$N is blown out of $S shoes and right onto $S ass!", FALSE,
        ch, NULL, victim, TO_CHAR);
    obj_to_room(obj, victim->in_room);
  }
  else
  {
    send_to_char("You are knocked on your ass!\r\n", victim);
    act("$N is knocked on his ass!", FALSE, ch, NULL, victim,
        TO_NOTVICT);
    act("$N is knocked on his ass!", FALSE, ch, NULL, victim, TO_CHAR);
  }
  if (damage(victim, victim, GET_HIT(victim) > 1 ? GET_HIT(victim) / 2 : 0, TYPE_UNDEFINED) != -1)
    GET_POS(victim) = POS_RESTING;
  return;
}

ACMD(do_blowup)
{
  struct obj_data *target = NULL;
  char arg[MAX_STRING_LENGTH];

  one_argument(argument, arg);

  /* no target given */
  if (!*arg)
  {
    send_to_char("What do you want to blow up?\r\n", ch);
    return;
  }

  /* target isn't in the room or can't be seen */
  if (!
      (target =get_obj_in_list_vis(ch, arg, NULL,IN_ROOM(ch)->contents)))
  {
    send_to_char("What is that?\r\n", ch);
    return;
  }

  /* ok, we have a target, let's blow the thing up (pick a weapon to use later) */
  act("You pull out a missile launcher and aim it at $p.",
      FALSE, ch, target, NULL, TO_CHAR);
  act("$n pulls out a missile launcher and aims it at $p.",
      FALSE, ch, target, NULL, TO_ROOM);
  act("You pull the trigger and blow up $p, sending debris everywhere.",
      FALSE, ch, target, NULL, TO_CHAR);
  act("$n pulls the trigger and blows up $p, sending debris everywhere.",
      FALSE, ch, target, NULL, TO_ROOM);

  /* purge the object */
  extract_obj(target);
}

void explosion_messages(room_rnum room, int damage, struct obj_data *target)
{
  struct char_data *tch;

  while (room->people != NULL)
  {
    tch = room->people;
    switch (damage)
    {
    case 1:
      act("A missile strikes the $p, causing considerable damage.",
          FALSE, tch, target, 0, TO_CHAR);
      act("A missile strikes the $p, causing considerable damage.",
          FALSE, tch, target, 0, TO_ROOM);
      break;
    case 2:
      act("The $p shakes and groans under the impact of a missile.",
          FALSE, tch, target, 0, TO_CHAR);
      act("The $p shakes and groans under the impact of a missile.",
          FALSE, tch, target, 0, TO_ROOM);
      break;
    default:
      act("A missile whizzes past, missing your ship by inches.\r\n"
          "{cRRED ALERT!!! Enemy ship attacking! RED ALERT!!!{cx",
          FALSE, tch, target, 0, TO_ROOM);
      break;
    }
    return;
  }

  /* the room is empty */
  return;
}

ACMD(do_jump)
{
  struct obj_data *obj = NULL;
  char arg[MAX_STRING_LENGTH];

  one_argument(argument, arg);

  if (*arg)
  {
    if ((obj =
           get_obj_in_list_vis(ch, arg, NULL, IN_ROOM(ch)->contents)))
    {
      if (CAN_SEE_OBJ(ch, obj))
      {	/* Can they see the hurdle */
        if (GET_OBJ_TYPE(obj) == ITEM_PORTAL_HURDLE)
        {	/* Is it the right type */
          if (GET_OBJ_VAL(obj, 0) != NOWHERE)
          {	/* Pick the right value */
            if (RIDDEN_BY(ch))
              char_from_room(RIDDEN_BY(ch));
            if (RIDING(ch))
              dismount_char(ch);
            char_from_room(ch);
            act("You jump over $p.", FALSE, ch, obj, 0,   TO_CHAR);
            act("$n jumps over $p.", FALSE, ch, obj, 0,   TO_ROOM);
            char_to_room(ch, world_vnum[GET_OBJ_VAL(obj, 0)]);
            act("$n jumps into the room.", FALSE, ch, NULL, 0,   TO_ROOM);
            if (RIDDEN_BY(ch))
              char_to_room(RIDDEN_BY(ch), world_vnum[GET_OBJ_VAL(obj, 0)]);
            entry_memory_mtrigger(ch);
            greet_mtrigger(ch, -1);
            greet_memory_mtrigger(ch);
            enter_wtrigger(IN_ROOM(ch), ch, -1);
          }
          else if (real_room(GET_OBJ_VAL(obj, 1)) != NULL)
          {
            /* This should ALWAYS be the right one */
            if (RIDDEN_BY(ch))	/* if they are being ridden, bring along the mount */
              char_from_room(RIDDEN_BY(ch));
            char_from_room(ch);	/* move them to the room */
            act("$n jumps over $p and out of sight.", FALSE, ch, obj, 0, TO_ROOM);	/* tell the room they left */
            act("You jump over $p.", FALSE, ch, obj, 0, TO_CHAR);	/* tell the player */
            char_to_room(ch, real_room(GET_OBJ_VAL(obj, 1)));	/* send the player/mob to the room */
            if (RIDING(ch))
            {
              act("$n's horse refuses to jump and $s flies over its head, landing with a THUMP!", FALSE, ch, NULL, 0, TO_ROOM);
              act("You fly over your horse's head as it refuses to jump, landing with a THUMP!", FALSE, ch, NULL, RIDING(ch), TO_CHAR);
              damage(ch, ch, number(1, ((GET_HIT(ch)/3) - 1)),  TYPE_UNDEFINED);
            }
            else
              act("$n jumps into the room.", FALSE, ch, NULL, 0, TO_ROOM);	/* Notify the room they jumped into */
            if (RIDDEN_BY(ch))	/* bring the player back to the new room */
              char_to_room(RIDDEN_BY(ch),
                           real_room(GET_OBJ_VAL(obj, 1)));

            entry_memory_mtrigger(ch);
            greet_mtrigger(ch, -1);
            greet_memory_mtrigger(ch);
            enter_wtrigger(IN_ROOM(ch), ch, -1);
          }
          look_at_room(ch, 1);	/* make them look so they know they moved */
          if (RIDING(ch))	/* if they are riding, they are jumping off the mounts back */
            dismount_char(ch);
          if (RIDDEN_BY(ch))
            look_at_room(RIDDEN_BY(ch), 1);	/* make the rider look too */
          return;
        }
      }
      /* they tried to jump the wrong object type */
      act("You jump over $p.", FALSE, ch, obj, 0, TO_CHAR);
      act("$n jumps over $p.", FALSE, ch, obj, 0, TO_ROOM);
    }
    else
    {
      send_to_char("You can't find that.\r\n", ch);
      return;
    }
  }
  else
  {
    send_to_char("What do you want to jump over?\r\n", ch);
    return;
  }
}

EVENT2(mine_msg1)
{
  send_to_char("You labor hard as you mine for buried treasure.\r\n",
               CAUSER_CH);
  act("$n begins digging in search of buried treasures.", FALSE,
      CAUSER_CH, NULL, 0, TO_ROOM);
  return;
}

EVENT2(find_nugget)
{
  struct obj_data *obj = NULL;
  int value = 0, keep_mineral = 0;

  value = number(1, 100);
  keep_mineral = number(1, 100);

  if (CAN_MINE_GOLD(CAUSER_CH))
  {
    if (value <= 10)
    {
      send_to_char("You found a gold nugget.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(43, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 90)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)),
                      ROOM_GOLD_DEPOSIT);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }

  if (CAN_MINE_SILVER(CAUSER_CH))
  {
    if (value <= 15)
    {
      send_to_char("You found a silver nugget.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(42, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 85)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)),
                      ROOM_SILVER_DEPOSIT);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }

  if (CAN_MINE_COPPER(CAUSER_CH))
  {
    if (value <= 20)
    {
      send_to_char("You found a copper nugget.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(40, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 80)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)),
                      ROOM_COPPER_DEPOSIT);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }

  if (CAN_MINE_IRON(CAUSER_CH))
  {
    if (value <= 25)
    {
      send_to_char("You found an iron nugget.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(41, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 75)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)),
                      ROOM_IRON_DEPOSIT);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }

  if (CAN_MINE_ANTIMONY(CAUSER_CH))
  {
    if (value <= 30)
    {
      send_to_char("You found an antimony nugget.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(44, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 70)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)),
                      ROOM_ANTIMONY_DEPOSIT);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }

  if (CAN_MINE_CHROMIUM(CAUSER_CH))
  {
    if (value <= 35)
    {
      send_to_char("You found a chromium nugget.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(45, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 65)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)),
                      ROOM_CHROMIUM_DEPOSIT);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }
  if (CAN_MINE_TIN(CAUSER_CH))
  {
    if (value <= 35)
    {
      send_to_char("You found a tin nugget.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(46, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 65)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)), ROOM_TIN_DEPOSIT);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }
  if (CAN_MINE_PLASTONIUM(CAUSER_CH))
  {
    if (value <= 35)
    {
      send_to_char("You found a plastonium nugget.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(47, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 65)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)), ROOM_PLASTONIUM_DEPOSIT);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }

  if (CAN_MINE_STONE(CAUSER_CH))
  {
    if (value <= 40)
    {
      send_to_char("You found a building stone.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(46, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 60)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)), ROOM_QUARRY);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }

  if (CAN_MINE_COAL(CAUSER_CH))
  {
    if (value <= 45)
    {
      send_to_char("You found a lump of coal.\r\n", CAUSER_CH);
      act("$n found something.", FALSE, CAUSER_CH, NULL, 0, TO_ROOM);
      obj = read_object(47, VIRTUAL);
      obj_to_room(obj, IN_ROOM(CAUSER_CH));
      if (keep_mineral < 55)
        REMOVE_BIT_AR(ROOM_FLAGS(IN_ROOM(CAUSER_CH)),
                      ROOM_COAL_MINE);
      return;
    }
    else
    {
      send_to_char("Your efforts turn up nothing of value.\r\n",
                   CAUSER_CH);
      return;
    }
  }

}

ACMD(do_mine)
{

  if (IS_NPC(ch))
    return;

  if (IS_AFFECTED(ch, AFF_BLIND))
  {
    send_to_char("You can't see anything, you're blind!\r\n", ch);
    return;
  }

  if (!AWAKE(ch))
  {
    send_to_char("In your dreams?\r\n", ch);
    return;
  }

  if (CAN_MINE(ch))
  {
    send_to_char("You begin to mine the area.", ch);
    add_event2(5, mine_msg1, ch, NULL, 0);
    add_event2(10, find_nugget, ch, NULL, 0);
    WAIT_STATE(ch, 10 * PULSE_EVENT);
  }
  else
  {
    send_to_char("You can't mine here.\r\n", ch);
    return;
  }
}

ACMD(do_mpdelayed)
{
  int delay;
  char arg1[100];
  char buf[MAX_INPUT_LENGTH];
  *arg1 = '\0';
  /*
    if (!IS_NPC(ch) || IS_AFFECTED(ch, AFF_CHARM))
      {
        send_to_char("Huh?\r\n", ch);
        return;
      }
  */
  half_chop(argument, arg1, buf);
  delay = atoi(arg1);

  if (!delay)
    log("Mpdelayed - the delay is invalid.");
  else
  {
    new_send_to_char(ch, "Ok. Executing '%s' in %d seconds.\r\n", buf,
                     delay);
    add_event2(delay, delayed_command, ch, NULL, strdup(buf));
  };
}

int has_metal_detector(struct char_data *ch)
{
  if (GET_LEVEL(ch) >= LVL_IMPL)
    return (1);

  if (HAS_BODY(ch, WEAR_HOLD) && GET_EQ(ch, WEAR_HOLD) &&
      GET_OBJ_TYPE(GET_EQ(ch, WEAR_HOLD)) == ITEM_METAL_DETECTOR)
    return (1);

  return (0);
}

