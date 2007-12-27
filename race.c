#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "db.h"
#include "utils.h"
#include "spells.h"
#include "interpreter.h"
#include "comm.h"

extern const char *mob_races[];
const char * race_name(struct char_data *ch);
int has_body(CHAR_DATA *ch, int flag);

const char *race_abbrevs[] =
  {
    "Fau",
    "Cen",
    "Elf",
    "Dwa",
    "Ind",
    "Gri",
    "Mar",
    "SWf",
    "Wrm",
    "Tod",
    "Bor",
    "Wlf",
    "Lin",
    "\n"
  };

const char *pc_race_types[] =
  {
    "Faun",
    "Centaur",
    "Elf",
    "Dwarf",
    "Indian",
    "Gringo",
    "Martian",
    "Spacewolf",
    "Toad",
    "Boar",
    "Wolf",
    "Lion",
    "\n"
  };

int has_body(CHAR_DATA *ch, int flag)
{
  if (IS_NPC(ch))
    return TRUE;

  if ((flag < NUM_BODY_PARTS) && IS_SET(races[(int)GET_RACE(ch)].body_bits, (1 << flag)))
    return TRUE;

  if ((flag >= NUM_BODY_PARTS) && IS_SET(EXTRA_BODY(ch), 1 << (flag - NUM_BODY_PARTS)))
    return TRUE;

  return FALSE;
}

const char * race_name(struct char_data *ch)
{
  if (IS_NPC(ch))
    return (mob_races[(int)GET_MRACE(ch)]);
  else
    return (pc_race_types[(int)GET_RACE(ch)]);

}


/* The menu for choosing a race in interpreter.c: */
#if 0
const char *race_menu =
  "\r\n"
  "Select a Race:        |Melee|Magic|\r\n"
  "--Race----------Speed-|Damg-|Damg-|-Defnce-Attk--UniquePositions--\r\n"
  "  [F]aun       - +100    0% -20%    +10   +25   Tail, Hips, Horns\r\n"
  "  [C]entaur    - +100  +10%   0%      0   +10   Tail, Backlegs, Backfeet\r\n"
  "  [E]lf        - +150  -10% +10%    -40   +15   Hips, Ankles(x2)\r\n"
  "  [D]warf      - +100  -10%   0%    +40   +10   Hips, Ankles(x2)\r\n"
  "  [I]ndian     - +100    0%   0%      0     0   Hips, Ankles(x2)\r\n"
  "  [G]ringo     - +100    0%   0%      0     0   Hips, Ankles(x2)\r\n"
  "  [M]artian    - +180    0%  +5%    -50    +5   Antenna, Ankles(x2)\r\n"
  "  [S]pace-wolf - +110  +15% -20%      0   -10   Hips, Tail, Lower-Neck\r\n";
#else
const char *race_menu =
  "\r\n"
  "{cySelect a Race (More info given):  \r\n"
  "--Race---------\r\n"
  "  {cg[{cGF{cg]aun       \r\n"
  "  {cg[{cGC{cg]entaur    \r\n"
  "  {cg[{cGE{cg]lf        \r\n"
  "  {cg[{cGD{cg]warf      \r\n"
  "  {cg[{cGI{cg]ndian     \r\n"
  "  {cg[{cGG{cg]ringo     \r\n"
  "  {cg[{cGM{cg]artian    \r\n"
  "  {cg[{cGS{cg]pace-wolf {c0\r\n";
#endif


/* body standard 21 parts */
#define BODY_STANDARD (BODY_LIGHT|BODY_BODY|BODY_HEAD|BODY_LEGS|BODY_FEET| \
                       BODY_HANDS|BODY_ARMS|BODY_FOCUS|BODY_ABOUT|BODY_WIELD|BODY_WIELD_2| \
		       BODY_SHIELD|BODY_FINGER_R|BODY_FINGER_L|BODY_EYES|BODY_FACE| \
		       BODY_HOLD|BODY_EAR_R|BODY_EAR_L|BODY_WRIST_R|BODY_WRIST_L| \
		       BODY_NECK_1|BODY_WAIST)

#define HUMANOID   	(BODY_STANDARD|BODY_HIPS|BODY_ANKLE_R|BODY_ANKLE_L)

#define FAUN    	(BODY_STANDARD|BODY_TAIL|BODY_HIPS|BODY_HORNS)

#define CENTAUR         (BODY_STANDARD|BODY_TAIL|BODY_LEGS_2|BODY_FEET_2)

#define MARTIAN         (BODY_STANDARD|BODY_ANKLE_R|BODY_ANKLE_L|BODY_ANTENNA)

#define SWOLF           (BODY_STANDARD|BODY_TAIL|BODY_HIPS|BODY_NECK_2)



#define DEFAULT    	(BODY_STANDARD|BODY_ANKLE_R|BODY_ANKLE_L)

//  Player race structure

const struct race_data races[NUM_RACES] =
  {
    {
      RACE_FAUN, FAUN
    }
    , {
      RACE_CENTAUR, CENTAUR}, {
      RACE_ELF, HUMANOID}, {
      RACE_DWARF, HUMANOID}, {
      RACE_INDIAN, HUMANOID}, {
      RACE_GRINGO, HUMANOID}, {
      RACE_MARTIAN, MARTIAN}, {
      RACE_SPACE_WOLF, SWOLF}
  };


//** I made these function so you could have error checking and
//** to provide the easy ability to add things like poly
void set_race(struct char_data *ch, int race)
{
  long long remove, r;
  struct obj_data *obj;
  extern const char *body[];
  void obj_to_char(struct obj_data *object, struct char_data *ch);
  struct obj_data *unequip_char(struct char_data *ch, int pos);
  char buf[MAX_STRING_LENGTH];

  remove = GET_BODY(ch);

  GET_RACE(ch) = races[race].race;
  //GET_BODY(ch) = races[race].body_bits;

  remove = remove - (remove & GET_BODY(ch));

  for (r = 0; r < NUM_BODY_PARTS; r++)
    if (IS_SET(remove, (1 << r)) && (obj = GET_EQ(ch, r)))
    {
      snprintf(buf,sizeof(buf),
               "Your %s disappears and your $p falls to the ground!",
               body[r]);
      act(buf, TRUE, ch, obj, 0, TO_CHAR);
      obj_to_char(unequip_char(ch, r), ch);
    }
}

int parse_race(char arg)
{
  switch (LOWER(arg))
  {
  case 'f':
    return RACE_FAUN;
    break;
  case 'c':
    return RACE_CENTAUR;
    break;
  case 'e':
    return RACE_ELF;
    break;
  case 'd':
    return RACE_DWARF;
    break;
  case 'i':
    return RACE_INDIAN;
    break;
  case 'g':
    return RACE_GRINGO;
    break;
  case 'm':
    return RACE_MARTIAN;
    break;
  case 's':
    return RACE_SPACE_WOLF;
    break;
  default:
    return RACE_UNDEFINED;
    break;
  }
}

ACMD(do_race)
{
  int r;
  char body_parts[512];
  extern const char *body[];
  char arg[MAX_INPUT_LENGTH];
  one_argument(argument, arg);

  if (*arg)
  {
    r = parse_race(*arg);
    sprintbit(races[r].body_bits, body, body_parts);
    new_send_to_char(ch, "%s: %s\r\n", pc_race_types[r], body_parts);
  }
  else
  {
    for (r = 1; r < 8; r++)
    {
      sprintbit(races[r].body_bits, body, body_parts);
      new_send_to_char(ch, "%s: %s\r\n", pc_race_types[r],
                       body_parts);
    }
  }

}

/*
long find_race_bitvector(char arg) {
  
  switch (arg) {
    case '1': return (1 << RACE_FAUN);
    case '2': return (1 << RACE_CENTAUR);
    case '3': return (1 << RACE_ELF);
    case '4': return (1 << RACE_DWARF);
    case '5': return (1 << RACE_INDIAN);
    case '6': return (1 << RACE_GRINGO);
    case '7': return (1 << RACE_MARTIAN);
    case '8': return (1 << RACE_SPACE_WOLF);
    default: return (0);
  }
}
*/
int invalid_race(struct char_data *ch, struct obj_data *obj)
{
  if ((IS_OBJ_STAT(obj, ITEM_ANTI_FAUN) && IS_FAUN(ch)) ||
      (IS_OBJ_STAT(obj, ITEM_ANTI_CENTAUR) && IS_CENTAUR(ch)) ||
      (IS_OBJ_STAT(obj, ITEM_ANTI_DWARF) && IS_DWARF(ch)) ||
      (IS_OBJ_STAT(obj, ITEM_ANTI_ELF) && IS_ELF(ch)) ||
      (IS_OBJ_STAT(obj, ITEM_ANTI_INDIAN) && IS_INDIAN(ch)) ||
      (IS_OBJ_STAT(obj, ITEM_ANTI_GRINGO) && IS_GRINGO(ch)) ||
      (IS_OBJ_STAT(obj, ITEM_ANTI_MARTIAN) && IS_MARTIAN(ch)) ||
      (IS_OBJ_STAT(obj, ITEM_ANTI_SPACE_WOLF) && IS_SPACE_WOLF(ch)))
    return (1);
  else
    return (0);
}