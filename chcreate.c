#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "comm.h"
#include "interpreter.h"
#include "db.h"
#include "utils.h"
#include "spells.h"
#include "handler.h"
#include "mail.h"
#include "screen.h"
#include "dg_scripts.h"
#include "clan.h"
#include "constants.h"
#include "genolc.h"
#include "oasis.h"
#include "tedit.h"
#include "improved-edit.h"
#include "descriptor.h"

/* local */
void con_display_spec(Descriptor *d);
void con_display_class(Descriptor *d);
void con_display_race(Descriptor *d);
void con_display_stats(Descriptor *d);
void con_display_sex(Descriptor *d, int valid);
void con_display_ansi(Descriptor *d);
void con_display_new_here(Descriptor *d);
void explain_stats(Descriptor *d);
void con_display_email(Descriptor *d);
void line_sep(Descriptor *d);
/*external*/
extern const char *class_menu;
extern const char *race_menu;
extern const char *pc_race_types[];
extern const char *pc_class_types[];
extern char *motd;

int parse_class(char arg);
int parse_race(char arg);

ACMD(do_help);

void con_character_creation(Descriptor *d, char *arg)
{
  
  char race_selection[256];
  char class_selection[256];
  int load_result;
  
  void set_race(Character *ch, int race);
  
  switch (SUB_STATE(d))
  {
    
  case STATE_ANSI:
    switch (*arg)
    {
    case 'y':
    case 'Y':
      SET_BIT_AR(PRF_FLAGS(d->character), PRF_COLOR_1);
      SET_BIT_AR(PRF_FLAGS(d->character), PRF_COLOR_2);
      d->Output( "\r\n{cRColor on.{c0\r\n");
      break;
    case 'n':
    case 'N':
      d->Output( "\r\nColor off.\r\n");
      break;
    default:
      con_display_ansi(d);
      return;
    }
    con_display_new_here(d);
    break;
  case STATE_NEW_HERE:
    switch (*arg)
    {
    case '1':
      GET_NEWBIE_STATUS(d->character) = NEWB_NEW;
      break;
    case '2':
      GET_NEWBIE_STATUS(d->character) = NEWB_4DNEW;
      break;
    case '3':
      GET_NEWBIE_STATUS(d->character) = NEWB_OLD;
      break;
    default:
      d->Output( "\r\nInvalid answer sorry.\r\n");
      con_display_new_here(d);
      return;
      break;
    }
    con_display_sex(d, TRUE);
    break;
    
  case STATE_QSEX:       /* query sex of new user         */
    switch (*arg)
    {
    case 'm':
    case 'M':
      d->character->player.sex = SEX_MALE;
      break;
    case 'f':
    case 'F':
      d->character->player.sex = SEX_FEMALE;
      break;
#if 0
    case 'n':
    case 'N':
      d->character->player.sex = SEX_NEUTRAL;
      break;
#endif
    default:
      con_display_sex(d, FALSE);
      return;
    }
    
    con_display_class(d);
    break;
    
  case STATE_QRACE:
    load_result = parse_race(*arg);
    GET_RACE(d->character) = load_result;
    if (load_result == RACE_UNDEFINED)
    {
    d->Output("\r\nThat's not a race.\r\nRace: ");
      return;
    }
    strcpy(race_selection, pc_race_types[load_result]);
    d->Output("\r\n");
    
    do_help(d->character, race_selection, 0, 0);
    d->Output("Are you sure you want %s? ", race_selection);
    SUB_STATE(d) = STATE_CONFIRM_QRACE;
    break;
    
  case STATE_CONFIRM_QRACE:
    if (UPPER(*arg) == 'Y')
    {
      void race_abils(Character *ch);
      set_race(d->character, GET_RACE(d->character));
      
      d->character->real_abils.str_add = 0;
      d->character->real_abils.str = 15;
      d->character->real_abils.intel = 13 + (IS_CASTER(GET_CLASS(d->character)));
      d->character->real_abils.wis = 13;
      d->character->real_abils.dex = 13 + (IS_ROGUE(GET_CLASS(d->character)));
      d->character->real_abils.con = 13 + (IS_FIGHTER(GET_CLASS(d->character)));
      d->character->real_abils.cha = 13;
      CREATE_POINTS(d->character) = 10;
      race_abils(d->character);
      d->character->aff_abils = d->character->real_abils;
      explain_stats(d);
    }
    else
    {
      con_display_race(d);
      return;
    }
    break;
    
  case STATE_CONFIRM_STATS:
    if (compares("NEXT", arg))
    {
      con_display_spec(d);
    }
    else if (compares("INFO", arg))
    {
      explain_stats(d);
    }
    else
    {
      if (*arg)
      {
        char abil[MAX_INPUT_LENGTH], *amou;
        amou = one_argument(arg, abil);
        skip_spaces(&amou);
        if (!(*abil && amou && *amou && choose_real_abils(d->character, *abil, atoi(amou))))
          d->Output( "Invalid input: type NEXT to move on, or the stat letter and an amount!"  );
        
        d->character->aff_abils = d->character->real_abils;
      }
      con_display_stats(d);
      return;
      
    }
    break;
    
  case STATE_QCLASS:
    load_result = parse_class(*arg);
    GET_CLASS(d->character) = load_result;
    
    if (load_result == CLASS_UNDEFINED)
    {
    d->Output("\r\nThat's not a class.\r\nClass: ");
      return;
    }
    else
    {
      strcpy(class_selection, pc_class_types[load_result]);
      d->Output("\r\n");
    }
    do_help(d->character, class_selection, 0, 0);
    d->Output("Are you sure you want to be a %s? ", class_selection);
    SUB_STATE(d) = STATE_CONFIRM_QCLASS;
    break;
    
  case STATE_CONFIRM_QCLASS:
    if (UPPER(*arg) == 'Y')
    {
      con_display_race(d);
      break;
    }
    else
    {
      con_display_class(d);
      break;
    }
    break;
  case STATE_QSPEC:
    con_display_spec(d);
    break;
  case STATE_LOYAL:
    if (UPPER(*arg) == 'A')
      improve_sub(d->character, SUB_LOYALSPEED, 100);
    else if (UPPER(*arg) == 'B')
      improve_sub(d->character, SUB_LOYALATTACK, 100);
    else  if (UPPER(*arg) == 'C')
      improve_sub(d->character, SUB_LOYALDEFEND, 100);
    else if (UPPER(*arg) == 'D')
      improve_sub(d->character, SUB_LOYALDAMAGE, 100);
    else
    {
      con_display_spec(d);
      return;
    }
    con_display_email(d);
    break;
  case STATE_EMAIL:
    if (!arg || !*arg)
    {
      GET_EMAIL(d->character) = strdup("none");
    }
    else
    {
      GET_EMAIL(d->character) = strdup(arg);
    }
    
    if (GET_PFILEPOS(d->character) < 0)
      GET_PFILEPOS(d->character) =
      create_entry(GET_PC_NAME(d->character));
    init_char(d->character);
    save_char(d->character);
    save_player_index();
    line_sep(d);
    d->Output("%s", motd);
  d->Output("\r\n*** PRESS RETURN: ");
    STATE(d) = CON_RMOTD;
    
    new_mudlog(NRM, LVL_GOD, TRUE, "%s [%s] new player.", GET_NAME(d->character),
               d->host);
    break;
    
    
  }
  
}

void con_display_class(Descriptor *d)
{
  line_sep(d);
  d->Output("\r\n%s\r\n{cyClass: {c0", class_menu);
  SUB_STATE(d) = STATE_QCLASS;
}

void con_display_race(Descriptor *d)
{
  line_sep(d);
  d->Output("\r\n%s\r\n{cyRace: {c0",race_menu);
  SUB_STATE(d) = STATE_QRACE;
}
void explain_stats(Descriptor *d)
{
  line_sep(d);
  d->Output(
                  "{cyThe stats are as follows:{c0\r\n\r\n"
                  "{cgWisdom (WIS){cw\r\n"
                  "Gets you more practice sessions and mana when you level, and adds to magic damage.\r\n"
                  "Increases the chance of becoming better at a skill/spell through practice.\r\n\r\n{c0"
                  "{cgConstitution (CON){cw\r\n"
                  "Increases the max health gained when you level and health regeneration rate.\r\n"
                  "For fighters and rogues it increases the damage of their weapons.{c0\r\n\r\n"
                  "{cgStrength (STR){cw\r\n"
                  "Adds to your max carriable weight, and your speed.\r\n"
                  "For fighters and rogues it increases the damage of their weapons.{c0\r\n\r\n"
                  "{cgDexterity (DEX){cw\r\n"
                  "Increases attack rating and speed and armor class, lets you carry more items.\r\n"
                  "Dex is also good for thief skills, like picking locks and back stabbing.{c0\r\n\r\n"
                  "{cgIntelligence (INT){cw\r\n"
                  "Greater gain from practice sessions and gives you more mana when you level.\r\n"
                  "Increases the chance of becoming better at a skill/spell through practice.\r\n"
                  "Is the primary affect for increasing damage of all attack spells{c0\r\n\r\n"
                  "{cgCharisma (CHA){cw\r\n"
                  "Increases your minimum magic damage, extends the duration of spells,\r\n"
                  "lowers cost of items in shops, and increases max health gained when leveling.\r\n\r\n{c0"
                  "{cy------------When ready hit enter------------{c0 \r\n\r\n");
  SUB_STATE(d) = STATE_CONFIRM_STATS;
}
void con_display_stats(Descriptor *d)
{
#if 1
  line_sep(d);
  d->Output(
                  "{ccHere you can add extra points your stats.\r\n\r\n"
                  
                  "Within the game equipment is also used to increase these stats further.\r\n\r\n"
                  
                  "You can also take points from a stat till it reaches 0.{c0\r\n"
                  "22 is the max usable amount in all stats except charisma, which is 100.\r\n"
                  
                  "\r\n    {cy==================\r\n"
                  "      Points left: {cC%-2d{cy  -- Give your character some stats.\r\n"
                  "    ------------------\r\n"
                  "    {cg({cGS{cg)trength    : {cc%-2d{c0\r\n"
                  "    {cg({cGI{cg)nteligence : {cc%-2d{c0\r\n"
                  "    {cg({cGW{cg)isdom      : {cc%-2d{c0\r\n"
                  "    {cgc({cGO{cg)nstitution: {cc%-2d{c0\r\n"
                  "    {cg({cGD{cg)exterity   : {cc%-2d{c0\r\n"
                  "    {cg({cGC{cg)harisma    : {cc%-2d{c0\r\n"
                  "    {cy------------------\r\n"
                  "Type a letter then an amount. eg: S 3 or C -2 :\r\n"
                  "To see the info on stats again type INFO\r\n"
                  "When finished type NEXT to continue:  {c0",
                  CREATE_POINTS(d->character),
                  GET_STR(d->character), GET_INT(d->character), GET_WIS(d->character),
                  GET_CON(d->character), GET_DEX(d->character),  GET_CHA(d->character));
#else
  
#endif
  SUB_STATE(d) = STATE_CONFIRM_STATS;
}
void con_display_spec(Descriptor *d)
{
  line_sep(d);
  d->Output("\r\n"
                  "{cyChoose your starting bonus Subskill:\r\n\r\n"
                  "{cg[{cGA{cg] Speed -- {cP+50 to speed{c0\r\n\r\n"
                  "{cg[{cGB{cg] Attack - {cP+50 to attack rating{c0\r\n\r\n"
                  "{cg[{cGC{cg] Evade  - {cP+50 to evasion rating{c0\r\n\r\n"
                  "{cg[{cGD{cg] Damage - {cP+15%% to damage{c0\r\n\r\n"
                  "{cyPlease choose a letter: {c0");
  SUB_STATE(d) = STATE_LOYAL;
}
void con_display_sex(Descriptor *d, int valid)
{
  line_sep(d);
  if (!valid)
    d->Output( "\r\nThat is not a valid sex...\r\n");
  d->Output( "\r\n{cyWhat is your sex {cGM{cgale{cy or {cGF{cgemale{cy?{c0 ");
  SUB_STATE(d) = STATE_QSEX;
}
void con_display_ansi(Descriptor *d)
{
  line_sep(d);
  d->Output( "\r\nDoes your mudclient support color (Y/N)? ");
  SUB_STATE(d) = STATE_ANSI;
}

void con_display_new_here(Descriptor *d)
{
  line_sep(d);
  d->Output( "\r\n"
                  "  {cyPlease select from the following:{c0\r\n\r\n"
                  "  {cG1 {cc- I am new to mudding.\r\n"
                  "  {cG2 {cc- I am familiar with mudding, but new to 4Dimensions.\r\n"
                  "  {cG3 {cc- I am an old hand at 4Dimensions.{c0\r\n\r\n"
                  "  Number:");
  SUB_STATE(d) = STATE_NEW_HERE;
}
void con_display_email(Descriptor *d)
{
  line_sep(d);
  d->Output( "\r\n"
                  "  {cWGiving your email is optional, but it helps us provide better service\r\n"
                  "  and security. Your email if given means you will get a notification should\r\n"
                  "  anything happen with the game server (new address, extended downtime, etc)\r\n"
                  "  If you lose your password, having a valid email address is the only way to\r\n"
                  "  get it back. Your details won't be given to any 3rd parties, and is for use\r\n"
                  "  within this game only.\r\n\r\n"
                  "  (type {ccnone{cW if you do not wish to give your email)\r\n\r\n"
                  "  {cyWhat is your email address? {c0");
  SUB_STATE(d) = STATE_EMAIL;
}

void line_sep(Descriptor *d)
{
  d->Output(
                  "[H[J"
                  "\r\n"
                  "\r\n"
                  "----------------------------------------------------------------------"
                  "\r\n"
                  "\r\n"
                 );
}
