/**************************************************************************
*   File: interpreter.h                                 Part of CircleMUD *
*  Usage: header file: public procs, macro defs, subcommand defines       *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */
#define READ_SIZE 256

#define ACMD(name)  \
   void name(Character *ch, char *argument, int cmd, int subcmd)

#define ACTION(name)  \
   long name(Character *ch, Character *vict, struct obj_data *obj, \
   struct room_data *room, int *num)

#define ASKILL(name)  \
   int name(Character *ch, Character *vict, \
   struct obj_data *obj, char *argument)

#define ASUB(name)  \
   int name(Character *ch, Character *vict, \
   struct obj_data *obj, char *argument)

ACMD(do_move);

#define CMD_NAME (complete_cmd_info[cmd].command)
#define CMD_IS(cmd_name) (!strcmp(cmd_name, complete_cmd_info[cmd].command))
#define IS_MOVE(cmdnum) (complete_cmd_info[cmdnum].command_pointer == do_move)

void command_interpreter(Character *ch, char *argument);
int search_block(char *arg, const char **list, int exact);
char lower(char c);
char *one_argument(char *argument, char *first_arg);
char *one_word(char *argument, char *first_arg);
char *any_one_arg(char *argument, char *first_arg);
char *two_arguments(char *argument, char *first_arg, char *second_arg);
int fill_word(char *argument);
void half_chop(char *string, char *arg1, char *arg2);
void nanny(struct descriptor_data *d, char *arg);
int is_abbrev(const char *arg1, const char *arg2);
int is_number(const char *str);
int find_command(const char *command);
void skip_spaces(char **string);
char *delete_doubledollar(char *string);
//
void write_aliases(Character *ch);
void read_aliases(Character *ch);
void delete_aliases(const char *charname);

/* WARNING: if you have added diagonal directions and have them at the
 * beginning of the command list.. change this value to 11 or 15 (depending) */
/* reserve these commands to come straight from the cmd list then start
 * sorting */
#define RESERVE_CMDS                7


/* for compatibility with 2.20: */
#define argument_interpreter(a, b, c) two_arguments(a, b, c)


struct command_info
{
  const char *command;
  const char *sort_as;
  byte minimum_position;
  void (*command_pointer) (Character * ch, char *argument, int cmd, int subcmd);
  sh_int minimum_level;
  int subcmd;
  long cmd_bits;
};

struct ignore
{
  char *ignore;
  struct ignore *next;
};

/*
 * Necessary for CMD_IS macro.  Borland needs the structure defined first
 * so it has been moved down here.
 */
cpp_extern struct command_info *complete_cmd_info;
extern const struct command_info cmd_info[];

/*
 * Alert! Changed from 'struct alias' to 'struct alias_data' in bpl15
 * because a Windows 95 compiler gives a warning about it having similiar
 * named member.
 */
struct alias_data
{
  char *alias;
  char *replacement;
  int type;
  struct alias_data *next;
};

#define ALIAS_SIMPLE	0
#define ALIAS_COMPLEX	1

#define ALIAS_SEP_CHAR	';'
#define ALIAS_VAR_CHAR	'$'
#define ALIAS_GLOB_CHAR	'*'

/*
 * SUBCOMMANDS
 *   You can define these however you want to, and the definitions of the
 *   subcommands are independent from function to function.
 */

#define SCMD_WAKE 1
#define SCMD_ROUSE 2
/* fencing */
#define SCMD_FENCE 	1
#define SCMD_GATE  	2

/* prompts */
//Normal Prompt
#define SCMD_PROMPT  1 
//Battle Prompt
#define SCMD_BPROMPT 2 

/* directions */
#define SCMD_NORTH	1
#define SCMD_EAST	2
#define SCMD_SOUTH	3
#define SCMD_WEST	4
#define SCMD_UP		5
#define SCMD_DOWN	6

/* do_gen_ps */
#define SCMD_INFO       0
#define SCMD_HANDBOOK   1
#define SCMD_CREDITS    2
#define SCMD_NEWS       3
#define SCMD_WIZLIST    4
#define SCMD_POLICIES   5
#define SCMD_VERSION    6
#define SCMD_IMMLIST    7
#define SCMD_MOTD	8
#define SCMD_IMOTD	9
#define SCMD_CLEAR	10
#define SCMD_WHOAMI	11

#define SCMD_SAY  0
#define SCMD_RSAY 1

/* do_gen_tog */
#define SCMD_NOSUMMON   0
#define SCMD_NOHASSLE   1
#define SCMD_BRIEF      2
#define SCMD_COMPACT    3
#define SCMD_NOTELL	4
#define SCMD_NOAUCTION	5
#define SCMD_DEAF	6
#define SCMD_NOGOSSIP	7
#define SCMD_NOGRATZ	8
#define SCMD_NOWIZ	9
#define SCMD_QUEST	10
#define SCMD_ROOMFLAGS	11
#define SCMD_NOREPEAT	12
#define SCMD_HOLYLIGHT	13
#define SCMD_SLOWNS	14
#define SCMD_AUTOEXIT	15
#define SCMD_AFK	16
#define SCMD_AUTOSPLIT  17
#define SCMD_AUTOLOOT   18
#define SCMD_AUTOASSIST 19
#define SCMD_AUTOGOLD   20
#define SCMD_ARENA      21
#define SCMD_XAP_OBJS   22
#define SCMD_KEEPTITLE  23
#define SCMD_NOIC	24
#define SCMD_TRACK	25
#define SCMD_NONEWBIE	26
#define SCMD_BATTLESPAM 27
#define SCMD_MAIL       28
#define SCMD_NOCTALK    29
#define SCMD_AFKTELL    30
#define SCMD_MOVEMSG    31
#define SCMD_MOUNTABLE  32
#define SCMD_NOHERO	33
#define SCMD_PTIME	34
#define SCMD_AUTOSAC    35
#define SCMD_CLS        36
#define SCMD_BUILDWALK  37
#define SCMD_COMPRESS   38
#define SCMD_AUTOZLIB   39
#define SCMD_NOOOC      40
#define SCMD_PAGEWRAP   41
#define SCMD_REPLYLOCK  42
#define SCMD_BUSY	43
#define SCMD_AGGRO	44
#define SCMD_NOBRAG	45
#define SCMD_NOGATE     46
#define SCMD_RP		47
#define SCMD_FISHTALLY  48
#define SCMD_NOTELEPORT 49
#define SCMD_AUTOGROUP  50

/* do_wizutil */
#define SCMD_REROLL	0
#define SCMD_PARDON     1
#define SCMD_NOTITLE    2
#define SCMD_SQUELCH    3
#define SCMD_FREEZE	4
#define SCMD_THAW	5
#define SCMD_UNAFFECT	6
#define SCMD_ZDELETE	7

#define SCMD_SILENCE    8

/*do_register (for pk) */
#define SCMD_REGISTER    0
#define SCMD_UNREGISTER    1

/* do_spec_com */
#define SCMD_WHISPER	0
#define SCMD_ASK	1

/* do_gen_com */
#define SCMD_HOLLER	0
#define SCMD_SHOUT	1
#define SCMD_GOSSIP	2
#define SCMD_AUCTION	3
#define SCMD_GRATZ	4
#define SCMD_IC		5
#define SCMD_NEWBIE1	6
#define SCMD_NEWBIE2	7
#define SCMD_HERO 	8
#define SCMD_NEWBIE	9
#define SCMD_OOC        10
/* do_shutdown */
#define SCMD_SHUTDOW	0
#define SCMD_SHUTDOWN   1

/* do_quit */
#define SCMD_QUI	0
#define SCMD_QUIT	1

/* do_date */
#define SCMD_DATE	0
#define SCMD_UPTIME	1

/* do_commands */
#define SCMD_COMMANDS	0
#define SCMD_SOCIALS	1
#define SCMD_WIZHELP	2

/* do_drop */
#define SCMD_DROP	0
#define SCMD_JUNK	1
#define SCMD_DONATE	2
#define SCMD_SPILL      3

/* do_gen_write */
#define SCMD_BUG	0
#define SCMD_TYPO	1
#define SCMD_IDEA	2

/* do_look */
#define SCMD_LOOK	0
#define SCMD_READ	1

/* do_qcomm */
#define SCMD_QSAY	0
#define SCMD_QECHO	1

/* do_pour */
#define SCMD_POUR	0
#define SCMD_FILL	1

/* do_poof */
#define SCMD_POOFIN	0
#define SCMD_POOFOUT	1

/* do_hit */
#define SCMD_HIT	0
#define SCMD_MURDER	1

/* do_eat */
#define SCMD_EAT	0
#define SCMD_TASTE	1
#define SCMD_DRINK	2
#define SCMD_SIP	3

/* do_use */
#define SCMD_USE	0
#define SCMD_QUAFF	1
#define SCMD_RECITE	2

/* do_echo */
#define SCMD_ECHO	0
#define SCMD_EMOTE	1
#define SCMD_POSE	2
#define SCMD_RECHO	3

/* do_gen_door */
#define SCMD_OPEN       0
#define SCMD_CLOSE      1
#define SCMD_UNLOCK     2
#define SCMD_LOCK       3
#define SCMD_PICK       4

/*. do_olc .*/
#define SCMD_OLC_REDIT  0
#define SCMD_OLC_OEDIT  1
#define SCMD_OLC_ZEDIT  2
#define SCMD_OLC_MEDIT  3
#define SCMD_OLC_SEDIT  4
#define SCMD_OLC_TRIGEDIT  5
#define SCMD_OLC_HEDIT  6
#define SCMD_OLC_AEDIT  7
#define SCMD_OLC_VEDIT 8

#define SCMD_OASIS_REDIT	0
#define SCMD_OASIS_OEDIT	1
#define SCMD_OASIS_ZEDIT	2
#define SCMD_OASIS_MEDIT	3
#define SCMD_OASIS_SEDIT	4
#define SCMD_OASIS_CEDIT	5
#define SCMD_OLC_SAVEINFO	7
#define SCMD_OASIS_RLIST 	8
#define SCMD_OASIS_MLIST	9
#define SCMD_OASIS_OLIST	10
#define SCMD_OASIS_SLIST	11
#define SCMD_OASIS_ZLIST        12
#define SCMD_OASIS_AEDIT        13
#define SCMD_OASIS_TRIGEDIT     14
#define SCMD_OASIS_TLIST        15
#define SCMD_OASIS_LINKS        16
#define SCMD_OASIS_HEDIT        17
#define SCMD_OASIS_VEDIT        18
#define SCMD_OASIS_VLIST        19

/*  do_innate  */
#define SCMD_SINNATE    0
#define SCMD_RINNATE    1

/* do trust bits */

#define WIZ_BAN_GRP     (1 <<  0)
#define WIZ_DSPLN_GRP	(1 <<  1)
#define WIZ_EDIT_GRP	(1 <<  2)
#define WIZ_HEAL_GRP	(1 <<  3)
#define WIZ_HOUSE_GRP	(1 <<  4)
#define WIZ_IMM1_GRP	(1 <<  5)
#define WIZ_IMM2_GRP	(1 <<  6)
#define WIZ_IMPL_GRP	(1 <<  7)
#define WIZ_KILL_GRP	(1 <<  8)
#define WIZ_LOAD_GRP	(1 <<  9)
#define WIZ_OLC_GRP	(1 << 10)
#define WIZ_QUEST_GRP	(1 << 11)
#define WIZ_SEN_GRP	(1 << 12)
#define WIZ_TELE_GRP	(1 << 13)
#define WIZ_TRIG_GRP	(1 << 14)
#define WIZ_MARRY_GRP	(1 << 15)
#define WIZ_GOTO_GRP    (1 << 16)
#define WIZ_GLOBAL_GRP    (1 << 17)
#define WIZ_HEDIT_GRP    (1 << 18)
#define WIZ_IMM3_GRP	(1 << 19)

/*do_give*/
#define SCMD_GIVE 0
#define SCMD_SLIP 1

/* * do_assemble * These constants *must* corespond with
     the ASSM_xxx constants in * assemblies.h. */
#define SCMD_ASSEMBLE  101
#define SCMD_BAKE      102
#define SCMD_BREW      103
#define SCMD_CRAFT     104
#define SCMD_FLETCH    105
#define SCMD_KNIT      106
#define SCMD_MAKE      107
#define SCMD_MIX       108
#define SCMD_THATCH    109
#define SCMD_WEAVE     110
#define SCMD_FORGE     111
