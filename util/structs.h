/**************************************************************************
*   File: structs.h                                     Part of CircleMUD *
*  Usage: header file for central structures and contstants               *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

/*
 * Intended use of this macro is to allow external packages to work with
 * a variety of CircleMUD versions without modifications.  For instance,
 * an IS_CORPSE() macro was introduced in pl13.  Any future code add-ons
 * could take into account the CircleMUD version and supply their own
 * definition for the macro if used on an older version of CircleMUD.
 * You are supposed to compare this with the macro CIRCLEMUD_VERSION()
 * in utils.h.  See there for usage.
 */
#define _CIRCLEMUD	0x030011 /* Major/Minor/Patchlevel - MMmmPP */

/*
 * If you want equipment to be automatically equipped to the same place
 * it was when players rented, set the define below to 1.  Please note
 * that this will require erasing or converting all of your rent files.
 * And of course, you have to recompile everything.  We need this feature
 * for CircleMUD 3.0 to be complete but we refuse to break binary file
 * compatibility.
 */
#define USE_AUTOEQ	0	/* TRUE/FALSE aren't defined yet. */

/* preamble *************************************************************/

/*
 * Eventually we want to be able to redefine the below to any arbitrary
 * value.  This will allow us to use unsigned data types to get more
 * room and also remove the '< 0' checks all over that implicitly
 * assume these values. -gg 12/17/99
 */
#define NOWHERE    -1    /* nil reference for room-database	*/
#define NOTHING	   -1    /* nil reference for objects		*/
#define NOBODY	   -1    /* nil reference for mobiles		*/

#define SPECIAL(name) \
   int (name)(Character *ch, void *me, int cmd, char *argument)

/* misc editor defines **************************************************/

/* format modes for format_text */
#define FORMAT_INDENT		(1 << 0)


/* room-related defines *************************************************/


/* The cardinal directions: used as index to Room.dir_option[] */
#define NORTH          0
#define EAST           1
#define SOUTH          2
#define WEST           3
#define UP             4
#define DOWN           5


/* Room flags: used in Room.room_flags */
/* WARNING: In the world files, NEVER set the bits marked "R" ("Reserved") */
#define ROOM_DARK		0   /* Dark			        */
#define ROOM_DEATH		1   /* Death trap		        */
#define ROOM_NOMOB		2   /* MOBs not allowed		        */
#define ROOM_INDOORS		3   /* Indoors			        */
#define ROOM_PEACEFUL		4   /* Violence not allowed	        */
#define ROOM_SOUNDPROOF		5   /* Shouts, gossip blocked	        */
#define ROOM_NOTRACK		6   /* Track won't go through	        */
#define ROOM_NOMAGIC		7   /* Magic not allowed		*/
#define ROOM_TUNNEL		8   /* room for only 1 pers	        */
#define ROOM_PRIVATE		9   /* Can't teleport in		*/
#define ROOM_GODROOM		10  /* LVL_GOD+ only allowed	        */
#define ROOM_HOUSE		11  /* (R) Room is a house	        */
#define ROOM_HOUSE_CRASH	12  /* (R) House needs saving	        */
#define ROOM_ATRIUM		13  /* (R) The door to a house	        */
#define ROOM_OLC		14  /* (R) Modifyable/!compress	        */
#define ROOM_BFS_MARK		15  /* (R) breath-first srch mrk	*/
#define ROOM_VEHICLE		16  /* Vehicles are allowed             */
#define ROOM_NORECALL           17  /* Can't recall out                 */
#define ROOM_ARENA	        18  /* Arena 				*/
#define ROOM_GOOD		19  /* Room is good to player		*/
#define ROOM_EVIL		20  /* Room is evil to player		*/
#define ROOM_HP			21  /* HP   Gain			*/
#define ROOM_MANA		22  /* Mana Gain			*/
#define ROOM_MOVE		23  /* Move Gain			*/
#define ROOM_ROLEPLAY		24  /* Allow only Role Players		*/
#define ROOM_NOTELEPORT_IN	25  /* Can't teleport into the room	*/
#define ROOM_NOTELEPORT_OUT	26  /* Can't teleport out of the room   */
#define ROOM_NOSUMMON_IN	27  /* Cant summon players into the room*/
#define ROOM_NOSUMMON_OUT	28  /* Cant summon players out of the room*/
#define ROOM_WILDERNESS		29  /* Wilderness room, used for automapping*/
#define ROOM_NOVIEW		30  /* Dont display the room on the map */
#define ROOM_DO_NOT_USE         31  /* DO NOT USE			*/
#define ROOM_GOLD_DEPOSIT       32  /* Room has gold in it              */
#define ROOM_SILVER_DEPOSIT     33  /* Room has silver in it            */
#define ROOM_COPPER_DEPOSIT     34  /* Room has copper in it            */
#define ROOM_IRON_DEPOSIT       35  /* Room has iron in it              */
#define ROOM_ANTIMONY_DEPOSIT   36  /* Room has antimony in it          */
#define ROOM_CHROMIUM_DEPOSIT   37  /* Room has chromium in it          */
#define ROOM_QUARRY             38  /* Room is a stone quarry           */
#define ROOM_COAL_MINE          39  /* Room has coal in it              */
#define ROOM_SPRING             40  /* Room has underground spring      */

#define ZONE_OPEN		(1 <<  0)
#define ZONE_CLOSED		(1 <<  1)
#define ZONE_NORECALL		(1 <<  2)
#define ZONE_NOSUMMON_OUT	(1 <<  3)
#define ZONE_NOTELEPORT_IN	(1 <<  4)
#define ZONE_NOSUMMON_IN	(1 <<  5)
#define ZONE_NOTELEPORT_OUT	(1 <<  6)

/* Exit info: used in Room.dir_option.exit_info */
#define EX_ISDOOR		(1 << 0)   /* Exit is a door		*/
#define EX_CLOSED		(1 << 1)   /* The door is closed	*/
#define EX_LOCKED		(1 << 2)   /* The door is locked	*/
#define EX_PICKPROOF		(1 << 3)   /* Lock can't be picked	*/
#define EX_HIDDEN		(1 << 4)   /* Exit is hidden 		*/


/* Sector types: used in Room.sector_type */
#define SECT_INSIDE         	 0   /* Indoors				*/
#define SECT_CITY           	 1   /* In a city			*/
#define SECT_FIELD          	 2   /* In a field			*/
#define SECT_FOREST         	 3   /* In a forest			*/
#define SECT_HILLS          	 4   /* In the hills			*/
#define SECT_MOUNTAIN       	 5   /* On a mountain			*/
#define SECT_WATER_SWIM     	 6   /* Swimmable water			*/
#define SECT_WATER_NOSWIM   	 7   /* Water - need a boat		*/
#define SECT_UNDERWATER	    	 8   /* Underwater			*/
#define SECT_FLYING         	 9   /* Wheee!				*/
#define SECT_DESERT         	10   /* In a Desert			*/
#define SECT_SPACE	    	11   /* In outer space			*/
#define SECT_ROAD		12   /* On a Road			*/
#define SECT_ENTRANCE		13   /* Entrance to a zone		*/
#define SECT_ATMOSPHERE		14   /* Entrance to a planet		*/
#define SECT_SUN		15   /* Into the Sun			*/
#define SECT_BLACKHOLE		16   /* Into a Black Hole		*/
#define SECT_VEHICLE		17   /* Internal use only		*/


/* char and mob-related defines *****************************************/


/* PC classes */
#define CLASS_UNDEFINED   -1
#define CLASS_MAGE        0
#define CLASS_PRIEST      1
#define CLASS_THIEF       2
#define CLASS_WARRIOR     3
#define CLASS_HUNTER      4
#define CLASS_RANGER      5
#define CLASS_GYPSY       6
#define CLASS_ESPER       7

#define NUM_CLASSES       8  /* This must be the number of classes!! */

/* NPC classes (currently unused - feel free to implement!) */
#define CLASS_OTHER       0
#define CLASS_UNDEAD      1
#define CLASS_HUMANOID    2
#define CLASS_ANIMAL      3
#define CLASS_DRAGON      4
#define CLASS_GIANT       5

/* Races */
#define RACE_UNDEFINED  -1
#define RACE_FAUN        0
#define RACE_CENTAUR     1
#define RACE_ELF         2
#define RACE_DWARF       3
#define RACE_INDIAN      4
#define RACE_GRINGO      5
#define RACE_MARTIAN     6
#define RACE_SPACE_WOLF  7
#define RACE_WORM        8
#define RACE_TOAD        9
#define RACE_BOAR       10
#define RACE_WOLF       11
#define RACE_LION       12

#define NUM_RACES       13

/* Sex */
#define SEX_NEUTRAL   0
#define SEX_MALE      1
#define SEX_FEMALE    2


/* Positions */
#define POS_DEAD       0	/* dead			*/
#define POS_MORTALLYW  1	/* mortally wounded	*/
#define POS_INCAP      2	/* incapacitated	*/
#define POS_STUNNED    3	/* stunned		*/
#define POS_SLEEPING   4	/* sleeping		*/
#define POS_RESTING    5	/* resting		*/
#define POS_SITTING    6	/* sitting		*/
#define POS_FIGHTING   7	/* fighting		*/
#define POS_STANDING   8	/* standing		*/


/* Player flags: used by Character.char_specials.act */
#define PLR_KILLER	0   /* Player is a player-killer		*/
#define PLR_THIEF	1   /* Player is a player-thief			*/
#define PLR_FROZEN	2   /* Player is frozen				*/
#define PLR_DONTSET     3   /* Don't EVER set (ISNPC bit)		*/
#define PLR_WRITING	4   /* Player writing (board/mail/olc)		*/
#define PLR_MAILING	5   /* Player is writing mail			*/
#define PLR_CRASH	6   /* Player needs to be crash-saved		*/
#define PLR_SITEOK	7   /* Player has been site-cleared		*/
#define PLR_NOSHOUT	8   /* Player not allowed to shout/goss		*/
#define PLR_NOTITLE	9   /* Player not allowed to set title		*/
#define PLR_DELETED	10  /* Player deleted - space reusable		*/
#define PLR_LOADROOM	11  /* Player uses nonstandard loadroom		*/
#define PLR_NOWIZLIST	12  /* Player shouldn't be on wizlist		*/
#define PLR_NODELETE	13  /* Player shouldn't be deleted		*/
#define PLR_INVSTART	14  /* Player should enter game wizinvis	*/
#define PLR_CRYO	15  /* Player is cryo-saved (purge prog)	*/
#define PLR_ROLEPLAYER  16  /* Player is a Role Player			*/
#define PLR_PK		17  /* Player participates in PK activites	*/
#define PLR_NEWBIE_HLPR	18  /* Player is a designated newbie helper	*/

#define MOB_RACE_HUMANOID 1	/* Mob carries money			*/
#define MOB_RACE_ANIMAL   2	/* Mob doesn't carry money		*/
#define MOB_RACE_EXOTIC   3	/* Mob could carry money		*/

/* Mobile flags: used by Character.char_specials.act */
#define MOB_SPEC         0  /* Mob has a callable spec-proc		*/
#define MOB_SENTINEL     1  /* Mob should not move			*/
#define MOB_SCAVENGER    2  /* Mob picks up stuff on the ground		*/
#define MOB_ISNPC        3  /* (R) Automatically set on all Mobs	*/
#define MOB_AWARE	 4  /* Mob can't be backstabbed			*/
#define MOB_AGGRESSIVE   5  /* Mob hits players in the room		*/
#define MOB_STAY_ZONE    6  /* Mob shouldn't wander out of zone		*/
#define MOB_WIMPY        7  /* Mob flees if severely injured		*/
#define MOB_AGGR_EVIL	 8  /* auto attack evil PC's			*/
#define MOB_AGGR_GOOD	 9  /* auto attack good PC's			*/
#define MOB_AGGR_NEUTRAL 10 /* auto attack neutral PC's			*/
#define MOB_MEMORY	 11 /* remember attackers if attacked		*/
#define MOB_HELPER	 12 /* attack PCs fighting other NPCs		*/
#define MOB_NOCHARM	 13 /* Mob can't be charmed			*/
#define MOB_NOSUMMON	 14 /* Mob can't be summoned			*/
#define MOB_NOSLEEP	 15 /* Mob can't be slept			*/
#define MOB_NOBASH	 16 /* Mob can't be bashed (e.g. trees)		*/
#define MOB_NOBLIND	 17 /* Mob can't be blinded			*/
#define MOB_MOUNTABLE	 18 /* Is the mob mountable? (DAK)		*/
#define MOB_NOPUSH	 19 /* Mob can't be pushed			*/
#define MOB_HERD       	 20 /* Mob is part of a herd or group		*/
#define MOB_NOSHOOT	 21 /* Mob can't be shot			*/
#define MOB_NOPOISON	 22 /* Mob can't be poisoned			*/
#define MOB_EDIBLE	 23 /* Can eat the mob's corpse			*/
#define MOB_SKINABLE	 24 /* Can skin the mob				*/
#define MOB_USABLE01     25 /* Mob can be mated with other animals	*/
#define MOB_USABLE02	 26 /* 						*/
#define MOB_UNDEAD	 27 /* Mob is undead				*/
#define MOB_NOFREEZE	 28 /* Mob can't be frozen			*/
#define MOB_CAN_MATE	 29 /* Mob can be mated with other animals      */

/* Preference flags: used by Character.player_specials.pref 		*/
#define PRF_BRIEF       0  /* Room descs won't normally be shown	*/
#define PRF_COMPACT     1  /* No extra CRLF pair before prompts		*/
#define PRF_DEAF	2  /* Can't hear shouts				*/
#define PRF_NOTELL	3  /* Can't receive tells			*/
#define PRF_DISPHP	4  /* Display hit points in prompt		*/
#define PRF_DISPMANA	5  /* Display mana points in prompt		*/
#define PRF_DISPMOVE	6  /* Display move points in prompt		*/
#define PRF_AUTOEXIT	7  /* Display exits in a room			*/
#define PRF_NOHASSLE	8  /* Aggr mobs won't attack			*/
#define PRF_QUEST	9  /* On quest					*/
#define PRF_SUMMONABLE	10 /* Can be summoned				*/
#define PRF_NOREPEAT	11 /* No repetition of comm commands		*/
#define PRF_HOLYLIGHT	12 /* Can see in dark				*/
#define PRF_COLOR_1	13 /* Color (low bit)				*/
#define PRF_COLOR_2	14 /* Color (high bit)				*/
#define PRF_NOWIZ	15 /* Can't hear wizline			*/
#define PRF_LOG1	16 /* On-line System Log (low bit)		*/
#define PRF_LOG2	17 /* On-line System Log (high bit)		*/
#define PRF_NOAUCT	18 /* Can't hear auction channel		*/
#define PRF_NOGOSS	19 /* Can't hear gossip channel			*/
#define PRF_NOGRATZ	20 /* Can't hear grats channel			*/
#define PRF_ROOMFLAGS	21 /* Can see room flags (ROOM_x)		*/
#define PRF_AUTOASSIST  22 /* Autoassists in a fight			*/
#define PRF_AFK		23 /* Player is afk				*/
#define PRF_AUTOSPLIT	24 /* Player autosplits				*/
#define PRF_AUTOLOOT	25 /* Player autloots				*/
#define PRF_AUTOGOLD	27 /* Player gets gold automatically    	*/
#define PRF_ARENA	28 /* can see arena messages			*/
#define PRF_NONEWBIE	29 /* Can't hear the newbie channel		*/
#define PRF_KEEPTITLE	30 /* Player keeps title upon levelling		*/
#define PRF_NOIC	31 /* Player can't use this channel		*/
#define PRF_BATTLESPAM  32 /* Player will not see battlespam            */
#define PRF_MAIL        33 /* Player will not see mail message in prompt*/
#define PRF_BUSY	47 /* Player is busy				*/
#define PRF_AGGRO	48 /* Player is aggro				*/
#define PRF_NOBRAG      49 /* player can't hear Brags 			*/
#define PRF_RP		50 /* Player is roleplaying			*/

/* Affect bits: used in Character.char_specials.saved.affected_by */
/* WARNING: In the world files, NEVER set the bits marked "R" ("Reserved") */
#define AFF_DONOTUSE          0
#define AFF_BLIND             1 	   /* (R) Char is blind		*/
#define AFF_INVISIBLE         2	           /* Char is invisible		*/
#define AFF_DETECT_ALIGN      3	           /* Char is sensitive to align*/
#define AFF_DETECT_INVIS      4            /* Char can see invis chars  */
#define AFF_DETECT_MAGIC      5	           /* Char is sensitive to magic*/
#define AFF_SENSE_LIFE        6	           /* Char can sense hidden life*/
#define AFF_WATERWALK	      7	           /* Char can walk on water	*/
#define AFF_SANCTUARY         8	           /* Char protected by sanct.	*/
#define AFF_GROUP             9	           /* (R) Char is grouped	*/
#define AFF_CURSE             10	   /* Char is cursed		*/
#define AFF_INFRAVISION       11	   /* Char can see in dark	*/
#define AFF_POISON_1          12	   /* (R) Char is poisoned	*/
#define AFF_PROTECT_EVIL      13	   /* Char protected from evil  */
#define AFF_PROTECT_GOOD      14	   /* Char protected from good  */
#define AFF_SLEEP             15           /* (R) Char magically asleep	*/
#define AFF_NOTRACK	      16	   /* Char can't be tracked	*/
#define AFF_TAMED   	      17	   /* Char has been tamed (DAK) */
#define AFF_POLYMORPH	      18	   /* Char is polymorphed	*/
#define AFF_SNEAK             19	   /* Char can move quietly	*/
#define AFF_HIDE              20	   /* Char is hidden		*/
#define AFF_FLY               21	   /* Room for future expansion	*/
#define AFF_CHARM             22	   /* Char is charmed		*/
#define AFF_HOLD              23           /* Char is snared            */
#define AFF_GILLS	      24	   /* Char can breath underwater*/
#define AFF_UNUSED     	      25	   /* Unused			*/
#define AFF_POISON_2	      26	   /* Char is poisoned level 2  */
#define AFF_POISON_3	      27           /* Char is poisoned level 3  */
#define AFF_POISON_4   	      28	   /* Char is poisoned level 4  */
#define AFF_FIRE_SHIELD	      29	   /* Fire Shield		*/
#define AFF_STONESKIN	      30	   /* Skin is stone		*/
#define AFF_DONOTUSE	      31	   /* DO NOT USE		*/
#define AFF_HASTE	      32	   /* Char moves faster	        */
#define AFF_SHIELD	      33	   /* dam/2 from missiles	*/
#define AFF_PROT_FIRE	      34	   /* Char is prot from fire	*/
#define AFF_FREEZING	      35	   /* Char is freezing		*/
#define AFF_ACIDED	      36	   /* Char is covered with acid */
#define AFF_BURNING	      37	   /* Char is on fire		*/
#define AFF_PROT_COLD	      38	   /* Char is prot from cold	*/


/* Modes of connectedness: used by descriptor_data.state */
#define CON_PLAYING	    0		/* Playing - Nominal state	*/
#define CON_CLOSE	    1		/* Disconnecting		*/
#define CON_GET_NAME	    2		/* By what name ..?		*/
#define CON_NAME_CNFRM	    3		/* Did I get that right, x?	*/
#define CON_PASSWORD	    4		/* Password:			*/
#define CON_NEWPASSWD	    5		/* Give me a password for x	*/
#define CON_CNFPASSWD	    6		/* Please retype password:	*/
#define CON_QSEX	    7		/* Sex?				*/
#define CON_QCLASS	    8		/* Class?			*/
#define CON_RMOTD	    9		/* PRESS RETURN after MOTD	*/
#define CON_MENU	   10		/* Your choice: (main menu)	*/
#define CON_EXDESC	   11		/* Enter a new description:	*/
#define CON_CHPWD_GETOLD   12		/* Changing passwd: get old	*/
#define CON_CHPWD_GETNEW   13		/* Changing passwd: get new	*/
#define CON_CHPWD_VRFY     14		/* Verify new password		*/
#define CON_DELCNF1	   15		/* Delete confirmation 1	*/
#define CON_DELCNF2	   16		/* Delete confirmation 2	*/
#define CON_DISCONNECT	   17		/* In-game disconnection	*/
#define CON_OEDIT          18		/*. OLC mode - object edit     .*/
#define CON_REDIT          19		/*. OLC mode - room edit       .*/
#define CON_ZEDIT          20		/*. OLC mode - zone info edit  .*/
#define CON_MEDIT          21		/*. OLC mode - mobile edit     .*/
#define CON_SEDIT          22		/*. OLC mode - shop edit       .*/
#define CON_TRIGEDIT       23		/*. OLC mode - trigger edit    .*/
#define CON_HEDIT	   24		/*. OLC mode - help editor     .*/
#define CON_AEDIT	   25		/*. OLC mode - action editor   .*/
#define CON_TEXTED	   26		/*. OLC mode - text editor     .*/
#define CON_QRACE	   27		/* Race Selection		*/
#define CON_CONFIRM_QRACE  28           /* Confirm Race Selection       */
#define CON_CONFIRM_QCLASS 29           /* Confirm Class Selection      */
#define CON_CONFIRM_STATS  30		/* Roll stats 			*/

/* Character equipment positions: used as index for Character.equipment[] */
/* NOTE: Don't confuse these constants with the ITEM_ bitvectors
   which control the valid places you can wear a piece of equipment */
#define WEAR_LIGHT      0
#define WEAR_FINGER_R   1
#define WEAR_FINGER_L   2
#define WEAR_NECK_1     3
#define WEAR_NECK_2     4
#define WEAR_BODY       5
#define WEAR_HEAD       6
#define WEAR_LEGS       7
#define WEAR_FEET       8
#define WEAR_HANDS      9
#define WEAR_ARMS      10
#define WEAR_SHIELD    11
#define WEAR_ABOUT     12
#define WEAR_WAIST     13
#define WEAR_WRIST_R   14
#define WEAR_WRIST_L   15
#define WEAR_WIELD     16
#define WEAR_HOLD      17
#define WEAR_FACE      18
#define WEAR_EYES      19
#define WEAR_HIPS      20
#define WEAR_EAR_R     21
#define WEAR_EAR_L     22
#define WEAR_ANKLE_R   23
#define WEAR_ANKLE_L   24
#define WEAR_HORNS     25
#define WEAR_ANTENNA   26
#define WEAR_TAIL      27
#define WEAR_WIELD_2   28
#define WEAR_LEGS_2    29
#define WEAR_FEET_2    30

#define NUM_WEARS      31	/* This must be the # of eq positions!! */

//**  This controls which body part that a race has.. It does not
//** control where an item can be worn, but rather if it can be
//** worn by that player or race...
#define BODY_LIGHT      (1 <<  0)
#define BODY_FINGER_R   (1 <<  1)
#define BODY_FINGER_L   (1 <<  2)
#define BODY_NECK_1     (1 <<  3)
#define BODY_NECK_2     (1 <<  4)
#define BODY_BODY       (1 <<  5)
#define BODY_HEAD       (1 <<  6)
#define BODY_LEGS       (1 <<  7)
#define BODY_FEET       (1 <<  8)
#define BODY_HANDS      (1 <<  9)
#define BODY_ARMS       (1 << 10)
#define BODY_SHIELD     (1 << 11)
#define BODY_ABOUT      (1 << 12)
#define BODY_WAIST      (1 << 13)
#define BODY_WRIST_R    (1 << 14)
#define BODY_WRIST_L    (1 << 15)
#define BODY_WIELD      (1 << 16)
#define BODY_HOLD       (1 << 17)
#define BODY_FACE	(1 << 18)
#define BODY_EYES	(1 << 19)
#define BODY_HIPS	(1 << 20)
#define BODY_EAR_R	(1 << 21)
#define BODY_EAR_L	(1 << 22)
#define BODY_ANKLE_R	(1 << 23)
#define BODY_ANKLE_L	(1 << 24)
#define BODY_HORNS      (1 << 25)
#define BODY_ANTENNA    (1 << 26)
#define BODY_TAIL       (1 << 27)
#define BODY_WIELD_2    (1 << 28)
#define BODY_LEGS_2	(1 << 29)
#define BODY_FEET_2	(1 << 30)

/* object-related defines ********************************************/

/* Item types: used by obj_data.obj_flags.type_flag */
#define ITEM_LIGHT       	1	/* Item is a light source	*/
#define ITEM_SCROLL      	2	/* Item is a scroll		*/
#define ITEM_WAND        	3	/* Item is a wand		*/
#define ITEM_STAFF       	4	/* Item is a staff		*/
#define ITEM_WEAPON      	5	/* Item is a weapon		*/
#define ITEM_FIREWEAPON  	6	/* Unimplemented		*/
#define ITEM_MISSILE     	7	/* Unimplemented		*/
#define ITEM_TREASURE    	8	/* Item is a treasure, not gold	*/
#define ITEM_ARMOR       	9	/* Item is armor		*/
#define ITEM_POTION      	10 	/* Item is a potion		*/
#define ITEM_WORN        	11	/* Unimplemented		*/
#define ITEM_OTHER       	12	/* Misc object			*/
#define ITEM_TRASH       	13	/* Trash - shopkeeps won't buy	*/
#define ITEM_TRAP        	14	/* Unimplemented		*/
#define ITEM_CONTAINER   	15	/* Item is a container		*/
#define ITEM_NOTE        	16	/* Item is note 		*/
#define ITEM_DRINKCON    	17	/* Item is a drink container	*/
#define ITEM_KEY         	18	/* Item is a key		*/
#define ITEM_FOOD        	19	/* Item is food			*/
#define ITEM_MONEY       	20	/* Item is money (gold)		*/
#define ITEM_PEN         	21	/* Item is a pen		*/
#define ITEM_BOAT        	22	/* Item is a boat		*/
#define ITEM_FOUNTAIN    	23	/* Item is a fountain		*/
#define ITEM_THROW       	24      /* Item can be thrown as weapon */
#define ITEM_GRENADE     	25      /* Item is a grenade      	*/
#define ITEM_BOW         	26      /* shoots arrows 		*/
#define ITEM_SLING       	27      /* shoots rocks 		*/
#define ITEM_CROSSBOW    	28      /* shoots bolts 		*/
#define ITEM_BOLT        	29      /* ammo for crossbow		*/
#define ITEM_ARROW       	30      /* ammo for bow			*/
#define ITEM_ROCK        	31      /* ammo for sling		*/
#define ITEM_VEHICLE     	32      /* vehicle object		*/
#define ITEM_V_CONTROLS  	33      /* vehicle steering wheel	*/
#define ITEM_V_HATCH     	34      /* vehicle exit			*/
#define ITEM_V_WINDOW    	35      /* vehicle window		*/
#define ITEM_PORTAL      	36      /* item is a portal(ROOM)	*/
#define ITEM_GUN         	37      /* item is a firearm		*/
#define ITEM_AMMO        	38      /* item is ammo for a firearm	*/
#define ITEM_WINGS	 	39	/* item allows you to fly	*/
#define ITEM_SPACESUIT	 	40	/* item is a spacesuit		*/
#define ITEM_AQUALUNG	 	41	/* item is an aqualung		*/
#define ITEM_CLIMBABLE   	42	/* can climb item		*/
#define ITEM_POISON_1    	43	/* level 1 poison - low hp gain */
#define ITEM_POISON_2	 	44	/* level 2 poison - no hp gain	*/
#define ITEM_POISON_3	 	45	/* level 3 poison - hp loss	*/
#define ITEM_POISON_4	 	46	/* level 4 poison - no cure	*/
#define ITEM_ANTIDOTE_1	 	47	/* cure for level 1 poison	*/
#define ITEM_ANTIDOTE_2  	48	/* cure for level 2 poison	*/
#define ITEM_ANTIDOTE_3  	49	/* cure for level 3 poison	*/
#define ITEM_DESCENDABLE 	50	/* can climb down object	*/
#define ITEM_PORTAL_BUSH 	51	/* item is a portal (bush)	*/
#define ITEM_PORTAL_WATER	52	/* item is a portal (water)	*/
#define ITEM_PORTAL_HOLE	53	/* item is a portal (hold)	*/
#define ITEM_MEAT               54      /* item is meat                 */
#define ITEM_NUGGET             55      /* item is a gold nugger        */
#define ITEM_METAL_DETECTOR     56      /* a metal detector             */
#define ITEM_TREE               57      /* item is lumber               */
#define ITEM_OAK_BARK           58      /* oak-bark for tanning         */
#define ITEM_ANVIL              59      /* an anvil to hammer on        */
#define ITEM_HAMMER             60      /* a hammer                     */
#define ITEM_GRINDSTONE         61      /* grindstone to sharpen items  */
#define ITEM_OIL                62      /* oil for the grindstone       */
#define ITEM_CLUB               63      /* weapon type club             */
#define ITEM_AXE                64      /* weapon type axe              */
#define ITEM_SWORD              65      /* weapon type sword            */
#define ITEM_DAGGER             66      /* weapon type dagger           */
#define ITEM_WHIP               67      /* weapon type whip             */
#define ITEM_MACE               68      /* weapon type mace             */
#define ITEM_LANCE              69      /* weapon type lance            */
#define ITEM_PICKAXE            70      /* a pickaxe for mining         */
#define ITEM_GALL_NUT           71      /* gall nuts???                 */
#define ITEM_SKIN           	72      /* mobs skin?                   */
#define ITEM_FURNITURE          73      /* item is furniture            */
#define ITEM_PORTAL_HURDLE	74	/* item is a portal (hurdle)	*/
#define ITEM_THERMAL_PROT 	75	/* item protects from heat	*/

/* Take/Wear flags: used by obj_data.obj_flags.wear_flags */
#define ITEM_WEAR_TAKE		0  /* Item can be taken		*/
#define ITEM_WEAR_FINGER	1  /* Can be worn on finger	*/
#define ITEM_WEAR_NECK		2  /* Can be worn around neck 	*/
#define ITEM_WEAR_BODY		3  /* Can be worn on body 	*/
#define ITEM_WEAR_HEAD		4  /* Can be worn on head 	*/
#define ITEM_WEAR_LEGS		5  /* Can be worn on legs	*/
#define ITEM_WEAR_FEET		6  /* Can be worn on feet	*/
#define ITEM_WEAR_HANDS		7  /* Can be worn on hands	*/
#define ITEM_WEAR_ARMS		8  /* Can be worn on arms	*/
#define ITEM_WEAR_SHIELD	9  /* Can be used as a shield	*/
#define ITEM_WEAR_ABOUT		10 /* Can be worn about body 	*/
#define ITEM_WEAR_WAIST 	11 /* Can be worn around waist 	*/
#define ITEM_WEAR_WRIST		12 /* Can be worn on wrist 	*/
#define ITEM_WEAR_WIELD		13 /* Can be wielded		*/
#define ITEM_WEAR_HOLD		14 /* Can be held		*/
#define ITEM_WEAR_FACE          15
#define ITEM_WEAR_EYES		16
#define ITEM_WEAR_HIPS          17
#define ITEM_WEAR_EAR           18
#define ITEM_WEAR_ANKLE         19
#define ITEM_WEAR_HORNS         20
#define ITEM_WEAR_ANTENNA       21
#define ITEM_WEAR_TAIL          22


/* Extra object flags: used by obj_data.obj_flags.extra_flags */
#define ITEM_GLOW          	0   /* Item is glowing			*/
#define ITEM_HUM           	1   /* Item is humming			*/
#define ITEM_NORENT        	2   /* Item cannot be rented		*/
#define ITEM_NODONATE      	3   /* Item cannot be donated		*/
#define ITEM_NOINVIS	   	4   /* Item cannot be made invis	*/
#define ITEM_INVISIBLE     	5   /* Item is invisible		*/
#define ITEM_MAGIC         	6   /* Item is magical			*/
#define ITEM_NODROP        	7   /* Item is cursed: can't drop	*/
#define ITEM_BLESS         	8   /* Item is blessed			*/
#define ITEM_ANTI_GOOD     	9   /* Not usable by good people	*/
#define ITEM_ANTI_EVIL     	10  /* Not usable by evil people	*/
#define ITEM_ANTI_NEUTRAL  	11  /* Not usable by neutral people	*/
#define ITEM_ANTI_MAGE		12  /* Not usable by mages		*/
#define ITEM_ANTI_PRIEST   	13  /* Not usable by clerics		*/
#define ITEM_ANTI_THIEF	   	14  /* Not usable by thieves		*/
#define ITEM_ANTI_WARRIOR  	15  /* Not usable by warriors		*/
#define ITEM_NOSELL	   	16  /* Shopkeepers won't touch it	*/
#define ITEM_ANTI_FAUN     	17  /* Not usable by fauns          	*/
#define ITEM_ANTI_CENTAUR  	18  /* Not usable by centaurs       	*/
#define ITEM_ANTI_ELF      	19  /* Not usable by elves          	*/
#define ITEM_ANTI_DWARF    	20  /* Not usable by dwarves        	*/
#define ITEM_LIVE_GRENADE  	21  /* grenade's pin has been pulled	*/
#define ITEM_ANTI_INDIAN   	22  /* Not usable by Indians		*/
#define ITEM_ANTI_GRINGO   	23  /* Not usable by Gringos		*/
#define ITEM_ANTI_MARTIAN  	24  /* Not usable by Martians		*/
#define ITEM_ANTI_SPACE_WOLF 	25  /* Not usable by Space Wolves   	*/
#define ITEM_ANTI_HUNTER   	26  /* Not usable by Hunters	        */
#define ITEM_ANTI_RANGER        27  /* Not usable by Rangers	        */
#define ITEM_ANTI_GYPSY     	28  /* Not usable by Gypsys 	        */
#define ITEM_ANTI_ESPER    	29  /* Not usable by Espers 	        */
#define ITEM_MELT_DROP	   	30  /* Disappears if dropped	 	*/
#define ITEM_BURIED        	31  /* Item is buried			*/
#define ITEM_PC_CORPSE	   	32  /* Item is a players corpse		*/
#define ITEM_NPC_CORPSE	   	33  /* Item is a NPC corpse		*/
#define ITEM_ARTIFACT      	34  /* Item is an Artifact - QIC        */
#define	ITEM_UNIQUE_SAVE   	35  /* Item is unique			*/
#define ITEM_NO_LOCATE     	36  /* Item can't be located with spell */
#define ITEM_HIDDEN	   	37  /* Item is hidden			*/
#define ITEM_POISONED_1	   	38  /* Item has been poisoned		*/
#define ITEM_POISONED_2	   	39  /* Item has been poisoned level 2	*/
#define ITEM_POISONED_3	   	40  /* Item has been poisoned level 3   */
#define ITEM_POISONED_4    	41  /* Item has been poisoned level 4	*/
#define ITEM_EDIBLE	   	42  /* Item is edible			*/
#define ITEM_MOB_SKIN      	43  /* Item is a skin			*/
#define ITEM_NO_DISARM	   	44  /* Item can't be disarmed if wielded*/
#define ITEM_ANTI_MALE		45  /* Item can't be used by a male 	*/
#define ITEM_ANTI_FEMALE	46  /* Item can't be used by a female 	*/
#define ITEM_CONTRACEPTIVE	47  /* Item is a contraceptive		*/
#define ITEM_ANTI_MEXICAN	48  /* Item can't be used by a Mexican	*/
#define ITEM_ANTI_CYBORG	49  /* Item can't be used by a Cyborg	*/

/* Modifier constants used with obj affects ('A' fields) */
#define APPLY_NONE              0	/* No effect			*/
#define APPLY_STR               1	/* Apply to strength		*/
#define APPLY_DEX               2	/* Apply to dexterity		*/
#define APPLY_INT               3	/* Apply to constitution	*/
#define APPLY_WIS               4	/* Apply to wisdom		*/
#define APPLY_CON               5	/* Apply to constitution	*/
#define APPLY_CHA		6	/* Apply to charisma		*/
#define APPLY_CLASS             7	/* Reserved			*/
#define APPLY_LEVEL             8	/* Reserved			*/
#define APPLY_AGE               9	/* Apply to age			*/
#define APPLY_CHAR_WEIGHT      10	/* Apply to weight		*/
#define APPLY_CHAR_HEIGHT      11	/* Apply to height		*/
#define APPLY_MANA             12	/* Apply to max mana		*/
#define APPLY_HIT              13	/* Apply to max hit points	*/
#define APPLY_MOVE             14	/* Apply to max move points	*/
#define APPLY_GOLD             15	/* Reserved			*/
#define APPLY_EXP              16	/* Reserved			*/
#define APPLY_AC               17	/* Apply to Armor Class		*/
#define APPLY_HITROLL          18	/* Apply to hitroll		*/
#define APPLY_DAMROLL          19	/* Apply to damage roll		*/
#define APPLY_SAVING_PARA      20	/* Apply to save throw: paralz	*/
#define APPLY_SAVING_ROD       21	/* Apply to save throw: rods	*/
#define APPLY_SAVING_PETRI     22	/* Apply to save throw: petrif	*/
#define APPLY_SAVING_BREATH    23	/* Apply to save throw: breath	*/
#define APPLY_SAVING_SPELL     24	/* Apply to save throw: spells	*/
#define APPLY_RACE             25       /* Apply to race                */


/* Container flags - value[1] */
#define CONT_CLOSEABLE      (1 << 0)	/* Container can be closed	*/
#define CONT_PICKPROOF      (1 << 1) 	/* Container is pickproof	*/
#define CONT_CLOSED         (1 << 2)	/* Container is closed		*/
#define CONT_LOCKED         (1 << 3)	/* Container is locked		*/


/* Some different kind of liquids for use in values of drink containers */
#define LIQ_WATER      0
#define LIQ_BEER       1
#define LIQ_WINE       2
#define LIQ_ALE        3
#define LIQ_DARKALE    4
#define LIQ_WHISKY     5
#define LIQ_LEMONADE   6
#define LIQ_FIREBRT    7
#define LIQ_LOCALSPC   8
#define LIQ_SLIME      9
#define LIQ_MILK       10
#define LIQ_TEA        11
#define LIQ_COFFE      12
#define LIQ_BLOOD      13
#define LIQ_SALTWATER  14
#define LIQ_CLEARWATER 15


/* other miscellaneous defines *******************************************/


/* Player conditions */
#define DRUNK        0
#define FULL         1
#define THIRST       2


/* Sun state for sunlight */
#define SUN_DARK	0
#define SUN_RISE	1
#define SUN_LIGHT	2
#define SUN_SET		3


/* Sky conditions for weather */
#define SKY_CLOUDLESS	0
#define SKY_CLOUDY	1
#define SKY_RAINING	2
#define SKY_LIGHTNING	3


/* Rent codes */
#define RENT_UNDEF      0
#define RENT_CRASH      1
#define RENT_RENTED     2
#define RENT_CRYO       3
#define RENT_FORCED     4
#define RENT_TIMEDOUT   5

/* Romance Codes */
#define SINGLE          0
#define DATING          1
#define ENGAGED         2
#define MARRIED         3
#define ASKED_OUT       4
#define PROPOSED_TO     5
#define ASKING          6
/* MatingMod Codes */
#define CANT            -69
#define MALE            -2
#define NOT_PREG        -1
#define ASKING_P	-3

/* Array stuff */
#define RF_ARRAY_MAX    4
#define PM_ARRAY_MAX    4
#define PR_ARRAY_MAX    4
#define AF_ARRAY_MAX    4
#define TW_ARRAY_MAX    4
#define EF_ARRAY_MAX    4

/* Auctioning states */

#define AUC_NULL_STATE          0   /* not doing anything */
#define AUC_OFFERING            1   /* object has been offfered */
#define AUC_GOING_ONCE          2   /* object is going once! */
#define AUC_GOING_TWICE         3   /* object is going twice! */
#define AUC_LAST_CALL           4   /* last call for the object! */
#define AUC_SOLD                5   /* Auction cancle states */
#define AUC_NORMAL_CANCEL       6   /* normal cancellation of auction */
#define AUC_QUIT_CANCEL         7   /* auction canclled because player quit */
#define AUC_WIZ_CANCEL          8   /* auction cancelled by a god */
/* Other auctioneer functions */
#define AUC_STAT                9   
#define AUC_BID                 10  


/* Character.internal_flags (INT_XXX) ************************************/
#define INT_MARK	(1 <<  0)


/* other #defined constants **********************************************/

/*
 * **DO**NOT** blindly change the number of levels in your MUD merely by
 * changing these numbers and without changing the rest of the code to match.
 * Other changes throughout the code are required.  See coding.doc for
 * details.
 *
 * LVL_IMPL should always be the HIGHEST possible immortal level, and
 * LVL_IMMORT should always be the LOWEST immortal level.  The number of
 * mortal levels will always be LVL_IMMORT - 1.
 */
#define LVL_IMPL	56
#define LVL_SEN		55
#define LVL_BLD	        54
#define LVL_NBLD	53
#define LVL_GOD		52
#define LVL_IMMORT	51
#define LVL_HERO	LVL_IMMORT

/* Level of the 'freeze' command */
#define LVL_FREEZE	LVL_GRGOD
#define LVL_GRGOD	LVL_SEN

#define NUM_OF_DIRS	6	/* number of directions in a room (nsewud) */
#define MAGIC_NUMBER	(0x06)	/* Arbitrary number that won't be in a string */

#define OPT_USEC	100000	/* 10 passes per second */
#define PASSES_PER_SEC	(1000000 / OPT_USEC)
#define RL_SEC		* PASSES_PER_SEC

#define PULSE_ZONE      (10 RL_SEC)
#define PULSE_MOBILE    (10 RL_SEC)
#define PULSE_VIOLENCE  ( 2 RL_SEC)
#define PULSE_AUCTION   (60 RL_SEC)

/* Variables for the output buffering system */
#define MAX_SOCK_BUF            (12 * 1024) /* Size of kernel's sock buf   */
#define MAX_PROMPT_LENGTH       96          /* Max length of prompt        */
#define GARBAGE_SPACE		32          /* Space for **OVERFLOW** etc  */
#define SMALL_BUFSIZE		1024        /* Static output buffer size   */
/* Max amount of output that can be buffered */
#define LARGE_BUFSIZE	   (MAX_SOCK_BUF - GARBAGE_SPACE - MAX_PROMPT_LENGTH)

#define HISTORY_SIZE		5	/* Keep last 5 commands. */
#define MAX_STRING_LENGTH	16384
#define MAX_INPUT_LENGTH	512	/* Max length per *line* of input */
#define MAX_RAW_INPUT_LENGTH	1024	/* Max size of *raw* input */
#define MAX_MESSAGES		400
#define MAX_NAME_LENGTH		20  /* Used in char_file_u *DO*NOT*CHANGE* */
#define MAX_PWD_LENGTH		10  /* Used in char_file_u *DO*NOT*CHANGE* */
#define MAX_TITLE_LENGTH	80  /* Used in char_file_u *DO*NOT*CHANGE* */
#define HOST_LENGTH		30  /* Used in char_file_u *DO*NOT*CHANGE* */
#define EXDSCR_LENGTH		240 /* Used in char_file_u *DO*NOT*CHANGE* */
#define MAX_TONGUE		3   /* Used in char_file_u *DO*NOT*CHANGE* */
#define MAX_SKILLS		200 /* Used in char_file_u *DO*NOT*CHANGE* */
#define MAX_AFFECT		32  /* Used in char_file_u *DO*NOT*CHANGE* */
#define MAX_OBJ_AFFECT		6 /* Used in obj_file_elem *DO*NOT*CHANGE* */

/*
 * A MAX_PWD_LENGTH of 10 will cause BSD-derived systems with MD5 passwords
 * and GNU libc 2 passwords to be truncated.  On BSD this will enable anyone
 * with a name longer than 5 character to log in with any password.  If you
 * have such a system, it is suggested you change the limit to 20.
 *
 * Please note that this will erase your player files.  If you are not
 * prepared to do so, simply erase these lines but heed the above warning.
 */
#if defined(HAVE_UNSAFE_CRYPT) && MAX_PWD_LENGTH == 10
#error You need to increase MAX_PWD_LENGTH to at least 20.
#error See the comment near these errors for more explanation.
#endif

/**********************************************************************
* Structures                                                          *
**********************************************************************/


typedef signed char		sbyte;
typedef unsigned char		ubyte;
typedef signed short int	sh_int;
typedef unsigned short int	ush_int;
#if !defined(__cplusplus)	/* Anyone know a portable method? */
typedef char			bool;
#endif

#if !defined(CIRCLE_WINDOWS) || defined(LCC_WIN32)    /* Hm, sysdep.h? */
typedef char			byte;
#endif

typedef int	room_vnum;	/* A room's vnum type 			*/
typedef int	obj_vnum;	/* An object's vnum type 		*/
typedef int	mob_vnum;	/* A mob's vnum type 			*/
typedef int  	zone_vnum;	/* A virtual zone number		*/

typedef int	room_rnum;	/* A room's real (internal) number type */
typedef int	obj_rnum;	/* An object's real (internal) num type */
typedef int	mob_rnum;	/* A mobile's real (internal) num type  */
typedef int	zone_rnum;	/* A real zone number			*/

/*
 * Bitvector type for 32 bit unsigned long bitvectors.
 * 'unsigned long long' will give you at least 64 bits if you have GCC.
 *
 * Since we don't want to break the pfiles, you'll have to search throughout
 * the code for "bitvector_t" and change them yourself if you'd like this
 * extra flexibility.
 */
typedef unsigned long int     bitvector_t;

/* Extra description: used in objects, mobiles, and rooms */
struct extra_descr_data {
   char	*keyword;                 /* Keyword in look/examine          */
   char	*description;             /* What to see                      */
   struct extra_descr_data *next; /* Next in list                     */
};



/* object-related structures ******************************************/

#define NUM_OBJ_VAL_POSITIONS 	10

/* object flags; used in obj_data */
struct obj_flag_data {
   int	value[NUM_OBJ_VAL_POSITIONS];	/* Values of the item (see list) */
   byte type_flag;			/* Type of item			 */
   int	wear_flags[TW_ARRAY_MAX];	/* Where you can wear it         */
   int	extra_flags[EF_ARRAY_MAX];	/* If it hums, glows, etc.       */
   int	weight;				/* Weight what else              */
   int	cost;				/* Value when sold (gp.)         */
   int	cost_per_day;			/* Cost to keep pr. real day     */
   int	timer;				/* Timer for object              */
   int	bitvector[AF_ARRAY_MAX];	/* To set chars bits             */
   int obj_innate;     			/* Variable to hold the spell    */
};


/* Used in obj_file_elem *DO*NOT*CHANGE* */
struct obj_affected_type {
   byte location;      /* Which ability to change (APPLY_XXX) */
   sbyte modifier;     /* How much it changes by              */
};


/* ================== Memory Structure for Objects ================== */
struct obj_data {
   obj_vnum item_number;	/* Where in data-base		      */
   room_rnum in_room;		/* In what room -1 when conta/carr    */
   int vroom;                   /* for corpse saving */
   struct obj_flag_data obj_flags;/* Object information               */
   struct obj_affected_type affected[MAX_OBJ_AFFECT];  /* affects     */

   char	*name;                    /* Title of object :get etc.        */
   char	*description;		  /* When in room                     */
   char *smell;			  /* The objects smell		      */
   char *taste;			  /* The objects taste		      */
   char *feel;			  /* The objects feel		      */
   char	*short_description;       /* when worn/carry/in cont.         */
   char	*action_description;      /* What to write when used          */
   struct extra_descr_data *ex_description; /* extra descriptions     */
   Character *carried_by;  /* Carried by :NULL in room/conta   */
   Character *worn_by;	  /* Worn by?			      */
   sh_int worn_on;		  /* Worn where?		      */

   struct obj_data *in_obj;       /* In what object NULL when none    */
   struct obj_data *contains;     /* Contains objects                 */

   long id;                       /* used by DG triggers              */
   struct trig_proto_list *proto_script; /* list of default triggers  */
   struct script_data *script;    /* script info for the object       */

   struct obj_data *next_content; /* For 'contains' lists             */
   struct obj_data *next;         /* For the object list              */
   Character *sitting_here;/* Who is sitting in it (null if none)   */
};
/* ======================================================================= */


/* ====================== File Element for Objects ======================= */
/*                 BEWARE: Changing it will ruin rent files		   */
struct obj_file_elem {
   obj_vnum item_number;
   sh_int locate; /* that's the (1+)wear-location (when equipped) or
		     (20+)index in obj file (if it's in a container) BK */
   int	value[NUM_OBJ_VAL_POSITIONS];
   int	extra_flags[EF_ARRAY_MAX];
   int	weight;
   int	timer;
   int	bitvector[AF_ARRAY_MAX];
   struct obj_affected_type affected[MAX_OBJ_AFFECT];
};


/* header block for rent files.  BEWARE: Changing it will ruin rent files  */
struct rent_info {
   int	time;
   int	rentcode;
   int	net_cost_per_diem;
   int	gold;
   int	account;
   int	nitems;
   int	spare0;
   int	spare1;
   int	spare2;
   int	spare3;
   int	spare4;
   int	spare5;
   int	spare6;
   int	spare7;
};

#define QIC_OWNERS	30  /* number of owners to store on each object */

struct qic_data {
   obj_vnum  vnum;   /* vnum of QIC item */
   int	    limit;  /* what the QIC limit is */
   int      items;  /* current number of items in the game */
   long     owners[QIC_OWNERS]; /* lists QIC_OWNERS of the item owners */
};

/* ======================================================================= */


/* room-related structures ************************************************/


struct room_direction_data {
   char	*general_description;       /* When look DIR.			*/

   char	*keyword;		/* for open/close			*/

   sh_int /*bitvector_t*/ exit_info;	/* Exit info			*/
   obj_vnum key;		/* Key's number (-1 for no key)		*/
   room_rnum to_room;		/* Where direction leads (NOWHERE)	*/
};


/* ================== Memory Structure for room ======================= */
Room {
   room_vnum number;		/* Rooms number	(vnum)		        */
   zone_rnum zone;                 /* Room zone (for resetting)            */
   int	  sector_type;          /* sector type (move/hide)              */
   char	  *name;                /* Rooms name 'You are ...'             */
   char	  *description;         /* Shown when entered                   */
   char   *smell;		/* smell description			*/
   char   *listen;		/* listen description			*/
   struct extra_descr_data *ex_description; /* for examine/look         */
   struct extra_descr_data *look_above_description; /* for look above   */
   struct extra_descr_data *look_behind_description; /* for look behind */
   struct extra_descr_data *look_under_description; /* for look under   */
   struct room_direction_data *dir_option[NUM_OF_DIRS]; /* Directions   */
   int    room_flags[RF_ARRAY_MAX];	/* DEATH,DARK ... etc           */

   byte   light;                /* Number of lightsources in room       */
   SPECIAL(*func);

   struct trig_proto_list *proto_script; /* list of default triggers    */
   struct script_data *script;  /* script info for the object           */

   struct obj_data *contents;   /* List of items in room                */
   Character *people;    /* List of NPC / PC in room             */
};
/* ==================================================================== */


/* char-related structures ************************************************/


/* memory structure for characters */
struct memory_rec_struct {
   long	id;
   struct memory_rec_struct *next;
};

typedef struct memory_rec_struct memory_rec;


/* This structure is purely intended to be an easy way to transfer */
/* and return information about time (real or mudwise).            */
struct time_info_data {
   int hours, day, month;
   sh_int year;
};


/* These data contain information about a players time data */
struct time_data {
   time_t birth;    /* This represents the characters age                */
   time_t logon;    /* Time of the last logon (used to calculate played) */
   int	played;     /* This is the total accumulated time played in secs */
};


/* general player-related info, usually PC's and NPC's */
struct char_player_data {
   char	passwd[MAX_PWD_LENGTH+1]; /* character's password      */
   char	*name;	       /* PC / NPC s name (kill ...  )         */
   char	*short_descr;  /* for NPC 'actions'                    */
   char	*long_descr;   /* for 'look'			       */
   char	*description;  /* Extra descriptions                   */
   char	*title;        /* PC / NPC's title                     */
   byte sex;           /* PC / NPC's sex                       */
   byte chclass;       /* PC / NPC's class		       */
   byte race;          /* PC / NPC's race                      */
   int body_bits;      /* Which body parts do you have	       */
   byte level;         /* PC / NPC's level                     */
   int	hometown;      /* PC s Hometown (zone)                 */
   struct time_data time;  /* PC's AGE in days                 */
   ubyte weight;       /* PC / NPC's weight                    */
   ubyte height;       /* PC / NPC's height                    */
   byte was_class;     /* PC's Previous class			*/
   byte was_class1;    /* PC's Previous class			*/
   byte was_class2;    /* PC's Previous class			*/
   int romance;		/* Romance Involvement Variable 	*/
   char *partner;	/* Name of Romance Partner		*/
   int ticks_left;	/* Timer variable			*/
};


/* Char's abilities.  Used in char_file_u *DO*NOT*CHANGE* */
struct char_ability_data {
   sbyte str;
   sbyte str_add;      /* 000 - 100 if strength 18             */
   sbyte intel;
   sbyte wis;
   sbyte dex;
   sbyte con;
   sbyte cha;
};


/* Char's points.  Used in char_file_u *DO*NOT*CHANGE* */
struct char_point_data {
   sh_int mana;
   sh_int max_mana;     /* Max mana for PC/NPC			   */
   sh_int hit;
   sh_int max_hit;      /* Max hit for PC/NPC                      */
   sh_int move;
   sh_int max_move;     /* Max move for PC/NPC                     */

   sh_int armor;        /* Internal -100..100, external -10..10 AC */
   int	gold;           /* Money carried                           */
   int	bank_gold;	/* Gold the char has in a bank account	   */
   int	exp;            /* The experience of the player            */

   sbyte hitroll;       /* Any bonus or penalty to the hit roll    */
   sbyte damroll;       /* Any bonus or penalty to the damage roll */
};


/* 
 * char_special_data_saved: specials which both a PC and an NPC have in
 * common, but which must be saved to the playerfile for PC's.
 *
 * WARNING:  Do not change this structure.  Doing so will ruin the
 * playerfile.  If you want to add to the playerfile, use the spares
 * in player_special_data.
 */
struct char_special_data_saved {
   int	alignment;		/* +-1000 for alignments                */
   long	idnum;			/* player's idnum; -1 for mobiles	*/
   int	act[PM_ARRAY_MAX];	/* act flag for NPC's; player flag for PC's */

   int	affected_by[AF_ARRAY_MAX];	/* Bitvector for spells/skills affected by */
   sh_int apply_saving_throw[5]; /* Saving throw (Bonuses)		*/
};


/* Special playing constants shared by PCs and NPCs which aren't in pfile */
struct char_special_data {
   Character *fighting;	/* Opponent				*/
   Character *hunting;	/* Char hunted by this char		*/
   Character *riding;	// Who are they riding? (DAK)
   Character *ridden_by; // Who is riding them? (DAK)
   byte position;		/* Standing, fighting, sleeping, etc.	*/

   int	carry_weight;		/* Carried weight			*/
   byte carry_items;		/* Number of items carried		*/
   int	timer;			/* Timer for update			*/

   struct char_special_data_saved saved; /* constants saved in plrfile	*/
   struct obj_data *chair;	/* object the char is sitting in	*/
   Character *next_in_chair; /* The next person in the chair	*/
};


/*
 *  If you want to add new values to the playerfile, do it here.  DO NOT
 * ADD, DELETE OR MOVE ANY OF THE VARIABLES - doing so will change the
 * size of the structure and ruin the playerfile.  However, you can change
 * the names of the spares to something more meaningful, and then use them
 * in your new code.  They will automatically be transferred from the
 * playerfile into memory when players log in.
 */
struct player_special_data_saved {
   byte skills[MAX_SKILLS+1];	/* array of skills plus skill 0		*/
   byte PADDING0;		/* used to be spells_to_learn		*/
   bool talks[MAX_TONGUE];	/* PC s Tongues 0 for NPC		*/
   int	wimp_level;		/* Below this # of hit points, flee!	*/
   byte freeze_level;		/* Level of god who froze char, if any	*/
   sh_int invis_level;		/* level of invisibility		*/
   //room_vnum load_room;	/* Which room to place char in		*/
   room_vnum spare;		/* Spare sh_int -> was load_room	*/
   int	pref[PR_ARRAY_MAX];	/* preference flags for PC's.		*/
   ubyte bad_pws;		/* number of bad password attemps	*/
   sbyte conditions[3];         /* Drunk, full, thirsty			*/

   /* spares below for future expansion.  You can change the names from
      'sparen' to something meaningful, but don't change the order.  */

   ubyte clan_rank;
   ubyte orig_race;
   ubyte brass_tokens;
   ubyte bronze_tokens;
   ubyte silver_tokens;
   ubyte gold_tokens;
   int spells_to_learn;		/* How many can you learn yet this level*/
   int clan;
   int rip_cnt;
   int kill_cnt;
   int dt_cnt;
   int bet_amt;
   int betted_on;
   int load_room;		/* Where the character will load	*/
   int spare02;
   int spare03;
   int spare04;
   long	cmd;
   /*mordecai*/
   long	tier;
   long	tier1;
   long	tier2;
   long	tier3;
   };

/*
 * Specials needed only by PCs, not NPCs.  Space for this structure is
 * not allocated in memory for NPCs, but it is for PCs and the portion
 * of it labelled 'saved' is saved in the playerfile.  This structure can
 * be changed freely; beware, though, that changing the contents of
 * player_special_data_saved will corrupt the playerfile.
 */
struct player_special_data {
    struct player_special_data_saved saved;

    char	*poofin;		/* Description on arrival of a god.  */
    char	*poofout;		/* Description upon a god's exit.    */
    struct alias_data *aliases;		/* Character's aliases		     */
    long last_tell;			/* idnum of last tell from           */
    void *last_olc_targ;		/* olc control			     */
    int last_olc_mode;			/* olc control			     */
    struct ignore *ignorelist;

};


/* Specials used by NPCs, not PCs */
struct mob_special_data {
   byte last_direction;     	/* The last direction the monster went      */
   int	attack_type;        	/* The Attack Type Bitvector for NPC's      */
   byte default_pos;        	/* Default position for NPC                 */
   memory_rec *memory;	    	/* List of attackers to remember	    */
   byte damnodice;          	/* The number of damage dice's	            */
   byte damsizedice;        	/* The size of the damage dice's            */
   int  race;		    	/* The mobs race - seperate from PC's       */
   int  type;			/* The type of animal ... bird, cat, dog??? */
   bool pregnant;	        /* Is the mob pregnant			    */
   int  due_date;		/* How long until it gives birth	    */
};


/* An affect structure.  Used in char_file_u *DO*NOT*CHANGE* */
struct affected_type {
   sh_int type;          /* The type of spell that caused this      */
   sh_int duration;      /* For how long its effects will last      */
   sbyte modifier;       /* This is added to apropriate ability     */
   byte location;        /* Tells which ability to change(APPLY_XXX)*/
   long /*bitvector_t*/	bitvector; /* Tells which bits to set (AFF_XXX) */

   struct affected_type *next;
};


/* Structure used for chars following other chars */
struct follow_type {
   Character *follower;
   struct follow_type *next;
};


/* ================== Structure for player/non-player ===================== */
Character {
   int pfilepos;			 /* playerfile pos		  */
   mob_rnum nr;                          /* Mob's rnum			  */
   room_rnum in_room;                    /* Location (real room number)	  */
   room_rnum was_in_room;		 /* location for linkdead people  */
   int wait;				 /* wait for how many loops	  */

   struct char_player_data player;       /* Normal data                   */
   struct char_ability_data real_abils;	 /* Abilities without modifiers   */
   struct char_ability_data aff_abils;	 /* Abils with spells/stones/etc  */
   struct char_point_data points;        /* Points                        */
   struct char_special_data char_specials;	/* PC/NPC specials	  */
   struct player_special_data *player_specials; /* PC specials		  */
   struct mob_special_data mob_specials;	/* NPC specials		  */

   struct affected_type *affected;       /* affected by what spells       */
   struct obj_data *equipment[NUM_WEARS];/* Equipment array               */

   struct obj_data *carrying;            /* Head of list                  */
   Descriptor *desc;         /* NULL for mobiles              */

   long id;                            /* used by DG triggers             */
   struct trig_proto_list *proto_script; /* list of default triggers      */
   struct script_data *script;         /* script info for the object      */
   struct script_memory *memory;       /* for mob memory triggers         */

   Character *next_in_room;     /* For room->people - list         */
   Character *next;             /* For either monster or ppl-list  */
   Character *next_fighting;    /* For fighting list               */

   struct follow_type *followers;        /* List of chars followers       */
   Character *master;             /* Who is char following?        */
   long cmd2;				/* These wizcmds aren't saved     */
   byte internal_flags;			/* Flags used internally - not saved */
};
/* ====================================================================== */


/* ==================== File Structure for Player ======================= */
/*             BEWARE: Changing it will ruin the playerfile		  */
struct char_file_u {
   /* char_player_data */
   char	name[MAX_NAME_LENGTH+1];
   char	description[EXDSCR_LENGTH];
   char	title[MAX_TITLE_LENGTH+1];
   byte sex;
   byte chclass;
   byte race;
   byte level;
   sh_int hometown;
   time_t birth;   /* Time of birth of character     */
   int	played;    /* Number of secs played in total */
   ubyte weight;
   ubyte height;
   byte was_class;
   byte was_class1;
   byte was_class2;
   byte was_class3;


   char	pwd[MAX_PWD_LENGTH+1];    /* character's password */

   struct char_special_data_saved char_specials_saved;
   struct player_special_data_saved player_specials_saved;
   struct char_ability_data abilities;
   struct char_point_data points;
   struct affected_type affected[MAX_AFFECT];

   time_t last_logon;		/* Time (in secs) of last logon */
   char host[HOST_LENGTH+1];	/* host of last logon */
   int romance;
   char partner[MAX_NAME_LENGTH+1];
   int ticks_left;
};
/* ====================================================================== */


/* descriptor-related structures ******************************************/


struct txt_block {
   char	*text;
   int aliased;
   struct txt_block *next;
};


struct txt_q {
   struct txt_block *head;
   struct txt_block *tail;
};


Descriptor {
   socket_t	descriptor;	/* file descriptor for socket		*/
   char	host[HOST_LENGTH+1];	/* hostname				*/
   byte	bad_pws;		/* number of bad pw attemps this login	*/
   byte idle_tics;		/* tics idle at password prompt		*/
   int	connected;		/* mode of 'connectedness'		*/
   int	desc_num;		/* unique num assigned to desc		*/
   time_t login_time;		/* when the person connected		*/
   char *showstr_head;		/* for keeping track of an internal str	*/
   char **showstr_vector;	/* for paging through texts		*/
   int  showstr_count;		/* number of pages to page through	*/
   int  showstr_page;		/* which page are we currently showing?	*/
   char	**str;			/* for the modify-str system		*/
   char pagebuf[16384];         /* personal buffer space                */
   size_t max_str;	        /*		-			*/
   char *backstr;		/* added for handling abort buffers	*/
   long	mail_to;		/* name for mail system			*/
   int	has_prompt;		/* is the user at a prompt?             */
   char	inbuf[MAX_RAW_INPUT_LENGTH];  /* buffer for raw input		*/
   char	last_input[MAX_INPUT_LENGTH]; /* the last input			*/
   char small_outbuf[SMALL_BUFSIZE];  /* standard output buffer		*/
  //   char *output;		/* ptr to the current output buffer	*/
   char **history;		/* History of commands, for ! mostly.	*/
   int	history_pos;		/* Circular array position.		*/
   int  bufptr;			/* ptr to end of current output		*/
   int	bufspace;		/* space left in the output buffer	*/
   struct txt_block *large_outbuf; /* ptr to large buffer, if we need it */
   struct txt_q input;		/* q of unprocessed input		*/
   Character *character;	/* linked to char			*/
   Character *original;	/* original char if switched		*/
   Descriptor *snooping; /* Who is this char snooping	*/
   Descriptor *snoop_by; /* And who is snooping this char	*/
   Descriptor *next; /* link to next descriptor		*/
   struct olc_data *olc;	     /*. OLC info - defined in olc.h   .*/
};


/* other miscellaneous structures ***************************************/


struct msg_type {
   char	*attacker_msg;  /* message to attacker */
   char	*victim_msg;    /* message to victim   */
   char	*room_msg;      /* message to room     */
};


struct message_type {
   struct msg_type die_msg;	/* messages when death			*/
   struct msg_type miss_msg;	/* messages when miss			*/
   struct msg_type hit_msg;	/* messages when hit			*/
   struct msg_type god_msg;	/* messages when hit on god		*/
   struct message_type *next;	/* to next messages of this kind.	*/
};


struct message_list {
   int	a_type;			/* Attack type				*/
   int	number_of_attacks;	/* How many attack messages to chose from. */
   struct message_type *msg;	/* List of messages.			*/
};


struct dex_skill_type {
   sh_int p_pocket;
   sh_int p_locks;
   sh_int traps;
   sh_int sneak;
   sh_int hide;
};


struct dex_app_type {
   sh_int reaction;
   sh_int miss_att;
   sh_int defensive;
};


struct str_app_type {
   sh_int tohit;    /* To Hit (THAC0) Bonus/Penalty        */
   sh_int todam;    /* Damage Bonus/Penalty                */
   sh_int carry_w;  /* Maximum weight that can be carrried */
   sh_int wield_w;  /* Maximum weight that can be wielded  */
};


struct wis_app_type {
   byte bonus;       /* how many practices player gains per lev */
};


struct int_app_type {
   byte learn;       /* how many % a player learns a spell/skill */
};


struct con_app_type {
   sh_int hitp;
   sh_int shock;
};


struct title_type {
   char	*title_m;
   char	*title_f;
   int	exp;
};


/* element in monster and object index-tables   */
struct index_data {
   int	vnum;	/* virtual number of this mob/obj		*/
   int	number;		/* number of existing units of this mob/obj	*/
   struct qic_data *qic; /*QIC info database                            */
   SPECIAL(*func);

   char *farg;         /* string argument for special function          */
   struct trig_data *proto;     /* for triggers... the trigger          */
};

/* linked list for mob/object prototype trigger lists */
struct trig_proto_list {
  int vnum;                             /* vnum of the trigger   */
  struct trig_proto_list *next;         /* next trigger          */
};

/* used in the socials */
struct social_messg {
  int act_nr;
  char *command;               /* holds copy of activating command */
  char *sort_as;             /* holds a copy of a similar command or
                             * abbreviation to sort by for the parser */
  int hide;                  /* ? */
  int min_victim_position;   /* Position of victim */
  int min_char_position;     /* Position of char */
  int min_level_char;          /* Minimum level of socialing char */

  /* No argument was supplied */
  char *char_no_arg;
  char *others_no_arg;

  /* An argument was there, and a victim was found */
  char *char_found;
  char *others_found;
  char *vict_found;

  /* An argument was there, as well as a body part, and a victim was found */
  char *char_body_found;
  char *others_body_found;
  char *vict_body_found;

  /* An argument was there, but no victim was found */
  char *not_found;

  /* The victim turned out to be the character */
  char *char_auto;
  char *others_auto;

  /* If the char cant be found search the char's inven and do these: */
  char *char_obj_found;
  char *others_obj_found;
};

/****  Event-driven engine structs  ******************************/

#define PULSE_EVENT     (1 RL_SEC)

#define EVENT2(name) void (name)(void *causer, void *victim, long info)

struct event_info2 {
  int ticks_to_go;
  EVENT2(*func);
  void *causer,*victim;
  void *info;
  struct event_info2 *next;
};
