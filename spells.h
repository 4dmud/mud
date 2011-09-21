/*
 * ************************************************************************
 * File: spells.h                                      Part of CircleMUD *
 * Usage: header file: constants and fn prototypes for spell system       * *
 * 
 * All rights reserved.  See license.doc for complete information.        * *
 * 
 * Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
 * CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               * 
 ***********************************************************************
 */
/*
* $Log: spells.h,v $
* Revision 1.14  2007/06/10 02:18:40  w4dimenscor
* changed all entries in the code of 'color' to 'colour', but i now regret it.
*
* Revision 1.13  2006/08/25 10:22:44  w4dimenscor
* added command to fix peoples skills back to the practiced amount they were at
*
* Revision 1.12  2006/08/18 11:09:59  w4dimenscor
* updated some clan functions to use vectors instead of malloccing memory, and also sorted clan lists and updated their layout
*
* Revision 1.11  2006/08/17 10:53:49  w4dimenscor
* moved the subs and skills from the char class to the player specials struct, converted them to vectors, and made them sorted.
*
* Revision 1.10  2006/05/21 11:02:27  w4dimenscor
* converted game from being C code to C++
* to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
*
* Revision 1.9  2006/04/21 12:46:45  w4dimenscor
* Fixed gcc 4.1 compile time errors. Game will now compile in GCC4
*
* Revision 1.8  2006/02/17 22:19:54  w4dimenscor
* Fixed error for ubuntu that doesnt like empty array declarations, moved ice shield to a better place and fixed its messages, added auto auction fixes, allowed mounts to gain exp properly
*
* Revision 1.7  2006/01/23 05:23:19  w4dimenscor
* sorry self. another. _can't remember the changes_ entry
*
* Revision 1.6  2005/08/07 04:12:39  w4dimenscor
* Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
*
* Revision 1.5  2005/03/17 12:42:13  w4dimenscor
* Added skill smash
*
* Revision 1.4  2005/02/25 05:02:46  w4dimenscor
* added new commands and a few little changes - i forget what eek
*
* Revision 1.3  2005/02/04 20:46:11  w4dimenscor
* Many changes - i couldn't connect to this for a while
*
* Revision 1.2  2004/11/20 02:33:25  w4dimenscor
* updated and cleaned up the script system
*
* Revision 1.1.1.1  2004/11/12 02:16:55  w4dimenscor
* Initial clean submission of 4Dimensions src code
*
* Revision 1.38  2004/09/04 03:46:51  molly
* made it so only one cost for recovering corpses, and skillist is sorted
*
* Revision 1.36  2004/08/15 01:12:31  molly
* aqdded logging to several files, fixed error in the setting of immtitles. fixed typo in busy
*
*/

#define DEFAULT_STAFF_LVL	12
#define DEFAULT_WAND_LVL	12

#define SINFO spell_info[spellnum]


#define CLASS(ch, i) ((i) == 0 ? GET_CLASS(ch) : (i) == 1 ? GET_REMORT(ch) : \
		     (i) == 2 ? GET_REMORT_TWO(ch) : GET_REMORT_THREE(ch))

#define HAS_CLASS(ch, i) (CLASS((ch), (i)) |= GET_CLASS(ch))
#define CLASS_SKILL(ch, i, skl ) 	(HAS_CLASS((ch), (i)) && \
	(LVL_IMMORT -1) >= spell_info[(skl)].min_level[CLASS((ch), (i))])


#define MAX_SPELL_AFFECTS 5	/* change if more needed */

#define FIRST_PRE(j) (spell_info[(j)].first_prereq)
#define SECOND_PRE(j) (spell_info[(j)].second_prereq)

#define CAST_UNDEFINED	-1
#define CAST_SPELL	0
#define CAST_POTION	1
#define CAST_WAND	2
#define CAST_STAFF	3
#define CAST_SCROLL	4
#define CAST_BREATH	5

#define MAG_DAMAGE	(1 <<  0)
#define MAG_AFFECTS	(1 <<  1)
#define MAG_UNAFFECTS	(1 <<  2)
#define MAG_POINTS	(1 <<  3)
#define MAG_ALTER_OBJS	(1 <<  4)
#define MAG_GROUPS	(1 <<  5)
#define MAG_MASSES	(1 <<  6)
#define MAG_AREAS	(1 <<  7)
#define MAG_SUMMONS	(1 <<  8)
#define MAG_CREATIONS	(1 <<  9)
#define MAG_MANUAL	(1 << 10)
#define MAG_ROOM	(1 << 11)
#define MAG_STANCE      (1 << 12)
#define MAG_ROOM_AFFECTS (1 << 13)

#define BURN_DAMAGE	15

#define ELEM_NONE 0
#define ELEM_FIRE 1
#define ELEM_ICE  2
#define ELEM_EARTH 3
#define ELEM_AIR  4
#define ELEM_ELEC  5
#define ELEM_WATER 6
#define ELEM_LIGHT 7
#define ELEM_DARK  8
#define ELEM_SPIRIT 9
#define ELEM_MIND 10
#define ELEM_DEATH 11

#define E_NONE (1 << 0)
#define E_FIRE (1 << 1)
#define E_ICE  (1 << 2)
#define E_EARTH (1 << 3)
#define E_AIR  (1 << 4)
#define E_ELEC  (1 << 5)
#define E_WATER (1 << 6)
#define E_LIGHT (1 << 7)
#define E_DARK  (1 << 8)
#define E_SPIRIT (1 << 9)
#define E_MIND (1 << 10)
#define E_DEATH (1 << 11)

enum spell_list {
    TYPE_UNDEFINED = -1,
    SPELL_RESERVED_DBC,	/* SKILL NUMBER ZERO -- RESERVED */

    /* PLAYER SPELLS -- Numbered from 1 to MAX_SPELLS */

    SPELL_ARMOR, //1
    SPELL_TELEPORT,
    SPELL_BLESS,
    SPELL_BLINDNESS,
    SPELL_BURNING_HANDS,
    SPELL_CALL_LIGHTNING,
    SPELL_CHARM,
    SPELL_CHILL_TOUCH,
    SPELL_CLONE,
    SPELL_COLOUR_SPRAY,//10
    SPELL_CONTROL_WEATHER,
    SPELL_CREATE_FOOD,
    SPELL_CREATE_WATER,
    SPELL_CURE_BLIND,
    SPELL_CURE_CRITIC,
    SPELL_CURE_LIGHT,
    SPELL_CURSE,
    SPELL_DETECT_ALIGN,
    SPELL_DETECT_INVIS,
    SPELL_DETECT_MAGIC,//20
    SPELL_DETECT_POISON,
    SPELL_DISPEL_EVIL,
    SPELL_EARTHQUAKE,
    SPELL_ENCHANT_WEAPON,
    SPELL_ENERGY_DRAIN ,
    SPELL_FIREBALL,
    SPELL_HARM,
    SPELL_HEAL,
    SPELL_INVISIBLE,
    SPELL_LIGHTNING_BOLT,//30
    SPELL_LOCATE_OBJECT,
    SPELL_MAGIC_MISSILE,
    SPELL_POISON, //--?? message?
    SPELL_PROT_FROM_EVIL,
    SPELL_REMOVE_CURSE ,
    SPELL_SANCTUARY,
    SPELL_SHOCKING_GRASP,
    SPELL_SLEEP,
    SPELL_STRENGTH,
    SPELL_SUMMON ,//40
    SPELL_SUFFOCATE,
    SPELL_WORD_OF_RECALL,
    SPELL_ANTIDOTE_1,
    SPELL_SENSE_LIFE,
    SPELL_ANIMATE_DEAD,
    SPELL_DISPEL_GOOD,
    SPELL_GROUP_ARMOR,
    SPELL_GROUP_HEAL,
    SPELL_GROUP_RECALL,
    SPELL_INFRAVISION,//50
    SPELL_WATERWALK	,
    SPELL_GATE,
    SPELL_MINOR_IDENTIFY,
    SPELL_REMOVE_ALIGNMENT,
    SPELL_LOCATE_PERSON,
    SPELL_POISON_2,
    SPELL_POISON_3,
    SPELL_POISON_4,
    SPELL_ANTIDOTE_2,
    SPELL_ANTIDOTE_3,//60
    SPELL_EVIL_EYE,
    SPELL_ABSOLVE,
    SPELL_CHAIN_LIGHTNING,
    SPELL_RECHARGE,
    SPELL_METEOR_SHOWER,
    SPELL_STONESKIN,
    SPELL_STEELSKIN,
    SPELL_HOLD_PERSON,
    SPELL_PARALYZE,
    SPELL_HOLY_WORD,//70 --
    SPELL_HOLY_SHOUT, // --
    SPELL_HASTE,
    SPELL_SHIELD,
    SPELL_GROUP_SHIELD,
    SPELL_ACID_ARROW, //-- ala
    SPELL_FLAME_ARROW, // -- ala
    SPELL_CONE_OF_COLD, // -- ala
    SPELL_KNOCK,
    SPELL_PROT_FIRE,
    SPELL_PROT_COLD,//80
    SPELL_EARTH_ELEMENTAL,
    SPELL_WATER_ELEMENTAL,
    SPELL_AIR_ELEMENTAL,
    SPELL_FIRE_ELEMENTAL ,
    SPELL_FIRE_SHIELD,
    SPELL_LIFE_TRANSFER ,
    SPELL_MANA_TRANSFER,
    SPELL_PROT_FROM_GOOD,
    SPELL_MIND_FIRE,
    SPELL_MIND_ELEC	,//90
    SPELL_MIND_WATER,
    SPELL_MIND_ICE	,
    SPELL_SHIELD_ICE,
    SPELL_SHIELD_THORN,
    SPELL_SHIELD_MANA,
    SPELL_SHIELD_MIRROR,
    SPELL_SHIELD_HOLY,
    SPELL_SHIELD_STATIC,
    SPELL_FORTIFY_MIND,
    SPELL_FORTIFY_BODY,//100
    SPELL_SWEET_DREAMS,
    SPELL_DIVINE_MIND,
    SPELL_NUMB_MIND,
    SPELL_SLOW,
    SPELL_FLIGHT,
    SPELL_BATTLE_RAGE,
    SPELL_ENCHANT_ARMOR,
    SPELL_MAGIC_BUBBLE,
    SPELL_PSI_PANIC	,
    SPELL_NIGHTMARE	,//110
    SPELL_VITALIZE	,
    SPELL_DISPELL_SANCTURY,
    SPELL_FORSEE,
    SPELL_MANA_BLAST,
    SPELL_CONFUSE,
    SPELL_CORRUPT_ARMOR,
    SPELL_WEAKEN,
    SPELL_SOULSMASH,
    SPELL_DEMONSHRIEK,
    SPELL_LIFESUCK,//120
    SPELL_BURNINGSKULL,
    SPELL_HEARTSQUEEZE,
    SPELL_FACEMELT,
    SPELL_ELECTRIC_BLAST,
    SPELL_INFERNO,
    SPELL_WATER_TO_WINE,
    SPELL_MIDAS_TOUCH,//127
    SPELL_POLYMORPH,
    SPELL_DARKNESS, 
    SPELL_RESERVE,   // SYSTEM STUFF
    SKILL_BACKSTAB, // 131
    SKILL_BASH,
    SKILL_HIDE,
    SKILL_KICK,
    SKILL_PICK_LOCK,//135
    SKILL_FLANK,
    SKILL_RESCUE,
    SKILL_SNEAK,
    SKILL_STEAL,
    SKILL_TRACK,//140
    SKILL_MOUNT,
    SKILL_RIDING,
    SKILL_TAME,
    SKILL_SNARE ,
    SKILL_THROW ,//145
    SKILL_BOW,
    SKILL_SLING,
    SKILL_CROSSBOW,
    SKILL_DUAL,
    SKILL_CIRCLE,//150
    SKILL_BLACKJACK,
    SKILL_SECOND_ATTACK,
    SKILL_FIREARM,
    SKILL_PUSH,
    SKILL_SCAN,//155
    SKILL_BREW,
    SKILL_SCRIBE,
    SKILL_TINKER,
    SKILL_POISON_WEAPON,
    SKILL_RETREAT,//160
    SKILL_FILET,
    SKILL_DISARM,
    SKILL_FORAGE,
    SKILL_TRAP_AWARE,
    SKILL_PARRY,//165
    SKILL_MOUNTED_COMBAT,
    SKILL_TRAMPLE,
    SKILL_JOUST,
    SKILL_GRAPPLE,
    SKILL_DRUNK,//170
    SKILL_HANDTOHAND,
    SKILL_MELEE ,
    SKILL_THIRD_ATTACK,
    SKILL_HAMSTRING,
    SKILL_SHORT_BLADE,//175
    SKILL_DODGE,
    SKILL_PHASE,
    SKILL_CHARGE,
    SKILL_GRIP,
    SKILL_FACE,//180
    SKILL_FOCUS,
    SKILL_MARTIAL_ARTS,
    SKILL_SLIP,
    SKILL_MANIPULATE,
    SKILL_HOLY_STRENGTH,//185
    SKILL_BESERK,
    SKILL_MEDITATE,
    SKILL_SING_WOOD	,
    SKILL_HYPERACTIVITY,
    SKILL_TRUE_STRIKE,//190
    SKILL_STRANGLE,
    SKILL_FORTIFY,
    SKILL_MANIFEST,
    SKILL_SCALP,
    SKILL_BRACE,//195
    SKILL_BEHEAD,
    SKILL_BLADE_DANCE,
    SKILL_LONGARM,
    SKILL_CLEAVE,//199
    SKILL_SMASH,  // 200
    SPELL_IDENTIFY, 
    SPELL_FIRE_BREATH,  
    SPELL_GAS_BREATH, 
    SPELL_FROST_BREATH, 
    SPELL_ACID_BREATH,  
    SPELL_LIGHTNING_BREATH, 
    SPELL_BURN,
    SPELL_FREEZE,
    SPELL_ACID,            // 209
    SPELL_RESIST_FIRE,     //210
    SPELL_RESIST_COLD,
    SPELL_RESIST_ELEC,
    SPELL_WALL_FIRE,
    SPELL_WALL_FORCE,
    SPELL_DISPEL_BUBBLE,
    SKILL_TRIP,
    SKILL_SLIT,
    SKILL_THRUST,
    SPARE219,
    SPARE220,
    SPARE221,
    SPARE222,
    SPARE223,
    SPARE224,
    SPARE225,
    SPARE226,
    SPARE227,
    SPARE228,
    SPARE229,
    SPARE230,
    SPARE231,
    SPARE232,
    SPARE233,
    SPARE234,
    SPARE235,
    SPARE236,
    SPARE237,
    SPARE238,
    SPARE239,
    SPARE240,
    SPARE241,
    SPARE242,
    SPARE243,
    SPARE244,
    SPARE245,
    SPARE246,
    SPARE247,
    SPARE248,
    SPARE249,
    SPARE250,
    SPARE251,
    SPARE252,
    SPARE253,
    SPARE254,
    SPARE255,
    SPARE256,
    SPARE257,
    SPARE258,
    SPARE259,
    SPARE260,
    SPARE261,
    SPARE262,
    SPARE263,
    SPARE264,
    SPARE265,
    SPARE266,
    SPARE267,
    SPARE268,
    SPARE269,
    SPARE270,
    SPARE271,
    SPARE272,
    SPARE273,
    SPARE274,
    SPARE275,
    SPARE276,
    SPARE277,
    SPARE278,
    SPARE279,
    SPARE280,
    SPARE281,
    SPARE282,
    SPARE283,
    SPARE284,
    SPARE285,
    SPARE286,
    SPARE287,
    SPARE288,
    SPARE289,
    SPARE290,
    SPARE291,
    SPARE292, 
    SPELL_SILENCED , 
    SPELL_IMMFREEZE,
    SPELL_DG_AFFECT,
    TYPE_UNDERWATER,
    TYPE_SPACE,	
    TYPE_DESERT,
    SPELL_OUTCAST, 
    /* 300 - 399 are reserved for attack types only */
    TYPE_HIT,    /* 300 */
    TYPE_STING, 
    TYPE_WHIP,  
    TYPE_SLASH, 
    TYPE_BITE,  
    TYPE_BLUDGEON,  /* 305 */
    TYPE_CRUSH,    
    TYPE_POUND,   
    TYPE_CLAW,   
    TYPE_MAUL,  
    TYPE_THRASH,  /* 310 */
    TYPE_PIERCE,  
    TYPE_BLAST,	
    TYPE_PUNCH,
    TYPE_STAB,
    TYPE_KICK,  /* 315 */
    TYPE_GORE,
    TYPE_ATK_ORB,
    TYPE_ATK_SPARK,
    TYPE_ATK_PULSE,
    TYPE_ATK_BEAM,   /* 320 */
    TYPE_ATK_SPEAR,		
    TYPE_ATK_BOLT,	
    TYPE_ATK_BLAST,
    TYPE_ATK_BURST,			
    TYPE_ATK_DISCHARGE,	 /* 325 */
    TYPE_ATK_ERUPTION,
    TYPE_ATK_TORRENT,
    TYPE_ATK_TORPEDO,
    SPARE_ATT_TYPE329,
    SPARE_ATT_TYPE330,
    SPARE_ATT_TYPE331,
    SPARE_ATT_TYPE332,
    SPARE_ATT_TYPE333,
    SPARE_ATT_TYPE334,
    SPARE_ATT_TYPE335,
    SPARE_ATT_TYPE336,
    SPARE_ATT_TYPE337,
    SPARE_ATT_TYPE338,
    SPARE_ATT_TYPE339,
    SPARE_ATT_TYPE340,
    SPARE_ATT_TYPE341,
    SPARE_ATT_TYPE342,
    SPARE_ATT_TYPE343,
    SPARE_ATT_TYPE344,
    SPARE_ATT_TYPE345,
    SPARE_ATT_TYPE346,
    SPARE_ATT_TYPE347,
    SPARE_ATT_TYPE348,
    SPARE_ATT_TYPE349,
    SPARE_ATT_TYPE350,
    SPARE_ATT_TYPE351,
    SPARE_ATT_TYPE352,
    SPARE_ATT_TYPE353,
    SPARE_ATT_TYPE354,
    SPARE_ATT_TYPE355,
    SPARE_ATT_TYPE356,
    SPARE_ATT_TYPE357,
    SPARE_ATT_TYPE358,
    SPARE_ATT_TYPE359,
    SPARE_ATT_TYPE360,
    SPARE_ATT_TYPE361,
    SPARE_ATT_TYPE362,
    SPARE_ATT_TYPE363,
    SPARE_ATT_TYPE364,
    SPARE_ATT_TYPE365,
    SPARE_ATT_TYPE366,
    SPARE_ATT_TYPE367,
    SPARE_ATT_TYPE368,
    SPARE_ATT_TYPE369,
    SPARE_ATT_TYPE370,
    SPARE_ATT_TYPE371,
    SPARE_ATT_TYPE372,
    SPARE_ATT_TYPE373,
    SPARE_ATT_TYPE374,
    SPARE_ATT_TYPE375,
    SPARE_ATT_TYPE376,
    SPARE_ATT_TYPE377,
    SPARE_ATT_TYPE378,
    SPARE_ATT_TYPE379,
    SPARE_ATT_TYPE380,
    SPARE_ATT_TYPE381,
    SPARE_ATT_TYPE382,
    SPARE_ATT_TYPE383,
    SPARE_ATT_TYPE384,
    SPARE_ATT_TYPE385,
    SPARE_ATT_TYPE386,
    SPARE_ATT_TYPE387,
    SPARE_ATT_TYPE388,
    SPARE_ATT_TYPE389,
    SPARE_ATT_TYPE390,
    SPARE_ATT_TYPE391,
    SPARE_ATT_TYPE392,
    SPARE_ATT_TYPE393,
    SPARE_ATT_TYPE394,
    SPARE_ATT_TYPE395,
    SPARE_ATT_TYPE396,
    SPARE_ATT_TYPE397,
    SPARE_ATT_TYPE398,
    TYPE_SUFFERING,      /* 399 */
    /* add all new skills/spells above here */
    MAX_SKILLS
};

/*
 * NON-PLAYER AND OBJECT SPELLS AND SKILLS The practice levels for the spells
 * and skills below are _not_ recorded in the playerfile; therefore, the
 * intended use is for spells and skills associated with objects (such as
 * SPELL_IDENTIFY used with scrolls of identify) or non-players (such as
 * NPC-only spells).
 */

/* NEW NPC/OBJECT SPELLS can be inserted here up to 399 */


/* WEAPON ATTACK TYPES */
/*
#define TYPE_HIT                     	300
#define TYPE_STING                   	301
#define TYPE_WHIP                    	302
#define TYPE_SLASH                   	303
#define TYPE_BITE                    	304
#define TYPE_BLUDGEON                	305
#define TYPE_CRUSH                   	306
#define TYPE_POUND                   	307
#define TYPE_CLAW                    	308
#define TYPE_MAUL                    	309
#define TYPE_THRASH                  	310
#define TYPE_PIERCE                  	311
#define TYPE_BLAST		     	312
#define TYPE_PUNCH		     	313
#define TYPE_STAB		     	314
#define TYPE_KICK		     	315
#define TYPE_GORE		     	316
*/
/* new attack types can be added here - up to TYPE_SUFFERING */
/*SPELL ATTACk TYPES */
/*
#define TYPE_ATK_ORB			317
#define TYPE_ATK_SPARK			318
#define TYPE_ATK_PULSE			319
#define TYPE_ATK_BEAM			320
#define TYPE_ATK_SPEAR			321
#define TYPE_ATK_BOLT			322
#define TYPE_ATK_BLAST			323
#define TYPE_ATK_BURST			324
#define TYPE_ATK_DISCHARGE		325
#define TYPE_ATK_ERUPTION		326
#define TYPE_ATK_TORRENT		327
#define TYPE_ATK_TORPEDO		328

#define TYPE_SUFFERING		     	399
*/
#define TYPE_SUB(sub)		(TYPE_SUFFERING + (sub))


//subskills enum in structs.h

#define MAG  (1 << CLASS_MAGE)
#define PRI  (1 << CLASS_PRIEST)
#define WAR  (1 << CLASS_WARRIOR)
#define THI  (1 << CLASS_THIEF)
#define GYP  (1 << CLASS_GYPSY)
#define HUN  (1 << CLASS_HUNTER)
#define RAN  (1 << CLASS_RANGER)
#define ESP  (1 << CLASS_ESPER)


#define STAT_SUB_HP  (1 << 0)
#define STAT_SUB_MA  (1 << 1)
#define STAT_SUB_MV  (1 << 2)


#define THING_SKILL 0
#define THING_SUB   1


#define SAVING_PARA   0
#define SAVING_ROD    1
#define SAVING_PETRI  2
#define SAVING_BREATH 3
#define SAVING_SPELL  4

#define TAR_IGNORE      (1 << 0)
#define TAR_CHAR_ROOM   (1 << 1)
#define TAR_CHAR_WORLD  (1 << 2)
#define TAR_FIGHT_SELF  (1 << 3)
#define TAR_FIGHT_VICT  (1 << 4)
#define TAR_SELF_ONLY   (1 << 5)/* Only a check, use with i.e. TAR_CHAR_ROOM */
#define TAR_NOT_SELF   	(1 << 6)/* Only a check, use with i.e. TAR_CHAR_ROOM */
#define TAR_OBJ_INV     (1 << 7)
#define TAR_OBJ_ROOM    (1 << 8)
#define TAR_OBJ_WORLD   (1 << 9)
#define TAR_OBJ_EQUIP	(1 << 10)
#define TAR_AREA_ROOM   (1 << 11) /*spell does affect to all in that room, if room is indoors can have adverse affects*/
#define TAR_AREA_AREA   (1 << 12) /*spell does affect to all in that room and ajacent rooms, used with TAR_AREA_ROOM*/
#define TAR_AREA_DIR    (1 << 13) /*spell does affect to a victim in a particular direction, used with TAR_AREA_ROOM */
#define TAR_AREA_ZONE   (1 << 14) /*spell affects everything in the zone */
int magic_distance(Character *ch, int spellnum, int dir, Character *victim);
#define TIERNUM current_class_is_tier_num(ch)




struct spell_info_type {
    byte min_position;		/* Position for caster   */
    int mana_min;		/* Min amount of mana used by a spell
      				 * (highest lev) */
    int mana_max;		/* Max amount of mana used by a spell
      				 * (lowest lev) */
    int mana_change;		/* Change in mana used by spell from
      				 * lev to lev */

    int min_level;
    int routines;
    byte violent;
    int targets;		/* See below for use with TAR_XXX  */
    const char *name;
    int first_prereq;	/* prerequisite
      					 * skill/spell one */
    int second_prereq;	/* prerequisite
      					 * skill/spell two */
    int tier;
    int wait;
    int classes;
    int gm;
    const char *wear_off_msg;   /* Easier to declare everything here */
    int type;                   /* skill/spell or other */

    spell_info_type() {
    min_position = 0;		/* Position for caster   */
    mana_min = 0;		/* Min amount of mana used by a spell
      				 * (highest lev) */
    mana_max = 0;		/* Max amount of mana used by a spell
      				 * (lowest lev) */
    mana_change = 0;		/* Change in mana used by spell from
      				 * lev to lev */

    min_level = 0;
    routines = 0;
    violent = 0;
    targets = 0;		/* See below for use with TAR_XXX  */
    name = "!UNUSED!";
    first_prereq = TYPE_UNDEFINED;
    second_prereq = TYPE_UNDEFINED;
    tier = 0;
    wait = 0;
    classes = 0;
    gm = 0;
    wear_off_msg = "";
    type = 0;
    }

};



struct sub_skill_info_type {
    byte min_position;		/* Position for caster   */
    int stat;		/* uses hitp, mana, or move points */
    int cost;		        /* the cost in units */
    int percentage;		/* the cost in percentage */
    /*
       where actual cost is the lower of the two, percentage or cost 
       for example if the cost is 200 hitpoints, and the percentage is 15%
       and the player has 1000 hp, the percentage hp is 150 points
       then the actual cost will be 150, because the percentage 
       is lower then the cost value -- mordipie
    */
    int routines;
    byte violent;
    int targets;		/* See below for use with TAR_XXX  */
    const char *name; /*the name of the subskill*/
    int perent;  /* is this a true subskill? if so which skill is it the child of, if not -1 */
    int stat_type;
    int flags;
    int cl_type; /*who can use this sub?*/

};

extern struct sub_skill_info_type sub_info[(int)TOP_SUB_DEFINE];
// maybe split off all the class restricted values into a seperate table???

struct spell_class_type {
    int min_level;
    int first_prereq;
    int second_prereq;
    sh_int tier;
    int val[2];
};
/*
 * Possible Targets:
 * 
 * bit 0 : IGNORE TARGET bit 1 : PC/NPC in room bit 2 : PC/NPC in world bit 3 :
 * Object held bit 4 : Object in inventory bit 5 : Object in room bit 6 :
 * Object in world bit 7 : If fighting, and no argument, select tar_char as
 * self bit 8 : If fighting, and no argument, select tar_char as victim
 * (fighting) bit 9 : If no argument, select self, if argument check that it
 * IS self.
 * 
 */

#define SPELL_TYPE_SPELL   0
#define SPELL_TYPE_POTION  1
#define SPELL_TYPE_WAND    2
#define SPELL_TYPE_STAFF   3
#define SPELL_TYPE_SCROLL  4





#define ASPELL(spellname) \
void	spellname(int level, Character *ch, \
		  Character *victim, struct obj_data *obj, char *strarg)

#define MANUAL_SPELL(spellname)	spellname(level, caster, cvict, ovict, tar_str);

ASPELL(spell_create_water);
ASPELL(spell_recall);
ASPELL(spell_teleport);
ASPELL(spell_summon);
ASPELL(spell_locate_object);
ASPELL(spell_charm);
ASPELL(spell_identify);
ASPELL(spell_minor_identify);
ASPELL(spell_enchant_weapon);
ASPELL(spell_detect_poison);
ASPELL(spell_gate);
ASPELL(spell_polymorph);
ASPELL(spell_control_weather);
ASPELL(spell_call_lightning );
ASPELL(spell_chain_lightning);
ASPELL(spell_recharge);
ASPELL(spell_midas_touch);
ASPELL(spell_water_to_wine);
ASPELL(spell_remove_alignment);
ASPELL(spell_enchant_armor);
ASPELL(spell_psi_panic);
ASPELL(spell_knock);
/* basic magic calling functions */

int find_skill_num(char *name);

int
mag_damage(int level, Character *ch, Character *victim,
           int spellnum, int savetype);

void
mag_affects(int level, Character *ch, Character *victim,
            int spellnum, int savetype);

void mag_groups(int level, Character *ch, int spellnum,
                int savetype);

void mag_masses(int level, Character *ch, int spellnum,
                int savetype);

void mag_areas(int level, Character *ch, int spellnum,
               int savetype);

void
mag_summons(int level, Character *ch, struct obj_data *obj,
            int spellnum, int savetype);

void
mag_points(int level, Character *ch, Character *victim,
           int spellnum, int savetype);

void
mag_unaffects(int level, Character *ch, Character *victim,
              int spellnum, int type);

void
mag_alter_objs(int level, Character *ch, struct obj_data *obj,
               int spellnum, int type);

void mag_creations(int level, Character *ch, int spellnum, char *tar_str);
void mag_room_affects(int level, Character *ch, int spellnum);

int
call_magic(Character *caster, Character *cvict,
           struct obj_data *ovict, char *svict, int spellnum, int level,
           int casttype);

void
mag_objectmagic(Character *ch, struct obj_data *obj,
                char *argument);

int
cast_spell(Character *ch, Character *tch,
           struct obj_data *tobj, char *tar_str, int spellnum);


/* other prototypes */
void spell_level(int first_prereq, int second_prereq, int spell,
                 int chclass, int level, int tier, int val1, int val2);
void init_spell_levels(void);
const char *skill_name(int num);
float has_staff(Character *ch);
const char *sub_name(int num);
int knows_spell(Character *ch, int spell);
int elemental_type(int spell);
int immune_to(Character *ch, int elem);
float resist_elem(Character *ch, int elem);
int grand_master(Character *ch);
int spell_num(const char *name);
bool operator==(vector<skillspell_data>::iterator &d, int i);
bool operator==(vector<sub_list>::iterator &d, int i);
bool operator<(vector<skillspell_data>::iterator &a, vector<skillspell_data>::iterator &b);
bool operator<(vector<sub_list>::iterator &a, vector<sub_list>::iterator &b);
bool operator<(const sub_list &a, const sub_list &b);
bool operator<(const skillspell_data &a,const skillspell_data &b);
extern vector<spell_info_type> spell_info;


