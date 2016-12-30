/* ************************************************************************
*   File: constants.c                                   Part of CircleMUD *
*  Usage: Numeric and string contants used by the MUD                     *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */
/*
 * $Log: constants.c,v $
 * Revision 1.29  2007/11/18 13:56:25  w4dimenscor
 * Two typo fixes from Hal.
 *
 * Revision 1.28  2007/06/10 08:18:13  w4dimenscor
 * added new body parts CHEST and BACK
 *
 * Revision 1.27  2007/06/10 04:48:03  w4dimenscor
 * Needed to add the new professions and pgrp to this so they
 * would show up in the "prof" command.
 * Prometheus
 *
 * Revision 1.26  2007/06/10 02:18:39  w4dimenscor
 * changed all entries in the code of 'color' to 'colour', but i now regret it.
 *
 * Revision 1.25  2007/06/08 08:19:11  w4dimenscor
 * Added the ability to see where mobs and objects reset, removed cpp_extern, needs testing, and changed assemblies so that you can specify a vnum instead of a name
 *
 * Revision 1.24  2007/05/19 18:06:12  w4dimenscor
 * Added an object trigger type that triggers when the object
 * enters a room (currently only through dg_dest).
 * --Thotter
 *
 * Revision 1.23  2007/03/01 23:19:41  w4dimenscor
 * Added a new object trigger type for containers that checks wether something is being put into them. %object% refers to the object being put in. Also added %object.is_inobj% which returns the UID of the object it is in, or nothing if it isn't in a container. --Thotter
 *
 * Revision 1.22  2006/10/06 22:25:30  w4dimenscor
 * fixed staves crested from woodsing
 *
 * Revision 1.21  2006/05/21 11:02:26  w4dimenscor
 * converted game from being C code to C++
 * to use new_send_to_char(ch, 'blah') now, you use ch->Send('Blah')
 *
 * Revision 1.20  2006/04/30 13:36:18  w4dimenscor
 * stat on an imm no longer shows UNDEFINED instead of imm3, trust all adds the imm3 trust group too now and vstat uses pages.
 *
 * Revision 1.19  2006/04/03 23:31:35  w4dimenscor
 * Added new commands called pclean, it removes the files of anyone who is not in the player index from the lib directory.
 *
 * Revision 1.18  2006/03/13 19:07:40  w4dimenscor
 * Added a toggle for autogroup so you don't type Y to accept people in your group, and a commandthat lets you split involvement evenly, involve even
 *
 * Revision 1.17  2006/02/25 04:21:30  w4dimenscor
 * Fixed crash bug, where constants.c was missing 'NOTELEPORT' string
 *
 * Revision 1.16  2005/11/19 06:18:38  w4dimenscor
 * Fixed many bugs, and added features
 *
 * Revision 1.15  2005/10/23 05:21:46  w4dimenscor
 * Altered assemblies, and fixed a few mem leaks
 *
 * Revision 1.14  2005/10/09 01:54:08  w4dimenscor
 * Fixed the trigger type otrig-speech so it works as expected, make id num's save to the player index, fixed the trigger types
 *
 * Revision 1.13  2005/09/24 08:52:33  w4dimenscor
 * finished the assemblies code
 *
 * Revision 1.12  2005/09/24 07:11:51  w4dimenscor
 * Added the ability to SKIN mobs, and the ability to add skin to mobs in olc, added ability to set what log a tree ill make and how many it will make
 *
 * Revision 1.11  2005/08/28 10:00:54  w4dimenscor
 * added RPL flag, RPL note group
 *
 * Revision 1.10  2005/08/19 08:51:14  w4dimenscor
 * fixed the variables not working
 *
 * Revision 1.9  2005/08/07 04:12:39  w4dimenscor
 * Manu changes and command have been made, sorry for the lack of description. Main changes include command landscape, fixes to helpfile stuff, subskill fixes
 *
 * Revision 1.8  2005/06/26 04:37:00  w4dimenscor
 * Changed pose, possibly fixed namechange (untested), fixed up some help adding stuff
 *
 * Revision 1.7  2005/06/21 08:53:40  w4dimenscor
 * added in better help finder and help editor, a la mordecai
 *
 * Revision 1.6  2005/04/23 12:18:12  w4dimenscor
 * Fixed some buffer read errors in the fread_string function, also fixed (temp) an index search issue for real_trigger()
 *
 * Revision 1.5  2005/04/06 07:16:28  w4dimenscor
 * added dg variables: is_roleplay, is_peaceful. Added the GATE, and RP to constants.c
 *
 * Revision 1.4  2005/02/25 05:02:45  w4dimenscor
 * added new commands and a few little changes - i forget what eek
 *
 * Revision 1.3  2005/02/04 20:46:11  w4dimenscor
 * Many changes - i couldn't connect to this for a while
 *
 * Revision 1.2  2004/11/20 19:43:28  w4dimenscor
 * added aggro flag
 *
 * Revision 1.1.1.1  2004/11/12 02:16:04  w4dimenscor
 * Initial clean submission of 4Dimensions src code
 *
 * Revision 1.54  2004/08/31 10:06:55  molly
 * make speed bonus from mounts only when you know mounted combat, changed max multi of magicmissile from 1.6 to 1.7, change who layout, fix error with room editing
 *
 * Revision 1.53  2004/08/29 05:05:00  molly
 * ] - Shield's (items) can now be used at the same time as using a focus.
 * ] - Attack points of low level mobs is now slightly higher.
 * ] - Max mob level is now 150 instead of 70.
 * ] - Magic missile's damage multiplier is now based on your percentage of mana left. (100% = 2.0 multi, 50% = 1.5 multi)
 * ] - Kick's damage is now based on how much faster you are than your opponent. (ranges from 1.3 - 2.3 multi)
 * ] - Beserker in the tier names was changes to Berserker
 * ] - Objects that supply magic have their weight based on their magic postential energy.
 * ] - Hero's, Helpers, and Imms can now use socials from afar.
 * ] - The display of Tiers in the wholist is mildly changed.
 * ] - Copyovers no longer boot 90% of the people playing.
 * ] - Score now displays damage more correctly.
 *
 * Revision 1.51  2004/08/15 01:20:32  molly
 * added a versioning structure
 *
 */

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "interpreter.h" /* alias_data */
#include "house.h"
#include "constants.h"
#include "utils.h"
#include "versionlist.h"



const char * fuse_locations[] =
{
    "TopCenter",
    "TopLeft",
    "TopRight",
    "Center",
    "LowerLeft",
    "LowerRight",
    ""
};

const char * vein_type [] =
{
    "vein"
};

const char * dimension_types[] =
{
    "General",
    "Medieval",
    "Future",
    "Old West",
    "Pre Historic",
    ""
};

const char *elemental_types[] =
{
    "none",
    "fire",
    "ice",
    "earth",
    "air",
    "electricity",
    "water",
    "light",
    "dark",
    "spirit",
    "mind",
    "death"
};

const char *potion_names[] =
{
    "milky white",
    "bubbling white",
    "glowing ivory",
    "glowing blue",
    "bubbling yellow",
    "light green",
    "gritty brown",
    "blood red",
    "swirling purple",
    "flickering green",
    "cloudy blue",
    "glowing red",
    "sparkling white",
    "incandescent blue",
    "\n"
};

const char *tree_names[] =
{
    "pine",              //0
    "oak",
    "willow",
    "dogwood",
    "ironwood",               //4
    "fir",
    "maple",
    "elder",
    "elm", //8
    "\n"
};

const char *age_desc_tree[] =
{
    "seedling",               //0
    "sapling",
    "young",
    "yearling",
    "mature",
    "old",
    "aging",
    "ancient",
    "petrified",         //8
    "\n"
};

const char *age_desc_staff[] =
{
    "very green",        //0
    "green",
    "supple",
    "thick",
    "solid",
    "worn",
    "aged",
    "gnarled",
    "glassy",            //8
    "\n"
};


const  char *random_desc[] =
{
    "slick",             //0
    "slimy",
    "greasy",
    "smooth",
    "shiny",
    "hollow",
    "glowing",
    "long",
    "plain",
    "sharp",
    "skamakowitz",
    "mysoginist",
    "octogenarian",
    "geocentric",
    "unctous",
    "transfinite",
    "phantasmagorical",       //16
    "\n"
};

const char *material_names[] =
{
    "base-material", // 0, default
    "glass",
    "iron",
    "gold",
    "silver",
    "copper",
    "zinc",
    "chromium",
    "tungsten",
    "carbon",
    "coal",		 // 10
    "basalt",
    "silica",
    "diamond",
    "plastic",
    "tin",
    "leather",
    "hair",
    "wool",
    "wood",
    "magic-wood",	 // 20
    "mithril",
    "steel",
    "brass",
    "bronze",
    "pewter",
    "platinum",
    "plasteel",
    "ceramic",
    "obsidian",
    "stone",	 // 30
    "adamantite",
    "onyx",
    "marble",
    "ruby",
    "sapphire",
    "emerald",
    "gemstone",
    "granite",
    "crystal",
    "earth",	 // 40
    "clay",
    "mortar",
    "bone",
    "organic",
    "ivory",
    "horn",
    "botanical",
    "wool",
    "cotton",
    "hemp",		 // 50
    "linen",
    "silk",
    "nylon",
    "paper",
    "currency",
    "energy",	 // 56
    "\n"
};

const char *material_group_names[] =
{
    "base-material", //0
    "metal",
    "stone",
    "organic",
    "textile",
    "wood",
    "other"          //6
};

/* material_group_names [ material_groups[i] ] corresponds to material_names[i] */
const int material_groups[] =
{
    0, 	// base-material, base-material
    2,	// glass, stone
    1,	// iron, metal
    1,	// gold, metal
    1,	// silver, metal
    1,	// copper, metal
    1, 	// zinc, metal
    1, 	// chromium, metal
    2,	// tungsten, stone
    2,	// carbon, stone
    2,	// coal, stone
    2,	// basalt, stone
    2,	// silica, stone
    2,	// diamond, stone
    6,	// plastic, other
    1,	// tin, metal
    3,	// leather, organic
    3,	// hair, organic
    3,	// wool, organic
    5,	// wood, wood
    5,	// magic-wood, wood
    1,	// mithril, metal
    1,	// steel, metal
    1,	// brass, metal
    1,	// bronze, metal
    1,	// pewter, metal
    1,	// platinum, metal
    1,	// plasteel, metal
    2,	// ceramic, stone
    2,	// obsidian, stone
    2,	// stone, stone
    2,	// adamantite, stone
    2,	// onyx, stone
    2,	// marble, stone
    2,	// ruby, stone
    2,	// sapphire, stone
    2, 	// emerald, stone
    2,	// gemstone, stone
    2,	// granite, stone
    2,	// crystal, stone
    2,	// earth, stone
    2,	// clay, stone
    2,	// mortar, stone
    3,	// bone, organic
    3,	// organic, organic
    3,	// ivory, organic
    3,	// horn, organic
    3,	// botanical, organic
    4,	// wool, textile
    4,	// cotton, textile
    4,	// hemp, textile
    4,	// linen, textile
    4,	// silk, textile
    4,	// nylon, textile
    5,	// paper, wood
    6,	// currency, other
    6	// energy, other
};

const char *colour_names[] =
{
    "none",
    "white",
    "black",
    "red",
    "yellow",
    "blue",
    "green",
    "orange",
    "violet",
    "emerald",
    "firecoloured",
    "purple"         // 11
};

const char *quality_names[] =
{
    "horrible",
    "crappy",
    "average",
    "decent",
    "good",
    "excellent",
    "magnificent"    // 6
};

const char *origins[] =
{
    "none",
    "hardwood",
    "softwood",
    "specialwood",
    "fruitwood",
    "junkwood",
    "large_animal",
    "reptile",
    "small_animal",
    "normal_animal",
    "furry_animal"
};

const vector< vector<string> > origin_names =
{
    { "none" },
    // 1 ORIGIN_HARDWOOD
    { "alder",
      "ash",
      "aspen",
      "beech",
      "birch",
      "chestnut",
      "elder",
      "elm",
      "maple",
      "oak",
      "plane" },
    // 2 ORIGIN_SOFTWOOD
    { "cedar",
      "cypress",
      "fir",
      "larch",
      "pine",
      "sycamore" },
    // 3 ORIGIN_SPECIALWOOD
    { "carob",
      "cinnamon",
      "ebony",
      "ginkgo",
      "ironwood",
      "judas",
      "juniper",
      "shadowfell",
      "yew" },
    // 4 ORIGIN_FRUITWOOD
    { "almond",
      "apple",
      "cherry",
      "orange",
      "pear",
      "plum" },
    // 5 ORIGIN_JUNKWOOD
    { "cotton",
      "dogwood",
      "paloverde",
      "spindleberry",
      "willow" },
    // 6 ORIGIN_LARGE_ANIMAL
    { "buffalo",
      "elephant",
      "krelloc",
      "mammoth" },
    // 7 ORIGIN_REPTILE
    { "cobra",
      "crocodile",
      "firelizard",
      "python",
      "rattlesnake",
      "reptile",
      "sea snake",
      "serpent",
      "snake" },
    // 8 ORIGIN_SMALL_ANIMAL
    { "calf",
      "kid",
      "lamb" },
    // 9 ORIGIN_NORMAL_ANIMAL
    { "antelope",
      "bronco",
      "buck",
      "bull",
      "cattle",
      "colt",
      "cow",
      "crow",
      "deer",
      "donkey",
      "elk",
      "ewe",
      "gelding",
      "goat",
      "heifer",
      "hind",
      "horse",
      "kangaroo",
      "mare",
      "mule",
      "mustang",
      "oryx",
      "ox",
      "pig",
      "pony",
      "ram",
      "sheep",
      "sow",
      "stag",
      "stallion",
      "steer",
      "swine" },
    // 10 ORIGIN_FURRY_ANIMAL
    { "badger",
      "bear",
      "beaver",
      "bobcat",
      "bunny",
      "cheetah",
      "chipmunk",
      "cougar",
      "coyote",
      "ermine",
      "ferret",
      "fox",
      "giraffe",
      "hare",
      "leopard",
      "lion",
      "marmot",
      "marten",
      "mink",
      "mole",
      "muskrat",
      "otter",
      "panther",
      "polecat",
      "prairie dog",
      "puma",
      "rabbit",
      "racoon",
      "rat",
      "rodent",
      "silverfox",
      "skunk",
      "squirrel",
      "tiger",
      "vole",
      "weasel",
      "wolf",
      "wolverine",
      "woodchuck",
      "worg" }
};

/* strings corresponding to ordinals/bitvectors in structs.h ***********/

const char * profession_names[] =
{
    "misc",
    "necromancer",
    "dragonmaster",
    "performer",
    "woodsman",
    "beastmaster",
    "glazier",
    "scribe",
    "businessman",
    "commerce",
    "miner",
    "brewer",
    "combatant",
    "tanner",
    "miller",
    "weaver",  // Changing this from craftsman to weaver -> Prom
    "blacksmith",
    "tailor",
    "tech",
    "management",
    "pilot",
    "farrier",
    "goldsmith",
    "metallurgist",
    "weaponsmith",
    "furrier",
    "shoemaker",
    "glovemaker",
    "armourer",
    "carder",
    "spinner",
    "dressmaker",
    "carpenter",
    "cabinetmaker",
    "shipbuilder",
    "quarrier",
    "collier",
    "brickmaker",
    "mason",
    "mirrormaker",
    "contractor",
    "artisan",
    "glasswork",
    "leatherwork",
    "metalwork",
    "stonework",
    "textilework",
    "woodwork",
    "paladin",
    "jedi",
    "bravo",
    "spacepilot",
    "vampire",
    "werewolf",
    "\n"

};

const char * profession_group_names[] =
{
    "misc",
    "metalworker",
    "leatherworker",
    "textileworker",
    "woodworker",
    "stoneworker",
    "artist",
    "fermenter", // Changing this from brewery to fermenter -> Prom
    "glassworker",
    "combat",	// The next 4 added from subskills.h -> Prom
    "affliction",
    "summoning",
    "control",
    "\n"

};

const char *rp_group_names[] =
{
    "None",
    "{cBJ{cCe{cGs{cYt{cPe{cRr{c0",
    "{crBitch{c0",
    "{cLRiddler{c0",
    "{cbMadman{c0",
    "{cYOrsini{c0",
    "{cpLolthite{c0",
    "{cGCthulyte{c0",
        "{cWAlderisio{c0",
        "{cWFearless{c0",
        "{cLGalliano{c0",
    "{cRS{cra{cRv{cra{cRg{cre{cRs{c0",
    "\n"
};
/* (Note: strings for class definitions in class.c instead of here) */


/* cardinal directions */
const char *dirs[] =
{
    "north",
    "east",
    "south",
    "west",
    "up",
    "down",
    "\n"
};


const char *opp_dirs[] =
{
    "south",
    "west",
    "north",
    "east",
    "down",
    "up",
    "\n"
};

const char *zone_bits[] =
{
    "OPEN",
    "CLOSED",
    "!RECALL",
    "!SUMMON OUT",
    "!TELEPORT_IN",
    "!SUMMON_IN",
    "!TELEPORT_OUT",
    "HEALING_WIND",
    "ACID_RAIN",
    "DARKNESS",
    "LIGHT",
    "SANDSTORM",
    "PK_ONLY",
    "RP_ONLY",
    "REMORTS_ONLY",
    "!CHEATER",
    "\n"
};

/* ROOM_x */
const char *room_bits[] =
{
    "DARK",
    "DEATH",
    "!MOB",
    "INDOORS",
    "PEACEFUL",
    "SOUNDPROOF",
    "!TRACK",
    "!MAGIC",
    "TUNNEL",
    "PRIVATE",
    "GODROOM",
    "HOUSE",
    "HCRSH",
    "ATRIUM",
    "OLC",
    "*",            /* BFS MARK */
    "VEHICLE",
    "!RECALL",
    "ARENA",
    "GOOD",
    "EVIL",
    "HP",
    "MANA",
    "MOVE",
    "ROLEPLAY",
    "!TELEPORT_IN",
    "!TELEPORT_OUT",
    "!SUMMON_IN",
    "!SUMMON_OUT",
    "WILDERNESS",
    "!VIEW",
    "DO_NOT_USE",
    "GOLD_DEPOSIT",
    "SILVER_DEPOSIT",
    "COPPER_DEPOSIT",
    "IRON_DEPOSIT",
    "ANTIMONY_DEPOSIT",
    "CHROMIUM_DEPOSIT",
    "QUARRY",
    "COAL_MINE",
    "SPRING",
    "PASTURE",
    "DRAGONPORT",
    "TIN_DEPOSIT",
    "PLASTONIUM_DEPOSIT",
        "BURNING",
    "ARTISAVE",
    "IRRADIATED",
    "PETSHOP",
    "CRASHPROOF",
    "\n"
};


/* EX_x */
const char *exit_bits[] =
{
    "DOOR",
    "CLOSED",
    "LOCKED",
    "PICKPROOF",
    "HIDDEN",
    "FENCE_WIRE",
    "FENCE_WOOD",
    "FENCE_MESH",
    "GATE_CLOSED",
    "GATE_OPEN",
    "\n"
};


/* SECT_ */
const char *sector_types[] =
{
    "Inside",   //0
    "City",
    "Field",
    "Forest",
    "Hills",
    "Mountains",  //5
    "Water (Swim)",
    "Water (No Swim)",
    "Underwater",
    "Air",
    "Desert",  //10
    "Space",
    "Road",
    "Entrance",
    "Atmosphere",
    "Sun",  //15
    "Black Hole",
    "Vehicle(coded)",
    "Swamp",
    "Reef",
    "DeathTrap",  //20
    "Snow",
    "Ice",
    "Prairie",
    "Badlands",
    "Rail",   //25
    //"DeathTrap", -- Seeing if this fixes the issue with DT showing up as Tundra (Prom)
    "\n"
};


/*
 * SEX_x
 * Not used in sprinttype() so no \n.
 */
const char *genders[] =
{
    "neutral",
    "male",
    "female",
    "\n"
};

const char *mob_races[] =
{
    "Humanoid",
    "Animal",
    "Exotic"
};

/* POS_x */
const char *position_types[] =
{
    "Dead",
    "Mortally wounded",
    "Incapacitated",
    "Stunned",
    "Sleeping",
    "Resting",
    "Sitting",
    "Fighting",
    "Standing",
    "\n"
};


/* PLR_x */
const char *player_bits[] =
{
    "KILLER",
    "THIEF",
    "FROZEN",
    "DONTSET",
    "WRITING",
    "MAILING",
    "CSH",
    "SITEOK",
    "NOSHOUT",
    "NOTITLE",
    "DELETED",
    "LOADRM",
    "!WIZL",
    "!DEL",
    "INVST",
    "CRYO",
    "RP",
    "PK",
    "N_HLPR",
    "NOT DEAD YET",
    "COVENTRY",
    "CHEATER",
    "SPEEDWALK",
    "LOYAL",
    "HERO",
    "DYING",
    "RPL",
        "IMM",
        "JESTER",
        "CTHULYTE",
        "ALDERISIO",
        "BITCH",
        "RIDDLER",
        "LOLTHYTE",
        "FEARLESS",
        "GALLIANO",
        "CLASSLESS",
        "STATSLESS",
        "!DT",
    "\n"
};



/* MOB_x */
const char *action_bits[] =
{
    "SPEC",
    "SENTINEL",
    "SCAVENGER",
    "ISNPC",
    "AWARE",
    "AGGR",
    "STAY-ZONE",
    "WIMPY",
    "AGGR_EVIL",
    "AGGR_GOOD",
    "AGGR_NEUTRAL",
    "MEMORY",
    "HELPER",
    "!CHARM",
    "!SUMMN",
    "!SLEEP",
    "!BASH",
    "!BLIND",
    "MOUNTABLE",
    "!PUSH",
    "AGGR_SEX_FEMALE",
    "!SHOOT",
    "!POISON",
    "EDIBLE",
    "SKINABLE",
    "AGGR_SEX_MALE",
    "AGGR_SEX_NEUTRAL",
    "UNDEAD",
    "!FREEZE",
    "CAN_MATE",
    "NOTDEADYET",
    "POISONS 1",
    "POISONS 2",
    "AIR",
    "WATER",
    "FIRE",
    "EARTH",
    "HERD_CATTLE",
    "SWIMS",
    "WIZINVIS",
    "STAY_SECTOR",
    "HEALER",
    "POSTMASTER",
    "QUESTMOB",
    "TRAINER",
    "BOSS",
    "HERD_SHEEP",
    "HERD_HORSE",
    "\n"
};


/* PRF_x */
const char *preference_bits[] =
{
    "BRIEF",
    "COMPACT",
    "DEAF",
    "!TELL",
    "D_HP",
    "D_MANA",   // 5
    "D_MOVE",
    "AUTOEX",
    "!HASS",
    "QUEST",
    "SUMN",     // 10
    "!REP",
    "LIGHT",
    "C1",
    "C2",
    "!WIZ",     // 15
    "L1",
    "L2",
    "!AUC",
    "!GOS",
    "!GTZ",     // 20
    "RMFLG",
    "ASSIST",
    "AFK",
    "AUTOSPLIT",
    "AUTOLOOT", // 25
    "AUTOGOLD",
    "ARENA",
    "!NEWBIE",
    "KEEPTITLE",
    "!IC",      // 30
    "BATTLESPAM",
    "MAIL_NOTICE",
    "!CTALK",
    "AFKTELL",
    "!MOVEMSG", // 35
    "MOUNTABLE",
    "!HERO",
    "T1",
    "AUTOSAC",
    "D_AUTO",   // 40
    "CLS",
    "BLDWLK",
    "ZLIB",
    "!OOC",
    "PAGEWRAP",  // 45
    "REPLYLOCK",
    "BUSY",
    "AGGRO",
    "!BRAG",
    "GATE",      // 50
    "RP",
    "NODEEDSPAM",
    "NOTELEPORT",
    "AUTOGROUP",
    "RETIRED",   // 55
    "NOTITLE",
    "DEEDMASTER",
    "NOGRAPHICS",
    "REVERSELIST",
    "\n"
};


/* AFF_x */
const char *affected_bits[] =
{
    "DONOTUSE",               /* REQUIRED BY 128 BIT */
    "BLIND",
    "INVIS",
    "DET-ALIGN",
    "DET-INVIS",
    "DET-MAGIC",
    "SENSE-LIFE",
    "WATWALK",
    "SANCT",
    "GROUP",
    "CURSE",
    "INFRA",
    "POISON-1",
    "PROT-EVIL",
    "PROT-GOOD",
    "SLEEP",
    "!TRACK",
    "TAMED",
    "UNUSED",
    "SNEAK",
    "HIDE",
    "FLY",
    "CHARM",
    "HOLD",
    "GILLS",
    "MARK",
    "POISON-2",
    "POISON-3",
    "POISON-4",
    "FSHIELD",
    "STONESKIN",
    "DONOTUSE",
    "HASTE",
    "SHIELD",
    "PROT_FIRE",
    "FREEZING",
    "ACIDED",
    "BURNING",
    "PROT_COLD",
    "HYPER",
    "FORT-MIND",
    "FORT-BODY",
    "BERSERK",
    "BLADEDANCE",
    "BRACE",
    "HOLY-STR",
    "MIND-FIRE",
    "MIND-WATER",
    "MIND-ICE",
    "SLOW",
    "PROCRASTINATE",
    "FROZEN",
    "MIND-ELECTRIC",
    "JUDO",
    "GODLY-BLESSING",
    "SHIELD-HOLY",
    "PHASE",
    "NUMB-MIND",
    "TRUE-STRIKING",
    "SHIELD-ICE",
    "SHIELD-STATIC",
    "BLUR",
    "MARTIAL-ARTS",
    "DIVINE-MIND",
    "SWEET-DREAMS",
    "SHIELD-THORNS",
    "SHIELD-MANA",
    "SHIELD-MIRROR",
    "FORSEE",
    "CONFUSED",
    "MAGIC-BUBBLE",
    "CORRUPTED",
    "WEAKENED",
    "DODGE",
    "DRAIN-BLOOD",
    "FURY-ATTACKS",
    "GRIP",
    "MEDITATING",
    "CONCENTRATION",
    "BATTLE-RAGE",
    "SUFFOCATION",
    "STUCK",
    "DRUNKEN-MASTER",
    "POLY_LION",
    "POLY_WOLF",
    "POLY_BEAR",
    "POLY_BOAR",
    "POLY_TOAD",
    "IMMFREEZE",
    "SILENCED",
    "SNARE",
        "OUTCAST",
        "RESIST_FIRE",
        "RESIST_COLD",
        "RESIST_ELEC",
    "Prom's stuff",
    "Prom's stuff",
    "Mana Regen",
    "POISON-PAUSE",
    "MUTATED",
    "RADIATED",
        "\n"
};




/* CON_x */
const char *connected_types[] =
{
    "Playing",
    "Disconnecting",
    "Get name",
    "Confirm name",
    "Get password",
    "Get new PW",
    "Confirm new PW",
    "Select sex",
    "Select class",
    "Reading MOTD",
    "Main Menu",
    "Get descript.",
    "Changing PW 1",
    "Changing PW 2",
    "Changing PW 3",
    "Self-Delete 1",
    "Self-Delete 2",
    "Disconnecting",

    "Object edit",
    "Room edit",
    "Zone edit",
    "Mobile edit",
    "Shop edit",
    "Text edit",
    "Config edit",
    "Social edit",
    "Trig Edit",
    "Help Edit",
    "Select Race",
    "Confirm Race",
    "Confirm Class",
    "Confirm Stats",
    "Line Input",        // CON_LINE_INPUT (
    "Specialize",
    "Loyal",
    "Note Edit",
    "Ident",
    "Account Add",
    "Account remove",
    "Account Choose",
    "Account Join",
    "Account Manage",
    "Color Select",
    "Newbie Status",
    "Email",
    "Create Character",
    "Vehicle Editor",
    "Help Selection",
    "Assembly Edit",
    "\n"
};

const char *creation_state_types[] =
{
    "ansi colour?",
    "new here?",
    "sex?",
    "class?",
    "class confirm",
    "race?",
    "race confirm",
    "choose stats",
    "bonus skill?",
    "choose bonus skill",
    "email?"

};

const char *newbie_status[] =
{
    "none",
    "Completely new to mudding",
    "New to 4d - but has mudded before",
    "Already knows 4d"

};

const char *stance_change[] =
{
    "You change your stance.\r\n",
    "You shift your weight to the other foot.\r\n",
    "You do a halfstep to the side.\r\n",
    "\n"
};

const char *fly_stance_change[] =
{
    "You change your mid-air position.\r\n",
    "You shift the air pressure around you.\r\n",
    "You swoop slightly to the side.\r\n",
    "\n"
};


/*
 * WEAR_x - for eq list
 * Not use in sprinttype() so no \n.
 */
const char *where[] =
{
    "<used as light>       ",
    "<on right finger>     ",
    "<on left finger>      ",
    "<worn around neck>    ",
    "<worn around scruff>  ",
    "<worn on body>        ",
    "<worn on head>        ",
    "<worn on legs>        ",
    "<worn on feet>        ",
    "<worn on hands>       ",
    "<worn on arms>        ",
    "<worn as shield>      ",
    "<worn about body>     ",
    "<worn about waist>    ",
    "<worn on right wrist> ",
    "<worn on left wrist>  ",
    "<wielded>             ",
    "<held>                ",
    "<worn on face>        ",
    "<worn over eyes>      ",
    "<worn on hips>        ",
    "<worn in right ear>   ",
    "<worn in left ear>    ",
    "<worn on right ankle> ",
    "<worn on left ankle>  ",
    "<worn on horns>       ",
    "<worn on antenna>     ",
    "<worn on tail>        ",
    "<wielded offhand>     ",
    "<worn on hind legs>   ",
    "<worn on hind hooves> ",
    "<focusing through>    ",
    "<on right thumb>      ",
    "<on left thumb>       ",
    "<worn as saddle>      ",
    "<through ear tip>     ",
    "<on left shoulder>    ",
    "<on right shoulder>   ",
    "<used as crest>       ",
    "<on left thigh>       ",
    "<on right thigh>      ",
    "<on left knee>        ",
    "<on right knee>       ",
    "<floating around>     ",
    "<worn on back>        ",
    "<worn on chest>       "
};
const char *body_pos[] =
{
    "<worn on body>        ",
    "<worn on back>        ",
    "<worn on body>        ",
    "<worn on body>        ",
    "<worn on body>        ",
    "<worn on body>        ",
    "<enveloping body>     ",
    "<worn on back>        "
};
const char *about_body_pos[] =
{
    "<concealing the body> ",
    "<worn over body>      ",
    "<worn about body>     ",
    "<worn about body>     ",
    "<worn about body>     ",
    "<worn about body>     ",
    "<hiding the body>     ",
    "<worn over body>      "
};
const char *feet_pos[] =
{
    "<worn on hooves>      ",
    "<worn on fore hooves> ",
    "<worn on feet>        ",
    "<worn on feet>        ",
    "<worn on feet>        ",
    "<worn on feet>        ",
    "<worn on feet>        ",
    "<worn on hind paws>   "
};
const char *hands_pos[] =
{
    "<worn on hands>       ",
    "<worn on hands>       ",
    "<worn on hands>       ",
    "<worn on hands>       ",
    "<worn on hands>       ",
    "<worn on hands>       ",
    "<worn on hands>       ",
    "<worn fore paws>      "
};
const char *arms_pos[] =
{
    "<worn on arms>        ",
    "<worn on arms>        ",
    "<covering arms>       ",
    "<worn on arms>        ",
    "<worn on arms>        ",
    "<worn on arms>        ",
    "<worn on arms>        ",
    "<worn on fore legs>   "
};
const char *right_ear_pos[] =
{
    "<worn in right ear>   ",
    "<worn in right ear>   ",
    "<in upper left ear>   ",
    "<worn in right ear>   ",
    "<worn in right ear>   ",
    "<worn in right ear>   ",
    "<worn in right valve> ",
    "<worn in right ear>   "
};
const char *left_ear_pos[] =
{
    "<worn in left ear>    ",
    "<worn in left ear>    ",
    "<in lower left ear>   ",
    "<worn in left ear>    ",
    "<worn in left ear>    ",
    "<worn in left ear>    ",
    "<worn in left valve>  ",
    "<worn in left ear>    "
};
const char *front_leg_pos[] =
{
    "<worn on legs>        ",
    "<worn on fore legs>   ",
    "<worn on legs>        ",
    "<worn on legs>        ",
    "<worn on legs>        ",
    "<worn on legs>        ",
    "<covering the legs>   ",
    "<worn on hind legs>   "
};
/*
"Faun",
    "Centaur",
    "Elf",
    "Dwarf",
    "Indian",
    "Gringo",
    "Martian",
    "Spacewolf",
    */

const char *disp_where ( int pos, Character *ch )
{
    int race;
    if ( IS_NPC ( ch ) )
        return ( where[pos] );

    race = GET_RACE ( ch );
    if ( race == RACE_GLADIATOR )
    {
        race = RACE_GRINGO;
    }

    switch ( pos )
    {
        case WEAR_BODY:
            return body_pos[race];
            break;
        case WEAR_ABOUT:
            return about_body_pos[race];
            break;
        case WEAR_FEET:
            return feet_pos[race];
            break;
        case WEAR_LEGS:
            return front_leg_pos[race];
            break;
        case WEAR_ARMS:
            return arms_pos[race];
            break;
        case WEAR_HANDS:
            return hands_pos[race];
            break;
        case WEAR_EAR_R:
            return right_ear_pos[race];
            break;
        case WEAR_EAR_L:
            return left_ear_pos[race];
            break;
        case WEAR_SHIELD:
            if ( GET_EQ ( ch, WEAR_FOCUS ) )
                return "<hovering infront>    ";
            else
                return where[pos];
            break;
        default:
            return ( where[pos] );
            break;
    }

}

const char *body[] =
{
    "light",
    "finger",
    "finger_2",
    "neck",
    "neck_2",
    "body",
    "head",
    "legs",
    "feet",
    "hands",
    "arms",
    "shield",
    "about",
    "waist",
    "wrist",
    "wrist_2",
    "wield",
    "hold",
    "face",
    "eyes",
    "hips",
    "ear",
    "ear_2",
    "ankle",
    "ankle_2",
    "horns",
    "antenna",
    "tail",
    "wield_2",
    "legs_2",
    "feet_2",
    "focusing",
    "finger_3",
    "finger_4",
    "saddle",
    "eartip",
    "shoulder_l",
    "shoulder_r",
    "crest",
    "thigh_l",
    "thight_r",
    "knee_l",
    "knee_r",
    "floating",
    "\n"
};

/* WEAR_x - for stat */
const char *equipment_types[] =
{
    "Used as light",
    "Worn on right finger",
    "Worn on left finger",
    "First worn around Neck",
    "Second worn around Neck",
    "Worn on body",
    "Worn on head",
    "Worn on legs",
    "Worn on feet",
    "Worn on hands",
    "Worn on arms",
    "Worn as shield",
    "Worn about body",
    "Worn around waist",
    "Worn around right wrist",
    "Worn around left wrist",
    "Wielded",
    "Held",
    "Worn on face",
    "Worn over eyes",
    "Worn on hips",
    "Worn in right ear",
    "Worn in left ear",
    "Worn on right ankle",
    "Worn on left ankle",
    "Worn on horns",
    "Worn on antenna",
    "Worn on tail",
    "Wielded second",
    "Worn on hind legs",
    "Worn on hind feet",
    "Focusing through",
    "Worn on right thumb",
    "Worn on left thumb",
    "Worn on saddle",
    "Worn through ear tip",
    "Worn on left shoulder",
    "Worn on right shoulder",
    "Used as crest",
    "Worn on left thigh",
    "Worn on right thigh",
    "Worn on left knee",
    "Worn on right knee",
    "Floating about",
    "\n"
};

/* Wear order to be viewed by players. RG 6/23/98 */
const int wear_order_index[NUM_WEARS] =
{
    WEAR_HORNS,
    WEAR_ANTENNA,
    WEAR_HEAD,
    WEAR_FACE,
    WEAR_EYES,
    WEAR_EAR_TIP,
    WEAR_EAR_R,
    WEAR_EAR_L,
    WEAR_NECK_1,
    WEAR_NECK_2,
    WEAR_SHOULDER_R,
    WEAR_SHOULDER_L,
    WEAR_ABOUT,
    WEAR_BACK,
    WEAR_BODY,
    WEAR_SADDLE,
    WEAR_CHEST,
    WEAR_CREST,
    WEAR_ARMS,
    WEAR_WRIST_R,
    WEAR_WRIST_L,
    WEAR_HANDS,
    WEAR_FINGER_R,
    WEAR_THUMB_R,
    WEAR_FINGER_L,
    WEAR_THUMB_L,
    WEAR_FOCUS,
    WEAR_WIELD,
    WEAR_WIELD_2,
    WEAR_HOLD,
    WEAR_LIGHT,
    WEAR_SHIELD,
    WEAR_HIPS,
    WEAR_TAIL,
    WEAR_WAIST,
    WEAR_THIGH_R,
    WEAR_THIGH_L,
    WEAR_LEGS,
    WEAR_LEGS_2,
    WEAR_KNEE_R,
    WEAR_KNEE_L,
    WEAR_ANKLE_R,
    WEAR_ANKLE_L,
    WEAR_FEET,
    WEAR_FEET_2,
    WEAR_FLOATING
};

/* ITEM_x (ordinal object types) */
const char *item_types[] =
{
    "UNDEFINED",         //0
    "light",
    "scroll",
    "wand",
    "staff",
    "weapon",            //5
    "<NOT USED>",
    "<NOT USED>",
    "treasure",
    "armor",
    "potion",            //10
    "worn",
    "other",
    "trash",
    "trap",
    "container",         //15
    "note",
    "liquid container",
    "key",
    "food",
    "money",             //20
    "pen",
    "boat",
    "fountain",
    "throw",
    "grenade",           //25
    "bow",
    "sling",
    "crossbow",
    "bolt",
    "arrow",             //30
    "rock",
    "vehicle",
    "vehicle control",
    "vehicle exit",
    "vehicle window",         //35
    "room portal",
    "gun",
    "ammo",
    "wings",
    "spacesuit",         //40
    "aqualung",
    "climbable",
    "level 1 poison",
    "level 2 poison",
    "level 3 poison",              //45
    "level 4 poison",
    "level 1 antidote",
    "level 2 antidote",
    "level 3 antidote",
    "descendable",       //50
    "bush portal",
    "water portal",
    "hole portal",
    "meat",
    "nugget",            //55
    "metal detector",
    "tree",
    "bark",
    "anvil",
    "hammer",            //60
    "grindstone",
    "oil",
    "ore",
    "axe",
    "gem cluster",            //65
    "element",
    "shovel",
    "wood",
    "machine",
    "pickaxe",           //70
    "nut",
    "skin",
    "furniture",
    "portal hurdle",
    "thermal protection",          //75
    "radio",
    "minor focus",
    "major focus",
    "lightsaber hilt",
    "zone flag",
    "locker",
    "garotte",
    "vial",
    "bankbook",
    "spacebike",
        "vehicle2",
    "crafting gem",
    "template",
    "crafting base",
    "radiation_proof",
    "bodybag",
    "\n"
};

const char *vial_types[] =
{
    "it will heal you",
    "it will fill your magical energy",
    "it will fill your movement energy",
    "it will replenish your stamina"

};


/* ITEM_WEAR_ (wear bitvector) */
const char *wear_bits[] =
{
    "TAKE",
    "FINGER",
    "NECK",
    "BODY",
    "HEAD",
    "LEGS",
    "FEET",
    "HANDS",
    "ARMS",
    "SHIELD",
    "ABOUT",
    "WAIST",
    "WRIST",
    "WIELD",
    "HOLD",
    "FACE",
    "EYES",
    "HIPS",
    "EAR",
    "ANKLE",
    "HORNS",
    "ANTENNA",
    "TAIL",
    "FOCUS",
    "SHOULDER",
    "CREST",
    "THIGH",
    "KNEE",
    "FLOATING",
    "BACK",
    "CHEST",
    "\n"
};


/* ITEM_x (extra bits) */
const char *extra_bits[] =
{
    "glowing",
    "humming",
    "unrentable",
    "undonateable",
    "anti-invisible",
    "invisible",
    "magic",
    "cursed",
    "blessed",
    "anti-good",
    "anti-evil",
    "anti-neutral",
    "anti-mage",
    "anti-priest",
    "anti-thief",
    "anti-warrior",
    "unsellable",
    "anti-faun",
    "anti-centaur",
    "anti-elf",
    "anti-dwarf",
    "Live Grenade",
    "anti-indian",
    "anti-gringo",
    "anti-martian",
    "anti-spacewolf",
    "anti-hunter",
    "anti-ranger",
    "anti-gypsy",
    "anti-esper",
    "melt-on-drop",
    "(buried)",
    "player corpse",
    "mob corpse",
    "artifact",
    "unique",
    "unlocateable",
    "hidden",
    "poison type 1",
    "poison type 2",
    "poison type 3",
    "poison type 4",
    "edible",
    "skinable",
    "anti-disarm",
    "anti-male",
    "anti-female",
    "contraceptive",
    "<UNUSED>",
    "<UNUSED>",
    "tinkered",
    "randomized",
    "enhanced",
    "modified",
    "lightsaber",
    "two-handed",
    "<UNUSED>",
    "<UNUSED>",
    "life-stealing",
    "mana-stealing",
    "move-stealing",
    "undisplayed",
    "extracting-(CODE)",
    "shiftable",
    "key-rent",
    "pk-corpse",
    "beheaded-corpse",
    "anti-dt",
    "fire-focus",
    "ice-focus",
    "earth-focus",
    "water-focus",
    "electric-focus",
    "air-focus",
    "spirit-focus",
    "death-focus",
    "mind-focus",
    "no-loot",
    "no-sac",
    "crashproof",
    "\n"
};

const char * colour_option_list[] =
{
    "Alice Blue",
    "Antique White",
    "Aqua",
    "Aquamarine",
    "Azure",
    "Beige",
    "Bisque",
    "Black",
    "Blanched Almond",
    "Blue",
    "Blue Violet",
    "Brown",
    "Burly Wood",
    "Cadet Blue",
    "Chartreuse",
    "Chocolate",
    "Coral",
    "Cornflower Blue",
    "Cornsilk",
    "Crimson",
    "Cyan",
    "Dark Blue",
    "Dark Cyan",
    "Dark Golden Rod",
    "Dark Gray",
    "Dark Green",
    "Dark Khaki",
    "Dark Magenta",
    "Dark Olive Green",
    "Dark orange",
    "Dark Orchid",
    "Dark Red",
    "Dark Salmon",
    "Dark Sea Green",
    "Dark Slate Blue",
    "Dark Slate Gray",
    "Dark Turquoise",
    "Dark Violet",
    "Deep Pink",
    "Deep Sky Blue",
    "Dim Gray",
    "Dodger Blue",
    "Feldspar",
    "Fire Brick",
    "Floral White",
    "Forest Green",
    "Fuchsia",
    "Gainsboro",
    "Ghost White",
    "Gold",
    "Golden Rod",
    "Gray",
    "Green",
    "Green Yellow",
    "Honey Dew",
    "Hot Pink",
    "Indian Red",
    "Indigo",
    "Ivory",
    "Khaki",
    "Lavender",
    "Lavender Blush",
    "Lawn Green",
    "Lemon Chiffon",
    "Light Blue",
    "Light Coral",
    "Light Cyan",
    "Light Gold",
    "Light Grey",
    "Light Green",
    "Light Pink",
    "Light Salmon",
    "Light Sea Green",
    "Light Sky Blue",
    "Light Slate Blue",
    "Light Slate Gray",
    "Light Steel Blue",
    "Light Yellow",
    "Lime",
    "Lime Green",
    "Linen",
    "Magenta",
    "Maroon",
    "Midnight Blue",
    "Mint Cream",
    "Misty Rose",
    "Moccasin",
    "Navajo White",
    "Navy",
    "Old Lace",
    "Olive",
    "Olive Drab",
    "Orange" ,
    "Orange Red",
    "Orchid" ,
    "Pale Golden Rod"  ,
    "Pale Green",
    "Pale Turquoise",
    "Pale Violet Red",
    "Papaya Whip",
    "Peach Puff",
    "Peru",
    "Pink",
    "Plum",
    "Powder Blue",
    "Purple",
    "Red" ,
    "Rosy Brown" ,
    "Royal Blue",
    "Saddle Brown",
    "Salmon"  ,
    "Sandy Brown",
    "Sea Green"  ,
    "Sea Shell" ,
    "Sienna",
    "Silver",
    "Sky Blue",
    "Slate Blue",
    "Slate Gray" ,
    "Snow"  ,
    "Spring Green",
    "Steel Blue"  ,
    "Tan"  ,
    "Teal" ,
    "Thistle",
    "Tomato",
    "Turquoise",
    "Violet" ,
    "Violet Red",
    "Wheat"  ,
    "White"  ,
    "White Smoke",
    "Yellow",
    "Yellow Green",
    "\n"
};



/* APPLY_x */
const char *apply_types[] =
{
    "Nothing",
    "Strength",
    "Dexterity",
    "Intelligence",
    "Wisdom",
    "Constitution",
    "Charisma",
    "Health-regen",
    "Move-regen",
    "Age",
    "Weight",
    "Height",
    "Maxmana",
    "Maxhit",
    "Maxmove",
    "Mana-regen",
    "Experience",
    "Armor",
    "Hitroll",
    "Damroll",
    "Paralyze Defence",
    "Rod Defence",
    "Petrify Defence",
    "Breath Defence",
    "Spell Defence",
    "Race",
    "Speed",
    "Coolness",
    "Tunnel-Speed",
    "Tunnel-Bonus",
    "Tunnel-Stealth",
    "Tunnel-Protection",

    "\n"
};


/* CONT_x */
const char *container_bits[] =
{
    "CLOSEABLE",
    "PICKPROOF",
    "CLOSED",
    "LOCKED",
    "\n",
};


/* LIQ_x */
const char *drinks[] =
{
    "water",
    "beer",
    "wine",
    "ale",
    "dark ale",
    "whisky",
    "lemonade",
    "firebreather",
    "local speciality",
    "slime mold juice",
    "milk",
    "tea",
    "coffee",
    "blood",
    "salt water",
    "clear water",
    "\n"
};


/* other constants for liquids ******************************************/


/* one-word alias for each drink */
const char *drinknames[] =
{
    "water",
    "beer",
    "wine",
    "ale",
    "ale",
    "whisky",
    "lemonade",
    "firebreather",
    "local",
    "juice",
    "milk",
    "tea",
    "coffee",
    "blood",
    "salt",
    "water",
    "\n"
};


/* effect of drinks on hunger, thirst, and drunkenness -- see values.doc */
const int drink_aff[][3] =
{
    {0, 1, 10},
    {3, 2, 5},
    {5, 2, 5},
    {2, 2, 5},
    {1, 2, 5},
    {6, 1, 4},
    {0, 1, 8},
    {10, 0, 0},
    {3, 3, 3},
    {0, 4, -8},
    {0, 3, 6},
    {0, 1, 6},
    {0, 1, 6},
    {0, 2, -1},
    {0, 1, -2},
    {0, 0, 13}
};


/* colour of the various drinks */
const char *colour_liquid[] =
{
    "clear",
    "brown",
    "clear",
    "brown",
    "dark",
    "golden",
    "red",
    "green",
    "clear",
    "light green",
    "white",
    "brown",
    "black",
    "red",
    "clear",
    "crystal clear",
    "\n"
};


/*
 * level of fullness for drink containers
 * Not used in sprinttype() so no \n.
 */
const char *fullness[] =
{
    "less than half ",
    "about half ",
    "more than half ",
    ""
};


/* str, int, wis, dex, con applies **************************************/


/* [ch] strength apply (all) */
/*to hit, to dam, carry, wield*/
const struct str_app_type str_app[] =
{
    {-5, -4, 0,  0 }
    ,          /* str = 0 */
    {-5, -4, 3,  1 },         /* str = 1 */
    {-3, -2, 3,  2 },
    {-3, -1, 10, 3 },
    {-2, -1, 25, 4 },
    {-2, -1, 55, 5 },         /* str = 5 */
    {-1, 0, 80,  6 },
    {-1, 0, 90,  7 },
    {0, 0, 100,  8 },
    {0, 0, 100,  9 },
    {0, 0, 115,  10},         /* str = 10 */
    {0, 0, 115,  11},
    {0, 0, 140,  12},
    {0, 0, 140,  13},
    {0, 0, 170,  14},
    {0, 0, 170,  15},         /* str = 15 */
    {0, 1, 195,  16},
    {1, 1, 220,  18},
    {1, 2, 255,  20},         /* str = 18 */
    {3, 7, 300,  22},
    {3, 8, 340,  24},         /* str = 20 */
    {4, 9, 400,  26},
    {4, 10, 460, 28}, //22
    {5, 11, 1130,30}, //23
    {6, 12, 1440,40},
    {7, 14, 1750,40},         /* str = 25 */
    {5, 11, 500, 30},         /* str = 18/0 - 18-50 */
    {6, 13, 560, 33},         /* str = 18/51 - 18-75 */
    {7, 16, 600, 34},         /* str = 18/76 - 18-90 */
    {8, 18, 660, 39},         /* str = 18/91 - 18-99 */
    {9, 22, 700, 45}          /* str = 18/100 */
};



/* [dex] skill apply (thieves only) */
const struct dex_skill_type dex_app_skill[] =
{
    {-99, -99, -90, -99, -60}
    ,     /* dex = 0 */
    {-90, -90, -60, -90, -50},     /* dex = 1 */
    {-80, -80, -40, -80, -45},
    {-70, -70, -30, -70, -40},
    {-60, -60, -30, -60, -35},
    {-50, -50, -20, -50, -30},     /* dex = 5 */
    {-40, -40, -20, -40, -25},
    {-30, -30, -15, -30, -20},
    {-20, -20, -15, -20, -15},
    {-15, -10, -10, -20, -10},
    {-10, -5, -10, -15, -5},  /* dex = 10 */
    {-5, 0, -5, -10, 0},
    {0, 0, 0, -5, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},          /* dex = 15 */
    {0, 5, 0, 0, 0},
    {5, 10, 0, 5, 5},
    {10, 15, 5, 10, 10}, /* dex = 18 */
    {15, 20, 10, 15, 15},
    {15, 20, 10, 15, 15},     /* dex = 20 */
    {20, 25, 10, 15, 20},
    {20, 25, 15, 20, 20},
    {25, 25, 15, 20, 20},
    {25, 30, 15, 25, 25},
    {25, 30, 15, 25, 25} /* dex = 25 */
};



/* [dex] apply (all) */
const struct dex_app_type dex_app[] =
{
    {-7, -7, 6}
    ,          /* dex = 0 */
    {-6, -6, 5},         /* dex = 1 */
    {-4, -4, 5},
    {-3, -3, 4},
    {-2, -2, 3},
    {-1, -1, 2},         /* dex = 5 */
    {0, 0, 1},
    {0, 0, 0},
    {0, 0, 0},
    {0, 0, 0},
    {0, 0, 0},           /* dex = 10 */
    {0, 0, 0},
    {0, 0, 0},
    {0, 0, 0},
    {0, 0, 0},
    {0, 1, -1},               /* dex = 15 */
    {1, 1, -2},
    {2, 2, -3},
    {3, 3, -5},               /* dex = 18 */
    {3, 3, -5},
    {3, 3, -5},               /* dex = 20 */
    {4, 4, -5},
    {4, 4, -5},
    {4, 4, -5},
    {5, 5, -6},
    {5, 5, -6}           /* dex = 25 */
};



/* [con] apply (all) */
const struct con_app_type con_app[] =
{
    {-4, 20}
    ,               /* con = 0 */
    {-3, 25},            /* con = 1 */
    {-2, 30},
    {-2, 35},
    {-1, 40},
    {-1, 45},            /* con = 5 */
    {-1, 50},
    {-1, 55},
    {-1, 60},
    {-1, 65},
    { 0, 70},            /* con = 10 */
    { 0, 75},
    { 0, 80},
    { 0, 85},
    { 0, 88},
    { 1, 90},            /* con = 15 */
    { 2, 95},
    { 2, 97},
    { 4, 99},            /* con = 18 */
    { 3, 99},
    { 4, 99},            /* con = 20 */
    { 5, 99},
    { 6, 99},
    { 7, 99},
    { 8, 99},
    { 9, 99}             /* con = 25 */
};



/* [int] apply (all) */
const struct int_app_type int_app[] =
{
    {3}
    ,               /* int = 0 */
    {5},            /* int = 1 */
    {7},
    {8},
    {9},
    {10},           /* int = 5 */
    {11},
    {12},
    {13},
    {15},
    {17},           /* int = 10 */
    {19},
    {22},
    {25},
    {30},
    {38},           /* int = 15 */
    {48},
    {51},
    {58},           /* int = 18 */
    {59},
    {60},           /* int = 20 */
    {61},
    {62},
    {63},
    {64},
    {65}            /* int = 25 */
};

/* [wis] apply (all) */
const struct wis_app_type wis_app[] =
{
    {0}
    ,               /* wis = 0 */
    {0},            /* wis = 1 */
    {0},
    {0},
    {0},
    {0},            /* wis = 5 */
    {0},
    {0},
    {0},
    {0},
    {0},            /* wis = 10 */
    {0},
    {2},
    {2},
    {3},
    {3},            /* wis = 15 */
    {4},
    {5},
    {6},            /* wis = 18 */
    {6},
    {6},            /* wis = 20 */
    {6},
    {6},
    {7},
    {7},
    {7}                  /* wis = 25 */
};



const char *spell_wear_off_msg[] =
{
    "RESERVED DB.C",          /* 0 */
    "You feel less protected.",    /* 1 */
    "!Teleport!",
    "You feel less righteous.",
    "You feel a cloak of blindness disolve.",
    "!Burning Hands!",        /* 5 */
    "!Call Lightning",
    "You feel more self-confident.",
    "You feel your strength return.",
    "!Clone!",
    "!Color Spray!",          /* 10 */
    "!Control Weather!",
    "!Create Food!",
    "!Create Water!",
    "!Cure Blind!",
    "!Cure Critic!",          /* 15 */
    "!Cure Light!",
    "You feel more optimistic.",
    "You feel less aware.",
    "Your eyes stop tingling.",
    "The detect magic wears off.", /* 20 */
    "The detect poison wears off.",
    "!Dispel Evil!",
    "!Earthquake!",
    "!Enchant Weapon!",
    "The drained feeling goes away.", /*25 */
//     "!Energy Drain!",         /* 25 */
    "!Fireball!",
    "!Harm!",
    "!Heal!",
    "You feel yourself exposed.",
    "!Lightning Bolt!",       /* 30 */
    "!Locate object!",
    "!Magic Missile!",
    "You feel less sick.",
    "You feel less protected.",
    "!Remove Curse!",         /* 35 */
    "The glowing white aura around your body fades.",
    "!Shocking Grasp!",
    "You feel less tired.",
    "You feel weaker.",
    "!Summon!",               /* 40 */
    "You gasp and feel fresh air flood into your lungs!",
    "!Word of Recall!",
    "!Remove Poison!",
    "You feel less aware of your surroundings.",
    "!Animate Dead!",         /* 45 */
    "!Dispel Good!",
    "!Group Armor!",
    "!Group Heal!",
    "!Group Recall!",
    "Your night vision seems to fade.", /* 50 */
    "Your feet seem less boyant.",
    "!Gate!",
    "!Identify!",
    "!Remove Alignment!",
    "You return to your natural form.", /* 55 */
    "You feel less sick!",
    "You feel less sick!",
    "!POISON_4!",
    "You feel less sick!",
    "You feel less sick!",    /* 60 */
    "!Evil Eye!",
    "!Absolve!",
    "!Chain Lightning!",
    "!Recharge!",
    "!Meteor Shower!",        /* 65 */
    "Your skin of rock crumbles.",
    "Your skin of steel becomes more supple.",
    "You are freed of your binds!",
    "You aren't paralyzed anymore.",
    "!Holy Word!",       /* 70 */
    "!Holy Shout!",
    "You feel sluggish again.",
    "You feel your force shield dwindle off!",
    "!Group Shield!",
    "Your skin stops stinging!",        /* 75 */
    "Your skin stops frying!",
    "You feel warm again.",
    "!Knock!",
    "You feel less insulated.",
    "You feel your shell of warmth dissipate.",   /* 80 */
    "The earth elemental disapears.",
    "!Water Elemental!",
    "!Air Elemental!",
    "!Fire Elemental!",
    "Your protective shield of fire fades.",
    "!Life Transfer!",
    "!Mana Transfer!",
    "Your unholy aura dissipates.",
    "Your mind chills out.",
    "Your mind stops crackling.",
    "Your mind drys out.",
    "Your mind thaws out.",
    "Your shield of ice melts.",
    "The thorns around you wither and die.",
    "Your pulsating mana shield dissipates.",
    "Your mirror shield shatters.",
    "The geas is lifted.",
    "Your shield of static collapses.",
    "Your mind's protection crumbles away.",
    "Your body's protection crumbles away.",
    "You wake up from the most refreshing dream.",
    "Your divine guidance leaves you.",
    "Feeling returns to your mind.",
    "The feeling of lethargy falls from your muscles.",
    "Your wings weaken, shrivel and fall off.",
    "The haze of red fades and you don't feel so angry.",
    "ENCHANT ARMOR",
    "Your Magic Bubble bursts.",
    "You stop panicing.",
    "The bad dreams stop.",
    "You don't feel so vitalized.",
    "DISPELL SANC",
    "You forget the future.",
    "MANA BLAST",
    "You feel less confused.",
    "Your aura of corruption fades.",
    "You feel less weak.",
    "SOULSMASH",
    "DEMONSHRIEK",
    "LIFESUCK",
    "BURNINGSKULL",
    "HEARTSQUEEZE",
    "FACEMELT",
    "ELECTRIC_BLAST",
    "INFERNO",
    "WATER_TO_WINE",
    "MIDAS_TOUCH",
    "\n"
};

// Added this for Skills wear off messages Skeleton. Prom
const char *skill_wear_off_msg[] =
{
// Add wear off messages here. Prom
    "Reserved",
    "!Backstab!",	//131
    "!Bash!",
    "!Hide!",
    "!Kick!",
    "!Pick_Lock!",
    "!Flank!",
        "!Rescue",
    "You are no longer feeling sneaky.",
        "!Steal",
    "!Track!",
    "!Mount!",
    "!Riding!",
    "!Tame!",
    "!Snare!",
    "!Throw!",
    "!Bow!",
    "!Sling!",		//145
    "!Crossbow!",
    "!Dual!",
    "!Circle!",
    "!Blackjack!",
    "!Second_Attack!",	//150
        "!Firearm!",
        "!Push!",
        "!Scan!",
        "!Brew!",
        "!Scribe!",		//155
        "!Tinker!",
        "!Poison_Weapon!",
        "!Retreat!",
        "!Filet!",
        "!Disarm!",		//160
        "!Forage!",
        "!Trap_Aware!",
        "!Parry!",
        "!Mounted_Combat!",
        "!Trample!",
        "!Joust!",		//165
        "!Grapple!",
        "You sober up.",
        "!HandtoHand!",
        "!Melee!",
        "!Third_Attack!",		//170
        "!Hamstring!",
        "!Short_Blade!",
        "!Dodge!",
        "!Phase!",
        "!Charge!",		//175
        "Your grip loosens.",
        "!Face!",
        "You lose your focus.",
        "You relax your martial arts stance.",
        "!Slip!",		//180
        "!Manipulate!",
        "!Holy_Strength!",
        "You calm down from your battle rage.",
        "You no longer have the patience to meditate.",
        "!Sing_Wood!",
        "Your hyperactivity returns to normal.",	//185
        "Your intensity for true strike has returned to normal.",
        "!Strangle!",
        "You feel less fortified.",
        "!Manifest!",
        "!Scalp!",		//190
        "You no longer brace for damage.",
        "!Behead!",
        "You slow down the whirr of your blades.",
        "!LongArm!",
        "!Cleave!",		//200
    "!Smash!",
    "\n"
};



const char *npc_class_types[] =
{
    "Normal",
    "Undead",
    "Caster",
    "Fighter",
    "Rogue",
    "Animal",
    "\n"
};

const int rev_dir[] =
{
    2,
    3,
    0,
    1,
    5,
    4
};

#if defined(CONFIG_OASIS_MPROG)
/*
 * Definitions necessary for MobProg support in OasisOLC
 */
const char *mobprog_types[] =
{
    "INFILE",
    "ACT",
    "SPEECH",
    "RAND",
    "FIGHT",
    "DEATH",
    "HITPRCNT",
    "ENTRY",
    "GREET",
    "ALL_GREET",
    "GIVE",
    "BRIBE",
    "\n"
};
#endif

const int movement_loss[] =
{
    1,                   /* Inside       */
    1,                   /* City         */
    2,                   /* Field        */
    3,                   /* Forest       */
    6,                   /* Hills        */
    10,                  /* Mountains    */
    7,                   /* Swimming     */
    2,                   /* Unswimable   */
    5,                   /* Flying       */
    6,                   /* Underwater   */
    10,                  /* Desert       */
    50,                  /* Space        */
    2,                   /* Road         */
    2,                   /* Entrance     */
    50,                  /* Atmosphere   */
    50,                  /* Sun          */
    2,                   /* Black Hole   */
    1,                   /*Vehicle*/
    6,                   /*swamp*/
    5,                   /*reef*/
    50,                  /*deathtrap*/
    8,                   /*snow*/
    4,                   /*ice*/
    2,                   /*prairie*/
    7,                   /*badlands*/
    6,                   /*rail*/
    0

};

/* Not used in sprinttype(). */
const char *weekdays[] =
{
    "the Day of the Moon",
    "the Day of the Bull",
    "the Day of the Deception",
    "the Day of Thunder",
    "the Day of Freedom",
    "the Day of the Great Gods",
    "the Day of the Sun"
};


/* Not used in sprinttype(). */
const char *month_name[] =
{
    "Month of Snow",        // 0 Changed from Month of Winter
    "Month of the Wolf", // Changed from Month of Winter Wolf
    "Month of the Frost Giant",
    "Month of the Old Forces",
    "Month of the Grand Struggle",
    "Month of the Buds", // Changed from Month of Spring
    "Month of Nature",
    "Month of Futility",
    "Month of the Dragon",
    "Month of the Sun",
    "Month of the Heat",
    "Month of the Battle",
    "Month of the Dark Shades",
    "Month of the Shadows",
    "Month of the Long Shadows",
    "Month of the Ancient Darkness",
    "Month of the Great Evil"
};

/* mob trigger types */
const char *trig_types[] =
{
    "Global",
    "Random",
    "Command",
    "Speech",
    "Act",
    "Death",
    "Greet",
    "Greet-All",
    "Entry",
    "Receive",
    "Fight",
    "HitPrcnt",
    "Bribe",
    "Load",
    "Memory",
    "Time",
    "Cast",
    "Leave",
    "Door",
    "FUNCTION",
    "UNUSED",
    "\n"
};


/* obj trigger types */
const char *otrig_types[] =
{
    "Global",
    "Random",
    "Command",
    "Speech",
    "ObjEnter",
    "Timer",
    "Get",
    "Drop",
    "Give",
    "Wear",
    "Assemble",
    "Remove",
    "put_in",
    "Load", /* 13 */
    "get_out",
    "Time",
    "Cast",
    "Leave",
    "Consume",
    "FUNCTION",
    "\n"
};


/* wld trigger types */
const char *wtrig_types[] =
{
    "Global",
    "Random",
    "Command",
    "Speech",
    "UNUSED",
    "Zone Reset",
    "Enter",
    "Drop",
    "UNUSED",
    "UNUSED",
    "UNUSED",
    "UNUSED",
    "UNUSED",
    "UNUSED",
    "UNUSED",
    "Time",
    "Cast",
    "Leave",
    "Door",
    "FUNCTION"
    "\n"
};

/* Use for Wizard Trust System */
const char *wiz_groups[] =
{
    "Ban",
    "Dspln",
    "Edit",
    "Heal",
    "House",
    "Imm1",
    "Imm2",
    "Impl",
    "Kill",
    "Load",
    "Olc",
    "Quest",
    "Sen",
    "Tele",
    "Trig",
    "Marry",
    "Goto",
    "Global",
    "Hedit",
    "Imm3",
    "\n"
};

const size_t room_bits_count = 	sizeof ( room_bits ) 	 / sizeof ( room_bits[0] )	 - 1;
const size_t action_bits_count =      sizeof ( action_bits ) 	 / sizeof ( action_bits[0] )	 - 1;
const size_t affected_bits_count =    sizeof ( affected_bits ) / sizeof ( affected_bits[0] )	 - 1;
const size_t extra_bits_count =       sizeof ( extra_bits ) 	 / sizeof ( extra_bits[0] )	 - 1;
const size_t wear_bits_count =        sizeof ( wear_bits ) 	 / sizeof ( wear_bits[0] )	 - 1;

const struct class_name_data class_name[] =
{

    {
        {
            "Mage",
            "HedgeWizard",
            "Illusionist",
            "Mystic",
            "Arcanist"
        }
    },

    { {
            "Priest",
            "Protector",
            "DivineKnight",
            "HighPriest",
            "SoulReaver"
        }
    },

    { {
            "Thief",
            "Prowler",
            "Assassin",
            "Shadow",
            "Nightmaster"
        }
    },

    { {
            "Warrior",
            "Berserker",
            "Skirmisher",
            "Mercenary",
            "BladeMaster"
        }
    },

    { {
            "Hunter",
            "Forager",
            "Tracker",
            "Woodsman",
            "Survivalist"
        }
    },

    { {
            "Ranger",
            "Wayfarer",
            "Wanderer",
            "Pathfinder",
            "TrailMaster"
        }
    },

    { {
            "Gypsy",
            "Nomad",
            "Trickster",
            "Tinkerer",
            "Dreamweaver"
        }
    },

    { {
            "Esper",
            "Mentalist",
            "Hypnotist",
            "Mesmerist",
            "Enchanter"
        }
    }
};


/* obj trigger types */
const char *cast_types[] =
{
    "Spell",
    "Potion",
    "Wand",
    "Staff",
    "Scroll",
    "Breath",
    "\n"
};



const char *magic_types[] =
{
    "Damage",
    "Affects",
    "Unaffects",
    "Points",
    "Alter Objects",
    "Groups",
    "Masses",
    "Areas",
    "Summons",
    "Creations",
    "Manual",
    "Room",
    "Stance",
    "\n"
};

const char *target_types[] =
{
    "(Ignore)",
    "(Player in Room)",
    "(In The World)",
    "(Self When Fighting)",
    "(Vict When Fighting)",
    "(Self Only)",
    "(Not Self)",
    "(Obj In Inventory)",
    "(Obj In Room)",
    "(Obj In World)",
    "(Obj In Equip)",
    "(All In Room)",
    "(All In An Area)",
    "(Directional)",
    "\n"
};

const char *tunnel_msgs[] =
{
    "You hit an especially hard vein of rock. It sends vibrations up your arms.",
    "Pieces of earth fall from the ceiling.",
    "Strains of Hi ho, Hi ho, it's off to work we go! run through your head as you dig, dig, dig.",
    "Sweat rolls down your face, leaving streaks through the build-up of dirt.",
    "You hit a soft spot as you dig, causing a small shower of dirt and stones.",
    "The handle on your $p starts to splinter. Can't anybody make anything of quality these days?",
    "Small stones sneak their way into your shoes and grind into your flesh with every step.",
    "$p creaks with use.",
    "A big fat worm drops from the spot you just struck with $p.",
    "A worm wriggles around blindly before finding a pile of loose soil to disappear into.",
    "And you thought farming was monotonous?",
    "The sound of rocks falling comes from somewhere to the south.",
    "A cloud of dust starts to build up in the room.",
    "All this tunnelling, you're bound to find something. Or...?",
    "Are we having fun yet?",
    "A spark lights the tunnel as $p hits a stone.",
    "A big spark flares as $p hits a stone.",
    "Your back starts to ache.",
    "You hear a scurrying sound from a nearby tunnel.",
    "There's a loud rumble in the distance.",
    "You dent the blade of $p on a stone.",
    "Strike, scoop, lift, empty. Strike, scoop, lift, and empty.",
    "A fine layer of dust covers you from head to toe.",
    "This tunnelling is hell on the hands. You are going to get calloused unless you find good gloves.",
    "All this digging. You must be close to the planet's core by now.",
    "You hear the sounds of running footsteps in the tunnel behind you.",
    "$p starts to slip from your sweaty grip as you impale the earth.",
    "A spider falls into your hair as you empty a load of soil from $p.",
    "A spider plops onto your head as you empty a load of soil from $p.",
    "$p breaks into a huge ant colony. They swarm up the handle of $p.",
    "$p creaks as you dig it into the earth.",
    "$p creaks as you dig it into the earth. Your back creaks along with it.",
    "Layers of moist, gritty earth work their way under your clothes.",
    "The dust works its way under your clothes and goes right for the sweaty spots.",
    "A black beetle clings to $p's edge and starts to walk along it.",
    "$p easily sinks deep into the earth. You'll be rich before you know it.",
    "$p hits something solid. Is that a stone or a bone?"  //36
};


/* Constants for Assemblies    *****************************************/
const char *AssemblyTypes[] =
{
    "assemble",
    "bake",
    "brew",
    "craft",
    "fletch",
    "knit",
    "make",
    "mix",
    "thatch",
    "weave",
    "forge",
    "\n"
};

const char *unused_spellname = "!UNUSED!";   /* So we can get &unused_spellname */


