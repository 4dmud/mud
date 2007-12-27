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
#include "dg_event.h"
#include "clan.h"


/*
#define GET_CLASS_TIER(ch)
#define GET_REMORT_TIER(ch)         ((ch)->player.was_class_spec)
#define GET_REMORT_TWO_TIER(ch)     ((ch)->player.was_class1_spec)
#define GET_REMORT_THREE_TIER(ch)   ((ch)->player.was_class2_spec)
*/


/* external variables */
extern int siteok_everyone;

/* external functions */
int parse_class(char arg);

/* local functions */
void remort_char(struct char_data *ch);
int has_class(struct char_data *ch, int chclass);
int class_count(struct char_data *ch, int chclass);
int parse_spec(char arg);
int can_level(struct char_data *ch);


/*tier fucntions by Mordecai*/
int highest_tier(struct char_data *ch);	/* returns dominant class */
int has_tier(struct char_data *ch, int chclass);	/* returns 0 for no, 1 for yes */
int tier_level(struct char_data *ch, int chclass);	/* returns 0 to 4 for how many of that class that ch has spec */
int count_tiers(struct char_data *ch);	/* returns 0 to 4 for how many tiers ch has non specific */
int current_class_is_tier(struct char_data *ch);	/* returns 0 or 1 */
int current_class_is_tier_num(struct char_data *ch);	/* returns 0 to 4 */
int num_casting(struct char_data *ch);


void remort_char(struct char_data *ch)
{
    GET_LEVEL(ch) = 1;
    GET_EXP(ch) = 1;
    GET_GROUP_EXP(ch) = 1;

    GET_MAX_HIT(ch) = 30 + REMORTS(ch);
    GET_MAX_STAMINA(ch) = 100 + REMORTS(ch);

    advance_level(ch);
    

    if (siteok_everyone)
	SET_BIT_AR(PLR_FLAGS(ch), PLR_SITEOK);
	
	if (GET_CLAN(ch) == 12) {
	GET_CLAN(ch) = 0;
	GET_CLAN_RANK(ch) = 0;
	}
	
  save_char(ch);
}

ACMD(do_remort)
{
    int i = 0, k;
    byte current, remort, rtwo;
    int tier[3];
    struct obj_data *char_eq[NUM_WEARS];
    #ifndef NO_EXTRANEOUS_TRIGGERS
    int ret = 0;
    #endif

    current = GET_CLASS(ch);
    remort = GET_REMORT(ch);
    rtwo = GET_REMORT_TWO(ch);

    tier[0] = GET_CLASS_TIER(ch);
    tier[1] = GET_REMORT_TIER(ch);
    tier[2] = GET_REMORT_TWO_TIER(ch);


    if (IS_NPC(ch) || GET_LEVEL(ch) != 50) {
	send_to_char("That is not a very good idea.\r\n", ch);
	return;
    }

    skip_spaces(&argument);

    if (!argument) {
	send_to_char
	    ("What class do you want to remort to?\r\nREMORT <class>\r\n",
	     ch);
	return;
    }

// did they supply a valid class
    if ((i = parse_class(*argument)) == CLASS_UNDEFINED) {
	send_to_char("That is not a valid class.\r\n", ch);
	return;
    }
// do they have the experience
    if (!can_level(ch)) {
	new_send_to_char(ch, "You need more experience before you can remort.\r\n");
	return;
    }
/* remove affects from eq and spells (from char_to_store) */
    /* Unaffect everything a character can be affected by */
    for (k = 0; k < NUM_WEARS; k++) {
	if (GET_EQ(ch, k)) {
	    char_eq[k] = unequip_char(ch, k);
#ifndef NO_EXTRANEOUS_TRIGGERS
	    if (remove_otrigger(char_eq[k], ch) == -1)
	    char_eq[k] = NULL;
#endif
	} else
	    char_eq[k] = NULL;
    }
    while (ch->affected)
	affect_remove(ch, ch->affected);
    
    /* lets make am a master of the class ifthey have done t4 already */
    if (current_class_is_tier_num(ch) == 4)    GET_MASTERY(ch, (int) GET_CLASS(ch)) = TRUE;
    
    // shift the previous classes down the list
    GET_REMORT_THREE(ch) = rtwo;
    GET_REMORT_TWO(ch) = remort;
    GET_REMORT(ch) = current;
    GET_CLASS(ch) = i;

    //shift the tiers down the list
    GET_CLASS_TIER(ch) = TRUE;
    GET_REMORT_TIER(ch) = tier[0];
    GET_REMORT_TWO_TIER(ch) = tier[1];
    GET_REMORT_THREE_TIER(ch) = tier[2];
    // Now set the stats

    remort_char(ch);		// advance them to level 1
    REMORTS(ch)++;
    send_to_all("{cGREMORT: %s has just %s %s%s{c0\r\n", 
    GET_NAME(ch), 
    ((current == i) ? "specialized further in" : "remorted to"), 
    simple_class_name(ch), 
    (grand_master(ch) ? " Grand Master!" : "!"));
    log("REMORT: %s has just remorted to %s", GET_NAME(ch), simple_class_name(ch));
    send_to_char("Enjoy your new class.\r\n", ch);
    
    for (i = 0; i < NUM_WEARS; i++) {
	if (char_eq[i]){
#ifndef NO_EXTRANEOUS_TRIGGERS
      if ((ret = wear_otrigger(char_eq[i], ch, i)) > 0)
#endif
      equip_char(ch, char_eq[i], i);
#ifndef NO_EXTRANEOUS_TRIGGERS
      else if (ret == 0)
        obj_to_char(char_eq[i], ch);
#endif
    }
    }
    
}
/*
Okay So,
 Now every mob in the game that is tier 0 now (if you dont know what a tier is
 have a look at www.timespace.co.nz/remorting.doc )
 Has a chance of randomly poping into the game as a tier 1 to 4
 chances of t4 being very slim.
 
 Tier 0 mobs (almost every standard mob) is now slower in battle, and mobs up to level
 51 (instead of 47) will be less damage then 'standard'.
 mobs over level 51 do +70% more damage then standard mobs, basicly seperating
 the mobs into two groups. One for the standard player, and one for the power player.
 Keeping (or attempting to) keep the game chalanging and interesting for all.

 Tiered mobs are faster and do more damage and take less damage then non tiered mobs,
 but they also GIVE more exp and more gold (and possibly load exra items)
 Currently all mobs in the game have a chance of becoming tiered,
but that will change to being only NON AGRESSIVE mobs shortly.
 Although this feature is in, it may go through a few changes just to get it balanced a lil better.

 On a different note, you will now notice that you have
another row on your WORTH display. Saying Mastered Classes.
 This shows you the classes that you have reached tier 4 in and THEN remorted from.
 As you get a noted master in each class, you will get a small bonus.
And if you get mastery in all 8 classes, you gain Supreme Mastery
and are able to access all skills + spells in the game, along with having the bonus for
every class's master.



*/

int has_class(struct char_data *ch, int chclass)
{

    if (chclass == -1) {
	log("SYSERR: null class asked for in has_class");
	return -1;
    }

    if ((chclass == GET_CLASS(ch)) || (chclass == GET_REMORT(ch))
	|| (chclass == GET_REMORT_TWO(ch))
	|| (chclass == GET_REMORT_THREE(ch)))
	return 1;
//if (GET_LEVEL(ch) > LVL_IMMORT)
//return 1;
    return 0;
}

int class_count(struct char_data *ch, int chclass)
{
    int count = 0;
    if (chclass == -1) {
	log("SYSERR: null class asked for in class_count");
	return -1;
    }
    if (GET_CLASS(ch) == chclass)
	count++;
    if (GET_REMORT(ch) == chclass)
	count++;
    if (GET_REMORT_TWO(ch) == chclass)
	count++;
    if (GET_REMORT_THREE(ch) == chclass)
	count++;

    return (count);
}

int parse_spec(char arg)
{
    arg = LOWER(arg);

    switch (arg) {
    case 'm':
	return (0);
    case 's':
	return (1);
    default:
	return CLASS_UNDEFINED;

    }
}


int highest_tier(struct char_data *ch)
{				/* returns dominant class */
    int count = 0, chclass;
    int temp = 0, counter = 0;
    
    if (IS_NPC(ch))
    return MOB_TIER(ch);

    for (chclass = 0; chclass < NUM_CLASSES; chclass++) {
	if (GET_CLASS(ch) == chclass && GET_CLASS_TIER(ch))
	    count++;
	if (GET_REMORT(ch) == chclass && GET_REMORT_TIER(ch))
	    count++;
	if (GET_REMORT_TWO(ch) == chclass && GET_REMORT_TWO_TIER(ch))
	    count++;
	if (GET_REMORT_THREE(ch) == chclass && GET_REMORT_THREE_TIER(ch))
	    count++;

	if (count > counter) {
	    counter = count;
	    temp = chclass;
	}
	count = 0;
    }

    return (counter);

}

int dominant_tier(struct char_data *ch)
{				/* returns dominant class */
    int count = 0, chclass;
    int temp = 0, counter = 0;
    
    if (IS_NPC(ch))
    return GET_CLASS(ch);

    for (chclass = 0; chclass < 8; chclass++) {
	if (GET_CLASS(ch) == chclass && GET_CLASS_TIER(ch))
	    count++;
	if (GET_REMORT(ch) == chclass && GET_REMORT_TIER(ch))
	    count++;
	if (GET_REMORT_TWO(ch) == chclass && GET_REMORT_TWO_TIER(ch))
	    count++;
	if (GET_REMORT_THREE(ch) == chclass && GET_REMORT_THREE_TIER(ch))
	    count++;

	if (count > counter) {
	    counter = count;
	    temp = chclass;
	}
	count = 0;
    }

    return (temp);

}

/*
GET_CLASS_TIER(ch)
GET_REMORT_TIER(ch)
GET_REMORT_TWO_TIER(ch)
GET_REMORT_THREE_TIER(ch)
*/

/* returns 0 for no, 1 for yes
 * If they have a tier of that class */
int has_tier(struct char_data *ch, int chclass)
{				/* returns 0 for no, 1 for yes */
if (IS_NPC(ch)) {
if (MOB_TIER(ch) > 0 && GET_CLASS(ch) == chclass)
return 1;
else
return 0;
}

    if (GET_CLASS(ch) == chclass && GET_CLASS_TIER(ch))
	return 1;
    if (GET_REMORT(ch) == chclass && GET_REMORT_TIER(ch))
	return 1;
    if (GET_REMORT_TWO(ch) == chclass && GET_REMORT_TWO_TIER(ch))
	return 1;
    if (GET_REMORT_THREE(ch) == chclass && GET_REMORT_THREE_TIER(ch))
	return 1;

    return 0;
}

int tier_level(struct char_data *ch, int chclass)
{				/* returns 0 to 4 for how many of that class that ch has spec */
int count = 0;
if (IS_NPC(ch)) {
if (MOB_TIER(ch) > 0 && GET_CLASS(ch) == chclass)
return MOB_TIER(ch);
else
return 0;
}
    
    if (GET_CLASS(ch) == chclass && GET_CLASS_TIER(ch))
	count++;
    if (GET_REMORT(ch) == chclass && GET_REMORT_TIER(ch))
	count++;
    if (GET_REMORT_TWO(ch) == chclass && GET_REMORT_TWO_TIER(ch))
	count++;
    if (GET_REMORT_THREE(ch) == chclass && GET_REMORT_THREE_TIER(ch))
	count++;

    return count;
}


int count_tiers(struct char_data *ch)
{				/* returns 0 to 4 for how many tiers ch has non specific */
int count = 0;
if (IS_NPC(ch)) 
return MOB_TIER(ch);

    if (GET_CLASS_TIER(ch))
	count++;
    if (GET_REMORT_TIER(ch))
	count++;
    if (GET_REMORT_TWO_TIER(ch))
	count++;
    if (GET_REMORT_THREE_TIER(ch))
	count++;

    return count;
}

int current_class_is_tier(struct char_data *ch)
{				/* returns 0 or 1 */
if (IS_NPC(ch)) 
return MOB_TIER(ch);

    return GET_CLASS_TIER(ch);
}
int current_class_is_tier_num(struct char_data *ch)
{				/* returns 0 to 4 */
    int chclass = GET_CLASS(ch);
    int count = 0;
    
    if (IS_NPC(ch)) 
return MOB_TIER(ch);
    
    if (!GET_CLASS_TIER(ch)) // current class is multi
    return 0;
    
    if (GET_CLASS_TIER(ch))
	count++;
    if (GET_REMORT(ch) == chclass && GET_REMORT_TIER(ch))
	count++;
    if (GET_REMORT_TWO(ch) == chclass && GET_REMORT_TWO_TIER(ch))
	count++;
    if (GET_REMORT_THREE(ch) == chclass && GET_REMORT_THREE_TIER(ch))
	count++;

    return count;
}
#define CASTER(cl) (cl == CLASS_MAGE || cl == CLASS_ESPER || cl == CLASS_PRIEST)
int num_casting(struct char_data *ch) {
    int chclass = GET_CLASS(ch);
    int count = 1;
    
    if (IS_NPC(ch)) 
return MOB_TIER(ch);
    

    
    if (GET_CLASS_TIER(ch) && CASTER(chclass))
	count++;
    if (CASTER(GET_REMORT(ch)) && GET_REMORT_TIER(ch))
	count++;
    if (CASTER(GET_REMORT_TWO(ch)) && GET_REMORT_TWO_TIER(ch))
	count++;
    if (CASTER(GET_REMORT_THREE(ch)) && GET_REMORT_THREE_TIER(ch))
	count++;

    return count;
}


#define MELEE(cl) (cl == CLASS_THIEF || cl == CLASS_RANGER || cl == CLASS_HUNTER || cl == CLASS_WARRIOR || cl == CLASS_GYPSY)
int num_melee_tier(struct char_data *ch) {
    int chclass = GET_CLASS(ch);
    int count = 0;
    
    if (IS_NPC(ch)) 
return MOB_TIER(ch);
    

    
    if (GET_CLASS_TIER(ch) && MELEE(chclass))
	count++;
    if (MELEE(GET_REMORT(ch)) && GET_REMORT_TIER(ch))
	count++;
    if (MELEE(GET_REMORT_TWO(ch)) && GET_REMORT_TWO_TIER(ch))
	count++;
    if (MELEE(GET_REMORT_THREE(ch)) && GET_REMORT_THREE_TIER(ch))
	count++;

    return count;
}

