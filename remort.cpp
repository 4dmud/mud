#include "config.h"
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
int parse_class ( char arg );

/* local functions */
void remort_char ( Character *ch );
int has_class ( Character *ch, int chclass );
int class_count ( Character *ch, int chclass );
int parse_spec ( char arg );
int can_level ( Character *ch );


/*tier functions by Mordecai*/
int highest_tier ( Character *ch );	/* returns dominant class */
int has_tier ( Character *ch, int chclass );	/* returns 0 for no, 1 for yes */
int tier_level ( Character *ch, int chclass );	/* returns 0 to 4 for how many of that class that ch has spec */
int count_tiers ( Character *ch );	/* returns 0 to 4 for how many tiers ch has non specific */
int current_class_is_tier ( Character *ch );	/* returns 0 or 1 */
int current_class_is_tier_num ( Character *ch );	/* returns 0 to 4 */
int num_casting ( Character *ch );


void remort_char ( Character *ch )
{
    int remorts = REMORTS ( ch );
    GET_LEVEL ( ch ) = 1;
    GET_EXP ( ch ) = 1;
    GET_GROUP_EXP ( ch ) = 1;

    REMORTS ( ch ) ++;
    GET_MAX_HIT ( ch ) = 30 + remorts;
    GET_MAX_STAMINA ( ch ) = 100 + remorts;
    if ( GET_CLAN ( ch ) == 12 && !PLR_FLAGGED(ch, PLR_NEWBIE_HLPR))
    {
        GET_CLAN ( ch ) = 0;
        GET_CLAN_RANK ( ch ) = 0;
        if (REMORTS(ch) <= 4)
          ch->Send("You were removed from the Seekers clan, because the time has come to move on.\r\n");
        else
          ch->Send("You were removed from the Seekers clan, because you're just too old!\r\n");
    }
        if ( GET_CLAN ( ch ) == 12 )
      ch->Send("Because you were a helper in Seekers, you were not booted upon remorting like non-helpers.\r\nIf you wish to leave Seekers you can go 2 south from recall and say, 'I want to leave Seekers'.\r\n");



    advance_level ( ch );
    GET_WIMP_LEV ( ch ) = GET_MAX_HIT ( ch ) /2;

    if ( siteok_everyone )
        SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_SITEOK );



}

ACMD ( do_remort )
{
    int i = 0, slotnr;
    sbyte current, remort, rtwo, rthree;
    char clas[MAX_INPUT_LENGTH];
    char slot[MAX_INPUT_LENGTH];

    current = GET_CLASS ( ch );
    remort = GET_REMORT ( ch );
    rtwo = GET_REMORT_TWO ( ch );
    rthree = GET_REMORT_THREE ( ch );

    if ( IS_NPC ( ch ) || GET_LEVEL ( ch ) != 50 || GET_RACE ( ch ) == RACE_GLADIATOR )
    {
        ch->Send ( "That is not a very good idea.\r\n" );
        return;
    }

    skip_spaces ( &argument );

    if ( !argument )
    {
        ch->Send ( "What class do you want to remort to?\r\nREMORT <class>\r\n" );
        return;
    }

    two_arguments ( argument, clas, slot );

    // did they supply a valid class
    for ( i = 0; i < NUM_CLASSES; ++i )
        if ( !strcasecmp ( clas, pc_class_types[i] ) )
            break;

    if ( i == NUM_CLASSES )
    {
        ch->Send ( "You can't remort to %s.\r\n", clas );
        return;
    }
    // do they have the experience
    if ( !can_level ( ch ) )
    {
        ch->Send ( "You need more experience before you can remort.\r\n" );
        return;
    }
    // did they supply a valid slot?
    if ( rthree != -1 && ( !is_number ( slot ) || ( slotnr = atoi ( slot ) ) < 1 || slotnr > 4 ) )
    {
        ch->Send ( "Usage: remort <class> <slot number>. Which slot do you want to discard? They're shown on your score sheet.\r\n" );
        return;
    }

    ch->MakeNaked();
    remove_all_normal_affects ( ch );

    bool already_gm = grand_master ( ch );

    /* lets make am a master of the class ifthey have done t4 already */
    if ( current_class_is_tier_num ( ch ) == 4 )
        GET_MASTERY ( ch, ( int ) GET_CLASS ( ch ) ) = TRUE;

    if ( rthree != -1 )
    {
        if ( slotnr > 3 )
            GET_REMORT_THREE ( ch ) = rtwo;
        if ( slotnr > 2 )
            GET_REMORT_TWO ( ch ) = remort;
        if ( slotnr > 1 )
            GET_REMORT ( ch ) = current;
        GET_CLASS ( ch ) = i;
    }
    else
    {
        // shift the previous classes down the list
        GET_REMORT_THREE ( ch ) = rtwo;
        GET_REMORT_TWO ( ch ) = remort;
        GET_REMORT ( ch ) = current;
        GET_CLASS ( ch ) = i;

        //shift the tiers down the list
        GET_CLASS_TIER ( ch ) = TRUE;
        GET_REMORT_TIER ( ch ) = TRUE;
        GET_REMORT_TWO_TIER ( ch ) = TRUE;
        GET_REMORT_THREE_TIER ( ch ) = TRUE;
    }

    // Now set the stats
    remort_char ( ch );		// advance them to level 1
    send_to_all ( "{cGREMORT: %s has just %s %s%s{c0\r\n",
                  GET_NAME ( ch ),
                  ( ( current == i ) ? "specialized further in" : "remorted to" ),
                  simple_class_name ( ch ),
                  ( grand_master ( ch ) ? " Grand Master!" : "!" ) );
    if ( !already_gm && grand_master ( ch ) )
        send_to_all ( "{cY%s has become a Grand Master!{c0\r\n", GET_NAME ( ch ) );
    log ( "REMORT: %s has just remorted to %s", GET_NAME ( ch ), simple_class_name ( ch ) );
    ch->Send ( "Enjoy being %s.\r\n", simple_class_name ( ch ) );
    ch->MakeClothed();
    ch->save();
}
/*
Okay So,
 Now every mob in the game that is tier 0 now (if you don't know what a tier is
 have a look at www.timespace.co.nz/remorting.doc )
 Has a chance of randomly poping into the game as a tier 1 to 4
 chances of t4 being very slim.

 Tier 0 mobs (almost every standard mob) is now slower in battle, and mobs up to level
 51 (instead of 47) will be less damage than 'standard'.
 mobs over level 51 do +70% more damage than standard mobs, basicly seperating
 the mobs into two groups. One for the standard player, and one for the power player.
 Keeping (or attempting to) keep the game chalanging and interesting for all.

 Tiered mobs are faster and do more damage and take less damage than non tiered mobs,
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

int has_class ( Character *ch, int chclass )
{

    if ( chclass == -1 )
    {
        log ( "SYSERR: null class asked for in has_class" );
        return -1;
    }
    if ( IS_NPC ( ch ) )
        return 0;

    if ( ( chclass == GET_CLASS ( ch ) ) || ( chclass == GET_REMORT ( ch ) )
            || ( chclass == GET_REMORT_TWO ( ch ) )
            || ( chclass == GET_REMORT_THREE ( ch ) ) )
        return 1;
    //if (GET_LEVEL(ch) > LVL_IMMORT)
    //return 1;
    return 0;
}

int class_count ( Character *ch, int chclass )
{
    if ( IS_NPC ( ch ) )
        return 0;
    int count = 0;
    if ( chclass == -1 )
    {
        log ( "SYSERR: null class asked for in class_count" );
        return -1;
    }
    if ( GET_CLASS ( ch ) == chclass )
        count++;
    if ( GET_REMORT ( ch ) == chclass )
        count++;
    if ( GET_REMORT_TWO ( ch ) == chclass )
        count++;
    if ( GET_REMORT_THREE ( ch ) == chclass )
        count++;

    return ( count );
}

int parse_spec ( char arg )
{
    arg = LOWER ( arg );

    switch ( arg )
    {
        case 'm':
            return ( 0 );
        case 's':
            return ( 1 );
        default:
            return CLASS_UNDEFINED;

    }
}


int highest_tier ( Character *ch )
{
    /* returns dominant class */
    int count = 0, chclass;
    int counter = 0;

    if ( IS_NPC ( ch ) )
        return MOB_TIER ( ch );

    for ( chclass = 0; chclass < NUM_CLASSES; chclass++ )
    {
        if ( GET_CLASS ( ch ) == chclass )
            count++;
        if ( GET_REMORT ( ch ) == chclass )
            count++;
        if ( GET_REMORT_TWO ( ch ) == chclass )
            count++;
        if ( GET_REMORT_THREE ( ch ) == chclass )
            count++;

        if ( count > counter )
        {
            counter = count;
        }
        count = 0;
    }

    return ( counter );

}

int dominant_tier ( Character *ch )
{
    /* returns dominant class */
    int count = 0, chclass;
    int temp = 0, counter = 0;

    if ( IS_NPC ( ch ) )
        return GET_CLASS ( ch );

    for ( chclass = 0; chclass < 8; chclass++ )
    {
        if ( GET_CLASS ( ch ) == chclass )
            count++;
        if ( GET_REMORT ( ch ) == chclass )
            count++;
        if ( GET_REMORT_TWO ( ch ) == chclass )
            count++;
        if ( GET_REMORT_THREE ( ch ) == chclass )
            count++;

        if ( count > counter )
        {
            counter = count;
            temp = chclass;
        }
        count = 0;
    }

    return ( temp );

}

/*
GET_CLASS_TIER(ch)
GET_REMORT_TIER(ch)
GET_REMORT_TWO_TIER(ch)
GET_REMORT_THREE_TIER(ch)
*/

/* returns 0 for no, 1 for yes
 * If they have a tier of that class */
int has_tier ( Character *ch, int chclass )
{
    /* returns 0 for no, 1 for yes */
    if ( IS_NPC ( ch ) )
    {
        if ( MOB_TIER ( ch ) > 0 && GET_CLASS ( ch ) == chclass )
            return 1;
        else
            return 0;
    }

    if ( GET_CLASS ( ch ) == chclass )
        return 1;
    if ( GET_REMORT ( ch ) == chclass )
        return 1;
    if ( GET_REMORT_TWO ( ch ) == chclass )
        return 1;
    if ( GET_REMORT_THREE ( ch ) == chclass )
        return 1;

    return 0;
}

int tier_level ( Character *ch, int chclass )
{
    /* returns 0 to 4 for how many of that class that ch has spec */
    int count = 0;
    if ( IS_NPC ( ch ) )
    {
        if ( MOB_TIER ( ch ) > 0 && GET_CLASS ( ch ) == chclass )
            return MOB_TIER ( ch );
        else
            return 0;
    }

    if ( GET_CLASS ( ch ) == chclass )
        count++;
    if ( GET_REMORT ( ch ) == chclass )
        count++;
    if ( GET_REMORT_TWO ( ch ) == chclass )
        count++;
    if ( GET_REMORT_THREE ( ch ) == chclass )
        count++;

    return count;
}



int current_class_is_tier_num ( Character *ch )
{
    /* returns 0 to 4 */
    int chclass = GET_CLASS ( ch );
    int count = 1;

    if ( IS_NPC ( ch ) )
        return MOB_TIER ( ch );

    if ( GET_REMORT ( ch ) == chclass )
        count++;
    if ( GET_REMORT_TWO ( ch ) == chclass )
        count++;
    if ( GET_REMORT_THREE ( ch ) == chclass )
        count++;

    return count;
}
#define CASTER(cl) (cl == CLASS_MAGE || cl == CLASS_ESPER || cl == CLASS_PRIEST)
int num_casting ( Character *ch )
{
    int chclass = GET_CLASS ( ch );
    int count = 1;

    if ( IS_NPC ( ch ) )
        return MOB_TIER ( ch );

    if ( CASTER ( chclass ) )
        count++;
    if ( CASTER ( GET_REMORT ( ch ) ) )
        count++;
    if ( CASTER ( GET_REMORT_TWO ( ch ) ) )
        count++;
    if ( CASTER ( GET_REMORT_THREE ( ch ) ) )
        count++;

    return count;
}


#define MELEE(cl) (cl == CLASS_THIEF || cl == CLASS_RANGER || cl == CLASS_HUNTER || cl == CLASS_WARRIOR || cl == CLASS_GYPSY)
int num_melee_tier ( Character *ch )
{
    int chclass = GET_CLASS ( ch );
    int count = 0;

    if ( IS_NPC ( ch ) )
        return MOB_TIER ( ch );



    if ( MELEE ( chclass ) )
        count++;
    if ( MELEE ( GET_REMORT ( ch ) ) )
        count++;
    if ( MELEE ( GET_REMORT_TWO ( ch ) ) )
        count++;
    if ( MELEE ( GET_REMORT_THREE ( ch ) ) )
        count++;

    return count;
}

