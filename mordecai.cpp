/* these are some commands that we can start with now
   to give places for people to spend their tokens
   the first command lets people transfer tokens in their
   account into practice sessions.
   This is helpful for newbies, but most players
   that have a few remorts have practice sessions in the thousands.
   This will also allow them to use up their practice sessions.
   100 practice sessions can equal 1 bronze token.
   20  practice sessions can equal 1 brass token.
   whereas it will cost 1 brass token to buy a practice session.
   (5 brass = bronze)

   The command will be: transfer <token|sessions> <number>

   transfer token 1 - will transfer 1 brass token into a training session.
   transfer session 1 - will transfer 20 sessions into a brass token.
   ------------------------

   the command convert will be used for players to make 1 larger
   denomonation in their account into the next smaller denomonation.

   ie: 1 gold into 10 silver.
       1 silver into 10 bronze.
       1 bronze into 5 brass.

       convert <brass|bronze|silver|gold>
       it will just place that amount in your account for you to deduct.

   -------------------------mord-----------------------------

O======================================================================O
|Name: Soulstar            Class: [ Thief      ] |Level:        [ 255] |
|Race: Faun                Sex:   [ Male       ] |Age:          [  26] |
|-------------------------------|----------------+---------------------|
|Str: [17] [17] [100] [100]     |Weight:  [  4%] |Brass  Tokens:[ 100] |
|Int: [25] [17] Wis: [25] [17]  |Items:   [  4%] |Bronze Tokens:[ 100] |
|Con: [25] [17] Dex: [25] [17]  |Thirst:  [ -4%] |Silver Tokens:[ 100] |
|Cha: [25] [17]                 |Full:    [ -4%] |Gold   Tokens:[ 100] |
|Gold: [                      ] |Drunk:   [-10%] |                     |
|Bank: [                      ] |                |Fame         :[ 100] |
|-------------------------------+----------------+---------------------|
|Exp. Gained  :  [  999999000 ] |PKills:[    12] |Bank:  [         0 ] |
|Exp. to Level:  [ 3294968296 ] |PDeath:[    12] |Gold:  [    983897 ] |
|-------------------------------+----------------+---------------------|
|Hit   Points: [ 3050]  [ 3050] |SPEEDMIN: [  8] |Alignment:   [-1000] |
|Mana  Points: [ 2702]  [ 2702] |SPEEDMAX: [ 20] |Hit Bonus:   [   23] |
|Move  Points: [ 1747]  [ 1747] |                |Dam Bonus:   [   78] |
|Damage Percent Resist: [  90%] |Mail:     [yes] |Practices:   [12343] |
O======================================================================O

     equip_char(ch, obj, where);

*/

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "house.h"
#include "screen.h"
#include "constants.h"
#include "oasis.h"
#include "dg_scripts.h"
#include "spells.h"
#include "genmob.h"


/* external functions */
int wep_hands(OBJ_DATA *wep);

ACMD ( do_swap )
{
    obj_data *w1 = GET_EQ ( ch, WEAR_WIELD );
    obj_data *w2 = GET_EQ ( ch, WEAR_WIELD_2 );

    if ( w1 == NULL || w2 == NULL )
{
        ch->Send ( "You are not dual wielding weapons!\r\n" );
        return;
    }


    if ( ( ( GET_OBJ_TYPE ( w1 ) != ITEM_WEAPON )
            || ( GET_OBJ_TYPE ( w2 ) != ITEM_WEAPON ) ) )
    {
        ch->Send ( "You are not dual wielding WEAPONS.\r\n" );
        return;
    }
    if ( wep_hands ( w1 ) == 2 ) {
        ch->Send("You can't swap a two-handed weapon to your offhand.\r\n");
        return;
    }

    w1 = unequip_char ( ch, WEAR_WIELD );
    w2 = unequip_char ( ch, WEAR_WIELD_2 );
    equip_char ( ch, w1, WEAR_WIELD_2 );
    equip_char ( ch, w2, WEAR_WIELD );

    act ( "You swap hands using $p and $P.", FALSE, ch, w1, w2,  TO_CHAR );
    act ( "$n swaps hands using $p and $P.", TRUE, ch, w1, w2,   TO_ROOM );
    return;
}


/*swap hands wielding weapons*/
/*
ACMD(do_suicide)
 {
     if (IS_NPC(ch))
      return;

     if ( argument[0] == '\0' || str_cmp(argument, "yes"))
     {
       send_to_char( "If you mean it, type suicide 'yes'.\r\n", ch );
       return;
     }

     act("$n gives up the will to live.",TRUE, ch, NULL, NULL, TO_ROOM );
     act("You surrender the will to live.",FALSE, ch, NULL, NULL, TO_CHAR);
     raw_kill( ch, ch );
     return;
 }

*/
