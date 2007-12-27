#include "conf.h"
#include "sysdep.h"

#include <sys/stat.h>

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "constants.h"
#include "dg_scripts.h"
#include "dg_event.h"
#include "trees.h"
#include "damage.h"
#include "fight.h"
#include "action.h"

#define LANA(string) (strchr("aeiouyAEIOUY", string[0]) ? "an" : "a")
#define CANA(string) (strchr("aeiouyAEIOUY", string[0]) ? "An" : "A")
#define TIER (current_class_is_tier_num(ch))


EVENTFUNC(message_event);
ASKILL(skill_manifest);
ACMD(do_flee);
int can_fight(Character *ch, Character *vict, int silent);
void make_manifest(Character *ch,struct obj_data *obj);
ASKILL(skill_manipulate);
/* extern variables */
int has_weapon(Character *ch);
void start_fighting_delay(Character *vict, Character *ch);
int skill_cost(int h, int m, int v, Character *ch);
int tier_level(Character *ch, int chclass);
ASUB(sub_throttle);
int strangle_affect(Character *ch, Character *vict, int num);
/* extern procedures */
int mag_manacost(Character *ch, int spellnum);
void improve_skill(Character *ch, int skill);
void make_focus(Character *ch, int type, struct obj_data *o);
int set_task(Character *ch, int task);

void weather_change(int zon);
/*local*/



ACTION(thing_lumberjack);
ACTION(thing_manifest);
ACTION(thing_singwood);
ACTION(thing_juggle);
ACTION(thing_throttle);
ACTION(thing_tunneling);




ACTION(thing_lumberjack)
{
  long time = 0;
  //const char *to_vict = NULL;
  const char *to_char = NULL;
  const char *to_room = NULL;
  obj_vnum lognum = -1;
int i;

  struct obj_data *object = GET_EQ(ch, WEAR_WIELD);

  if (!object || GET_OBJ_TYPE(object) != ITEM_AXE)
  {
    ch->Send( "You can't chop trees without a good axe!\r\n");
    *num = 0;
    time = 0;
  }

  if (!*num)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    return 0;
  }

  if (obj==NULL || IN_ROOM(obj)==NULL || (!HERE(obj, ch)))
  {
    /* object is gone.*/
    to_char = "You can't seem to concentrate on felling the tree.";
    *num = 0;
    time = 0;
  }
  else
  {
    switch (*num)
    {
    case 12:
      to_char = "You aim a stroke of the axe at the base of the tree trunk.";
      to_room = "$n aims a stroke of the axe at the base of the tree trunk.";
      time = (3 RL_SEC);
      break;
    case 11:
      to_room = "White chips of wood fly in the air as the axe bites into the trunk.";
      to_char = "White chips of wood fly in the air as the axe bites into the trunk.";
      time = (3 RL_SEC);
      break;
    case 10:
      to_char = "You keep chopping away at the trunk.";
      to_room = "$n keeps chopping away at the trunk.";
      time = (4 RL_SEC);
      break;
    case 9:
      to_room = "The forest rings with the sound of the rhythmic strokes of the axe.";
      to_char = "The forest rings with the sound of the rhythmic strokes of the axe.";
      time = (5 RL_SEC);
      break;
    case 8:
      to_char = "You pause for a moment stretching your back.";
      to_room = "$n pauses for a moment stretching $s back.";
      time = (2 RL_SEC);
      break;
    case 7:
      to_room = "Chopping down huge trees is hard work!";
      to_char = "Chopping down huge trees is hard work!";
      time = (5 RL_SEC);
      break;
    case 6:
      to_char = "Wiping the sweat from your brow you resume your work with the axe.";
      to_room = "Wiping the sweat from the brow $n resumes $s work with the axe.";
      time = (4 RL_SEC);
      break;
    case 5:
      to_room = "The trunk is almost hewn through now.";
      to_char = "The trunk is almost hewn through now.";
      time = (4 RL_SEC);
      break;
    case 4:
      to_room = "There is a creaking sound from fibres splintering, as the tree leans slightly.";
      to_char = "There is a creaking sound from fibres splintering, as the tree leans slightly.";
      time = (1 RL_SEC);
      break;
    case 3:
      to_char = "You yell 'TIMBER!'";
      to_room = "$n yells 'TIMBER!'";
      time = (2 RL_SEC);
      break;
    case 2:
      to_room = "The tree groans as the leaning increases rapidly.";
      to_char = "The tree groans as the leaning increases rapidly.";
      time = (1 RL_SEC);
      break;
    case 1:
      to_room = "With a crackle of broken twigs and an earthshattering BOOM! The tree hits the ground.";
      to_char = "With a crackle of broken twigs and an earthshattering BOOM! The tree hits the ground.";
      *num = 0;
      /*
       [52750] a heavy pine log                         
       17. [52751] a heavy oak log                          
       18. [52752] a heavy willow log                       
       19. [52753] a dogwood log                            
       20. [52754] a heavy Ironwood log                     
       21. [52755] a heavy fir log                          
       22. [52756] a heavy maple log                        
       23. [52757] a heavy elder log                        
       24. [52758] a heavy elm log                          
       25. [52759] a heavy beech log                        
       26. [52760] a heavy chestnut log                     
       27. [52761] a heavy spindleberry log                 
       28. [52762] a heavy ash log                          
       29. [52763] a heavy cypress log                      
       30. [52764] a heavy juniper log                      
       31. [52765] a heavy larch log                        
       32. [52766] a heavy yew log                          
       33. [52767] a heavy aspen log                        
       34. [52768] a heavy alder log                        
       35. [52769] a heavy birch log                        
       
       81. [52730] the pine tree                            
       82. [52731] the oak tree                             
       83. [52732] the willow tree                          
       84. [52733] the dogwood tree                         
       85. [52734] the ironwood tree                        
       86. [52735] the fir tree                             
       87. [52736] the maple tree                           
       88. [52737] the elder tree                           
       89. [52738] the elm tree                             
       90. [52739] the beech tree                           
       91. [52740] the chestnut tree                        
       92. [52741] the Spindleberry tree                    
       93. [52742] the Ash                                  
       94. [52743] the cypress tree                         
       95. [52744] the juniper tree                         
       96. [52745] the larch tree                           
       97. [52746] the yew tree                             
       98. [52747] the Aspen tree                           
       99. [52748] the alder tree                           
      100. [52749] the birch tree  
      */
      if (GET_OBJ_VAL(obj, 4) > 0 && real_object(GET_OBJ_VAL(obj, 4)) != NOTHING) {
lognum = GET_OBJ_VAL(obj, 4);
}
      if (GET_OBJ_VNUM(obj) >= 52730 && GET_OBJ_VNUM(obj) <= 52749)
      {
        lognum = GET_OBJ_VNUM(obj) + 20;
      }
      else if (GET_OBJ_VNUM(obj) == NOTHING)
      {
        lognum = 52750 + GET_OBJ_VAL(obj, 2);
      }
      else
      {
        ch->Send( "The tree disolves into sawdust.\r\n");
        return time;
      } 
if (real_object(lognum) == NOTHING) {
ch->Send( "The tree disolves into sawdust.\r\n");
        return time;
}
      
      
if (GET_OBJ_VAL(obj, 5) <= 0)
i = 1;
else
i = GET_OBJ_VAL(obj, 5);
for (;i>0;i--) 
if ((object = read_object(lognum, VIRTUAL)) != NULL)
      obj_to_room(object, IN_ROOM(ch));
      
if (obj != NULL)
      {
        extract_obj(obj);
        obj = NULL;
      }

    }
  }
  if (to_room!=NULL)
    act(to_room, FALSE, ch, obj, vict, TO_ROOM);


  if (to_char!=NULL)
    act(to_char, FALSE, ch, obj, vict, TO_CHAR);
  return time;

}

ACTION(thing_tunneling)
{
  long time = 0;
  const char *to_char = NULL;
  const char *to_room = NULL;
  struct obj_data *object = GET_EQ(ch, WEAR_WIELD);
  int check_mine_traps(Character *ch);
  void make_tunnel(Character *ch);

  if (!object)
  {
    ch->Send( "You can't tunnel without a tool!\r\n");
    *num = 0;
    return 0;
  }


  if (!*num)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    return 0;
  }

  if (!room || room->number != IN_ROOM(ch)->number || FIGHTING(ch) || GET_POS(ch) < POS_STANDING)
  {
    to_char = "You can't seem to concentrate on tunneling.";
    *num = 0;
    time = 0;
    return 0;
  }
  else if (R_EXIT(room, MINE_DIR(ch)))
  {
    to_char = "You discover a tunnel already exists where you are mining.";
    *num = 0;
    time = 0;
  }
  else
  {
    switch (*num)
    {
    default:
      switch (MINE_DIR(ch))
      {
      case NORTH:
        to_char = "You continue to tunnel north with $p.";
        to_room = "$n continues to tunnel north with $p.";
        break;
      case SOUTH:
        to_char = "You continue to tunnel south with $p.";
        to_room = "$n continues to tunnel south with $p.";
        break;
      case EAST:
        to_char = "You continue to tunnel east with $p.";
        to_room = "$n continues to tunnel east with $p.";
        break;
      case WEST:
        to_char = "You continue to tunnel west with $p.";
        to_room = "$n continues to tunnel west with $p.";
        break;
      case UP:
        to_char = "You continue to tunnel up with $p.";
        to_room = "$n continues to tunnel up with $p.";
        break;
      case DOWN:
        to_char = "You continue to tunnel down with $p.";
        to_room = "$n continues to tunnel down with $p.";
        break;
      }
      if (number(0, 1))
        to_char = tunnel_msgs[number(0, 36)];

      time = (long)((11 RL_SEC) * ((100.0 - IRANGE(0, MINE_SPEED(ch), 99))/100.0));
      log("%ld next tunnel hit is that many seconds.", time);
      new_send_to_char(ch ,"Next tunnel hit in %ld seconds.\r\n", time);
      break;
    case 1:
      *num = 0;
      log("Ending_tunneling start make_tunnel");
      make_tunnel(ch);
      return 0;
    }
  }

  if (to_room!=NULL)
    act(to_room, FALSE, ch, object, vict, TO_ROOM);

  if (to_char!=NULL)
    act(to_char, FALSE, ch, object, vict, TO_CHAR);

  if (check_mine_traps(ch) == -1)
    return -1;

  return time;

}

ACTION(thing_control_weather_worse)
{
  long time = 0;
  //const char *to_vict = NULL;
  const char *to_char = NULL;
  const char *to_room = NULL;
  int i;

  if (!*num)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    return 0;
  }
  i = GET_ROOM_ZONE(IN_ROOM(ch));
  if (!(number(0, 105) < GET_SKILL(ch, SPELL_CONTROL_WEATHER)))
  {
    to_char = "Your concentration falters. The magic stops.";
    to_room = "$n's concentration falters. The magic stops.";
    *num = 0;
  }
  else
  {
    switch (*num)
    {
    case 8:
      if (obj)
      {
        to_char = "You hold $p out infront of you.";
        to_room = "$n hold $s hands out infront of $mself.";
      }
      else
      {
        to_char = "You hold your hands out infront of you.";
        to_room = "$n holds $s hands out infront of $mself.";
      }
      zone_table[i].pressure -= GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (4 RL_SEC);
      break;
    case 7:
      to_char = "A slow pressure works its way along your spine as a gust of wind blows past, chilling exposed skin.";
      to_room = "A slow pressure works its way along $ns spine as a gust of wind blows past, chilling exposed skin..";
      zone_table[i].pressure -= GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (3 RL_SEC);
      break;
    case 6:
      zone_table[i].pressure -= GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (3 RL_SEC);
      break;
    case 5:
      to_char = "The pressure drops further, sounds become muffled.";
      to_room = "The pressure drops further, sounds become muffled.";
      zone_table[i].pressure -= GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (4 RL_SEC);
      break;
    case 4:
      zone_table[i].pressure -= GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (2 RL_SEC);
      break;
    case 3:
      to_char = "A boom of thunder shakes the ground.";
      to_room = "A boom of thunder shakes the ground.";
      zone_table[i].pressure -= GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (4 RL_SEC);
      break;
    case 2:
      to_char = "The sky opens up and a torrent of rain pours down.";
      to_room = "The sky opens up and a torrent of rain pours down.";
      zone_table[i].pressure -= GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (3 RL_SEC);
      break;
    case 1:
      to_char = "You lower your arms.";
      to_room = "$n lowers $s arms.";
      *num = 0;
      zone_table[i].pressure -= GET_INT(ch) + dice(2, 10);
      weather_change(i);
      break;
    }
  }
  if (to_room!=NULL)
    act(to_room, FALSE, ch, obj, vict, TO_ROOM);


  if (to_char!=NULL)
    act(to_char, FALSE, ch, obj, vict, TO_CHAR);
  return time;

}
ACTION(thing_control_weather_better)
{
  long time = 0;
  //const char *to_vict = NULL;
  const char *to_char = NULL;
  const char *to_room = NULL;
  int i;

  if (!*num)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    return 0;
  }
  i = GET_ROOM_ZONE(IN_ROOM(ch));
  if (!(number(0, 105) < GET_SKILL(ch, SPELL_CONTROL_WEATHER)))
  {
    to_char = "Your concentration falters. The magic stops.";
    to_room = "$n's concentration falters. The magic stops.";
    *num = 0;
  }
  else
  {
    switch (*num)
    {
    case 8:
      if (obj)
      {
        to_char = "You hold $p out infront of you.";
        to_room = "$n hold $s hands out infront of $mself.";
      }
      else
      {
        to_char = "You hold your hands out infront of you.";
        to_room = "$n holds $s hands out infront of $mself.";
      }
      zone_table[i].pressure += GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (3 RL_SEC);
      break;
    case 7:
      zone_table[i].pressure += GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (2 RL_SEC);
      break;
    case 6:
      to_char = "You concentrate and the air around your body starts to expand.";
      to_room = "$n concentrates and the air around $s body starts to expand.";
      zone_table[i].pressure += GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (2 RL_SEC);
      break;
    case 5:
      zone_table[i].pressure += GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (2 RL_SEC);
      break;
    case 4:
      to_char = "A warm breeze circles around your body.";
      to_room = "A warm breeze circles around $n's body.";
      zone_table[i].pressure += GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (3 RL_SEC);
      break;
    case 3:
      to_char = "The air becomes drier, clothing becomes crisper.";
      to_room = "The air becomes drier, clothing becomes crisper.";
      zone_table[i].pressure += GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (3 RL_SEC);
      break;
    case 2:
      zone_table[i].pressure += GET_INT(ch) + dice(2, 10);
      weather_change(i);
      time = (4 RL_SEC);
      break;
    case 1:
      to_char = "You lower your arms.";
      to_room = "$n lowers $s arms.";
      *num = 0;
      zone_table[i].pressure += GET_INT(ch) + dice(2, 10);
      weather_change(i);
      break;
    }
  }
  if (to_room!=NULL)
    act(to_room, FALSE, ch, obj, vict, TO_ROOM);


  if (to_char!=NULL)
    act(to_char, FALSE, ch, obj, vict, TO_CHAR);
  return time;

}

ACTION(thing_manifest)
{
  long time = 0;
  //const char *to_vict = NULL;
  const char *to_char = NULL;
  const char *to_room = NULL;

  if (!*num)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    return 0;
  }

  if (obj==NULL || (obj->carried_by != ch))
  {
    /* object is gone.*/
    to_char = "Your concentration falters. The magic stops.";
    to_room = "$n's concentration falters. The magic stops.";
    *num = 0;
  }
  else
  {
    switch (*num)
    {
    case 8:
      to_char = "You lie $p loosely atop your palm.";
      to_room ="$n lies $p loosely atop $s palm.";
      time = (4 RL_SEC);
      break;
    case 7:
      to_char = "You start waving your other hand in a circular motion above $p,\r\n"
                "all the while staring at it with intense concentration.";
      to_room = "$n starts waving $s other hand in a circular motion above $p,\r\n"
                "all the while staring at it with intense concentration.";
      time = (5 RL_SEC);
      break;
    case 6:
      to_char = "$p ignites in a blaze of magic light.";
      to_room = "$p ignites in a blaze of magic light.";
      time = (3 RL_SEC);
      break;
    case 5:
      to_char = "As you watch, $p begins to shrink.";
      to_room = "As you watch, $p begins to shrink.";
      time = (4 RL_SEC);
      break;
    case 4:
      to_char = "A cloud of mist billows from your palm as the weapon further glows.";
      to_room = "A cloud of mist billows from $n's palm as the weapon further glows.";
      time = (4 RL_SEC);
      break;
    case 3:
      to_char = "Around you, the mist entwines, circling $p.";
      to_room = "Around $n, the mist entwines, circling $p.";
      time = (4 RL_SEC);
      break;
    case 2:
      to_char = "The mist curls into a ball enshrouding $p and solidifies.";
      to_room = "The mist curls into a ball enshrouding $p and solidifies.";
      time = (3 RL_SEC);
      break;
    case 1:
      to_char = "With a bursting sound, the mystical light subsides.\r\n"
                "In your palm lies a shimmering orb.";
      to_room = "With a bursting sound, the mystical light subsides.\r\n"
                "In $n's palm lies a shimmering orb.";
      *num = 0;
      make_manifest(ch, obj);
      break;
    }
  }
  if (to_room!=NULL)
    act(to_room, FALSE, ch, obj, vict, TO_ROOM);


  if (to_char!=NULL)
    act(to_char, FALSE, ch, obj, vict, TO_CHAR);
  return time;

}

ACTION(thing_singwood)
{
  long time = 0;
  //const char *to_vict = NULL;
  const char *to_char = NULL;
  const char *to_room = NULL;

  if (!*num)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    return 0;
  }

  if (obj==NULL || IN_ROOM(obj)==NULL || (IN_ROOM(obj) != IN_ROOM(ch)))
  {
    /* object is gone.*/
    to_char = "You suddenly stumble the words and falter. The magic stops.";
    to_char = "$n suddenly stumble the words and falters. The magic stops.";
    *num = 0;

  }
  else if ((number(1, 1010) < 13))
  {
    to_char = "The tree shivers and dies!\r\n";
    to_room = "The tree shivers and dies!\r\n";
    extract_obj(obj);
    obj = NULL;
    damage(ch, ch, GET_LEVEL(ch), TYPE_UNDEFINED);
    *num = 0;
  }
  else
  {

    switch (*num)
    {
    case 11:
      to_char = "You clear your throat softly and begin to serenade in the direction of $p.";
      to_room = "$n clears $s throat softly and begins to serenade in the direction of $p.";
      time = (4 RL_SEC);
      break;
    case 10:
      to_char = "With your trained, melodious voice, you begin your song in a quiet pitch.";
      to_room = "With $s trained, melodious voice, $e begins $s song in a quiet pitch.";
      time = (5 RL_SEC);
      break;
    case 9:
      to_char = "You sense a curiosity from $p.";
      to_room = "You sense a curiosity from $p.";
      time = (4 RL_SEC);
      break;
    case 8:
      to_char = "Your voice strengthens into a dramatic chorus.";
      to_room = "$n's voice strengthens into a dramatic chorus.";
      time = (2 RL_SEC);
      break;
    case 7:
      to_char = "The trees leaves stir - it seems pleased with your song.";
      to_room = "The trees leaves stir - it seems pleased with $n's song.";
      time = (5 RL_SEC);
      break;
    case 6:
      to_char = "You continue in beautiful harmony, hitting all the notes with great accuracy.";
      to_room = "$n continues in beautiful harmony, hitting all the notes with great accuracy.";
      time = (4 RL_SEC);
      break;
    case 5:
      to_char = "At last you bring your voice to a mellow completion with a gentle breath.";
      to_room = "At last $n brings $s voice to a mellow completion with a gentle breath.";
      time = (4 RL_SEC);
      break;
    case 4:
      to_char = "You sense $p's awe of your enchanting song,\r\nwhich no doubt penetrated its tough bark surface.";
      to_room = "You sense $p's awe of $n's enchanting song,\r\nwhich no doubt penetrated its tough bark surface.";
      time = (4 RL_SEC);
      break;
    case 3:
      to_char = "$p's leaves begin to sway, then emit a faint glow.";
      to_room = "$p's leaves begin to sway, then emit a faint glow.";
      time = (2 RL_SEC);
      break;
    case 2:
      to_char = "Soon the entire trunk is also aglow, and continues to grow brighter.";
      to_room = "Soon the entire trunk is also aglow, and continues to grow brighter.";
      time = (7 RL_SEC);
      break;
    case 1:
      to_char = "As the light lessens, you notice the tree is gone.\r\n"
                "In its place, a mystical staff stands as an offering to the one who serenaded it.\r\n"
                "You pick it up.";
      to_room = "As the light lessens, you notice the tree is gone.\r\n"
                "In its place, a mystical staff stands as an offering to the one who serenaded it.\r\n"
                "$n picks it up.";
      make_focus(ch, SKILL_SING_WOOD, obj);
      improve_skill(ch, SKILL_SING_WOOD);
      time = 0;
      *num = 0;
    }
  }
  if (to_room!=NULL)
    act(to_room, FALSE, ch, obj, vict, TO_ROOM);


  if (to_char!=NULL)
    act(to_char, FALSE, ch, obj, vict, TO_CHAR);
  return time;
}
ASUB(sub_juggle)
{
  struct message_event_obj *msg = NULL;

  if (vict == ch)
    vict = NULL;
  if (GET_SUB(ch, SUB_JUGGLE) <= 0)
  {
    ch->Send( "You have no idea how to use that command!\r\n");
    return SUB_UNDEFINED;
  }

  if (get_sub_status(ch, SUB_JUGGLE) == STATUS_ON)
  {
    ch->Send( "You stop juggling.\r\n");
    act("$n stops juggling.", FALSE, ch, 0, 0, TO_ROOM);
    toggle_sub_status(ch, SUB_JUGGLE, STATUS_OFF);
    if (GET_TASK(ch) && GET_TASK(ch)->sub == SUB_JUGGLE)
      stop_task(ch);
    return SUB_UNDEFINED;
  }

  if (GET_MSG_RUN(ch) || GET_MESSAGE_EVENT(ch)!=NULL)
  {
    ch->Send( "You are in the middle of something else!\r\n");
    return SUB_UNDEFINED;
  }



  if (vict != NULL && get_sub_status(vict, SUB_JUGGLE) == STATUS_OFF)
  {
    ch->Send( "They arent even juggling!\r\n");
    return SUB_UNDEFINED;
  }



  if (vict)
  {
    act("You start to juggle a few colored balls to $N.", FALSE, ch, 0, vict, TO_CHAR);
    act("$n starts to juggle a few colored balls to you.", FALSE, ch, 0, vict, TO_VICT);
  }
  else
    act("You start to juggle a few colored balls.", FALSE, ch, 0, 0, TO_CHAR);
  GET_MSG_RUN(ch) = 1;
  toggle_sub_status(ch, SUB_JUGGLE, STATUS_ON);

  msg = new message_event_obj(ch, SUB_JUGGLE, THING_SUB,(vict == NULL ? number(1, 4) : number(5, 8)), (vict != NULL ? GET_ID(vict) : NOBODY), "");


  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, 1 RL_SEC, EVENT_TYPE_MESSAGE);

  return SUB_JUGGLE;
}

#define MAX_DIF 4

ACTION(thing_juggle)
{
  void reward_juggling(Character *ch, Character *vict,  int diff);
  //long time = 0;
  int diff = 1;
  //const char *to_vict = NULL;
  const char *to_char = NULL;
  const char *to_room = NULL;
  const char *to_vict = NULL;

  if (!*num || !ch)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    return 0;
  }

  /*maybe add a message to this some time?? -- mord*/
  if (get_sub_status(ch, SUB_JUGGLE) == STATUS_OFF || (GET_POS(ch) < POS_STANDING))
  {
    *num = 0;
    return 0;
  }
  if (vict == ch)
    vict = NULL;
  /* for this one, will be something slightly different from the others, as it will be continuous
     If num is 0 it will stop, and if time is 0 it will stop.
     So until then it wont stop you juggling.
     As you juggle, if you have an audence you will gain gold from them.
     Also depending on the difficulty of your juggling moves
     you will gain experience.
     If you are juggling with a partner, you will gain your normal amount of exp and gold, plus a lil more.
  */

  if (skill_cost(0, 0, 25, ch) &&  number(0, 110) - GET_DEX(ch) > GET_SUB(ch, SUB_JUGGLE))
  {
    switch (number(0, 3))
    {
    case 0:
      act("Some of the gathered crowd lose interest in the same routine and start booing\r\n"
          "at you till you give up and put your balls away.", FALSE, ch, 0 , 0, TO_CHAR);
      act("Some of the gathered crowd lose interest in the same routine and start booing\r\n"
          "at $n till $e gives up and put $s balls away.", FALSE, ch, 0 , 0, TO_ROOM);
    case 1:
      act("Someone coughs and causes you to lose concentration. \r\nBalls are scattered everywhere.",
          FALSE, ch, 0, 0, TO_CHAR);
      act("Someone coughs and causes $n to lose concentration. \r\nBalls are scattered everywhere.",
          FALSE, ch, 0, 0, TO_ROOM);

    case 2:
      act("You watch in horror as your balls mysteriously disappear into thin air.\r\n"
          "Maybe the Imms weren't as impressed as some others.", FALSE, ch, 0, 0, TO_CHAR);
      act("$n watches in horror as $s balls mysteriously disappear into thin air.\r\n"
          "Maybe the Imms weren't as impressed as some others.", FALSE, ch, 0, 0, TO_ROOM);
    case 3:
      ch->Send( "You loose concentration and drop the balls everywhere!!\r\n");
      act("$n looses concentration and drops the balls everywhere!!\r\n", FALSE, ch, 0, 0, TO_ROOM);
      break;
    }
    *num = 0;
    return 0;
  }

  if (vict != NULL && ((!HERE(vict, ch)) || get_sub_status(vict, SUB_JUGGLE) == STATUS_OFF))
  {
    vict = NULL;
  }
  /*if they did have a partner, but the partner quit, lets make them a solo act*/
  if (vict == NULL)
    *num = (number(2, 8));
  else
    *num = (number(9, 12));

  switch (*num)
  {
    /* 5 to 8 is juggling with a partner.*/
  case 12:
    diff = 2;
    to_char = "You spin two brightly colored balls in each hand.\r\n"
              "Every third moment you fire a ball high into the air,\r\n"
              "Finally you ping each spinning ball in a high arch over to $N.";
    to_room = "$n spins two brightly colored balls in each hand.\r\n"
              "Every third moment $e fires a ball high into the air,\r\n"
              "Finally pinging each spinning ball in a high arch over to $N.";
    to_vict = "$n spin two brightly colored balls in each hand.\r\n"
              "Every third moment you fire a ball high into the air,\r\n"
              "Finally $e pings each spinning ball in a high arch over to you.";

    break;
  case 11:
    to_char = "You pick up a few more balls and start a huge circle of\r\n"
              "brightly coloredm balls whizzing over your head!\r\n"
              "Finally as each ball falls you throw it back over to $N.";
    to_room = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over your head!\r\n"
              "Finally as each ball falls $n throws it back over to $N.";
    to_vict = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over $s head!\r\n"
              "Finally as each ball falls $n throws it back over to you.";

    break;
  case 10:
    to_char = "You pick up a few more balls and start a huge circle of\r\n"
              "brightly colored balls whizzing over your head!\r\n"
              "Finally as each ball falls you throw it back over to $N.";
    to_room = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over your head!\r\n"
              "Finally as each ball falls $n throws it back over to $N.";
    to_vict = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over $s head!\r\n"
              "Finally as each ball falls $n throws it back over to you.";

    break;
  case 9:
    to_char = "You pick up a few more balls and start a huge circle of\r\n"
              "brightly colored balls whizzing over your head!\r\n"
              "Finally as each ball falls you throw it back over to $N.";
    to_room = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over your head!\r\n"
              "Finally as each ball falls $n throws it back over to $N.";
    to_vict = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over $s head!\r\n"
              "Finally as each ball falls $n throws it back over to you.";

    break;
  case 8:
    to_char = "You throw some more balls into the air and catch them on your\r\n"
              "neck as you bend forward.";
    to_room = "$n throws some more balls into the air and catches them on $s\r\n"
              "neck as $e bends forward.";
    break;
  case 7:
    to_char = "You toss a couple of balls into the air, spin around on\r\n"
              "the spot a few times before catching them behind your back.";
    to_room = "$n tosses a couple of balls into the air, spins round on\r\n"
              "the spot a few times before catching them behind $s back.";
    break;
  case 6:
    to_char = "You quickly throw four balls into the air one after the\r\n"
              "other and catch them in your open pocket as they drop.";
    to_room = "$n quickly throws four balls into the air one after the\r\n"
              "other and catches them in $s open pocket as they drop.";
    break;
  case 5:
    to_char = "You juggle two balls in each hand, then add another.\r\n"
              "The five balls suddenly start to color the the room\r\n"
              "with vibrant magical flashes.";
    to_room = "$n juggles two balls in each hand, then adds another.\r\n"
              "The five balls suddenly start to color the the room\r\n"
              "with vibrant magical flashes.";

    break;
  case 4:
    diff = 2;
    to_char = "You juggle the balls into a sharp pyrimid shape.\r\n"
              "As each ball lands in your left hand you throw it at the ground\r\n"
              "causing it to bounce high overhead and fall into your right hand.";
    to_room = "$n juggles the balls into a sharp pyrimid shape.\r\n"
              "As each ball lands in $s left hand $e throws it at the ground\r\n"
              "causing it to bounce high overhead and fall into $s right hand.";

    break;
  case 3:
    diff = 2;
    to_char = "You toss a ball at someone in the audience and it rebounds off\r\n"
              "their head and back into the air.";
    to_room = "$n tosses a ball at someone in the audience and it rebounds off\r\n"
              "their head and back into the air.";

    break;
  case 2:
    to_char = "Dropping a multi coloured ball over your shoulder you heel kick\r\n"
              "it back up again and catch it in your left hand.";
    to_room = "Dropping a multi coloured ball over $s shoulder $n heel kicks\r\n"
              "it back up again and catch it in $s left hand.";





  }

  if (to_room!=NULL && vict)
    act(to_room, FALSE, ch, obj, vict, TO_NOTVICT);
  if (to_room!=NULL && !vict)
    act(to_room, FALSE, ch, obj, vict, TO_ROOM);

  if (to_vict != NULL)
    act(to_vict, FALSE, ch, obj, vict, TO_VICT);

  if (to_char!=NULL)
    act(to_char, FALSE, ch, obj, vict, TO_CHAR);
  reward_juggling(ch, vict,diff);
  return (10 RL_SEC);

}

/* how this works is.
   ch gains gold from mobs
   ch gains exp from juggling
   if vict exists, then they also gain a bit of exp.
   difficulty is a multiplyer for the exp and gold.
*/

void reward_juggling(Character *ch, Character *vict, int diff)
{
  gold_int amount = 0;
  gold_int gain = 0;
  gold_int exp = 0;
  Character *person = NULL;
  if (IN_ROOM(ch) == NULL)
    return;

  for (person = IN_ROOM(ch)->people; person ; person = person->next_in_room)
  {
    if (!number(0, 15))
      continue;
    if (person == vict)
      continue;
    if (person == ch)
      continue;
    if (get_sub_status(person, SUB_JUGGLE) == STATUS_ON)
      continue;
    if (!(GET_POS(person) == POS_STANDING || GET_POS(person) == POS_SITTING || GET_POS(person) == POS_RESTING))
      continue;
    if (AFF_FLAGGED(person, AFF_CHARM))
      continue;
    if (person->Gold(0, GOLD_HAND) < 10)
      continue;

    if (IS_NPC(person) && (number(0, 70) > GET_LEVEL(person)))
    {
      act("{cY$N applauds your juggling and throws you some gold!{c0", FALSE, ch, 0, person, TO_CHAR);
      act("{cY$N applauds $n's juggling and throws $m some gold!{c0", FALSE, ch, 0, person, TO_ROOM);
      amount += person->Gold(0, GOLD_HAND)/10;
      person->Gold(person->Gold(0, GOLD_HAND)/10, GOLD_HAND);
    }
    else
    {
      if (number(1, 70) > GET_LEVEL(person))
      {
        exp = number(10000, 25000);
        gain += IRANGE((exp/3),exp * (1 + ( (GET_LEVEL(person) - GET_LEVEL(ch))/35)), (exp * 2));
        if (vict == NULL)
        {
          act("{cY$N enjoys your juggling so much you gain some exp!{c0", FALSE, ch, 0, person, TO_CHAR);
        }
        else
        {
          act("{cY$N enjoys your juggling so much you gain some exp!{c0", FALSE, ch, 0, person, TO_CHAR);
          act("{cY$N enjoys your juggling so much you gain some exp!{c0", FALSE, vict, 0, person, TO_CHAR);
        }
      }
    }


  }

  if (amount)
  {
    ch->Send( "You gain a total of %lld gold coins!\r\n", amount);
    ch->Gold( amount, GOLD_HAND);
  }
  if (gain)
  {
    gain_exp(ch, gain);
    if (vict)
      gain_exp(vict, gain/3);
  }

}

ASKILL(skill_strangle)
{

  if (vict == ch)
    vict = NULL;

  if (GET_SKILL(ch, SKILL_STRANGLE) <= 0)
  {
    ch->Send( "You have no idea how to use that command!\r\n");
    return TYPE_UNDEFINED;
  }

  if (get_sub_status(ch, SUB_THROTTLE) == STATUS_ON)
  {
    ch->Send( "You release your grip on your victims neck.\r\n");
    toggle_sub_status(ch, SUB_THROTTLE, STATUS_OFF);
    toggle_sub_status(ch, SUB_GAROTTE, STATUS_OFF);
    return TYPE_UNDEFINED;
  }



  if (!vict)
  {
    ch->Send( "Strangle what poor unsuspecting victim?\r\n");
    return TYPE_UNDEFINED;
  }

  if (AFF_FLAGGED(vict, AFF_STUCK))
  {
    ch->Send( "They are too cautious!\r\n");
    return TYPE_UNDEFINED;
  }

  if (!can_fight(ch, vict, TRUE))
  {
    ch->Send( "You can't strangle %s!\r\n", GET_NAME(vict));
    return TYPE_UNDEFINED;
  }
  if (has_weapon(ch) || GET_EQ(ch, WEAR_FOCUS) || GET_EQ(ch, WEAR_SHIELD))
  {
    ch->Send( "You can't strangle %s with your hands full!\r\n", GET_NAME(vict));
    return TYPE_UNDEFINED;
  }
  if (!GET_SUB(ch, SUB_THROTTLE))
  {
    improve_sub(ch, SUB_THROTTLE, 10);
    ch->Send( "You gain the subskill THROTTLE as a combo with this skill.\r\n");
  }
  if (GET_EQ(ch, WEAR_WIELD) && GET_OBJ_TYPE(GET_EQ(ch, WEAR_WIELD)) == ITEM_GAROTTE && GET_SUB(ch, SUB_THROTTLE))
  {
    toggle_sub_status(ch, SUB_GAROTTE, STATUS_ON);
  }

  sub_throttle(ch, vict, NULL, NULL);

  return SKILL_STRANGLE;
}

ASUB(sub_throttle)
{
  struct message_event_obj *msg = NULL;
  struct affected_type af;

  if (GET_MSG_RUN(ch) || GET_MESSAGE_EVENT(ch)!=NULL)
  {
    ch->Send( "You are in the middle of something else!\r\n");
    toggle_sub_status(ch, SUB_GAROTTE, STATUS_OFF);
    toggle_sub_status(ch, SUB_THROTTLE, STATUS_OFF);
    return SUB_UNDEFINED;
  }

  act("You grab $N by the throat and press down hard on $S larynx.", FALSE, ch, 0, vict, TO_CHAR);
  act("Someone grabs you by your throat and presses down hard on your larynx!", FALSE, ch, 0, vict, TO_VICT);

  if (!AFF_FLAGGED(vict, AFF_STUCK))
  {
    af.type = SKILL_STRANGLE;
    af.expire = HOURS_TO_EXPIRE(2);
    af.modifier = -10;
    af.location = APPLY_STR;
    af.bitvector = AFF_STUCK;
    affect_to_char(vict, &af);
  }


  GET_MSG_RUN(ch) = 1;
  toggle_sub_status(ch, SUB_THROTTLE, STATUS_ON);

  msg = new message_event_obj(ch, SUB_THROTTLE, THING_SUB, 18, GET_ID(vict), "");
  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, 1 RL_SEC, EVENT_TYPE_MESSAGE);

  return SUB_THROTTLE;
}

ACTION(thing_throttle)
{
  const char *to_char = NULL;
  const char *to_room = NULL;
  const char *to_vict = NULL;
  char buf[MAX_INPUT_LENGTH];

  if (!*num || !ch || !vict)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    *num = 0;
    return 0;
  }


  if (!AFF_FLAGGED(vict, AFF_STUCK) ||
      !skill_cost(0, 0, 20, ch) ||
      GET_POS(vict) < POS_STANDING ||
      GET_POS(ch) < POS_STANDING ||
      has_weapon(ch) || GET_EQ(ch, WEAR_FOCUS) || GET_EQ(ch, WEAR_SHIELD)  ||
      ((number(0, 110) - GET_DEX(ch)) > GET_SKILL(ch, SKILL_STRANGLE)))
  {
    ch->Send( "You loose focus and stop strangling.\r\n");
    act("$n relaxes his grip.\r\n", FALSE, ch, 0, 0, TO_ROOM);
    start_fighting(vict, ch);
    *num = 0;
    return 0;
  }
  else if (((!HERE(vict,ch)) || get_sub_status(ch, SUB_THROTTLE) == STATUS_OFF))
  {
    if (ch)
      ch->Send( "You stop strangling.\r\n");
    *num = 0;
    return 0;
  }


  switch (*num%3)
  {
  case 0:
    to_char = "You squeeze on $N's throat brutally draining $S life.";
    to_vict = "Someone squeezes your throat so hard your eyes bulge!";
    break;
  case 1:
    to_char = "You press hard on $N's larynx suffocating the life from $M!";
    to_vict = "Someone presses hard on your larynx suffocating the life from you!";
    break;
  case 2:
    to_char = "You choke $N with your vice-like grip on $S neck!";
    to_vict = "Someone chokes you with $s vice-like grip on your neck!";
    break;
  }

  strcpy(buf, GET_NAME(ch));

  if (AFF_FLAGGED(vict, AFF_STUCK))
  {

    if (to_room!=NULL && vict)
      act(to_room, FALSE, ch, obj, vict, TO_NOTVICT);
    if (to_room!=NULL && !vict)
      act(to_room, FALSE, ch, obj, vict, TO_ROOM);

    if (to_vict != NULL)
      act(to_vict, FALSE, ch, obj, vict, TO_VICT);

    if (to_char!=NULL)
      act(to_char, FALSE, ch, obj, vict, TO_CHAR);

    if (strangle_affect(ch, vict, *num) <= 0)
      *num = 0;
  }
  return (2 RL_SEC);

}

int strangle_affect(Character *ch, Character *vict, int num)
{
  int diff, diff1, diff2;
  void die(Character *ch, Character *killer);

  diff = (GET_LEVEL(ch)*(tier_level(ch, CLASS_THIEF)+1) + (GET_SUB(ch, SUB_THROTTLE))) ;
  diff1 = ((GET_LEVEL(vict)*(current_class_is_tier_num(vict)+1)))*(num/10);

  diff2 = ((diff*100)/(diff+diff1));

  if (number(0, 100) > diff2)
  {
    affect_from_char(vict, SKILL_STRANGLE);
    toggle_sub_status(ch, SUB_GAROTTE, STATUS_OFF);
    toggle_sub_status(ch, SUB_THROTTLE, STATUS_OFF);
    act("You struggle free from $n's grasp!!", FALSE, ch, 0, vict, TO_VICT);
    act("$N struggles free from your grasp!!", FALSE, ch, 0, vict, TO_CHAR);
    start_fighting_delay(vict, ch);
    return 0;
  }


  alter_hit(vict, (GET_MAX_HIT(vict)/(get_sub_status(ch, SUB_GAROTTE) == STATUS_ON ? num : (num/2) )));

  update_pos(vict);

  if (GET_POS(vict) == POS_DEAD)
  {
    die(vict, ch);
    return -1;
  }

  return 1;

}

ACMD(do_struggle)
{
  int diff2 = (GET_LEVEL(ch));

  if (number(0, 100) > diff2 || !AFF_FLAGGED(ch, AFF_STUCK))
  {
    affect_from_char(ch, SKILL_STRANGLE);
    ch->Send("You struggle free!!\r\n");
    act("$n struggles free of these mortal coils!!", FALSE, ch, 0, 0, TO_ROOM);
    do_flee(ch, "", 0, 0);

    return;
  }
  ch->Send( "You struggle in a vain attempt to escape.\r\n");
}


ASUB(sub_tumble)
{
  struct message_event_obj *msg = NULL;

  if (vict == ch)
    vict = NULL;
  if (GET_SUB(ch, SUB_JUGGLE) <= 0)
  {
    ch->Send( "You have no idea how to use that command!\r\n");
    return SUB_UNDEFINED;
  }

  if (get_sub_status(ch, SUB_JUGGLE) == STATUS_ON)
  {
    ch->Send( "You stop juggling.\r\n");
    act("$n stops juggling.", FALSE, ch, 0, 0, TO_ROOM);
    toggle_sub_status(ch, SUB_JUGGLE, STATUS_OFF);

    if (GET_TASK(ch) && GET_TASK(ch)->sub == SUB_JUGGLE)
      stop_task(ch);
    return SUB_UNDEFINED;
  }

  if (GET_MSG_RUN(ch) || GET_MESSAGE_EVENT(ch)!=NULL)
  {
    ch->Send( "You are in the middle of something else!\r\n");
    return SUB_UNDEFINED;
  }



  if (vict != NULL && get_sub_status(vict, SUB_JUGGLE) == STATUS_OFF)
  {
    ch->Send( "They arent even juggling!\r\n");
    return SUB_UNDEFINED;
  }



  if (vict)
  {
    act("You start to juggle a few colored balls to $N.", FALSE, ch, 0, vict, TO_CHAR);
    act("$n starts to juggle a few colored balls to you.", FALSE, ch, 0, vict, TO_VICT);
  }
  else
    act("You start to juggle a few colored balls.", FALSE, ch, 0, 0, TO_CHAR);
  GET_MSG_RUN(ch) = 1;
  toggle_sub_status(ch, SUB_JUGGLE, STATUS_ON);

  msg = new message_event_obj(ch, SUB_JUGGLE, THING_SUB,(vict == NULL ? number(1, 4) : number(5, 8)),(vict != NULL ? GET_ID(vict) : NOBODY), "");
  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, 1 RL_SEC, EVENT_TYPE_MESSAGE);

  return SUB_JUGGLE;
}

ASUB(sub_clown)
{
  struct message_event_obj *msg = NULL;

  if (vict == ch)
    vict = NULL;
  if (GET_SUB(ch, SUB_JUGGLE) <= 0)
  {
    ch->Send( "You have no idea how to use that command!\r\n");
    return SUB_UNDEFINED;
  }


  if (get_sub_status(ch, SUB_JUGGLE) == STATUS_ON)
  {
    ch->Send( "You stop juggling.\r\n");
    act("$n stops juggling.", FALSE, ch, 0, 0, TO_ROOM);
    toggle_sub_status(ch, SUB_JUGGLE, STATUS_OFF);
    if (GET_TASK(ch) && GET_TASK(ch)->sub == SUB_JUGGLE)
      stop_task(ch);
    return SUB_UNDEFINED;
  }

  if (GET_MSG_RUN(ch) || GET_MESSAGE_EVENT(ch)!=NULL)
  {
    ch->Send( "You are in the middle of something else!\r\n");
    return SUB_UNDEFINED;
  }



  if (vict != NULL && get_sub_status(vict, SUB_JUGGLE) == STATUS_OFF)
  {
    ch->Send( "They arent even juggling!\r\n");
    return SUB_UNDEFINED;
  }



  if (vict)
  {
    act("You start to juggle a few colored balls to $N.", FALSE, ch, 0, vict, TO_CHAR);
    act("$n starts to juggle a few colored balls to you.", FALSE, ch, 0, vict, TO_VICT);
  }
  else
    act("You start to juggle a few colored balls.", FALSE, ch, 0, 0, TO_CHAR);
  GET_MSG_RUN(ch) = 1;
  toggle_sub_status(ch, SUB_JUGGLE, STATUS_ON);

msg = new message_event_obj(ch, SUB_JUGGLE, THING_SUB,(vict == NULL ? number(1, 4) : number(5, 8)),(vict != NULL ? GET_ID(vict) : NOBODY), "");
  GET_MESSAGE_EVENT(ch) = event_create(message_event, msg, 1 RL_SEC, EVENT_TYPE_MESSAGE);

  return SUB_JUGGLE;
}

ACTION(thing_tumble)
{
  void reward_juggling(Character *ch, Character *vict,  int diff);
  //long time = 0;
  int diff = 1;
  //const char *to_vict = NULL;
  const char *to_char = NULL;
  const char *to_room = NULL;
  const char *to_vict = NULL;

  if (!*num || !ch)
  {
    if (ch)
      ch->Send( "Broken for some reason!??\r\n");
    *num = 0;
    return 0;
  }


  if (!IS_STUCK(vict) || !skill_cost(0, 0, 50, ch) ||  (number(0, 110) - GET_DEX(ch) > GET_SKILL(ch, SKILL_STRANGLE)))
  {
    ch->Send( "You loose focus and stop strangling!!\r\n");
    act("$n looses concentration and drops the balls everywhere!!\r\n", FALSE, ch, 0, 0, TO_ROOM);
    *num = 0;
    return 0;
  }

  if (vict != NULL && ((!HERE(vict, ch)) || get_sub_status(vict, SUB_JUGGLE) == STATUS_OFF))
  {
    vict = NULL;
  }
  /*if they did have a partner, but the partner quit, lets make them a solo act*/
  if (vict == NULL)
    *num = (number(2, 8));
  else
    *num = (number(9, 12));

  switch (*num)
  {
    /* 5 to 8 is juggling with a partner.*/
  case 12:
    diff = 2;
    to_char = "You spin two brightly colored balls in each hand.\r\n"
              "Every third moment you fire a ball high into the air,\r\n"
              "Finally you ping each spinning ball in a high arch over to $N.";
    to_room = "$n spins two brightly colored balls in each hand.\r\n"
              "Every third moment $e fires a ball high into the air,\r\n"
              "Finally pinging each spinning ball in a high arch over to $N.";
    to_vict = "$n spin two brightly colored balls in each hand.\r\n"
              "Every third moment you fire a ball high into the air,\r\n"
              "Finally $e pings each spinning ball in a high arch over to you.";

    break;
  case 11:
    to_char = "You pick up a few more balls and start a huge circle of\r\n"
              "brightly colored balls whizzing over your head!\r\n"
              "Finally as each ball falls you throw it back over to $N.";
    to_room = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over your head!\r\n"
              "Finally as each ball falls $n throws it back over to $N.";
    to_vict = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over $s head!\r\n"
              "Finally as each ball falls $n throws it back over to you.";

    break;
  case 10:
    to_char = "You pick up a few more balls and start a huge circle of\r\n"
              "brightly colored balls whizzing over your head!\r\n"
              "Finally as each ball falls you throw it back over to $N.";
    to_room = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over your head!\r\n"
              "Finally as each ball falls $n throws it back over to $N.";
    to_vict = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over $s head!\r\n"
              "Finally as each ball falls $n throws it back over to you.";

    break;
  case 9:
    to_char = "You pick up a few more balls and start a huge circle of\r\n"
              "brightly colored balls whizzing over your head!\r\n"
              "Finally as each ball falls you throw it back over to $N.";
    to_room = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over your head!\r\n"
              "Finally as each ball falls $n throws it back over to $N.";
    to_vict = "$n picks up a few more balls and starts a huge circle of\r\n"
              "brightly colors whizzing over $s head!\r\n"
              "Finally as each ball falls $n throws it back over to you.";

    break;
  case 8:
    to_char = "You throw some more balls into the air and catch them on your\r\n"
              "neck as you bend forward.";
    to_room = "$n throws some more balls into the air and catches them on $s\r\n"
              "neck as $e bends forward.";
    break;
  case 7:
    to_char = "You toss a couple of balls into the air, spin around on\r\n"
              "the spot a few times before catching them behind your back.";
    to_room = "$n tosses a couple of balls into the air, spins round on\r\n"
              "the spot a few times before catching them behind $s back.";
    break;
  case 6:
    to_char = "You quickly throw four balls into the air one after the\r\n"
              "other and catch them in your open pocket as they drop.";
    to_room = "$n quickly throws four balls into the air one after the\r\n"
              "other and catches them in $s open pocket as they drop.";
    break;
  case 5:
    to_char = "You juggle two balls in each hand, then add another.\r\n"
              "The five balls suddenly start to color the the room\r\n"
              "with vibrant magical flashes.";
    to_room = "$n juggles two balls in each hand, then adds another.\r\n"
              "The five balls suddenly start to color the the room\r\n"
              "with vibrant magical flashes.";

    break;
  case 4:
    diff = 2;
    to_char = "You juggle the balls into a sharp pyrimid shape.\r\n"
              "As each ball lands in your left hand you throw it at the ground\r\n"
              "causing it to bounce high overhead and fall into your right hand.";
    to_room = "$n juggles the balls into a sharp pyrimid shape.\r\n"
              "As each ball lands in $s left hand $e throws it at the ground\r\n"
              "causing it to bounce high overhead and fall into $s right hand.";

    break;
  case 3:
    diff = 2;
    to_char = "You toss a ball at someone in the audience and it rebounds off\r\n"
              "their head and back into the air.";
    to_room = "$n tosses a ball at someone in the audience and it rebounds off\r\n"
              "their head and back into the air.";

    break;
  case 2:
    to_char = "Dropping a multi coloured ball over your shoulder you heel kick\r\n"
              "it back up again and catch it in your left hand.";
    to_room = "Dropping a multi coloured ball over $s shoulder $n heel kicks\r\n"
              "it back up again and catch it in $s left hand.";





  }

  if (to_room!=NULL && vict)
    act(to_room, FALSE, ch, obj, vict, TO_NOTVICT);
  if (to_room!=NULL && !vict)
    act(to_room, FALSE, ch, obj, vict, TO_ROOM);

  if (to_vict != NULL)
    act(to_vict, FALSE, ch, obj, vict, TO_VICT);

  if (to_char!=NULL)
    act(to_char, FALSE, ch, obj, vict, TO_CHAR);
  reward_juggling(ch, vict,diff);
  return (10 RL_SEC);

}





