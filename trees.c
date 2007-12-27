/*
*  4Dimensions Tree system
*
* Description:
*
*
* 
*
* Copyright: See COPYING file that comes with this distribution
*
*/

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
#include "genolc.h"
#include "trees.h"

/** local globals **/
int tree_total = 0;
int forest_room;
struct forest_data *forest = NULL;

struct stave_stat_table stave_table[MAX_TREE_TYPES] =
  {
    {APPLY_SPEED, 240,   5,    4},
    {APPLY_SPEED, 110,   25,   0},
    {APPLY_CHA,   9,     35,   0},
    {APPLY_WIS,   3,     42,    5},
    {APPLY_HIT,   1000,  47,    2},
    {APPLY_MANA,  2000,  52,    0},
    {APPLY_INT,   3,     62,   2},
    {APPLY_MANA,  1000,  72,   0},
    {APPLY_HIT,   500,   28,   1}
  } ;
struct obj_data *make_tree(int v0, int v1, int v2)
{

  struct obj_data *final_tree;

  int num = -1, t_age = 0, ran = number(0, 100);

  if (tree_total > TREE_MAX)
    return (NULL);

  for (num=0;num<MAX_TREE_TYPES;num++)
  {
  if (ran >= (num == 0 ? 0 : stave_table[num-1].chance) && ran <= stave_table[num].chance)
    break;
  }

  final_tree = create_obj();

  final_tree->item_number = NOTHING;
  final_tree->in_room = NULL;

  GET_OBJ_TYPE(final_tree) = ITEM_TREE;
  SET_BIT_AR(GET_OBJ_EXTRA(final_tree), ITEM_GLOW);
  //SET_BIT_AR(GET_OBJ_EXTRA(final_tree), ITEM_UNIQUE_SAVE);
  if (v0)           //time of creation
    GET_OBJ_VAL(final_tree, 0) = v0;
  else
    GET_OBJ_VAL(final_tree, 0) = (int) time(0);
  if (v1)           //age desc number (sapling, old, aging)
    GET_OBJ_VAL(final_tree, 1) = IRANGE(0, v1, 8);
  else
    GET_OBJ_VAL(final_tree, 1) = t_age;
  if (v2)           //type desc number (oak, willow, pine)
    GET_OBJ_VAL(final_tree, 2) = IRANGE(0, v2, 8);
  else
    GET_OBJ_VAL(final_tree, 2) = num;
  
  GET_OBJ_VAL(final_tree, 3) = 0;
  GET_OBJ_COST(final_tree) = 500;
  GET_OBJ_WEIGHT(final_tree) = 1;
  GET_OBJ_RENT(final_tree) = 0;
  GET_OBJ_TIMER(final_tree) = -1;

  parse_tree_name(final_tree);
  tree_total++;

  return (final_tree);
}


void parse_tree_name(struct obj_data *tree)
{
  struct extra_descr_data *new_descr = NULL;
  char buf2[MAX_INPUT_LENGTH];

  free_string(&tree->name);
  free_string(&tree->description);
  free_string(&tree->short_description);
  free_string(&tree->smell);
  free_string(&tree->feel);

  tree->smell = strdup("It smells good!\r\n");
  tree->feel = strdup("It feels alive.\r\n");

  if (tree->ex_description)
    free_ex_descriptions(tree->ex_description);

  snprintf(buf2, sizeof(buf2), "%s %s tree magictree",
           age_desc_tree[GET_OBJ_VAL(tree, 1)],
           tree_names[IRANGE(0, GET_OBJ_VAL(tree, 2), 8)]);
  tree->name = str_dup(buf2);


  snprintf(buf2, sizeof(buf2), "%s %s %s tree grows here.",
           CANA(age_desc_tree[GET_OBJ_VAL(tree, 1)]),
           age_desc_tree[GET_OBJ_VAL(tree, 1)],
           tree_names[IRANGE(0, GET_OBJ_VAL(tree, 2), 8)]);
  tree->description = str_dup(buf2);

  snprintf(buf2, sizeof(buf2), "%s %s %s tree",
           LANA(age_desc_tree[GET_OBJ_VAL(tree, 1)]),
           age_desc_tree[GET_OBJ_VAL(tree, 1)],
           tree_names[IRANGE(0, GET_OBJ_VAL(tree, 2), 8)]);
  tree->short_description = str_dup(buf2);

  /* extra description coolness! */
  CREATE(new_descr, struct extra_descr_data, 1);
  new_descr->keyword = str_dup(buf2);
  snprintf(buf2, sizeof(buf2), "It appears to be %s %s %s tree.",
           LANA(age_desc_tree[GET_OBJ_VAL(tree, 1)]),
           age_desc_tree[GET_OBJ_VAL(tree, 1)],
           tree_names[IRANGE(0, GET_OBJ_VAL(tree, 2), 8)]);
  new_descr->description = str_dup(buf2);
  new_descr->next = NULL;
  tree->ex_description = new_descr;
}

room_rnum find_forest_rand(void)
{

  int i = 0, r = number(0, forest_room);
  struct forest_data *temp;

  if (forest == NULL)
  {
    log("ERROR: forest rooms not initialized");
    return NULL;
  }

  temp = forest;
  while (i < r)
  {
    i++;
    if (i == r)
      break;
    temp = temp->next;
  }
  return (temp->room);
}

ACMD(forest_find)
{
  OBJ_DATA *tree, *next_tree;
  skip_spaces(&argument);
  if (!*argument)
  {
    room_rnum ffr = find_forest_rand();
    if (ffr)
      new_send_to_char(ch, "A forest room is [%d]\r\n", ffr->number);
  }
  else if (!strcmp(argument, "clear"))
  {
    for (tree = object_list; tree; tree = next_tree)
    {
      next_tree = tree->next;
      if ((GET_OBJ_TYPE(tree) == ITEM_TREE) && (GET_OBJ_VNUM(tree) == NOTHING))
      {
        tree_total--;
        extract_obj(tree);
      }
    }
    save_forest();
    new_send_to_char(ch, "Cleared and saved.\r\n");
  }
  else
  {
    new_send_to_char(ch, "Either forest, or forest clear\r\n");
  }
}


int load_forest(void)
{
  char line[MAX_INPUT_LENGTH];
  FILE *fl;
  int v0 = 0, v1 = 0, v2 = 0, v3 = 0, v4 = 0;
  int num = 0;

  if (!(fl = fopen(FOREST_FILE, "r")))
  {
    log("SYSERR: Couldn't open forest file %s",FOREST_FILE);
    return -1;
  }

  while (get_line(fl, line))
  {
    sscanf(line, "%d %d %d %d %d", &v4, &v0, &v1, &v2, &v3);
    load_tree(real_room(v4), v0, v1, v2, v3);
    num++;
  }
  fclose(fl);
  if (num)
    return (num);
  else
    return (-1);

}



int save_forest(void)
{
  FILE *fl;
  int count = 0;
  struct obj_data *k = NULL;



  if ((fl = fopen(FOREST_FILE, "w")) == NULL)
  {
    log("SYSERR: Can't write to '%s' forest file.", FOREST_FILE);
    return -1;
  }
  for (k = object_list; k; k = k->next)
  {
    if (k->in_room && GET_OBJ_TYPE(k) == ITEM_TREE)
    {
      /** lets clean this up **/
      count++;
      fprintf(fl, "%d %d %d %d %d\n",
              k->in_room->number, GET_OBJ_VAL(k, 0),
              GET_OBJ_VAL(k, 1), GET_OBJ_VAL(k, 2), GET_OBJ_VAL(k,3));
    }

  }
  if (count != tree_total)
  {
    log("Tree's resync'ing");
    tree_total = count;
  }
  fclose(fl);
  return 1;
}

void init_trees(int num)
{
  int i;
  for (i = num; i < TREE_MAX; i++)
    load_tree(NULL, 0, 0, 0, 0);
  save_forest();
}

void create_trees(void)
{
  int i;
  for (i = tree_total; i < TREE_MAX; i++)
    load_tree(NULL, 0, 0, 0, 0);
  save_forest();
}

int load_tree(room_rnum room, int v0, int v1, int v2, int v3)
{

  struct obj_data *tree;
  room_rnum rm = room;

  //room is the VNUM

  //make_tree() creates a tree prototype
  if ((tree = make_tree(v0, v1, v2)) == NULL)
    return (0);

  if (rm == NULL)
    rm = find_forest_rand();

  if (rm == NULL)
  {
    tree_total--;
    extract_obj(tree);
    return 0;
  }

  if (SECT(rm) != SECT_FOREST)
  {
    tree_total--;
    extract_obj(tree);
    return (0);
  }
#if 0
  if (v0)           //time of creation
    GET_OBJ_VAL(tree, 0) = v0;

  if (v1)           //age desc number (sapling, old, aging)
    GET_OBJ_VAL(tree, 1) = IRANGE(0, v1, 8);

  if (v2)           //type desc number (oak, willow, pine)
    GET_OBJ_VAL(tree, 2) = IRANGE(0, v2, 8);

  GET_OBJ_VAL(tree, 3) = rm == NULL ? NOWHERE : rm->number;

  if (((time(0) - GET_OBJ_VAL(tree, 0))/ (7 * SECS_PER_REAL_DAY)) > GET_OBJ_VAL(tree, 1))
    if (GET_OBJ_VAL(tree, 1) < MAX_TREE_AGE)
      GET_OBJ_VAL(tree, 1)++;

  parse_tree_name(tree);
#endif

  GET_OBJ_VAL(tree, 3) = rm == NULL ? NOWHERE : rm->number;
  obj_to_room(tree, rm);
  return (1);
}
void check_all_trees(void)
{
  //TODO: this, .. i don't think works right
  struct obj_data *obj, *tmp;
  time_t tm = time(0);
  int ext = 0, add = 0;

  log("TREES: Updating all trees...");

  for (obj = object_list; obj; obj = tmp)
  {
    tmp = obj->next;
    /*tree check*/
    if (GET_OBJ_TYPE(obj) == ITEM_TREE && GET_OBJ_VNUM(obj) == NOTHING)
    {
      /** current time minus the time the tree was last updated
          in seconds, devided by 7 days of seconds.
          then check if it is more then the age of the tree -- mord **/
      if (((tm - GET_OBJ_VAL(obj, 0)) / (5 * SECS_PER_REAL_DAY)) > GET_OBJ_VAL(obj, 1))
      {
        if (GET_OBJ_VAL(obj, 1) <= MAX_TREE_AGE)
        {
          add++;
          GET_OBJ_VAL(obj, 1)++;
          parse_tree_name(obj);
        }
        else
        {
          ext++;
          if (IN_ROOM(obj) != NULL)
            send_to_room(IN_ROOM(obj), "With a sigh and a whisper %s collapses to the forest floor.\r\n", obj->short_description);
          tree_total--;
          extract_obj(obj);
        }
      }
      /*tree check*/
    }
  }

  save_forest();
  log("TREES: Update complete. Created: %d Destroyed: %d", add, ext);

}

