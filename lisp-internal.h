#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "db.h"
#include "clan.h"
#include "descriptor.h"
#include "interpreter.h"
#include "dg_scripts.h"
extern vector <Room *> world_vnum;
extern zone_rnum top_of_zone_table;
extern vector <Zone> zone_table;

extern vector <clan_rec> clan;
extern int num_of_clans;
extern Descriptor *descriptor_list;
extern Character *character_list;


extern map<mob_vnum, Character *> mob_proto;
extern struct obj_data *obj_proto;

extern map <obj_vnum,obj_rnum> obj_vTor;
extern obj_list_type object_list;

typedef  map<long, Character *> ch_map;
typedef  map<long, obj_data *> obj_map;
extern ch_map ch_lookup_table;
extern obj_map obj_lookup_table;
extern struct index_data *obj_index;

extern struct index_data **trig_index;
extern unsigned int top_of_trigt;

extern obj_rnum top_of_objt;
extern PlayerIndex pi;
