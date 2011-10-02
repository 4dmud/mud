/* Defines for ASCII Automapper */

#define MAPX     10
#define MAPY     8 
/* You can change MAXDEPTH to 1 if the diagonal directions are confusing */
#define MAXDEPTH  2
#define MDIS(num)  ((sect + (num)) <= NUM_ROOM_SECTORS)
#define BOUNDARY(x, y) (((x) < 0) || ((y) < 0) || ((x) > MAPX) || ((y) > MAPY))

typedef struct  map_type                MAP_TYPE;

/* Structure for the map itself */
struct map_type
{
  char   tegn[5];  /* Character to print at this map coord */
  int    vnum;  /* Room this coord represents */
  int    depth; /* Recursive depth this coord was found at */
  int   can_see;
};

/* mapper.c */
void  draw_map (Character *ch);
char *get_exits(Character *ch);
char *msdp_map (Character *ch);

// Making changes suggested by Molly's email.
// Read that email to see the changes.
// Prom
static struct map_bit_data {
    char bit[2];
    char colour[5];
    char name[15];
    } map_bit[] = {
{"H", "{cW", "Inside"      },//SECT_INSIDE         
{"#", "{cW", "City"        },//SECT_CITY           
{"\"", "{cG", "Field"      },//SECT_FIELD          
{"@", "{cg", "Forest"      },//SECT_FOREST         
{"^", "{cG", "Hills"       },//SECT_HILLS          
{"^", "{cy", "Mountain"    },//SECT_MOUNTAIN       
{"=", "{cc", "Water-Swim"  },//SECT_WATER_SWIM     
{"=", "{cb", "Water-Boat"  },//SECT_WATER_NOSWIM   
{"~", "{cb", "Underwater"  },//SECT_UNDERWATER	    
{"%", "{cC", "Flying"      },//SECT_FLYING         
{"~", "{cY", "Desert"      },//SECT_DESERT         
{".", "{cW", "Space"       },//SECT_SPACE	
{"-", "{cY", "Road"        },//SECT_ROAD	
{"@", "{cY", "Entrance"    },//SECT_ENTRANCE	
{"@", "{cC", "Atmosphere"  },//SECT_ATMOSPHERE 
{"*", "{cY", "Sun"         },//SECT_SUN	
{"0", "{cL", "Blackhole"   },//SECT_BLACKHOLE	
{"<", "{cM", "Vehicle"     },//SECT_VEHICLE	
{"\"", "{cg", "Swamp"      },//SECT_SWAMP
{";", "{cL", "Reef"        },//SECT_REEF  
{"\"", "{cW", "Tundra"     },//SECT_TUNDRA
{"o", "{cW", "Snow"        },//SECT_SNOW
{":", "{cC", "Ice"         },//SECT_ICE
{"\"", "{cy", "Praire"     },//SECT_PRAIRIE
{"'", "{cr", "Badlands"    },//SECT_BADLANDS
{"+", "{cy", "Rail"        },//SECT_RAIL
{"D", "{cR", "DeathTrap"    },//SECT_DEATHTRAP

{"X", "{cx", "Unknown"     } //NUM_ROOM_SECTORS
    };


