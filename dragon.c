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

#define LANA(string) (strchr("aeiouyAEIOUY", string[0]) ? "an" : "a")
#define CANA(string) (strchr("aeiouyAEIOUY", string[0]) ? "An" : "A")
#define TIER (current_class_is_tier_num(ch))



/************************************************************************
Dragon age and modifyers
---------------------
Age_name
Age_modifyer *as a number*
size  *no real use, possible descriptive
Look_descrip 
bite_damage (add age_mod plus strenth)
claw_damage (always 2 attacks) (add half age_mod *round up*)
wing_damage (add age_mod)
Tail_damage (add 2 times age_mod)
carry_comp  (people/obj_weight)

*All dragons long and short descriptions.
long- A <color> <age_name> dragon rests here.
short A <color> <age_name> dragon



Wyrmling
1
tiny
This is a small dragon, no bigger than 2 feet long.
2d8
0
0
0
0/0


Very young
2
small
This is a small dragon, about 6 feet long and 4 feet wide.
2d12
2d6
0
0
0/0


Young
3
medim
This is a human size dragon, about 7 feet long and 5 feet wide
2d16
2d8
2d4
0
0/0


Juvenile
4
medim
This dragon is slightly larger than the avgerage person begin 8 feet long and 6 feet wide.
2d16
2d8
2d8
0
0/0


Young Adult
5
large
This is a large dragon, about 10 feet long and 5 feet wide.
4d12
2d16
2d12
0
1/0


Adult
6
large
This is a large dragon, about 10 feet long and 5 feet wide.
4d12
2d16
2d12
0
1/50


Old
7
huge
This dragon is very large, about 20 feet long and 10 feet wide.
4d16
4d12
2d16
4d12
3/150


Ancient
9
huge
This dragon is very large, about 20 feet long and 10 feet wide.
4d16
4d12
2d16
4d12
4/200


Wrym
9
gargantuan
This dragon is massive, about 40 feet by 20 feet wide.
8d12
4d16
4d12
2s16
5/250


Great wyrm
10
colossal
This dragon is this biggest! About 80 feet long and 40 feet wide!
8d16
8d12
4d16
8d12
6/300


----------------------
Dragons By Color

color
breath_damage
breath_type
immune


Black
(age_mod*2)d4
acid
acid


Blue
(age_mod*2)d8
Lighting
lighting


Green
(age_mod*2)d6
acid
acid


Red
(age_mod*2)d10
fire
fire


White
(age_mod*2)d6
cold
cold


Brass
(age_mod*2)d6
fire
fire


Bronze
(age_mod*2)d6
lightning
lightning


Copper
(age_mod*2)d4
acid
acid


Gold
(age_mod*2)d10
fire
fire

Silver
(age_mod*2)d8
cold
cold


Amythest
d8
Force
poison


Crystal
d6
Light
cold


Emerald
d6
Sonic  *makes deaf
poison


Sapphire
d4
Sonic *makes deaf
Lightning


Topaz
d8
Lighting
cold


*********************************************************************************/
