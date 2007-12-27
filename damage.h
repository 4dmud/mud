//
// C++ Interface: damage
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2005
//
// Copyright: See COPYING file that comes with this distribution
//
//

int damage(Character *ch, Character *victim, int dam,int attacktype);
void damage_count_free(Character *vict);
void damage_count(Character *vict, long id, int dam);
