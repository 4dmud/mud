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

#ifndef DAMAGE_H
#define DAMAGE_H

int damage(Character *ch, Character *victim, int dam,int attacktype);
void damage_count_free(Character *vict);
void damage_count(Character *vict, long id, int dam);
int room_affect_damage ( Character *ch, Character *vict, int dam );

#endif
