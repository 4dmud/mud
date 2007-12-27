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

int damage(struct char_data *ch, struct char_data *victim, int dam,int attacktype);
void damage_count_free(struct char_data *vict);
void damage_count(struct char_data *vict, long id, int dam);
