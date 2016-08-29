/************************************************************************
 * Generic OLC Library - Rooms / genwld.h			v1.0	*
 * Original author: Levork						*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/
#ifndef GENWLD_H
#define GENWLD_H

room_rnum add_room(Room *);
int delete_room(room_rnum);
int save_rooms(zone_rnum);
room_rnum duplicate_room(room_vnum to, room_rnum from);

#endif
