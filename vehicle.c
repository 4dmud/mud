#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "screen.h"
#include "house.h"
#include "constants.h"

struct obj_data *get_obj_in_list_type(int type,
					     struct obj_data *list);

room_rnum VEHICLE_ROOM = NULL;

void view_room_by_rnum(struct char_data *ch, room_rnum is_in);
void parse_room_name(int in_room, char *bufptr);
void parse_room_description(int in_room, char *bufptr);
void list_obj_to_char(struct obj_data *list, struct char_data *ch,
		      int mode, int show);
void list_char_to_char(struct char_data *list, struct char_data *ch);
void ASSIGNOBJ(int obj, SPECIAL(fname));

int enter_wtrigger(struct room_data *room, struct char_data *actor,
		   int dir);

ACMD(do_look);



struct obj_data *find_vehicle(int roomNum);

typedef struct {
    int dirNum;
    char *dirCmd;
} dirParseStruct;

#define DRIVE_DIRS 4
#define PILOT_DIRS 6

#ifndef EXITN
#  define EXITN(room, door)		(room->dir_option[door])
#endif

struct obj_data *find_vehicle_by_vnum(int vnum)
{
    struct obj_data *i;
    for (i = object_list; i; i = i->next)
	if (GET_OBJ_TYPE(i) == ITEM_VEHICLE)
	    if (GET_OBJ_VNUM(i) == vnum)
		return (i);

    return (0);
}

struct obj_data *has_vehicle(struct char_data *ch) {
if (SITTING(ch) && GET_OBJ_TYPE(SITTING(ch)) == ITEM_SPACEBIKE)
return SITTING(ch);
return NULL;
}

void auto_exits_by_rnum(struct char_data *ch, room_rnum is_in)
{
    int door;
    int found = 0;

    new_send_to_char(ch, "%s[ Exits: %s",CBGRN(ch, C_NRM),CBWHT(ch, C_NRM));

    for (door = 0; door < NUM_OF_DIRS; door++)
	if (EXITN(is_in, door) && EXITN(is_in, door)->to_room != NULL &&
	    !IS_SET(EXITN(is_in, door)->exit_info, EX_CLOSED)) {
	    new_send_to_char(ch, "%c ", LOWER(*dirs[door]));
	    found = 1;
	    }

    new_send_to_char(ch, "%s%s]%s\r\n", (found ? "" : "None! "), CBGRN(ch, C_NRM),   CCNRM(ch, C_NRM));
}


void view_room_by_rnum(struct char_data *ch, room_rnum is_in)
{
   VEHICLE_ROOM = is_in;
   LOOK(ch);
   VEHICLE_ROOM = NULL;
}


ACMD(do_drive)
{
    struct char_data *people;
    char buf2[MAX_STRING_LENGTH];
    char arg[MAX_STRING_LENGTH];
    int x, dir;
    const dirParseStruct dirParse[6] = {
	{SCMD_NORTH - 1, "north"},
	{SCMD_SOUTH - 1, "south"},
	{SCMD_EAST - 1, "east"},
	{SCMD_WEST - 1, "west"},
	{SCMD_UP - 1, "up"},
	{SCMD_DOWN - 1, "down"}
    };
    struct obj_data *vehicle, *controls, *vehicle_in_out;

    controls = get_obj_in_list_type(ITEM_V_CONTROLS,
				    IN_ROOM(ch)->contents);
    if (!controls) {
	send_to_char
	    ("ERROR!  Vehicle controls present yet not present!\r\n", ch);
	return;
    }
    vehicle = find_vehicle_by_vnum(GET_OBJ_VAL(controls, 0));
    if (!vehicle) {
	send_to_char("ERROR!  Vehicle has been lost somehow!\r\n", ch);
	return;
    }

    if (IS_AFFECTED(ch, AFF_BLIND)) {
	/* Blind characters can't drive! */
	send_to_char("You can't see the controls!\r\n", ch);
	return;
    }

    two_arguments(argument, arg, buf2);

    /* Gotta give us a direction... */
    if (!*arg) {
	send_to_char("Drive which direction?\r\n", ch);
	return;
    }

    /* Driving Into another Vehicle */
    if (is_abbrev(arg, "into")) {
	room_rnum was_in, is_in, is_going_to;

	if (!*buf2) {
	    send_to_char("Drive into what?\r\n", ch);
	    return;
	}

	vehicle_in_out = get_obj_in_list_vis(ch, buf2, NULL,vehicle->in_room->contents);
	if (!vehicle_in_out) {
	    send_to_char("Nothing here by that name!\r\n", ch);
	    return;
	}

	if (GET_OBJ_TYPE(vehicle_in_out) != ITEM_VEHICLE) {
	    send_to_char("Thats not a vehicle.\r\n", ch);
	    return;
	}

	is_going_to = real_room(GET_OBJ_VAL(vehicle_in_out, 0));

	if (!IS_SET_AR(ROOM_FLAGS(is_going_to), ROOM_VEHICLE)) {
	    send_to_char("That vehicle can't carry other vehicles.", ch);
	    return;
	}

	send_to_room(IN_ROOM(vehicle), "%s enters %s.\r\n", vehicle->short_description,
		vehicle_in_out->short_description);

	was_in = vehicle->in_room;
	obj_from_room(vehicle);
	obj_to_room(vehicle, is_going_to);

	is_in = vehicle->in_room;

	if (ch->desc != NULL)
	    view_room_by_rnum(ch, is_in);

	send_to_room(IN_ROOM(vehicle), "%s enters.\r\n", vehicle->short_description);


	return;
    } else if (is_abbrev(arg, "out")) {
	struct obj_data *hatch;

	hatch = get_obj_in_list_type(ITEM_V_HATCH,
				     vehicle->in_room->contents);
	if (!hatch) {
	    send_to_char("Nowhere to drive out of.\r\n", ch);
	    return;
	}

	vehicle_in_out = find_vehicle_by_vnum(GET_OBJ_VAL(hatch, 0));
	if (!vehicle_in_out) {
	    send_to_char
		("ERROR!  Vehicle to drive out of doesn't exist!\r\n", ch);
	    return;
	}

	send_to_room(IN_ROOM(vehicle), "%s exits %s.\r\n", vehicle->short_description,
		vehicle_in_out->short_description);

	obj_from_room(vehicle);
	obj_to_room(vehicle, vehicle_in_out->in_room);

	if (ch->desc != NULL)
	    view_room_by_rnum(ch, vehicle->in_room);

	send_to_room(IN_ROOM(vehicle), "%s drives out of %s.\r\n",
		vehicle->short_description,
		vehicle_in_out->short_description);

	return;
    } else
	for (x = 0; x < (GET_OBJ_VAL(vehicle, 1) ? PILOT_DIRS :
			 DRIVE_DIRS); x++)
	    /* Drive in a direction... */
	    if (is_abbrev(arg, dirParse[x].dirCmd)) {
		dir = dirParse[x].dirNum;
		/* Ok we found the direction! */
		if (ch == NULL || dir < 0 || dir >= NUM_OF_DIRS)
		    /* But something is invalid */
		    return;
		else if (!EXIT(vehicle, dir) || EXIT(vehicle, dir)->to_room == NULL) {
		    /* But there is no exit that way */
		    send_to_char("Alas, you cannot go that way...\r\n", ch);
		    return;
		} else
		    if (IS_SET(EXIT(vehicle, dir)->exit_info, EX_CLOSED)) {
		    /* But the door is closed */
		    if (EXIT(vehicle, dir)->keyword) {
			new_send_to_char(ch, "The %s seems to be closed.\r\n",
				fname(EXIT(vehicle, dir)->keyword));
		    } else
			send_to_char("It seems to be closed.\r\n", ch);
		    return;
		} else
		    if (!IS_SET_AR
			(ROOM_FLAGS(EXIT(vehicle, dir)->to_room),
			 ROOM_VEHICLE)) {
		    /* But the vehicle can't go that way */
		    send_to_char
			("The vehicle can't manage that terrain.\r\n", ch);
		    return;
		} else {
		    /* But nothing!  Let's go that way! */
		    room_rnum was_in, is_in;

		    send_to_room(IN_ROOM(vehicle), "%s leaves %s.\r\n",
			    vehicle->short_description, dirs[dir]);

		    was_in = IN_ROOM(vehicle);
		    obj_from_room(vehicle);
		    obj_to_room(vehicle,
				was_in->dir_option[dir]->to_room);

		    is_in = vehicle->in_room;

		    for (people = IN_ROOM(ch)->people;
			 people != NULL; people = people->next_in_room)
			if (people->desc != NULL)
			    view_room_by_rnum(people, is_in);

		    send_to_room(is_in, "%s enters from the %s.\r\n",
			    vehicle->short_description,
			    dirs[rev_dir[dir]]);

		    if (!enter_wtrigger(is_in, ch, dir))
			return;

		    return;
		}
	    }
    send_to_char("Thats not a valid direction.\r\n", ch);
    return;
}


SPECIAL(vehicle)
{
    struct obj_data *obj = NULL;
    char arg[MAX_INPUT_LENGTH];
    if (CMD_IS("enter")) {
	one_argument(argument, arg);

	if (!*arg) {
	    send_to_char("Enter what?\r\n", ch);
	    return (1);
	}

	obj =
	    get_obj_in_list_vis(ch, arg, NULL,
				IN_ROOM(ch)->contents);

	if (!obj) {
	    send_to_char("Nothing by that name is here to enter!\r\n", ch);
	    return (1);
	} else if (GET_OBJ_TYPE(obj) == ITEM_VEHICLE) {
	    
	

	act("You climb into $o.", TRUE, ch, obj, 0, TO_CHAR);
	act("$n climbs into $o.", TRUE, ch, obj, 0, TO_ROOM);
	move_char_to(ch, real_room(GET_OBJ_VAL(obj, 0)));
	act("$n climbs in.", TRUE, ch, 0, 0, TO_ROOM);
	do_look(ch, "", 0, 0);
	return (1);
	}
	return 0;
    }
    return (0);
}


SPECIAL(vehicle_controls)
{
    if (CMD_IS("drive") || CMD_IS("pilot")) {
	do_drive(ch, argument, cmd, 0);
	return (1);
    }
    return (0);
}


SPECIAL(vehicle_hatch)
{
    struct obj_data *hatch, *v;
    if (CMD_IS("leave")) {
	hatch =
	    get_obj_in_list_type(ITEM_V_HATCH,
				 IN_ROOM(ch)->contents);
	if (!hatch) {
	    send_to_char("ERROR!  Hatch is there, yet it isn't!\r\n", ch);
	    return (1);
	}
	v = find_vehicle_by_vnum(GET_OBJ_VAL(hatch, 0));
	if (!v) {
	    send_to_char("ERROR!  Vehicle has been lost somehow!\r\n", ch);
	    return (1);
	}
	act("$n leaves $o.", TRUE, ch, v, 0, TO_ROOM);
	act("You climb out of $o.", TRUE, ch, v, 0, TO_CHAR);
	move_char_to(ch, v->in_room);
	act("$n climbs out of $o.", TRUE, ch, v, 0, TO_ROOM);

	/* Now show them the room */
	do_look(ch, "", 0, 0);

	return (1);
    }
    return (0);
}


SPECIAL(vehicle_window)
{
    struct obj_data *viewport, *v;
    char arg[MAX_INPUT_LENGTH];
    if (CMD_IS("look")) {
	one_argument(argument, arg);
	if (is_abbrev(arg, "out")) {
	    viewport =
		get_obj_in_list_type(ITEM_V_WINDOW,
				     IN_ROOM(ch)->contents);
	    if (!viewport) {
		send_to_char
		    ("ERROR!  Viewport present, yet not present!\r\n", ch);
		return (1);
	    }
	    v = find_vehicle_by_vnum(GET_OBJ_VAL(viewport, 0));
	    if (!v) {
		send_to_char("ERROR!  Vehicle has been lost somehow!\r\n",
			     ch);
		return (1);
	    }
	    view_room_by_rnum(ch, v->in_room);
	    return (1);
	}
    }
    return (0);
}

/* assign special procedures to vehicular objects */
void assign_vehicles(void)
{
    SPECIAL(vehicle_controls);
    SPECIAL(vehicle_window);
    SPECIAL(vehicle_hatch);
    SPECIAL(vehicle);

    /* Assign the generic tank! */
    ASSIGNOBJ(210, vehicle);
    ASSIGNOBJ(211, vehicle_controls);
    ASSIGNOBJ(212, vehicle_hatch);
    ASSIGNOBJ(213, vehicle_window);

    ASSIGNOBJ(280, vehicle);
    ASSIGNOBJ(281, vehicle_controls);
    ASSIGNOBJ(282, vehicle_hatch);
    ASSIGNOBJ(283, vehicle_window);

    ASSIGNOBJ(559, vehicle);
    ASSIGNOBJ(560, vehicle_window);
    ASSIGNOBJ(561, vehicle_controls);
    ASSIGNOBJ(562, vehicle_hatch);

    ASSIGNOBJ(1837, vehicle);
    ASSIGNOBJ(1838, vehicle_controls);
    ASSIGNOBJ(1839, vehicle_window);
    ASSIGNOBJ(1840, vehicle_hatch);

    ASSIGNOBJ(2481, vehicle);
    ASSIGNOBJ(2482, vehicle_window);
    ASSIGNOBJ(2483, vehicle_hatch);
    ASSIGNOBJ(2484, vehicle_controls);

    ASSIGNOBJ(3141, vehicle);
    ASSIGNOBJ(3142, vehicle_controls);
    ASSIGNOBJ(3144, vehicle_window);
    ASSIGNOBJ(3143, vehicle_hatch);

    ASSIGNOBJ(3145, vehicle);
    ASSIGNOBJ(3146, vehicle_controls);
    ASSIGNOBJ(3148, vehicle_window);
    ASSIGNOBJ(3147, vehicle_hatch);

    ASSIGNOBJ(3150, vehicle);
    ASSIGNOBJ(3151, vehicle_controls);
    ASSIGNOBJ(3153, vehicle_window);
    ASSIGNOBJ(3152, vehicle_hatch);

    ASSIGNOBJ(3500, vehicle);
    ASSIGNOBJ(3501, vehicle_hatch);
    ASSIGNOBJ(3502, vehicle_controls);
    ASSIGNOBJ(3503, vehicle_window);

    ASSIGNOBJ(3504, vehicle);
    ASSIGNOBJ(3505, vehicle_hatch);
    ASSIGNOBJ(3506, vehicle_controls);
    ASSIGNOBJ(3507, vehicle_window);

    ASSIGNOBJ(3508, vehicle);
    ASSIGNOBJ(3509, vehicle_hatch);
    ASSIGNOBJ(3510, vehicle_controls);
    ASSIGNOBJ(3511, vehicle_window);

    ASSIGNOBJ(3512, vehicle);
    ASSIGNOBJ(3513, vehicle_hatch);
    ASSIGNOBJ(3514, vehicle_controls);
    ASSIGNOBJ(3515, vehicle_window);

    ASSIGNOBJ(3516, vehicle);
    ASSIGNOBJ(3517, vehicle_hatch);
    ASSIGNOBJ(3518, vehicle_controls);
    ASSIGNOBJ(3519, vehicle_window);

    ASSIGNOBJ(4706, vehicle);
    ASSIGNOBJ(4707, vehicle_window);
    ASSIGNOBJ(4708, vehicle_hatch);
    ASSIGNOBJ(4709, vehicle_controls);

    ASSIGNOBJ(6299, vehicle);
    ASSIGNOBJ(6260, vehicle_window);
    ASSIGNOBJ(6261, vehicle_hatch);
    ASSIGNOBJ(6262, vehicle_controls);

    ASSIGNOBJ(6324, vehicle);
    ASSIGNOBJ(6325, vehicle_window);
    ASSIGNOBJ(6326, vehicle_hatch);
    ASSIGNOBJ(6327, vehicle_controls);

    ASSIGNOBJ(6370, vehicle);
    ASSIGNOBJ(6371, vehicle_window);
    ASSIGNOBJ(6372, vehicle_hatch);
    ASSIGNOBJ(6373, vehicle_controls);

    ASSIGNOBJ(6901, vehicle);
    ASSIGNOBJ(6904, vehicle_window);
    ASSIGNOBJ(6902, vehicle_controls);
    ASSIGNOBJ(6903, vehicle_hatch);

    ASSIGNOBJ(6905, vehicle);
    ASSIGNOBJ(6908, vehicle_window);
    ASSIGNOBJ(6907, vehicle_controls);
    ASSIGNOBJ(6906, vehicle_hatch);

    ASSIGNOBJ(6909, vehicle);
    ASSIGNOBJ(6912, vehicle_window);
    ASSIGNOBJ(6911, vehicle_controls);
    ASSIGNOBJ(6910, vehicle_hatch);

    ASSIGNOBJ(7301, vehicle);
    ASSIGNOBJ(7348, vehicle_window);
    ASSIGNOBJ(7347, vehicle_controls);
    ASSIGNOBJ(7302, vehicle_hatch);

    ASSIGNOBJ(7577, vehicle);
    ASSIGNOBJ(7580, vehicle_window);
    ASSIGNOBJ(7578, vehicle_controls);
    ASSIGNOBJ(7579, vehicle_hatch);

    ASSIGNOBJ(8200, vehicle);
    ASSIGNOBJ(8220, vehicle_window);
    ASSIGNOBJ(8240, vehicle_hatch);
    ASSIGNOBJ(8260, vehicle_controls);

    ASSIGNOBJ(8201, vehicle);
    ASSIGNOBJ(8221, vehicle_window);
    ASSIGNOBJ(8241, vehicle_hatch);
    ASSIGNOBJ(8261, vehicle_controls);

    ASSIGNOBJ(8202, vehicle);
    ASSIGNOBJ(8222, vehicle_window);
    ASSIGNOBJ(8242, vehicle_hatch);
    ASSIGNOBJ(8262, vehicle_controls);

    ASSIGNOBJ(8203, vehicle);
    ASSIGNOBJ(8223, vehicle_window);
    ASSIGNOBJ(8243, vehicle_hatch);
    ASSIGNOBJ(8263, vehicle_controls);

    ASSIGNOBJ(8204, vehicle);
    ASSIGNOBJ(8224, vehicle_window);
    ASSIGNOBJ(8244, vehicle_hatch);
    ASSIGNOBJ(8264, vehicle_controls);

    ASSIGNOBJ(8205, vehicle);
    ASSIGNOBJ(8225, vehicle_window);
    ASSIGNOBJ(8245, vehicle_hatch);
    ASSIGNOBJ(8265, vehicle_controls);

    ASSIGNOBJ(8206, vehicle);
    ASSIGNOBJ(8226, vehicle_window);
    ASSIGNOBJ(8246, vehicle_hatch);
    ASSIGNOBJ(8266, vehicle_controls);

    ASSIGNOBJ(8207, vehicle);
    ASSIGNOBJ(8227, vehicle_window);
    ASSIGNOBJ(8247, vehicle_hatch);
    ASSIGNOBJ(8267, vehicle_controls);

    ASSIGNOBJ(8208, vehicle);
    ASSIGNOBJ(8228, vehicle_window);
    ASSIGNOBJ(8248, vehicle_hatch);
    ASSIGNOBJ(8268, vehicle_controls);

    ASSIGNOBJ(8209, vehicle);
    ASSIGNOBJ(8229, vehicle_window);
    ASSIGNOBJ(8249, vehicle_hatch);
    ASSIGNOBJ(8269, vehicle_controls);

    ASSIGNOBJ(8210, vehicle);
    ASSIGNOBJ(8230, vehicle_window);
    ASSIGNOBJ(8250, vehicle_hatch);
    ASSIGNOBJ(8270, vehicle_controls);

    ASSIGNOBJ(8211, vehicle);
    ASSIGNOBJ(8231, vehicle_window);
    ASSIGNOBJ(8251, vehicle_hatch);
    ASSIGNOBJ(8271, vehicle_controls);

    ASSIGNOBJ(8212, vehicle);
    ASSIGNOBJ(8232, vehicle_window);
    ASSIGNOBJ(8252, vehicle_hatch);
    ASSIGNOBJ(8272, vehicle_controls);

    ASSIGNOBJ(8213, vehicle);
    ASSIGNOBJ(8233, vehicle_window);
    ASSIGNOBJ(8253, vehicle_hatch);
    ASSIGNOBJ(8273, vehicle_controls);

    ASSIGNOBJ(8214, vehicle);
    ASSIGNOBJ(8234, vehicle_window);
    ASSIGNOBJ(8254, vehicle_hatch);
    ASSIGNOBJ(8274, vehicle_controls);

    ASSIGNOBJ(8215, vehicle);
    ASSIGNOBJ(8235, vehicle_window);
    ASSIGNOBJ(8255, vehicle_hatch);
    ASSIGNOBJ(8275, vehicle_controls);

    ASSIGNOBJ(8216, vehicle);
    ASSIGNOBJ(8236, vehicle_window);
    ASSIGNOBJ(8256, vehicle_hatch);
    ASSIGNOBJ(8276, vehicle_controls);

    ASSIGNOBJ(8217, vehicle);
    ASSIGNOBJ(8237, vehicle_window);
    ASSIGNOBJ(8257, vehicle_hatch);
    ASSIGNOBJ(8277, vehicle_controls);

    ASSIGNOBJ(8218, vehicle);
    ASSIGNOBJ(8238, vehicle_window);
    ASSIGNOBJ(8258, vehicle_hatch);
    ASSIGNOBJ(8278, vehicle_controls);

    ASSIGNOBJ(8219, vehicle);
    ASSIGNOBJ(8239, vehicle_window);
    ASSIGNOBJ(8259, vehicle_hatch);
    ASSIGNOBJ(8279, vehicle_controls);

    ASSIGNOBJ(4381, vehicle);
    ASSIGNOBJ(11518, vehicle_window);
    ASSIGNOBJ(11502, vehicle_controls);
    ASSIGNOBJ(11519, vehicle_hatch);

    ASSIGNOBJ(11510, vehicle);
    ASSIGNOBJ(11513, vehicle_window);
    ASSIGNOBJ(11511, vehicle_controls);
    ASSIGNOBJ(11512, vehicle_hatch);

    ASSIGNOBJ(13731, vehicle);
    ASSIGNOBJ(13733, vehicle_window);
    ASSIGNOBJ(13734, vehicle_controls);
    ASSIGNOBJ(13732, vehicle_hatch);

    ASSIGNOBJ(17627, vehicle);
    ASSIGNOBJ(17628, vehicle_window);
    ASSIGNOBJ(17630, vehicle_controls);
    ASSIGNOBJ(17629, vehicle_hatch);

    ASSIGNOBJ(18905, vehicle);
    ASSIGNOBJ(18919, vehicle_controls);
    ASSIGNOBJ(18921, vehicle_window);
    ASSIGNOBJ(18920, vehicle_hatch);

    ASSIGNOBJ(19301, vehicle);
    ASSIGNOBJ(19302, vehicle_controls);
    ASSIGNOBJ(19304, vehicle_window);
    ASSIGNOBJ(19303, vehicle_hatch);

    ASSIGNOBJ(19305, vehicle);
    ASSIGNOBJ(19306, vehicle_controls);
    ASSIGNOBJ(19308, vehicle_window);
    ASSIGNOBJ(19307, vehicle_hatch);

    ASSIGNOBJ(30053, vehicle);
    ASSIGNOBJ(30054, vehicle_window);
    ASSIGNOBJ(30056, vehicle_controls);
    ASSIGNOBJ(30055, vehicle_hatch);

    ASSIGNOBJ(30060, vehicle);
    ASSIGNOBJ(30061, vehicle_window);
    ASSIGNOBJ(30063, vehicle_controls);
    ASSIGNOBJ(30062, vehicle_hatch);

    

    ASSIGNOBJ(54152, vehicle);
    ASSIGNOBJ(54153, vehicle_window);
    ASSIGNOBJ(54155, vehicle_controls);
    ASSIGNOBJ(54154, vehicle_hatch);

    ASSIGNOBJ(54156, vehicle);
    ASSIGNOBJ(54157, vehicle_window);
    ASSIGNOBJ(54159, vehicle_controls);
    ASSIGNOBJ(54158, vehicle_hatch);

    ASSIGNOBJ(54160, vehicle);
    ASSIGNOBJ(54161, vehicle_window);
    ASSIGNOBJ(54163, vehicle_controls);
    ASSIGNOBJ(54162, vehicle_hatch);

}
