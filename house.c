/* ************************************************************************
*   File: house.c                                       Part of CircleMUD *
*  Usage: Handling of player houses                                       *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "comm.h"
#include "handler.h"
#include "db.h"
#include "interpreter.h"
#include "utils.h"
#include "house.h"
#include "constants.h"

extern int load_qic_check ( int rnum );
extern const int xap_objs;
extern int save_new_style;

struct obj_data *Obj_from_store ( struct obj_file_elem object,
			                                  int *location );
int Obj_to_store ( struct obj_data *obj, FILE * fl, int location );
int Obj_to_store_from ( struct obj_data *obj, FILE * fl, int locate );
struct obj_data *Obj_from_store_to ( struct obj_file_elem obj, int *locate );
struct obj_data *Obj_from_store ( struct obj_file_elem object,
			                                  int *location );
void House_listrent ( Character *ch, room_vnum vnum );
void count_items_in_list ( struct obj_data *obj, int& total_items );
int house_item_count ( room_vnum vnum );
void hcontrol_set_stable ( Character *ch, char *arg );
void house_load_mount ( Character *ch, int i );
struct house_control_rec house_control[MAX_HOUSES];
int num_of_houses = 0;

/* local functions */
int House_get_filename ( room_vnum vnum, char *filename, size_t maxlen );
int House_load ( room_vnum vnum );
int House_save ( struct obj_data *obj, FILE * fp, int locate );
void House_restore_weight ( struct obj_data *obj );
void House_delete_file ( int vnum );
int find_house ( room_vnum vnum );
void House_save_control ( void );
void hcontrol_list_houses ( Character *ch );
void hcontrol_build_house ( Character *ch, char *arg );
void hcontrol_destroy_house ( Character *ch, char *arg );
void hcontrol_pay_house ( Character *ch, char *arg );
ACMD ( do_hcontrol );
ACMD ( do_house );
int old_house_load ( room_rnum rnum, FILE *fl );

/** new house save stuff **/
struct obj_data * read_one_item ( FILE *fl, OBJ_DATA *temp, int *locate );
int relocate_obj ( room_rnum rnum, Character *ch, OBJ_DATA *temp, int locate, OBJ_DATA **cont_row );
int load_objects_to_room ( room_rnum rnum, FILE *fl );
int save_one_item ( OBJ_DATA *obj, FILE *fl, int locate );

/* First, the basics: finding the filename; loading/saving objects */

/* Return a filename given a house vnum */
int House_get_filename ( room_vnum vnum, char *filename, size_t maxlen )
{
	if ( vnum == NOWHERE )
		return ( 0 );

	snprintf ( filename, maxlen, LIB_HOUSE "%d.house", vnum );
	return ( 1 );
}
int House_get_new_filename ( room_vnum vnum, char *filename, size_t maxlen )
{
	if ( vnum == NOWHERE )
		return ( 0 );

	snprintf ( filename, maxlen, LIB_HOUSE "%d.nhouse", vnum );
	return ( 1 );
}

#define MAX_CONT_ROW	5
#define MAX_COFFIN_ROW  5



/* Load all objects for a house */
int House_load ( room_vnum vnum )
{
	FILE *fl = NULL;
	room_rnum rnum;
	char fname1[MAX_INPUT_LENGTH], fname2[MAX_INPUT_LENGTH];
	int retval = 0;

	if ( ( rnum = real_room ( vnum ) ) == NULL )
		return ( retval );
	House_get_new_filename ( vnum, fname1, sizeof ( fname1 ) );
	House_get_filename ( vnum, fname2, sizeof ( fname2 ) );
	if ( ( fl = fopen ( fname1, "r+b" ) ) != NULL )
	{
		/* no file found */
		retval = load_objects_to_room ( rnum, fl ); //from objsave
		fclose ( fl );
	}
	else if ( ( fl = fopen ( fname2, "r+b" ) ) != NULL )
	{
		/* no file found */
		retval = old_house_load ( rnum, fl );
		fclose ( fl );
	}

	return retval;

}

int old_house_load ( room_rnum rnum, FILE *fl )
{
	char line[READ_SIZE];		// kalten
	struct obj_data *obj = NULL;	// kalten
	struct obj_data *tmp, *next;	// kalten
	struct obj_data *cont_row[MAX_CONT_ROW];	// kalten
	int t[20], danger, zwei = 0;	// kalten
	int locate = 0, j, nr, k;	// kalten
	struct extra_descr_data *new_descr;	// kalten
	char buf2[MAX_INPUT_LENGTH];
	int retval = 0;

	log ( "House loading using old format." );

	for ( j = 0; j < MAX_CONT_ROW; j++ )
		cont_row[j] = NULL;	/* empty the containers */

	if ( !feof ( fl ) )
		get_line ( fl, line );


	while ( !feof ( fl ) )
	{
		/* This handles the XAP/Ascii objects */
		/* first, we get the number. Not too hard */
		if ( *line == '#' )
		{
			if ( sscanf ( line, "#%d", &nr ) != 1 )
			{
				continue;
			}
			/* we have the number, check it, load obj. */
			if ( nr == NOTHING )
			{
				/* then it is unique */
				obj = create_obj ( NOTHING );
			}
			else if ( nr < 0 )
			{
				continue;
			}
			else
			{
				// log("Obj nr: %d.", nr);
				obj = read_object ( nr, VIRTUAL );
				if ( nr >= 999999 || !obj )
				{
					log ( "Bad vnum in house %d, vnum %d", rnum->number, nr );
					do
					{
						get_line ( fl, line );
					}
					while ( !feof ( fl ) && ( *line != '#' ) );

					continue;
				}
			}

			get_line ( fl, line );
			retval = sscanf ( line, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
			                  t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6, t + 7,
			                  t + 8, t + 9, t + 10, t + 11, t + 12, t + 13, t + 14, t + 15, t + 16 );
			locate = t[0];
			if ( retval == 10 )
			{
				GET_OBJ_VAL ( obj, 0 ) = t[1];
				GET_OBJ_VAL ( obj, 1 ) = t[2];
				GET_OBJ_VAL ( obj, 2 ) = t[3];
				GET_OBJ_VAL ( obj, 3 ) = t[4];
				GET_OBJ_EXTRA ( obj ) [0] = t[5];
				GET_OBJ_EXTRA ( obj ) [1] = t[6];
				GET_OBJ_EXTRA ( obj ) [2] = t[7];
				GET_OBJ_EXTRA ( obj ) [3] = t[8];
				GET_OBJ_TIMER ( obj ) = t[9];
				/*---unknown----*/
				GET_OBJ_VAL ( obj, 4 ) = 0;
				GET_OBJ_VAL ( obj, 5 ) = 0;
				GET_OBJ_VAL ( obj, 6 ) = 0;
				GET_OBJ_VAL ( obj, 7 ) = 0;
				GET_OBJ_VAL ( obj, 8 ) = 0;
				GET_OBJ_VAL ( obj, 9 ) = 0;
				//GET_OBJ_INNATE(obj) = 0;
			}
			else
			{
				GET_OBJ_VAL ( obj, 0 ) = t[1];
				GET_OBJ_VAL ( obj, 1 ) = t[2];
				GET_OBJ_VAL ( obj, 2 ) = t[3];
				GET_OBJ_VAL ( obj, 3 ) = t[4];
				GET_OBJ_VAL ( obj, 4 ) = t[5];
				GET_OBJ_VAL ( obj, 5 ) = t[6];
				GET_OBJ_VAL ( obj, 6 ) = t[7];
				GET_OBJ_VAL ( obj, 7 ) = t[8];
				GET_OBJ_VAL ( obj, 8 ) = t[9];
				GET_OBJ_VAL ( obj, 9 ) = t[10];
				GET_OBJ_EXTRA ( obj ) [0] = t[11];
				GET_OBJ_EXTRA ( obj ) [1] = t[12];
				GET_OBJ_EXTRA ( obj ) [2] = t[13];
				GET_OBJ_EXTRA ( obj ) [3] = t[14];
				GET_OBJ_TIMER ( obj ) = t[15];
				GET_OBJ_INNATE ( obj ) = t[16];
			}


			get_line ( fl, line );
			/* read line check for xap. */
			if ( !strcasecmp ( "XAP", line ) )
			{	/* then this is a Xap Obj, requires
                                						   special care */
				if ( ( obj->name = fread_string ( fl, buf2 ) ) == NULL )
				{
					obj->name = strdup ( "undefined" );
				}

				if ( ( obj->short_description =
				            fread_string ( fl, buf2 ) ) == NULL )
				{
					obj->short_description = strdup ( "undefined" );
				}

				if ( ( obj->description = fread_string ( fl, buf2 ) ) == NULL )
				{
					obj->description = strdup ( "undefined" );
				}

				if ( ( obj->action_description =
				            fread_string ( fl, buf2 ) ) == NULL )
				{
					obj->action_description = strdup ( "undefined" );
				}

				if ( !get_line ( fl, line ) ||
				        ( sscanf ( line, "%d %d %d %d %d %d %d %d",
				                   t, t + 1, t + 2, t + 3, t + 4, t + 5, t + 6,
				                   t + 7 ) != 8 ) )
				{
					log ( "Format error in first numeric line (expecting _8_ args)" );
					return 0;
				}
				obj->obj_flags.type_flag = t[0];
				obj->obj_flags.wear_flags[0] = t[1];
				obj->obj_flags.wear_flags[1] = t[2];
				obj->obj_flags.wear_flags[2] = t[3];
				obj->obj_flags.wear_flags[3] = t[4];
				obj->obj_flags.weight = t[5];
				obj->obj_flags.cost = t[6];
				obj->obj_flags.cost_per_day = t[7];



				/* we're clearing these for good luck */

				for ( j = 0; j < MAX_OBJ_AFFECT; j++ )
				{
					obj->affected[j].location = APPLY_NONE;
					obj->affected[j].modifier = 0;
				}

				/* You have to null out the extradesc when you are parsing a xap_obj.
				   This is done right before the extradesc is read. */

				if ( obj->ex_description )
				{
					obj->ex_description = NULL;
				}

				get_line ( fl, line );
				for ( k = j = zwei = 0; !zwei && !feof ( fl ); )
				{
					switch ( *line )
					{
						case 'E':
							CREATE ( new_descr, struct extra_descr_data, 1 );
							if ( ( new_descr->keyword = fread_string ( fl, buf2 ) ) == NULL )
								new_descr->keyword = strdup ( "Undefined" );
							if ( ( new_descr->description = fread_string ( fl, buf2 ) ) == NULL )
								new_descr->description = strdup ( "Undefined" );
							new_descr->next = obj->ex_description;
							obj->ex_description = new_descr;
							get_line ( fl, line );
							break;
						case 'A':
							if ( j >= MAX_OBJ_AFFECT )
							{
								log ( "Too many object affections in loading house file" );
								danger = 1;
							}
							get_line ( fl, line );
							sscanf ( line, "%d %d", t, t + 1 );

							obj->affected[j].location = t[0];
							obj->affected[j].modifier = t[1];
							j++;
							get_line ( fl, line );
							break;

						case '$':
						case '#':
							zwei = 1;
							break;
						default:
							zwei = 1;
							break;
					}
				}		/* exit our for loop */
			}			/* exit our xap loop */
			obj_to_room ( obj, rnum );
			// log("Obj %s to house, %d.", obj->short_description, rnum);

			relocate_obj ( rnum, NULL, obj, locate, cont_row );

		}

	}				/* end of reading a xap object */

	tmp = rnum->contents;
	while ( tmp )
	{
		// count QIC in the room
		next = tmp->next_content;
		load_qic_check ( GET_OBJ_RNUM ( tmp ) );
		tmp = next;
	}


	return ( 1 );
}


/* Save all objects for a house (recursive; initial call must be followed
   by a call to House_restore_weight)  Assumes file is open already. */
int House_save ( struct obj_data *obj, FILE * fp, int locate )
{
	struct obj_data *tmp;
	int result;

	if ( obj )
	{
		House_save ( obj->next_content, fp, locate );
		House_save ( obj->contains, fp, MIN ( 0, locate ) - 1 );
		if ( save_new_style )
			result = save_one_item ( obj, fp, locate );
		else
			result = my_obj_save_to_disk ( fp, obj, locate );
		if ( !result )
			return ( 0 );

		for ( tmp = obj->in_obj; tmp; tmp = tmp->in_obj )
			GET_OBJ_WEIGHT ( tmp ) -= GET_OBJ_WEIGHT ( obj );
	}
	return ( 1 );
}


/* restore weight of containers after House_save has changed them for saving */
void House_restore_weight ( struct obj_data *obj )
{
	if ( obj )
	{
		House_restore_weight ( obj->contains );
		House_restore_weight ( obj->next_content );
		if ( obj->in_obj )
			GET_OBJ_WEIGHT ( obj->in_obj ) += GET_OBJ_WEIGHT ( obj );
	}
}


/* Save all objects in a house */
void House_crashsave ( room_vnum vnum )
{
	room_rnum rnum;
	char filename[MAX_STRING_LENGTH];
	char tempname[MAX_STRING_LENGTH];
	FILE *fp;

	if ( ( rnum = real_room ( vnum ) ) == NULL )
		return;
	if ( !House_get_new_filename ( vnum, filename, sizeof ( filename ) ) )
	{
		if ( !House_get_filename ( vnum, filename, sizeof ( filename ) ) )
			return;
	}

	snprintf ( tempname, sizeof ( tempname ), "%s%s", filename, ".tmp" );
	if ( ! ( fp = fopen ( tempname, "wb" ) ) )
	{
		new_mudlog ( NRM, LVL_GOD, TRUE, "unable to write house file: %s", filename );
		perror ( "SYSERR: Error saving house file" );
		return;
	}
	if ( !House_save ( rnum->contents, fp, 0 ) )
	{
		fclose ( fp );
		if ( remove ( tempname ) == -1 )
		{
			new_mudlog ( NRM, LVL_GOD, TRUE, "unable to remove temp file: %s", tempname );
			log ( "unable to remove temp file: %s", tempname );
		}
		return;
	}
	fclose ( fp );
	House_restore_weight ( rnum->contents );
	REMOVE_BIT_AR ( ROOM_FLAGS ( rnum ), ROOM_HOUSE_CRASH );
	if ( rename ( tempname, filename ) == -1 )
	{
		new_mudlog ( NRM, LVL_GOD, TRUE, "Major error (no disk space) can't save file: %s", tempname );
		//core_dump();
	}
}
void count_items_in_list ( struct obj_data *obj, int& total_items )
{
	for ( ; obj; obj = obj->next_content )
	{
		total_items++;
		if ( GET_OBJ_TYPE ( obj ) == ITEM_CONTAINER )
			count_items_in_list ( obj->contains, total_items );
	}
}



int house_item_count ( room_vnum vnum )
{
	room_rnum rnum;
	if ( ( rnum = real_room ( vnum ) ) == NULL )
	{
		return 0;
	}
	else
	{
		int total_items = 0;
		count_items_in_list ( rnum->contents, total_items );
		return ( total_items );
	}
}


/* Delete a house save file */
void House_delete_file ( int vnum )
{
	char filename[MAX_INPUT_LENGTH];
	FILE *fl;

	if ( !House_get_filename ( vnum, filename, sizeof ( filename ) ) )
		return;
	if ( ! ( fl = fopen ( filename, "rb" ) ) )
	{
		if ( errno != ENOENT )
			log ( "SYSERR: Error deleting house file #%d. (1): %s", vnum,
			      strerror ( errno ) );
		return;
	}
	fclose ( fl );
	if ( remove ( filename ) < 0 )
		log ( "SYSERR: Error deleting house file #%d. (2): %s", vnum,
		      strerror ( errno ) );
}


/* List all objects in a house file */
void House_listrent ( Character *ch, room_vnum vnum )
{
	FILE *fl;
	char filename[MAX_STRING_LENGTH];
	char buf[MAX_STRING_LENGTH];
	struct obj_file_elem object;
	struct obj_data *obj;

	if ( !House_get_filename ( vnum, filename, sizeof ( filename ) ) )
		return;
	if ( ! ( fl = fopen ( filename, "rb" ) ) )
	{
		ch->Send ( "No objects on file for house #%d.\r\n", vnum );
		return;
	}
	*buf = '\0';
	while ( !feof ( fl ) )
	{
		fread ( &object, sizeof ( struct obj_file_elem ), 1, fl );
		if ( ferror ( fl ) )
		{
			fclose ( fl );
			return;
		}
		/*
		    if (!feof(fl) && (obj = Obj_from_store(object, &i)) != NULL) {
		      sprintf(buf + strlen(buf), " [%5d] (%5dau) %s\r\n",
			      GET_OBJ_VNUM(obj), GET_OBJ_RENT(obj),
			      obj->short_description);
		      free_obj(obj, FALSE);
		    }
		*/
		if ( !feof ( fl ) )
			if ( real_object ( object.item_number ) > -1 )
			{
				obj = read_object ( object.item_number, VIRTUAL );
				if ( obj )
				{
					ch->Send ( "[%5d] (%5dau) %s\r\n",
					           GET_OBJ_VNUM ( obj ), GET_OBJ_RENT ( obj ),
					           obj->short_description );
					free_obj ( obj, FALSE );
				}
			}
	}

	fclose ( fl );
}




/******************************************************************
 *  Functions for house administration (creation, deletion, etc.  *
 *****************************************************************/

int find_house ( room_vnum vnum )
{
	int i;

	for ( i = 0; i < num_of_houses; i++ )
		if ( house_control[i].vnum == vnum )
			return ( i );

	return ( NOWHERE );
}



/* Save the house control information */
void House_save_control ( void )
{
	FILE *fl;

	if ( ! ( fl = fopen ( HCONTROL_FILE, "wb" ) ) )
	{
		perror ( "SYSERR: Unable to open house control file." );
		return;
	}
	/* write all the house control recs in one fell swoop.  Pretty nifty, eh? */
	fwrite ( house_control, sizeof ( struct house_control_rec ), num_of_houses,fl );

	fclose ( fl );
}


/* call from boot_db - will load control recs, load objs, set atrium bits */
/* should do sanity checks on vnums & remove invalid records */
void House_boot ( void )
{
	struct house_control_rec temp_house;
	room_rnum real_house, real_atrium;
	FILE *fl;
	int k;

	memset ( ( char * ) house_control, 0,
	         sizeof ( struct house_control_rec ) * MAX_HOUSES );

	if ( ! ( fl = fopen ( HCONTROL_FILE, "rb" ) ) )
	{
		if ( errno == ENOENT )
			log ( "   House control file '%s' does not exist.",
			      HCONTROL_FILE );
		else
			perror ( "SYSERR: " HCONTROL_FILE );
		return;
	}
	while ( !feof ( fl ) && num_of_houses < MAX_HOUSES )
	{
		fread ( &temp_house, sizeof ( struct house_control_rec ), 1, fl );

		if ( feof ( fl ) )
			break;

		//if ( pi.NameById ( temp_house.owner ) == NULL )
		//	continue;		/* owner no longer exists -- skip */

		log ( "Reading house: %d.", temp_house.vnum );

		if ( ( real_house = real_room ( temp_house.vnum ) ) == NULL )
			continue;		/* this vnum doesn't exist -- skip */

		if ( find_house ( temp_house.vnum ) != NOWHERE )
			continue;		/* this vnum is already a house -- skip */

		if ( ( real_atrium = real_room ( temp_house.atrium ) ) == NULL )
			continue;		/* house doesn't have an atrium -- skip */

		if ( temp_house.exit_num < 0 || temp_house.exit_num >= NUM_OF_DIRS )
			continue;		/* invalid exit num -- skip */

		if ( TOROOM ( real_house, temp_house.exit_num ) != real_atrium )
			continue;		/* exit num mismatch -- skip */

		temp_house.expantions = ( temp_house.expantions > 9000 || temp_house.expantions < 0 ) ? 0 : temp_house.expantions;


		house_control[num_of_houses++] = temp_house;

		SET_BIT_AR ( ROOM_FLAGS ( real_house ), ROOM_HOUSE );
		SET_BIT_AR ( ROOM_FLAGS ( real_house ), ROOM_PRIVATE );
		SET_BIT_AR ( ROOM_FLAGS ( real_atrium ), ROOM_ATRIUM );
		House_load ( temp_house.vnum );
		for ( k = 0; k < house_control[num_of_houses - 1].num_of_guests; k++ )
		{
			if ( ( pi.NameById ( house_control[num_of_houses - 1].guests[k] ) ) == NULL )
			{
				for ( ; k < house_control[num_of_houses - 1].num_of_guests; k++ )
					house_control[num_of_houses - 1].guests[k] =
					    house_control[num_of_houses - 1].guests[k + 1];
				house_control[num_of_houses - 1].num_of_guests--;
				continue;
			}
		}
	}

	fclose ( fl );
	House_save_control();
}
void purge_room ( int vnum )
{
	obj_data *obj, *obj_next;
	if ( !real_room ( vnum ) )
		return;
	for ( obj = real_room ( vnum )->contents; obj; obj = obj_next )
	{
		obj_next = obj->next_content;
		extract_obj ( obj );
	}

}
void Hcontrol_reload ( int vnum )
{
	log ( "Reloading house: %d.", vnum );

	if ( ( real_room ( vnum ) ) == NULL )
		return;		/* this vnum doesn't exist -- skip */
	House_load ( vnum );

}

/* "House Control" functions */

const char *HCONTROL_FORMAT =
    "Usage: hcontrol build <house vnum> <exit direction> <player name>\r\n"
    "       hcontrol destroy <house vnum>\r\n"
    "       hcontrol pay <house vnum>\r\n"
    "       hcontrol show\r\n"
    "       hcontrol save\r\n"
    "       hcontrol reload <house vnum>\r\n"
    "       hcontrol listrent <house vnum>\r\n"
    "       hcontrol calc <house vnum>\r\n"
    "       hcontrol stable <house vnum>\r\n"
    "       hcontrol expand <house vnum> <number of gold tokens paid>\r\n";
//"       hcontrol size <house vnum> <additional amount of items to hold (base is 500)>\r\n";

void hcontrol_list_houses ( Character *ch )
{
	ush_int i;
	char *timestr, *temp;
	char built_on[128], last_pay[128], own_name[128];

	if ( !num_of_houses )
	{
		send_to_char ( "No houses have been defined.\r\n", ch );
		return;
	}
	*ch << "Address  Atrium  Build Date  Guests  Owner        ExpandedSize Items Stable\r\n";
	*ch << "-------  ------  ----------  ------  ------------ ------------ ----- ------\r\n";

	for ( i = 0; i < num_of_houses; i++ )
	{
		/* Avoid seeing <UNDEF> entries from self-deleted people. -gg 6/21/98 */
		//if ( ( temp = pi.NameById ( house_control[i].owner ) ) == NULL )
		//	continue;
		temp = pi.NameById ( house_control[i].owner );

		if ( house_control[i].built_on )
		{
			timestr = asctime ( localtime ( & ( house_control[i].built_on ) ) );
			* ( timestr + 10 ) = '\0';
			strcpy ( built_on, timestr );
		}
		else
			strcpy ( built_on, "Unknown" );

		if ( house_control[i].last_payment )
		{
			timestr = asctime ( localtime ( & ( house_control[i].last_payment ) ) );
			* ( timestr + 10 ) = '\0';
			strcpy ( last_pay, timestr );
		}
		else
			strcpy ( last_pay, "None" );

		/* Now we need a copy of the owner's name to capitalize. -gg 6/21/98 */
		strcpy ( own_name, temp != NULL ? temp : "<deleted>" );

		ch->Send ( "%7d %7d  %-10s    %2d    %-12s %-13ld %-5d %-6s\r\n",
		           house_control[i].vnum, house_control[i].atrium, built_on,
		           house_control[i].num_of_guests, CAP ( own_name ), 500 + ( house_control[i].expantions * 200 ),
		           house_item_count ( house_control[i].vnum ), house_control[i].stable == 0 ? "No" : "Yes" );

		House_list_guests ( ch, i, TRUE );
	}
}



void hcontrol_build_house ( Character *ch, char *arg )
{
	char arg1[MAX_INPUT_LENGTH];
	struct house_control_rec temp_house;
	room_vnum virt_house, virt_atrium;
	room_rnum real_house, real_atrium;
	sh_int exit_num;
	long owner;

	if ( num_of_houses >= MAX_HOUSES )
	{
		send_to_char ( "Max houses already defined.\r\n", ch );
		return;
	}

	/* first arg: house's vnum */
	arg = one_argument ( arg, arg1 );
	if ( !*arg1 )
	{
		send_to_char ( HCONTROL_FORMAT, ch );
		return;
	}
	virt_house = atoi ( arg1 );
	if ( ( real_house = real_room ( virt_house ) ) == NULL )
	{
		send_to_char ( "No such room exists.\r\n", ch );
		return;
	}
	if ( ( find_house ( virt_house ) ) != NOWHERE )
	{
		send_to_char ( "House already exists.\r\n", ch );
		return;
	}

	/* second arg: direction of house's exit */
	arg = one_argument ( arg, arg1 );
	if ( !*arg1 )
	{
		send_to_char ( HCONTROL_FORMAT, ch );
		return;
	}
	if ( ( exit_num = search_block ( arg1, dirs, FALSE ) ) < 0 )
	{
		ch->Send ( "'%s' is not a valid direction.\r\n", arg1 );
		return;
	}
	if ( TOROOM ( real_house, exit_num ) == NULL )
	{
		ch->Send ( "There is no exit %s from room %d.\r\n",
		           dirs[exit_num], virt_house );
		return;
	}

	real_atrium = TOROOM ( real_house, exit_num );
	virt_atrium = GET_ROOM_VNUM ( real_atrium );

	if ( TOROOM ( real_atrium, rev_dir[exit_num] ) != real_house )
	{
		send_to_char ( "A house's exit must be a two-way door.\r\n", ch );
		return;
	}

	/* third arg: player's name */
	one_argument ( arg, arg1 );
	if ( !*arg1 )
	{
		send_to_char ( HCONTROL_FORMAT, ch );
		return;
	}
	if ( ( owner = pi.IdByName ( arg1 ) ) < 0 )
	{
		ch->Send ( "Unknown player '%s'.\r\n", arg1 );
		return;
	}

	temp_house.mode = HOUSE_PRIVATE;
	temp_house.vnum = virt_house;
	temp_house.atrium = virt_atrium;
	temp_house.exit_num = exit_num;
	temp_house.built_on = time ( 0 );
	temp_house.last_payment = 0;
	temp_house.owner = owner;
	temp_house.num_of_guests = 0;
	temp_house.stable = 0;
	temp_house.expantions = 0;

	house_control[num_of_houses++] = temp_house;

	SET_BIT_AR ( ROOM_FLAGS ( real_house ), ROOM_HOUSE );
	SET_BIT_AR ( ROOM_FLAGS ( real_house ), ROOM_PRIVATE );
	SET_BIT_AR ( ROOM_FLAGS ( real_atrium ), ROOM_ATRIUM );
	House_crashsave ( virt_house );

	send_to_char ( "House built.  Mazel tov!\r\n", ch );
	House_save_control();
}



void hcontrol_destroy_house ( Character *ch, char *arg )
{
	int i, j;
	room_rnum real_atrium, real_house;

	if ( !*arg )
	{
		send_to_char ( HCONTROL_FORMAT, ch );
		return;
	}
	if ( ( i = find_house ( atoi ( arg ) ) ) == NOWHERE )
	{
		send_to_char ( "Unknown house.\r\n", ch );
		return;
	}
	if ( ( real_atrium = real_room ( house_control[i].atrium ) ) == NULL )
		log ( "SYSERR: House %d had invalid atrium %d!", atoi ( arg ),
		      house_control[i].atrium );
	else
		REMOVE_BIT_AR ( ROOM_FLAGS ( real_atrium ), ROOM_ATRIUM );

	if ( ( real_house = real_room ( house_control[i].vnum ) ) == NULL )
		log ( "SYSERR: House %d had invalid vnum %d!", atoi ( arg ),
		      house_control[i].vnum );
	else
	{
		REMOVE_BIT_AR ( ROOM_FLAGS ( real_house ), ROOM_HOUSE );
		REMOVE_BIT_AR ( ROOM_FLAGS ( real_house ), ROOM_PRIVATE );
		REMOVE_BIT_AR ( ROOM_FLAGS ( real_house ), ROOM_HOUSE_CRASH );
	}

	House_delete_file ( house_control[i].vnum );

	for ( j = i; j < num_of_houses - 1; j++ )
		house_control[j] = house_control[j + 1];

	num_of_houses--;

	send_to_char ( "House deleted.\r\n", ch );
	House_save_control();

	/*
	 * Now, reset the ROOM_ATRIUM flag on all existing houses' atriums,
	 * just in case the house we just deleted shared an atrium with another
	 * house.  --JE 9/19/94
	 */
	for ( i = 0; i < num_of_houses; i++ )
		if ( ( real_atrium = real_room ( house_control[i].atrium ) ) != NULL )
			SET_BIT_AR ( ROOM_FLAGS ( real_atrium ), ROOM_ATRIUM );
}


void hcontrol_pay_house ( Character *ch, char *arg )
{
	int i;

	if ( !*arg )
		send_to_char ( HCONTROL_FORMAT, ch );
	else if ( ( i = find_house ( atoi ( arg ) ) ) == NOWHERE )
		send_to_char ( "Unknown house.\r\n", ch );
	else
	{
		new_mudlog ( NRM, MAX ( LVL_GOD, GET_INVIS_LEV ( ch ) ), TRUE, "Payment for house %s collected by %s.", arg,
		             GET_NAME ( ch ) );

		house_control[i].last_payment = time ( 0 );
		House_save_control();
		send_to_char ( "Payment recorded.\r\n", ch );
	}
}

void hcontrol_calc_house ( Character *ch, char *arg )
{
	room_rnum vroom;
	int rent = 0, i;
	struct obj_data *obj;

	if ( ( i = find_house ( atoi ( arg ) ) ) < 0 )
	{
		send_to_char ( "Unknown house.\r\n", ch );
		return;
	}

	if ( ( vroom = real_room ( house_control[i].vnum ) ) == NULL )
		return;

	for ( obj = vroom->contents; obj; obj = obj->next_content )
		rent += obj->obj_flags.cost;

	ch->Send ( "Current rent for %d is %d gold coins.\r\n",
	           house_control[i].vnum, rent );
	return;
}

/* The hcontrol command itself, used by imms to create/destroy houses */
ACMD ( do_hcontrol )
{
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

	half_chop ( argument, arg1, arg2 );

	if ( is_abbrev ( arg1, "build" ) )
		hcontrol_build_house ( ch, arg2 );
	else if ( is_abbrev ( arg1, "destroy" ) )
		hcontrol_destroy_house ( ch, arg2 );
	else if ( is_abbrev ( arg1, "pay" ) )
		hcontrol_pay_house ( ch, arg2 );
	else if ( is_abbrev ( arg1, "stable" ) )
		hcontrol_set_stable ( ch, arg2 );
	else if ( is_abbrev ( arg1, "show" ) )
		hcontrol_list_houses ( ch );
	else if ( is_abbrev ( arg1, "expand" ) )
		hcontrol_expand_house ( ch, arg2 );
	else if ( is_abbrev ( arg1, "save" ) )
	{
		House_save_all();
		send_to_char ( "Save complete.\r\n", ch );
	}
	else if ( is_abbrev ( arg1, "reload" ) )
	{
		Hcontrol_reload ( atoi ( arg2 ) );
		send_to_char ( "Reload complete.\r\n", ch );
	}
	else if ( is_abbrev ( arg1, "listrent" ) )
		House_listrent ( ch, atoi ( arg2 ) );
	else if ( is_abbrev ( arg1, "calculate" ) )
		hcontrol_calc_house ( ch, arg2 );
	else
		send_to_char ( HCONTROL_FORMAT, ch );
}


/* The house command, used by mortal house owners to assign guests */
ACMD ( do_house )
{
	int i;
	int j, id, k;
	char arg[MAX_INPUT_LENGTH];

	one_argument ( argument, arg );

	if ( !ROOM_FLAGGED ( IN_ROOM ( ch ), ROOM_HOUSE ) )
		send_to_char ( "You must be in your house to set guests.\r\n", ch );
	else if ( ( i = find_house ( GET_ROOM_VNUM ( IN_ROOM ( ch ) ) ) ) == NOWHERE )
		send_to_char ( "Um.. this house seems to be screwed up.\r\n", ch );
	else if ( !*arg )
		House_info ( ch, i, FALSE );
	else if ( !str_cmp ( arg, "mount" ) )
		house_load_mount ( ch, i );
	else if ( GET_IDNUM ( ch ) != house_control[i].owner && GET_LEVEL ( ch ) < LVL_IMPL )
		send_to_char ( "Only the primary owner can set guests or expand the house.\r\n", ch );
	else if ( !str_cmp ( arg, "expand" ) )
		house_expand_house ( ch, i );
	else if ( ( id = pi.IdByName ( arg ) ) < 0 )
		send_to_char ( "No such player.\r\n", ch );
	else if ( id == GET_IDNUM ( ch ) )
		send_to_char ( "It's your house!\r\n", ch );
	else
	{
		for ( j = 0; j < house_control[i].num_of_guests; j++ )
			if ( house_control[i].guests[j] == id )
			{
				for ( ; j < house_control[i].num_of_guests; j++ )
					house_control[i].guests[j] =
					    house_control[i].guests[j + 1];
				house_control[i].num_of_guests--;
				House_save_control();
				send_to_char ( "Guest deleted.\r\n", ch );
				return;
			}
		for ( k = 0; k < house_control[i].num_of_guests; k++ )
		{
			if ( ( pi.NameById ( house_control[i].guests[k] ) ) == NULL )
			{
				for ( ; k < house_control[i].num_of_guests; k++ )
					house_control[i].guests[k] =
					    house_control[i].guests[k + 1];
				house_control[i].num_of_guests--;
				House_save_control();
				continue;
			}
		}
		if ( house_control[i].num_of_guests == MAX_GUESTS )
		{
			send_to_char ( "You have too many guests.\r\n", ch );
			return;
		}
		j = house_control[i].num_of_guests++;
		house_control[i].guests[j] = id;
		House_save_control();
		send_to_char ( "Guest added.\r\n", ch );
	}
}



/* Misc. administrative functions */


/* crash-save all the houses */
void House_save_all ( void )
{
	int i;
	room_rnum real_house;

	for ( i = 0; i < num_of_houses; i++ )
		if ( ( real_house = real_room ( house_control[i].vnum ) ) != NULL )
			if ( ROOM_FLAGGED ( real_house, ROOM_HOUSE_CRASH ) )
				House_crashsave ( house_control[i].vnum );

}


/* note: arg passed must be house vnum, so there. */
int House_can_enter ( Character *ch, room_vnum house )
{
	int i, j;

	if ( GET_LEVEL ( ch ) >= LVL_GRGOD || ( i = find_house ( house ) ) == NOWHERE )
		return ( 1 );

	switch ( house_control[i].mode )
	{
		case HOUSE_PRIVATE:
			if ( GET_IDNUM ( ch ) == house_control[i].owner )
				return ( 1 );
			for ( j = 0; j < house_control[i].num_of_guests; j++ )
				if ( GET_IDNUM ( ch ) == house_control[i].guests[j] )
					return ( 1 );
	}

	return ( 0 );
}

void house_expand_house ( Character *ch, int i )
{
	if ( GET_GOLD_TOKEN_COUNT ( ch ) >= 1 )
	{
		GET_GOLD_TOKEN_COUNT ( ch ) -= 1;
		house_control[i].expantions += ( long ) 1;
		ch->Send ( "You expand your house by an extra 200 units for the price of 1 Gold Token.\r\n" );
		House_save_control();
	}
	else
		ch->Send ( "You can't afford to expand the house at this time, it costs 1 Gold Token.\r\n" );
}

void hcontrol_expand_house ( Character *ch, char *argument )
{

	int i = 0, house = 0, amount = 0;
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

	if ( !argument || !*argument )
	{
		ch->Send ( "To expand: \r\nhcontrol expand <house vnum> <num of 200 sized units>\r\n"
		           "To remove expansion:\r\nhcontrol expand <house vnum> <-num of 200 sized units>\r\n" );
		return;
	}
	argument = two_arguments ( argument, arg1, arg2 );

	if ( !*arg1  || !*arg2 || !is_number ( arg1 ) )
	{
		ch->Send ( "To expand: \r\nhcontrol expand <house vnum> <num of 200 sized units>\r\n"
		           "To remove expansions:\r\nhcontrol expand <house vnum> <-num of 200 sized units>\r\n" );
		return;
	}


	if ( !is_number ( arg2 ) && (*arg2 == '-' && !is_number(&(arg2[1]))) )
	{
		ch->Send ( "To expand: \r\nhcontrol expand <house vnum> <num of 200 sized units>\r\n"
		           "To remove expansion:\r\nhcontrol expand <house vnum> <-num of 200 sized units>\r\n" );
		return;

	}

	house = atoi ( arg1 );
	amount = atoi ( arg2 );

	if ( house <= 0 )
	{
		ch->Send ( "Positive numbers please.\r\n" );
		return;
	}

	if ( ( i = find_house ( house ) ) == NOWHERE )
	{
		ch->Send ( "That house doesn't seem to be in the house list.\r\n" );
		return;
	}


	house_control[i].expantions += ( long ) amount;

	ch->Send ( "%d units of 200 %s house %d (owner: %s) new capacity %d\r\n", (amount > 0 ? amount : -amount), (amount > 0 ? "added to" : "removed from"), house, pi.NameById ( house_control[i].owner ), house_capacity ( house ) );

	House_save_control();
}

int house_capacity ( room_vnum house )
{
	int h = find_house ( house );
	if ( h == NOWHERE )
		return 500;
	return 500 + ( 200 * house_control[h].expantions );
}
void hcontrol_set_stable ( Character *ch, char *argument )
{
	room_vnum house = NOWHERE;
	int i = 0;
	int remove = FALSE;
	char arg1[MAX_INPUT_LENGTH], arg2[MAX_INPUT_LENGTH];

	if ( !argument || !*argument )
	{
		ch->Send ( "To add a stable: \r\nhcontrol stable <house vnum> add\r\n"
		           "To remove the stable:\r\nhcontrol mount <house vnum> remove\r\n" );
		return;
	}
	argument = two_arguments ( argument, arg1, arg2 );

	if ( !*arg1  || !*arg2 || !is_number ( arg1 ) )
	{
		ch->Send ( "To add a stable: \r\nhcontrol stable <house vnum> add\r\n"
		           "To remove the stable:\r\nhcontrol stable <house vnum> remove\r\n" );
		return;
	}



	if ( !strcmp ( arg2, "remove" ) )
	{
		remove = TRUE;
	}
	else if ( !strcmp ( arg2, "add" ) )
	{
		remove = FALSE;
	}
	else
	{
		ch->Send ( "To add a stable: \r\nhcontrol stable <house vnum> add\r\n"
		           "To remove the stable:\r\nhcontrol stable <house vnum> remove\r\n" );
		return;
	}

	house = atoi ( arg1 );


	if ( ( i = find_house ( house ) ) == NOWHERE )
	{
		ch->Send ( "That house doesn't seem to be in the house list.\r\n" );
		return;
	}

	if ( remove )
	{
		house_control[i].stable = FALSE;

		ch->Send ( "Stable has been removerized.\r\n" );
		return;
	}
	else
	{
		house_control[i].stable = TRUE;
		ch->Send ( "Stable added to house %d (owner: %s)\r\n", house, pi.NameById ( house_control[i].owner ) );
	}

	House_save_control();
}
void house_load_mount ( Character *ch, int i )
{
	struct follow_type *f, *f_next;
	Character *pet, *next_ch, *tch;
	mob_vnum mount = ch->pet;

	if ( i < 0 )
		return;
	if ( mount <= 0 )
	{
		ch->Send ( "You haven't brought a mount.\r\n" );
		return;
	}
	if ( house_control[i].stable == 0 )
	{
		*ch << "This house doesn't have a stable.\r\n";
		return;
	}

	for ( f = ch->followers; f; f = f_next )
	{
		f_next = f->next;
		tch = f->follower;
		if ( !IS_NPC ( tch ) )
			continue;
		if ( GET_MOB_VNUM ( tch ) == mount )
		{
			if ( HERE ( tch, ch ) )
			{
				ch->Send ( "It is already here and following you!\r\n" );
				return;
			}
			else
			{
				ch->Send (
				    "Mount is already following you, but isn't here. Transfering...\r\n" );
				act ( "$n appears with an implosion of glittery pink sparkles.", FALSE, tch, 0, 0, TO_ROOM );
				move_char_to ( tch, IN_ROOM ( ch ) );
				return;
			}
		}
	}


	for ( tch = character_list;tch;tch = next_ch )
	{
		next_ch = tch->next;
		if ( GET_MOB_VNUM ( tch ) != mount )
			continue;

		ch->Send ( "A mount of yours was found running free!\r\n" );
		act ( "A frenzy of pink sparkles consumes $n.", FALSE, tch, 0, 0, TO_ROOM );
		ch->Send ( "It was destroyed gracefully... allowing you to have a new one now.\r\n" );
		extract_char ( tch );

	}
	pet = read_mobile ( mount );
	if ( pet == NULL )
	{
		ch->Send ( "Your pet has been assigned to your house incorrectly.\r\n" );
		return;
	}
	GET_EXP ( pet ) = 0;
	SET_BIT_AR ( AFF_FLAGS ( pet ), AFF_CHARM );
	char_to_room ( pet, IN_ROOM ( ch ) );
	SET_BIT_AR ( AFF_FLAGS ( pet ), AFF_GROUP );
	SET_BIT_AR ( MOB_FLAGS ( pet ), MOB_MOUNTABLE );

	/* Be certain that pets can't get/carry/use/wield/wear items */
	//IS_CARRYING_W(pet) = 10000;
// IS_CARRYING_N(pet) = 1100;
	GET_GOLD ( pet ) = 0;
	GET_EXP ( pet ) = 0;


	act ( "With a fury of glitter and sparkles $n appears.", FALSE, pet, 0, 0, TO_ROOM );

	add_follower ( pet, ch );
}


void House_list_guests ( Character *ch, int i, int quiet )
{
	int j;
	char *temp;

	if ( house_control[i].num_of_guests == 0 )
	{
		if ( !quiet )
			ch->Send ( "  Guests: None\r\n" );
		return;
	}

	ch->Send ( "  Guests: " );

	/* Avoid <UNDEF>. -gg 6/21/98 */
	for ( j = 0; j < house_control[i].num_of_guests; j++ )
	{
		if ( ( temp = pi.NameById ( house_control[i].guests[j] ) ) == NULL )
		{
			continue;
		}
		ch->Send ( "%s ", temp );
	}

	ch->Send ( "\r\n" );
}

void House_info ( Character *ch, int i, int quiet )
{
	int j;
	char *temp;

	ch->Send ( "{cW#======================================================#{c0\r\n" );
	ch->Send ( "   {cWHouse        :{cy %s\r\n", IN_ROOM ( ch )->name );
	ch->Send ( "   {cWOwner        :{cy %s\r\n", pi.NameById ( house_control[i].owner ) );
	ch->Send ( "   {cWHas a stable :{cy %s\r\n", house_control[i].stable == 0 ? "No" : "Yes" );
	ch->Send ( "   {cWCapacity     :{cy %d used of %d \r\n", house_item_count ( house_control[i].vnum ), house_capacity ( house_control[i].vnum ) );
	if ( house_control[i].num_of_guests == 0 )
	{
		if ( !quiet )
			send_to_char ( "   {cWGuests       : {cyNone\r\n", ch );
		return;
	}

	ch->Send ( "   {cWGuests       :{cy\r\n" );

	/* Avoid <UNDEF>. -gg 6/21/98 */
	for ( j = 0; j < house_control[i].num_of_guests; j++ )
	{
		if ( ( temp = pi.NameById ( house_control[i].guests[j] ) ) == NULL )
		{
			continue;
		}
		ch->Send ( "%s%-15s  %s", ! ( j % 2 ) ? "                  " : "", temp, ( j % 2 ? "\r\n" : " " ) );
	}
	ch->Send ( "\r\n{cW#======================================================#{c0\r\n" );
	ch->Send ( "\r\n" );
}
