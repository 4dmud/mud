/************************************************************************
 * OasisOLC - Objects / oedit.c					v2.0	*
 * Original author: Levork						*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "comm.h"
#include "interpreter.h"
#include "spells.h"
#include "utils.h"
#include "db.h"
#include "boards.h"
#include "constants.h"
#include "shop.h"
#include "genolc.h"
#include "genobj.h"
#include "genzon.h"
#include "oasis.h"
#include "improved-edit.h"
#include "dg_olc.h"
#include "fight.h"
#include "descriptor.h"
#include "strutil.h"
/*------------------------------------------------------------------------*/

/*
 * External variable declarations.
 */

extern struct attack_hit_type attack_hit_text[];
extern struct board_info_type board_info[];
extern const char *attachment_types[];

/*------------------------------------------------------------------------*/
extern zone_rnum real_zone_by_thing ( room_vnum vznum );
void oedit_disp_val5_menu ( Descriptor *d );
/*
 * Handy macros.
 */
#define S_PRODUCT(s, i) ((s)->producing[(i)])

/*------------------------------------------------------------------------*\
  Utility and exported functions
\*------------------------------------------------------------------------*/

ACMD ( do_oasis_oedit )
{
	int num = NOWHERE, save = 0, real_num;
	Descriptor *d;
	//char *buf3;
	char buf1[MAX_STRING_LENGTH];
	char buf2[MAX_STRING_LENGTH];

	/****************************************************************************/
	/** Parse any arguments.                                                   **/
	/****************************************************************************/
	//buf3 = 
	two_arguments ( argument, buf1, buf2 );

	/****************************************************************************/
	/** If there aren't any arguments...well...they can't modify nothing now   **/
	/** can they?                                                              **/
	/****************************************************************************/
	if ( !*buf1 )
	{
		ch->Send ( "Specify an object VNUM to edit.\r\n" );
		return;
	}
	else if ( !isdigit ( *buf1 ) )
	{
		if ( str_cmp ( "save", buf1 ) != 0 )
		{
			ch->Send ( "Yikes!  Stop that, someone will get hurt!\r\n" );
			return;
		}

		save = TRUE;

		if ( is_number ( buf2 ) )
			num = atoi ( buf2 );
		else if ( GET_OLC_ZONE ( ch ) > 0 )
		{
			zone_rnum zlok;

			if ( ( zlok = real_zone ( GET_OLC_ZONE ( ch ) ) ) == NOWHERE )
				num = NOWHERE;
			else
				num = zone_table[zlok].Bot();
		}

		if ( num == NOWHERE )
		{
			ch->Send ( "Save which zone?\r\n" );
			return;
		}
	}

	/****************************************************************************/
	/** If a numeric argument was given, get it.                               **/
	/****************************************************************************/
	if ( num == NOWHERE )
		num = atoi ( buf1 );

	/****************************************************************************/
	/** Check that whatever it is isn't already being edited.                  **/
	/****************************************************************************/
	for ( d = descriptor_list; d; d = d->next )
	{
		if ( STATE ( d ) == CON_OEDIT )
		{
			if ( d->olc && OLC_NUM ( d ) == num )
			{
				ch->Send ( "That object is currently being edited by %s.\r\n",
				           PERS ( d->character, ch ) );
				return;
			}
		}
	}

	/****************************************************************************/
	/** Point d to the builder's descriptor (for easier typing later).         **/
	/****************************************************************************/
	d = ch->desc;

	/****************************************************************************/
	/** Give the descriptor an OLC structure.                                  **/
	/****************************************************************************/
	if ( d->olc )
	{
		new_mudlog ( BRF, LVL_IMMORT, TRUE,
		             "SYSERR: do_oasis: Player already had olc structure." );
		free ( d->olc );
	}

	CREATE ( d->olc, struct oasis_olc_data, 1 );

	/****************************************************************************/
	/** Find the zone.                                                         **/
	/****************************************************************************/
	OLC_ZNUM ( d ) = save ? real_zone ( num ) : real_zone_by_thing ( num );
	if ( OLC_ZNUM ( d ) == NOWHERE )
	{
		ch->Send ( "Sorry, there is no zone for that number!\r\n" );

		/**************************************************************************/
		/** Free the descriptor's OLC structure.                                 **/
		/**************************************************************************/
		free ( d->olc );
		d->olc = NULL;
		return;
	}

	/****************************************************************************/
	/** Everyone but IMPLs can only edit zones they have been assigned.        **/
	/****************************************************************************/
	if ( !can_edit_zone ( ch, OLC_ZNUM ( d ) ) )
	{
		ch->Send ( "You do not have permission to edit this zone.\r\n" );
		new_mudlog ( BRF, LVL_IMPL, TRUE, "OLC: %s tried to edit zone %d allowed zone %d",
		             GET_NAME ( ch ), zone_table[OLC_ZNUM ( d ) ].number, GET_OLC_ZONE ( ch ) );

		/**************************************************************************/
		/** Free the descriptor's OLC structure.                                 **/
		/**************************************************************************/
		free ( d->olc );
		d->olc = NULL;
		return;
	}

	/****************************************************************************/
	/** If we need to save, save the objects.                                  **/
	/****************************************************************************/
	if ( save )
	{
		ch->Send ( "Saving all objects in zone %d.\r\n",
		           zone_table[OLC_ZNUM ( d ) ].number );
		new_mudlog ( CMP, MAX ( LVL_BUILDER, GET_INVIS_LEV ( ch ) ), TRUE,
		             "OLC: %s saves object info for zone %d.", GET_NAME ( ch ),
		             zone_table[OLC_ZNUM ( d ) ].number );

		/**************************************************************************/
		/** Save the objects in this zone.                                       **/
		/**************************************************************************/
		save_objects ( OLC_ZNUM ( d ) );

		/**************************************************************************/
		/** Free the descriptor's OLC structure.                                 **/
		/**************************************************************************/
		free ( d->olc );
		d->olc = NULL;
		return;
	}

	OLC_NUM ( d ) = num;

	/****************************************************************************/
	/** If this is a new object, setup a new object, otherwise setup the       **/
	/** existing object.                                                       **/
	/****************************************************************************/
	if ( ( real_num = real_object ( num ) ) != NOTHING )
		oedit_setup_existing ( d, real_num );
	else
		oedit_setup_new ( d );

	STATE ( d ) = CON_OEDIT;

	/****************************************************************************/
	/** Send the OLC message to the players in the same room as the builder.   **/
	/****************************************************************************/
	act ( "$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM );
	SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_WRITING );

	/****************************************************************************/
	/** Log the OLC message.                                                   **/
	/****************************************************************************/
	new_mudlog ( BRF, LVL_IMMORT, TRUE, "OLC: %s starts editing zone %d allowed zone %d",
	             GET_NAME ( ch ), zone_table[OLC_ZNUM ( d ) ].number, GET_OLC_ZONE ( ch ) );
}

void oedit_setup_new ( Descriptor *d )
{
	CREATE ( OLC_OBJ ( d ), struct obj_data, 1 );

	clear_object ( OLC_OBJ ( d ) );
	OLC_OBJ ( d )->name = strdup ( "unfinished object" );
	OLC_OBJ ( d )->description = strdup ( "An unfinished object is lying here." );
	OLC_OBJ ( d )->short_description = strdup ( "an unfinished object" );
	OLC_OBJ ( d )->smell = strdup ( "It doesn't smell too interesting.\r\n" );
	OLC_OBJ ( d )->taste = strdup ( "It tastes just like it should.\r\n" );
	OLC_OBJ ( d )->feel = strdup ( "It feels normal, as far as you can tell.\r\n" );
	GET_OBJ_TIMER ( OLC_OBJ ( d ) ) = -1;
	SET_BIT_AR ( GET_OBJ_WEAR ( OLC_OBJ ( d ) ), ITEM_WEAR_TAKE );
	OLC_VAL ( d ) = 0;
	OLC_ITEM_TYPE ( d ) = OBJ_TRIGGER;

	SCRIPT ( OLC_OBJ ( d ) ) = NULL;
	OLC_SCRIPT ( d ) = NULL;
	oedit_disp_menu ( d );
}

/*------------------------------------------------------------------------*/

void oedit_setup_existing ( Descriptor *d, int real_num )
{
	struct obj_data *obj;

	/*
	 * Allocate object in memory.
	 */
	CREATE ( obj, struct obj_data, 1 );
	copy_object ( obj, &obj_proto[real_num] );

	/*
	 * Attach new object to player's descriptor.
	 */
	OLC_OBJ ( d ) = obj;
	OLC_VAL ( d ) = 0;
	OLC_ITEM_TYPE ( d ) = OBJ_TRIGGER;
	dg_olc_script_copy ( d );
	/*
	 * The edited obj must not have a script.
	 * It will be assigned to the updated obj later, after editing.
	 */
	SCRIPT ( obj ) = NULL;
	OLC_OBJ ( d )->proto_script = NULL;

	oedit_disp_menu ( d );
}

/*------------------------------------------------------------------------*/

void oedit_save_internally ( Descriptor *d )
{
	int i;
	obj_rnum robj_num;
	Descriptor *dsc;
	struct obj_data *obj;
	obj_list_type tobjs;

	i = ( real_object ( OLC_NUM ( d ) ) == NOTHING );

	if ( ( robj_num = add_object ( OLC_OBJ ( d ), OLC_NUM ( d ) ) ) == NOTHING )
	{
		log ( "oedit_save_internally: add_object failed." );
		return;
	}
	/* Update triggers : */
	/* Free old proto list  */
	if ( obj_proto[robj_num].proto_script &&
	        obj_proto[robj_num].proto_script != OLC_SCRIPT ( d ) )
		free_proto_script ( obj_proto[robj_num].proto_script, OBJ_TRIGGER );

	obj_proto[robj_num].proto_script = OLC_SCRIPT ( d );
	OLC_SCRIPT ( d ) = NULL;

	/* this takes care of the objects currently in-game */
	for ( olt_it ij = object_list.begin(); ij != object_list.end(); ij++ )
	{
		obj = ( ij->second );
		if ( obj->item_number != robj_num )
			continue;
		tobjs[GET_ID ( obj ) ] = obj;
	}
	for ( olt_it ij = tobjs.begin(); ij != tobjs.end(); ij++ )
	{
		obj = ( ij->second );
		/* remove any old scripts */
		if ( SCRIPT ( obj ) )
			extract_script ( obj, OBJ_TRIGGER );
		if ( obj->proto_script != obj_proto[robj_num].proto_script )
			free_proto_script ( obj, OBJ_TRIGGER );
		copy_proto_script ( &obj_proto[robj_num], obj, OBJ_TRIGGER );
		assign_triggers ( obj, OBJ_TRIGGER );
	}
	/* end trigger update */

	if ( !i )	/* If it's not a new object, don't renumber. */
		return;

	/*
	 * Renumber produce in shops being edited.
	 */
	for ( dsc = descriptor_list; dsc; dsc = dsc->next )
		if ( STATE ( dsc ) == CON_SEDIT )
			for ( i = 0; S_PRODUCT ( OLC_SHOP ( dsc ), i ) != NOTHING; i++ )
				if ( S_PRODUCT ( OLC_SHOP ( dsc ), i ) >= robj_num )
					S_PRODUCT ( OLC_SHOP ( dsc ), i ) ++;


	/* Update other people in zedit too. From: C.Raehl 4/27/99 */
	for ( dsc = descriptor_list; dsc; dsc = dsc->next )
		if ( STATE ( dsc ) == CON_ZEDIT )
			for ( i = 0; OLC_ZONE ( dsc )->cmd[i].command != 'S'; i++ )
				switch ( OLC_ZONE ( dsc )->cmd[i].command )
				{
					case 'P':
						OLC_ZONE ( dsc )->cmd[i].arg3 += ( OLC_ZONE ( dsc )->cmd[i].arg3 >= robj_num );
						/* Fall through. */
					case 'E':
					case 'G':
					case 'O':
						OLC_ZONE ( dsc )->cmd[i].arg1 += ( OLC_ZONE ( dsc )->cmd[i].arg1 >= robj_num );
						break;
					case 'R':
						OLC_ZONE ( dsc )->cmd[i].arg2 += ( OLC_ZONE ( dsc )->cmd[i].arg2 >= robj_num );
						break;
					default:
						break;
				}
}

/*------------------------------------------------------------------------*/

void oedit_save_to_disk ( int zone_num )
{
	save_objects ( zone_num );
}

/**************************************************************************
 Menu functions
 **************************************************************************/
void oedit_disp_colours ( Descriptor *d )
{
	int i;
	get_char_colours ( d->character );
	for ( i = 0; i < MAX_COLOUR_OPTIONS; i++ )
	{
		d->Output ( "%s%3d%s) %s%-15s  %s%s", grn, i,nrm, cyn, colour_option_name ( i ), ( ! ( i%4 ) ? "\r\n" : " " ), nrm );
	}

	d->Output ( "Enter Color Number: " );
}

void oedit_disp_dubsing ( Descriptor *d )
{
	d->Output ( "1) Single Bladed\r\n2) Double Bladed\r\nEnter a number: " );
}
/*
 * For container flags.
 */
void oedit_disp_container_flags_menu ( Descriptor *d )
{
	char bits[MAX_STRING_LENGTH];
	get_char_colours ( d->character );
	clear_screen ( d );

	new_sprintbit ( GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ), container_bits, bits, sizeof ( bits ) );
	d->Output (
	    "%s1%s) CLOSEABLE\r\n"
	    "%s2%s) PICKPROOF\r\n"
	    "%s3%s) CLOSED\r\n"
	    "%s4%s) LOCKED\r\n"
	    "Container flags: %s%s%s\r\n"
	    "Enter flag, 0 to quit : ",
	    grn, nrm, grn, nrm, grn, nrm, grn, nrm, cyn, bits, nrm );
}

/*
 * For extra descriptions.
 */
void oedit_disp_extradesc_menu ( Descriptor *d )
{
	struct extra_descr_data *extra_desc = OLC_DESC ( d );

	get_char_colours ( d->character );
	clear_screen ( d );
	d->Output (
	    "Extra desc menu\r\n"
	    "%s1%s) Keyword: %s%s\r\n"
	    "%s2%s) Description:\r\n%s%s\r\n"
	    "%s3%s) Goto next description: %s\r\n"
	    "%s0%s) Quit\r\n"
	    "Enter choice : ",

	    grn, nrm, yel, ( extra_desc->keyword && *extra_desc->keyword ) ? extra_desc->keyword : "<NONE>",
	    grn, nrm, yel, ( extra_desc->description && *extra_desc->description ) ? extra_desc->description : "<NONE>",
	    grn, nrm, !extra_desc->next ? "<Not set>\r\n" : "Set.", grn, nrm );
	OLC_MODE ( d ) = OEDIT_EXTRADESC_MENU;
}

/* Horus - attachments for vehicles */
void oedit_disp_attachment_type(Descriptor *d)
{
  int i;

  d->Output("Attachment Types\r\n");

  for (i = 0; ; i++) {
      if (attachment_types[i][0] == '\n')
          break;
      d->Output(
          "%s%d%s) %s%s\r\n",
          grn, i + 1, nrm, yel, attachment_types[i]);
  }
  d->Output("%s0%s) Quit\r\n", grn, nrm);
}

void oedit_disp_attachment_menu(Descriptor *d)
{
  struct vehicle_attachment_data *attach = OLC_ATTACHMENT(d);
  char buf1[50], buf2[50], buf3[50];

      switch (attach->type) {
          /* Hyperjump allows ships to autopilot to a destination */
          case V_ATT_HYPERJUMP:
              sprintf(buf1, "hyperjump");
              sprintf(buf2, "speed boost"); /* adds speed to the ship */
              sprintf(buf3, "max speed");  /* maximum speed can be increased */
              break;
          case V_ATT_LASER:
              sprintf(buf1, "laser/cannon");
              sprintf(buf2, "firepower");   /* How much damage it can do */
              sprintf(buf3, "firing speed"); /* how quickly can it fire */
              break;
        /* locks into target ship, so lasers can fire even if they run away */
          case V_ATT_MISSILE_LOCK:
              sprintf(buf1, "missile lock");
              sprintf(buf2, "speed");  /* how quickly does it lock onto targ */
              sprintf(buf3, "range"); /* how many rooms away before it drops */
              break;
          default:
              sprintf(buf1, "<NONE>");
              sprintf(buf2, "value");
              sprintf(buf3, "max value");
      }
        
  get_char_colours ( d->character );
  clear_screen ( d );
  d->Output (
      "Attachment menu\r\n"
      "%s1%s) Type: %s%s\r\n"
      "%s2%s) %s: %s%d\r\n"  
      "%s3%s) %s: %s%d\r\n" 
      "%s4%s) Goto next attachment: %s%s\r\n"
      "%s0%s) Quit\r\n"
      "Enter choice : ",
      grn, nrm, yel, buf1, 
      grn, nrm, buf2, yel, attach ? attach->value : 0, 
      grn, nrm, buf3, yel, attach ? attach->max_value : 0, 
      grn, nrm, yel, (attach && attach->next) ? "Set" : "<Not set>",
      grn, nrm);

  OLC_MODE(d) = OEDIT_ATTACHMENT_MENU; 
}


/*
 * Ask for *which* apply to edit.
 */
void oedit_disp_prompt_apply_menu ( Descriptor *d )
{
	char apply_buf[MAX_STRING_LENGTH];
	int counter;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < MAX_OBJ_AFFECT; counter++ )
	{
		if ( OLC_OBJ ( d )->affected[counter].modifier )
		{
			new_sprinttype ( OLC_OBJ ( d )->affected[counter].location, apply_types, apply_buf, sizeof ( apply_buf ) );
			d->Output ( " %s%d%s) %+d to %s\r\n", grn, counter + 1, nrm,
			            OLC_OBJ ( d )->affected[counter].modifier, apply_buf );
		}
		else
		{
			d->Output ( " %s%d%s) None.\r\n", grn, counter + 1, nrm );
		}
	}
	d->Output ( "\r\nEnter affection to modify (0 to quit) : " );
	OLC_MODE ( d ) = OEDIT_PROMPT_APPLY;
}

/*
 * Ask for liquid type.
 */
void oedit_liquid_type ( Descriptor *d )
{
	int counter, columns = 0;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < NUM_LIQ_TYPES; counter++ )
	{
		d->Output ( " %s%2d%s) %s%-20.20s %s", grn, counter, nrm, yel,
		            drinks[counter], ! ( ++columns % 2 ) ? "\r\n" : "" );
	}
	d->Output ( "\r\n%sEnter drink type : ", nrm );
	OLC_MODE ( d ) = OEDIT_VALUE_3;
}

/*
 * The actual apply to set.
 */
void oedit_disp_apply_menu ( Descriptor *d )
{
	int counter, columns = 0;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < NUM_APPLIES; counter++ )
	{
		d->Output ( "%s%2d%s) %-20.20s %s", grn, counter, nrm,
		            apply_types[counter], ! ( ++columns % 2 ) ? "\r\n" : "" );
	}
	d->Output ( "\r\nEnter apply type (0 is no apply) : " );
	OLC_MODE ( d ) = OEDIT_APPLY;
}

/*
 * Weapon type.
 */
void oedit_disp_weapon_menu ( Descriptor *d )
{
	int counter, columns = 0;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < NUM_ATTACK_TYPES; counter++ )
	{
		d->Output ( "%s%2d%s) %-20.20s %s", grn, counter, nrm,
		            attack_hit_text[counter].singular,
		            ! ( ++columns % 2 ) ? "\r\n" : "" );
	}
	d->Output ( "\r\nEnter weapon type : " );
}

/*
 * Spell type.
 */
void oedit_disp_spells_menu ( Descriptor *d )
{
	int counter, columns = 0;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < NUM_SPELLS; counter++ )
	{
		if ( strcmp ( skill_name ( counter ), "!UNUSED!" ) )
			d->Output ( "%s%2d%s) %s%-20.20s %s", grn, counter, nrm, yel,
			            skill_name ( counter ), ! ( ++columns % 3 ) ? "\r\n" : "" );
	}
	d->Output ( "\r\n%sEnter spell choice (-1 for none) : ", nrm );
}

/*
 * Object value #1
 */
void oedit_disp_val1_menu ( Descriptor *d )
{
	OLC_MODE ( d ) = OEDIT_VALUE_1;
	switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
	{
		case ITEM_LIGHT:
			/*
			 * values 0 and 1 are unused.. jump to 2
			 */
			oedit_disp_val3_menu ( d );
			break;
		case ITEM_BOW:
		case ITEM_CROSSBOW:
		case ITEM_SLING:
		case ITEM_GUN:
			d->Output ( "Range (1 - 3) : " );
			break;
		case ITEM_ROCK:
		case ITEM_BOLT:
		case ITEM_ARROW:
		case ITEM_THROW:
			oedit_disp_val2_menu ( d );
			break;
		case ITEM_AMMO:
			d->Output ( "Number of bullets : " );
			break;
		case ITEM_SCROLL:
		case ITEM_WAND:
		case ITEM_STAFF:
		case ITEM_POTION:
		case ITEM_ANTIDOTE_1:
		case ITEM_ANTIDOTE_2:
		case ITEM_ANTIDOTE_3:
			d->Output ( "Spell level : " );
			break;
		case ITEM_WEAPON:
			/*
			 * This doesn't seem to be used if I remembe right.
			 */
			oedit_disp_val2_menu ( d );
			break;
		case ITEM_ARMOR:
			d->Output ( "Apply to AC : " );
			break;
		case ITEM_CONTAINER:
			d->Output ( "Max weight to contain : " );
			break;
		case ITEM_DRINKCON:
		case ITEM_FOUNTAIN:
			d->Output ( "Max drink units : " );
			break;
		case ITEM_FOOD:
			d->Output ( "Hours to fill stomach : " );
			break;
		case ITEM_MONEY:
			d->Output ( "Number of gold coins : " );
			break;
		case ITEM_NOTE:
			/*
			 * This is supposed to be language, but it's unused.
			 */
			break;
		case ITEM_GRENADE:
			d->Output ( "Ticks to countdown to explosion : " );
			break;
		case ITEM_VEHICLE:
			d->Output ( "Vnum of vehicle room : " );
			break;
		case ITEM_VEHICLE2:
			d->Output ( "Vnum of vehicle room (enter -1 for a default room which desc will equal the extra desc) : " );
			break;
/*
                case ITEM_VEHICLE2:
                        d->Output("Speed (1 - fastest, 20 - slowest) : ");
                        break;
*/
		case ITEM_V_WINDOW:
		case ITEM_V_CONTROLS:
		case ITEM_V_HATCH:
			d->Output ( "Obj number of vehicle : " );
			break;
		case ITEM_CLIMBABLE:
		case ITEM_DESCENDABLE:
		case ITEM_PORTAL:
		case ITEM_PORTAL_BUSH:
		case ITEM_PORTAL_WATER:
		case ITEM_PORTAL_HOLE:
		case ITEM_PORTAL_HURDLE:
			GET_OBJ_VAL ( OLC_OBJ ( d ), 0 ) = -1;
			oedit_disp_val2_menu ( d );
			break;
		case ITEM_FURNITURE:
			d->Output ( "Number of people that it can hold : " );
			break;
		case ITEM_LIGHTSABRE_HILT:
			oedit_disp_dubsing ( d );
			break;
		case ITEM_SPACEBIKE:
			oedit_disp_val3_menu ( d );
			break;
		case ITEM_GEM_CLUSTER:
			d->Output ( "Amount of fuel: " );
			break;
		case ITEM_TREE:
			oedit_disp_val5_menu ( d );
			break;
		case ITEM_FOCUS_MINOR:
		case ITEM_FOCUS_MAJOR:
			d->Output ( "Multi (orb) = value / 700, it will be between 1.05 and 1.50\r\n" 
                                    "Multi (orbstaff) = value / 1000, no limits for this multi.\r\n"
				    "The multi is doubled if it's a major focus.\r\n"
				    "If an extra flag like air_focus is set, then the elemental\r\n"
				    "bonus will be Cost/Day / 100.\r\n\r\n"	
				    "If you're making a staff, you can enter any value.\r\n"	
                                    "Value:" );
			break;
		default:
			oedit_disp_menu ( d );
	}
}

/*
 * Object value #2
 */
void oedit_disp_val2_menu ( Descriptor *d )
{
	OLC_MODE ( d ) = OEDIT_VALUE_2;
	switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
	{
		case ITEM_SCROLL:
		case ITEM_POTION:
		case ITEM_ANTIDOTE_1:
		case ITEM_ANTIDOTE_2:
		case ITEM_ANTIDOTE_3:
			oedit_disp_spells_menu ( d );
			break;
		case ITEM_WAND:
		case ITEM_STAFF:
			d->Output ( "Max number of charges : " );
			break;
		case ITEM_GUN:
			d->Output ( "Max number of bullets : " );
			break;
		case ITEM_ROCK:
		case ITEM_BOLT:
		case ITEM_ARROW:
		case ITEM_AMMO:
		case ITEM_THROW:
		case ITEM_GRENADE:
		case ITEM_WEAPON:
		case ITEM_LIGHTSABRE_HILT:
			d->Output ( "Number of damage dice : " );
			break;
		case ITEM_FOOD:
			/*
			 * Values 2 and 3 are unused, jump to 4...Odd.
			 */
			oedit_disp_val4_menu ( d );
			break;
		case ITEM_CONTAINER:
			/*
			 * These are flags, needs a bit of special handling.
			 */
			oedit_disp_container_flags_menu ( d );
			break;
		case ITEM_DRINKCON:
		case ITEM_FOUNTAIN:
			d->Output ( "Initial drink units : " );
			break;
		case ITEM_VEHICLE:
		case ITEM_VEHICLE2:
			d->Output ( "Can it fly? 0 = no, 1 = yes : " );
			break;
#if 0
                case ITEM_VEHICLE2:
                        /* Reserved for enter into room vnum */
                        oedit_disp_val3_menu(d);
                        break;
#endif
		case ITEM_CLIMBABLE:
		case ITEM_DESCENDABLE:
		case ITEM_PORTAL:
		case ITEM_PORTAL_BUSH:
		case ITEM_PORTAL_WATER:
		case ITEM_PORTAL_HOLE:
		case ITEM_PORTAL_HURDLE:
			d->Output ( "Vnum of room : " );
			break;
		case ITEM_FOCUS_MINOR:
		case ITEM_FOCUS_MAJOR:
			oedit_disp_val3_menu ( d );
			break;
		default:
			oedit_disp_menu ( d );
	}
}

/*
 * Object value #3
 */
void oedit_disp_val3_menu ( Descriptor *d )
{
	OLC_MODE ( d ) = OEDIT_VALUE_3;
	switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
	{
		case ITEM_LIGHT:
			d->Output ( "Number of hours (0 = burnt, -1 is infinite) : " );
			break;
		case ITEM_SCROLL:
		case ITEM_POTION:
			oedit_disp_spells_menu ( d );
			break;
		case ITEM_WAND:
		case ITEM_STAFF:
			d->Output ( "Number of charges remaining : " );
			break;
		case ITEM_GUN:
			d->Output ( "Number of bullets : " );
			break;
		case ITEM_ROCK:
		case ITEM_BOLT:
		case ITEM_ARROW:
		case ITEM_AMMO:
		case ITEM_THROW:
		case ITEM_GRENADE:
		case ITEM_WEAPON:
		case ITEM_LIGHTSABRE_HILT:
			d->Output ( "Size of damage dice : " );
			break;
		case ITEM_CONTAINER:
			d->Output ( "Vnum of key to open container (-1 for no key) : " );
			break;
		case ITEM_DRINKCON:
		case ITEM_FOUNTAIN:
			oedit_liquid_type ( d );
			break;
		case ITEM_VEHICLE:
		case ITEM_VEHICLE2:
			d->Output ( "How many hits to destroy vehicle (1-5): " );
			break;
/*
                case ITEM_VEHICLE2:
                        d->Output("Shield (1 - weakest, 100 - toughest) : ");
                        break;
*/
		case ITEM_CLIMBABLE:
		case ITEM_DESCENDABLE:
		case ITEM_PORTAL:
		case ITEM_PORTAL_BUSH:
		case ITEM_PORTAL_WATER:
		case ITEM_PORTAL_HOLE:
		case ITEM_PORTAL_HURDLE:
			d->Output ( "Timer, -1 for infinite : " );
			break;
		case ITEM_SPACEBIKE:
			d->Output ( "It costs 1 fuel per room in space to move.\r\nCurrent Fuel:" );
			break;
		case ITEM_FOCUS_MINOR:
		case ITEM_FOCUS_MAJOR:
			d->Output ( "Kind of focus ( 1 = staff, 2 = orb, 3 = orbstaff ):" );
			break;
		default:
			oedit_disp_menu ( d );
	}
}

/*
 * Object value #4
 */
void oedit_disp_val4_menu ( Descriptor *d )
{
	OLC_MODE ( d ) = OEDIT_VALUE_4;
	switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
	{
		case ITEM_SCROLL:
		case ITEM_POTION:
		case ITEM_WAND:
		case ITEM_STAFF:
			oedit_disp_spells_menu ( d );
			break;
		case ITEM_GUN:
			d->Output ( "Vnum of bullet : " );
			break;
		case ITEM_WEAPON:
			oedit_disp_weapon_menu ( d );
			break;
		case ITEM_LIGHTSABRE_HILT:
			oedit_disp_colours ( d );
			break;
		case ITEM_DRINKCON:
		case ITEM_FOUNTAIN:
		case ITEM_FOOD:
			d->Output ( "Poisoned (0 = not poison) : " );
			break;
/*
                case ITEM_VEHICLE2:
                        d->Output("Vehicle HP (1-100) : ");
                        break;
*/
		case ITEM_SPACEBIKE:
			d->Output ( "It costs 1 fuel per room in space to move.\r\nMax Fuel:" );
			break;
		case ITEM_FOCUS_MINOR:
		case ITEM_FOCUS_MAJOR:
			d->Output ( "Multi (staff) = value / 10000 + num_casting / 100 + 6 * level / 1000\r\n"
				    "where num_casting is the number of casting classes + 1.\r\n"
				    "The multi will be between 1.55 and 2.15, and is doubled if it's a\r\n"
				    "major focus. If an extra flag like air_focus is set, then the\r\n"
				    "elemental bonus will be Cost/Day / 100.\r\n\r\n"
				    "If you're not making a staff, you can enter any value.\r\n"
				    "Value:" );
			break;
		default:
			oedit_disp_menu ( d );
	}
}

/*
 * Object value #5
 */
void oedit_disp_val5_menu ( Descriptor *d )
{
	OLC_MODE ( d ) = OEDIT_VALUE_5;
	switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
	{
		case ITEM_TREE:
			d->Output ( "Give the VNUM of the log that this tree loads (or -1 for a default log): " );
			break;
/*
                case ITEM_VEHICLE2:
                        d->Output("It costs 1 fuel per room in space to move.\r\nMax Fuel: " );
                        break;
*/
	        case ITEM_WEAPON:
		        d->Output("Weapon length: ");
			break;
		default:
			oedit_disp_menu ( d );
	}
}
/*
 * Object value #6
 */
void oedit_disp_val6_menu ( Descriptor *d )
{
	OLC_MODE ( d ) = OEDIT_VALUE_6;
	switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
	{
		case ITEM_TREE:
			d->Output ( "Give the number of logs this tree could produce : " );
			break;
		default:
			oedit_disp_menu ( d );
	}
}

void oedit_disp_material_menu ( Descriptor *d )
{
	int mat, col = 0;
	OLC_MODE ( d ) = OEDIT_VALUE_10;
	get_char_colours ( d->character );
	// clear_screen(d);
	for ( mat = 1; mat < NUM_MATERIAL_TYPES; mat++ )
	{
		d->Output ( "%s%2d%s) %-20.20s %s", grn, mat, nrm,
		            material_name ( mat ), ! ( ++col % 3 ) ? "\r\n" : "" );
	}
	d->Output ( "\r\nEnter Material type : " );
}

/*
 * Object type.
 */
void oedit_disp_type_menu ( Descriptor *d )
{
	int counter, columns = 0;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < NUM_ITEM_TYPES; counter++ )
	{
		d->Output ( "%s%2d%s) %-20.20s %s", grn, counter, nrm,
		            item_types[counter], ! ( ++columns % 2 ) ? "\r\n" : "" );
	}
	d->Output ( "\r\nEnter object type : " );
}

/*
 * Object extra flags.
 */
void oedit_disp_extra_menu ( Descriptor *d )
{
	char bits[MAX_STRING_LENGTH];
	int counter, columns = 0;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < NUM_ITEM_FLAGS; counter++ )
	{
		d->Output ( "%s%2d%s) %-20.20s %s", grn, counter + 1, nrm,
		            extra_bits[counter], ! ( ++columns % 2 ) ? "\r\n" : "" );
	}
	sprintbitarray ( GET_OBJ_EXTRA ( OLC_OBJ ( d ) ), extra_bits, EF_ARRAY_MAX, bits, sizeof ( bits ) );
	d->Output ( "\r\nObject flags: %s%s%s\r\n"
	            "Enter object extra flag (0 to quit) : ",
	            cyn, bits, nrm );
}

/*
 * Object perm flags.
 */
void oedit_disp_perm_menu ( Descriptor *d )
{
	char bits[MAX_STRING_LENGTH];
	int counter, columns = 0;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < NUM_AFF_FLAGS; counter++ )
	{
		d->Output ( "%s%2d%s) %-20.20s %s", grn, counter + 1, nrm, affected_bits[counter], ! ( ++columns % 2 ) ? "\r\n" : "" );
	}
	sprintbitarray ( GET_OBJ_PERM ( OLC_OBJ ( d ) ), affected_bits, AF_ARRAY_MAX, bits, sizeof ( bits ) );
	d->Output ( "\r\nObject permanent flags: %s%s%s\r\n"
	            "Enter object perm flag (0 to quit) : ", cyn, bits, nrm );
}

/*
 * Object wear flags.
 */
void oedit_disp_wear_menu ( Descriptor *d )
{
	char bits[MAX_STRING_LENGTH];
	int counter, columns = 0;

	get_char_colours ( d->character );
	clear_screen ( d );

	for ( counter = 0; counter < NUM_ITEM_WEARS; counter++ )
	{
		d->Output ( "%s%2d%s) %-20.20s %s", grn, counter + 1, nrm,
		            wear_bits[counter], ! ( ++columns % 2 ) ? "\r\n" : "" );
	}
	sprintbitarray ( GET_OBJ_WEAR ( OLC_OBJ ( d ) ), wear_bits, TW_ARRAY_MAX, bits, sizeof ( bits ) );
	d->Output ( "\r\nWear flags: %s%s%s\r\n"
	            "Enter wear flag, 0 to quit : ", cyn, bits, nrm );
}

/*
 * Display main menu.
 */
void oedit_disp_menu ( Descriptor *d )
{
	char buf1[MAX_STRING_LENGTH];
	char buf2[MAX_STRING_LENGTH];
        char buf3[MAX_STRING_LENGTH];
	struct obj_data *obj;

	obj = OLC_OBJ ( d );
	get_char_colours ( d->character );
	clear_screen ( d );

	/*
	 * Build buffers for first part of menu.
	 */
	sprinttype ( GET_OBJ_TYPE ( obj ), item_types, buf1, sizeof ( buf1 ) );
	sprintbitarray ( GET_OBJ_EXTRA ( obj ), extra_bits, EF_ARRAY_MAX, buf2, sizeof ( buf2 ) );

	/*
	 * Build first half of menu.
	 */
/*
        if (GET_OBJ_TYPE(obj) == ITEM_VEHICLE2)
            sprintf(buf3, 
                  "%sK%s) Attachments : %s%s\r\n", 
                  grn, nrm, yel, obj->attachment ? "Set" : "<Not set>");
        else
*/
            sprintf(buf3, "  ");

	d->Output (
	    "-- Item number : [%s%d%s]\r\n"
	    "%s1%s) Namelist : %s%s\r\n"
	    "%s2%s) S-Desc   : %s%s\r\n"
	    "%s3%s) L-Desc   :-\r\n%s%s\r\n"
	    "%s4%s) A-Desc   :-\r\n%s%s"
	    "%s5%s) Type        : %s%s\r\n"
	    "%s6%s) Extra flags : %s%s\r\n",

	    cyn, OLC_NUM ( d ), nrm,
	    grn, nrm, yel, ( obj->name && *obj->name ) ? obj->name : "undefined",
	    grn, nrm, yel, ( obj->short_description && *obj->short_description ) ? obj->short_description : "undefined",
	    grn, nrm, yel, ( obj->description && *obj->description ) ? obj->description : "undefined",
	    grn, nrm, yel, ( obj->action_description && *obj->action_description ) ? obj->action_description : "<not set>\r\n",
	    grn, nrm, cyn, buf1,
	    grn, nrm, cyn, buf2
	);
	/*
	 * Send first half.
	 */

	/*
	 * Build second half of menu.
	 */
	sprintbitarray ( GET_OBJ_WEAR ( obj ), wear_bits, TW_ARRAY_MAX, buf1, sizeof ( buf1 ) );
	// sprintbitarray(GET_OBJ_PERM(obj), affected_bits, AF_ARRAY_MAX, buf2);

	d->Output (
	    "%s7%s) Wear flags  : %s%s\r\n"
	    "%s8%s) Weight      : %s%d\r\n"
	    "%s9%s) Cost        : %s%lld\r\n"
	    "%sA%s) Cost/Day    : %s%d\r\n"
	    "%sB%s) Timer       : %s%d\r\n"
	    "%sC%s) Values      : %s%d %d %d %d %d %d\r\n"
	    "%sD%s) Menu --->   : %sApplies\r\n"
	    "%sE%s) Menu --->   : %sExtra Desc\r\n"
	    "%sF%s) Innate      : %s(%d) %s%s\r\n"
	    "%sG%s) Smell Desc  :\r\n%s%s\r\n"
	    "%sH%s) Taste Desc  :\r\n%s%s\r\n"
	    "%sI%s) Feel Desc   :\r\n%s%s\r\n"
	    "%sJ%s) Material    : %s%s\r\n"
            "%s"
	    "%sM%s) Min Level   : %s%d\r\n"
	    "%sS%s) Script      : %s%s\r\n"
	    "%sQ%s) Quit\r\n"
	    "Enter choice : ",

	    grn, nrm, cyn, buf1,
	    grn, nrm, cyn, GET_OBJ_WEIGHT ( obj ),
	    grn, nrm, cyn, GET_OBJ_COST ( obj ),
	    grn, nrm, cyn, GET_OBJ_RENT ( obj ),
	    grn, nrm, cyn, GET_OBJ_TIMER ( obj ),
	    grn, nrm, cyn,
	    GET_OBJ_VAL ( obj, 0 ),
	    GET_OBJ_VAL ( obj, 1 ),
	    GET_OBJ_VAL ( obj, 2 ),
	    GET_OBJ_VAL ( obj, 3 ),
	    GET_OBJ_VAL ( obj, 4 ),
	    GET_OBJ_VAL ( obj, 5 ),
	    grn, nrm, cyn,
	    grn, nrm, cyn,
	    grn, nrm, cyn, GET_OBJ_INNATE ( obj ), skill_name ( GET_OBJ_INNATE ( obj ) ), nrm,
	    grn, nrm, yel, ( obj->smell && *obj->smell ) ? obj->smell : "<not set>\r\n",
	    grn, nrm, yel, ( obj->taste && *obj->taste ) ? obj->taste : "<not set>\r\n",
	    grn, nrm, yel, ( obj->feel  && *obj->feel ) ? obj->feel  : "<not set>\r\n",
	    grn, nrm, cyn, material_name ( GET_OBJ_VAL ( obj, 9 ) ),
            buf3, 
	    grn, nrm, cyn, GET_OBJ_LEVEL ( obj ),
	    grn, nrm, cyn, OLC_SCRIPT ( d ) ? "Set." : "Not Set.",
	    grn, nrm
	);
	OLC_MODE ( d ) = OEDIT_MAIN_MENU;
}

/***************************************************************************
 main loop (of sorts).. basically interpreter throws all input to here
 ***************************************************************************/

void oedit_parse ( Descriptor *d, char *arg )
{
	int num, max_val, min_val;
	char *oldtext = NULL;

	switch ( OLC_MODE ( d ) )
	{

		case OEDIT_CONFIRM_SAVESTRING:
			switch ( *arg )
			{
				case 'y':
				case 'Y':
					oedit_save_internally ( d );
					new_mudlog ( CMP, MAX ( LVL_BUILDER, GET_INVIS_LEV ( d->character ) ), TRUE,
					             "OLC: %s edits obj %d", GET_NAME ( d->character ), OLC_NUM ( d ) );
					if ( CONFIG_OLC_SAVE )
					{
						oedit_save_to_disk ( real_zone_by_thing ( OLC_NUM ( d ) ) );
						d->Output ( "Object saved to disk.\r\n" );
					}
					else
						d->Output ( "Object saved to memory.\r\n" );
					/* Fall through. */
				case 'n':
				case 'N':
					cleanup_olc ( d, CLEANUP_ALL );
					return;
				case 'a': /* abort quit */
				case 'A':
					oedit_disp_menu ( d );
					return;
				default:
					d->Output ( "Invalid choice!\r\n" );
					d->Output ( "Do you wish to save this object?(yes/no/abort)\r\n" );
					return;
			}

		case OEDIT_MAIN_MENU:
			/*
			 * Throw us out to whichever edit mode based on user input.
			 */
			switch ( *arg )
			{
				case 'q':
				case 'Q':
					if ( OLC_VAL ( d ) )  	/* Something has been modified. */
					{
						d->Output ( "Do you wish to save this object?(yes/no/abort)\r\n" );
						OLC_MODE ( d ) = OEDIT_CONFIRM_SAVESTRING;
					}
					else
						cleanup_olc ( d, CLEANUP_ALL );
					return;
				case '1':
					d->Output ( "Enter namelist : " );
					OLC_MODE ( d ) = OEDIT_EDIT_NAMELIST;
					break;
				case '2':
					d->Output ( "Enter short desc : " );
					OLC_MODE ( d ) = OEDIT_SHORTDESC;
					break;
				case '3':
					d->Output ( "Enter long desc :-\r\n| " );
					OLC_MODE ( d ) = OEDIT_LONGDESC;
					break;
				case '4':
					OLC_MODE ( d ) = OEDIT_ACTDESC;
					send_editor_help ( d );
					d->Output ( "Enter action description:\r\n\r\n" );
					if ( OLC_OBJ ( d )->action_description )
					{
						d->Output ( "%s", OLC_OBJ ( d )->action_description );
						oldtext = strdup ( OLC_OBJ ( d )->action_description );
					}
					string_write ( d, &OLC_OBJ ( d )->action_description, MAX_MESSAGE_LENGTH, 0, oldtext );
					OLC_VAL ( d ) = 1;
					break;
				case '5':
					oedit_disp_type_menu ( d );
					OLC_MODE ( d ) = OEDIT_TYPE;
					break;
				case '6':
					oedit_disp_extra_menu ( d );
					OLC_MODE ( d ) = OEDIT_EXTRAS;
					break;
				case '7':
					oedit_disp_wear_menu ( d );
					OLC_MODE ( d ) = OEDIT_WEAR;
					break;
				case '8':
					d->Output ( "Enter weight : " );
					OLC_MODE ( d ) = OEDIT_WEIGHT;
					break;
				case '9':
					d->Output ( "Enter cost : " );
					OLC_MODE ( d ) = OEDIT_COST;
					break;
				case 'a':
				case 'A':
					d->Output ( "Enter cost per day : " );
					OLC_MODE ( d ) = OEDIT_COSTPERDAY;
					break;
				case 'b':
				case 'B':
					d->Output ( "Enter timer : " );
					OLC_MODE ( d ) = OEDIT_TIMER;
					break;
				case 'c':
				case 'C':
					/*
					 * Clear any old values
					 */
					GET_OBJ_VAL ( OLC_OBJ ( d ), 0 ) = 0;
					GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ) = 0;
					GET_OBJ_VAL ( OLC_OBJ ( d ), 2 ) = 0;
					GET_OBJ_VAL ( OLC_OBJ ( d ), 3 ) = 0;
					GET_OBJ_VAL ( OLC_OBJ ( d ), 4 ) = 0;
					GET_OBJ_VAL ( OLC_OBJ ( d ), 5 ) = 0;
					OLC_VAL ( d ) = 1;
					oedit_disp_val1_menu ( d );
					break;
				case 'd':
				case 'D':
					oedit_disp_prompt_apply_menu ( d );
					break;
				case 'e':
				case 'E':
					/*
					 * If extra descriptions don't exist.
					 */
					if ( OLC_OBJ ( d )->ex_description == NULL )
					{
						CREATE ( OLC_OBJ ( d )->ex_description, struct extra_descr_data, 1 );
						OLC_OBJ ( d )->ex_description->next = NULL;
					}
					OLC_DESC ( d ) = OLC_OBJ ( d )->ex_description;
					oedit_disp_extradesc_menu ( d );
					break;
				case 'f':
				case 'F':
					/* Object innate spell */
					if ( GET_LEVEL ( d->character ) == LVL_IMPL )
					{
						oedit_disp_spells_menu ( d );
						d->Output ( "Enter spell # : " );
						OLC_MODE ( d ) = OEDIT_INNATE;
					}
					else
						d->Output ( "Only Implementors can use this." );
					break;
				case 'g':
				case 'G':
					OLC_MODE ( d ) = OEDIT_SMELL_DESCRIPTION;
					send_editor_help ( d );
					d->Output ( "Enter smell description:\r\n\r\n" );
					if ( OLC_OBJ ( d )->smell )
					{
						d->Output ( "%s", OLC_OBJ ( d )->smell );
						oldtext = strdup ( OLC_OBJ ( d )->smell );
					}
					string_write ( d, &OLC_OBJ ( d )->smell, MAX_MESSAGE_LENGTH, 0, oldtext );
					OLC_VAL ( d ) = 1;

					break;
				case 'h':
				case 'H':
					OLC_MODE ( d ) = OEDIT_TASTE_DESCRIPTION;
					send_editor_help ( d );
					d->Output ( "Enter taste description:\r\n\r\n" );
					if ( OLC_OBJ ( d )->taste )
					{
						d->Output ( "%s", OLC_OBJ ( d )->taste );
						oldtext = strdup ( OLC_OBJ ( d )->taste );
					}
					string_write ( d, &OLC_OBJ ( d )->taste, MAX_MESSAGE_LENGTH, 0, oldtext );
					OLC_VAL ( d ) = 1;
					break;
				case 'i':
				case 'I':
					OLC_MODE ( d ) = OEDIT_FEEL_DESCRIPTION;
					send_editor_help ( d );
					d->Output ( "Enter feel description:\r\n\r\n" );
					if ( OLC_OBJ ( d )->feel )
					{
						d->Output ( "%s", OLC_OBJ ( d )->feel );
						oldtext = strdup ( OLC_OBJ ( d )->feel );
					}
					string_write ( d, &OLC_OBJ ( d )->feel, MAX_MESSAGE_LENGTH, 0, oldtext );
					OLC_VAL ( d ) = 1;
					break;
				case 'j':
				case 'J':
					oedit_disp_material_menu ( d );
					OLC_MODE ( d ) = OEDIT_MATERIAL;
					break;
/*
                                case 'k':
                                case 'K':
                                        if (GET_OBJ_TYPE(OLC_OBJ(d)) == ITEM_VEHICLE2)
                                        {
                                            if (OLC_OBJ(d)->attachment == NULL)
                                            {
						CREATE ( OLC_OBJ(d)->attachment, struct vehicle_attachment_data, 1 );
                                                OLC_OBJ(d)->attachment->next = NULL;
                                            }
                                            OLC_ATTACHMENT(d) = OLC_OBJ(d)->attachment;
                                            oedit_disp_attachment_menu(d);
                                        }
                                        break;
*/
				case 'm':
				case 'M':
					d->Output ( "Enter new minimum level: " );
					OLC_MODE ( d ) = OEDIT_LEVEL;
					break;
					/*case 'p':
					case 'P':
					  oedit_disp_perm_menu(d);
					  OLC_MODE(d) = OEDIT_PERM;
					  break;*/
				case 's':
				case 'S':
					OLC_SCRIPT_EDIT_MODE ( d ) = SCRIPT_MAIN_MENU;
					dg_script_menu ( d );
					return;
				default:
					oedit_disp_menu ( d );
					break;
			}
			return;			/*
                                				 * end of OEDIT_MAIN_MENU
                                				 */
		case OLC_SCRIPT_EDIT:
			if ( dg_script_edit_parse ( d, arg ) )
				return;
			break;
		case OEDIT_EDIT_NAMELIST:
			if ( !genolc_checkstring ( d, arg ) )
				break;
			if ( OLC_OBJ ( d )->name )
				free ( OLC_OBJ ( d )->name );
			OLC_OBJ ( d )->name = ChToLower ( str_udup ( arg ) ); /** object names should always be in lower case **/

			break;

		case OEDIT_SHORTDESC:
			if ( !genolc_checkstring ( d, arg ) )
				break;
			if ( OLC_OBJ ( d )->short_description )
				free ( OLC_OBJ ( d )->short_description );
			OLC_OBJ ( d )->short_description = str_udup ( arg );
			break;

		case OEDIT_LONGDESC:
			if ( !genolc_checkstring ( d, arg ) )
				break;
			if ( OLC_OBJ ( d )->description )
				free ( OLC_OBJ ( d )->description );
			OLC_OBJ ( d )->description = str_udup ( arg );
			break;

		case OEDIT_TYPE:
			num = atoi ( arg );
			if ( ( num < 1 ) || ( num >= NUM_ITEM_TYPES ) )
			{
				d->Output ( "Invalid choice, try again : " );
				return;
			}
			else
				GET_OBJ_TYPE ( OLC_OBJ ( d ) ) = num;
			/* what's the boundschecking worth if we don't do this ? -- Welcor */
			GET_OBJ_VAL ( OLC_OBJ ( d ), 0 ) = GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ) =
			                                       GET_OBJ_VAL ( OLC_OBJ ( d ), 2 ) = GET_OBJ_VAL ( OLC_OBJ ( d ), 3 ) =
			                                                                              GET_OBJ_VAL ( OLC_OBJ ( d ), 4 ) =GET_OBJ_VAL ( OLC_OBJ ( d ), 5 ) = 0;
			break;

		case OEDIT_EXTRAS:
			num = atoi ( arg );
			if ( ( num < 0 ) || ( num > NUM_ITEM_FLAGS ) )
			{
				oedit_disp_extra_menu ( d );
				return;
			}
			else if ( num == 0 )
				break;
			else
			{
				TOGGLE_BIT_AR ( GET_OBJ_EXTRA ( OLC_OBJ ( d ) ), ( num - 1 ) );
				oedit_disp_extra_menu ( d );
				return;
			}

		case OEDIT_WEAR:
			num = atoi ( arg );
			if ( ( num < 0 ) || ( num > NUM_ITEM_WEARS ) )
			{
				d->Output ( "That's not a valid choice!\r\n" );
				oedit_disp_wear_menu ( d );
				return;
			}
			else if ( num == 0 )	/* Quit. */
				break;
			else
			{
				TOGGLE_BIT_AR ( GET_OBJ_WEAR ( OLC_OBJ ( d ) ), ( num - 1 ) );
				oedit_disp_wear_menu ( d );
				return;
			}

		case OEDIT_WEIGHT:
			GET_OBJ_WEIGHT ( OLC_OBJ ( d ) ) = LIMIT ( atoi ( arg ), 0, MAX_OBJ_WEIGHT );
			break;

		case OEDIT_COST:
			GET_OBJ_COST ( OLC_OBJ ( d ) ) = LIMIT ( atoi ( arg ), 0, MAX_OBJ_COST );
			break;

		case OEDIT_COSTPERDAY:
			GET_OBJ_RENT ( OLC_OBJ ( d ) ) = LIMIT ( atoi ( arg ), 0, MAX_OBJ_RENT );
			break;

		case OEDIT_TIMER:
			GET_OBJ_TIMER ( OLC_OBJ ( d ) ) = LIMIT ( atoi ( arg ), -1, MAX_OBJ_TIMER );
			break;

		case OEDIT_MATERIAL:
			GET_OBJ_MATERIAL ( OLC_OBJ ( d ) ) = LIMIT ( atoi ( arg ), 0, NUM_MATERIAL_TYPES );
			break;

		case OEDIT_INNATE:
			GET_OBJ_INNATE ( OLC_OBJ ( d ) ) = LIMIT ( atoi ( arg ), 0, NUM_SPELLS );
			break;

		case OEDIT_LEVEL:
			GET_OBJ_LEVEL ( OLC_OBJ ( d ) ) = LIMIT ( atoi ( arg ), 0, LVL_IMPL );
			break;

		case OEDIT_PERM:
			if ( ( num = atoi ( arg ) ) == 0 )
				break;
			if ( num > 0 && num <= NUM_AFF_FLAGS )
				TOGGLE_BIT_AR ( GET_OBJ_PERM ( OLC_OBJ ( d ) ), ( num - 1 ) );
			oedit_disp_perm_menu ( d );
			return;

		case OEDIT_VALUE_1:
			/*
			 * Lucky, I don't need to check any of these for out of range values.
			 * Hmm, I'm not so sure - Rv
			 */
			switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
			{
				case ITEM_FURNITURE:
					GET_OBJ_VAL ( OLC_OBJ ( d ), 0 ) = LIMIT ( atoi ( arg ), 0, MAX_PEOPLE_IN_CHAIR );
					oedit_disp_val2_menu ( d );
					break;
				case ITEM_WEAPON:
					GET_OBJ_VAL ( OLC_OBJ ( d ), 0 ) = MIN ( MAX ( atoi ( arg ), -50 ), 50 );
					break;
				case ITEM_CONTAINER:
					GET_OBJ_VAL ( OLC_OBJ ( d ), 0 ) = LIMIT ( atoi ( arg ), 0, MAX_CONTAINER_SIZE );
					break;
				default:
					GET_OBJ_VAL ( OLC_OBJ ( d ), 0 ) = atoi ( arg );
			}
			/*
			 * proceed to menu 2
			 */
			oedit_disp_val2_menu ( d );
			return;
		case OEDIT_VALUE_2:
			/*
			 * Here, I do need to check for out of range values.
			 */
			num = atoi ( arg );
			switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
			{
				case ITEM_SCROLL:
				case ITEM_POTION:
				case ITEM_ANTIDOTE_1:
				case ITEM_ANTIDOTE_2:
				case ITEM_ANTIDOTE_3:
					if ( num == 0 || num == -1 )
						GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ) = -1;
					else
						GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ) = LIMIT ( num, 1, NUM_SPELLS-1 );

					oedit_disp_val3_menu ( d );
					break;
				case ITEM_CONTAINER:
					/*
					 * Needs some special handling since we are dealing with flag values
					 * here.
					 */
					if ( num < 0 || num > 4 )
						oedit_disp_container_flags_menu ( d );
					else if ( num != 0 )
					{
						TOGGLE_BIT ( GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ), 1 << ( num - 1 ) );
						OLC_VAL ( d ) = 1;
						oedit_disp_val2_menu ( d );
					}
					else
						oedit_disp_val3_menu ( d );
					break;
				case ITEM_WEAPON:
					GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ) = LIMIT ( num, 1, MAX_WEAPON_NDICE );
					oedit_disp_val3_menu ( d );
					break;
				case ITEM_VEHICLE:
				case ITEM_VEHICLE2:
					GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ) = LIMIT ( num, 0, 1 );
					oedit_disp_val3_menu ( d );
					break;
				default:
					GET_OBJ_VAL ( OLC_OBJ ( d ), 1 ) = num;
					oedit_disp_val3_menu ( d );
			}
			return;

		case OEDIT_VALUE_3:
			num = atoi ( arg );
			/*
			 * Quick'n'easy error checking.
			 */
			switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
			{
				case ITEM_SCROLL:
				case ITEM_POTION:
					if ( num == 0 || num == -1 )
					{
						GET_OBJ_VAL ( OLC_OBJ ( d ), 2 ) = -1;
						oedit_disp_val4_menu ( d );
						return;
					}
					min_val = 1;
					max_val = NUM_SPELLS - 1;
					break;
				case ITEM_WEAPON:
					min_val = 1;
					max_val = MAX_WEAPON_SDICE;
					break;
				case ITEM_WAND:
				case ITEM_STAFF:
					min_val = 0;
					max_val = 20;
					break;
				case ITEM_DRINKCON:
				case ITEM_FOUNTAIN:
					min_val = 0;
					max_val = NUM_LIQ_TYPES - 1;
					break;
				case ITEM_KEY:
					min_val = 0;
					max_val = 999999;
					break;
				case ITEM_VEHICLE:
				case ITEM_VEHICLE2:
					min_val = 1;
					max_val = 5;
					break;
				default:
					min_val = -32000;
					max_val = 32000;
			}
			GET_OBJ_VAL ( OLC_OBJ ( d ), 2 ) = LIMIT ( num, min_val, max_val );
			oedit_disp_val4_menu ( d );
			return;

		case OEDIT_VALUE_4:
			num = atoi ( arg );
			switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
			{
				case ITEM_SCROLL:
				case ITEM_POTION:
					if ( num == 0 || num == -1 )
					{
						GET_OBJ_VAL ( OLC_OBJ ( d ), 3 ) = -1;
						oedit_disp_menu ( d );
						return;
					}
					min_val = 1;
					max_val = NUM_SPELLS - 1;
					break;
				case ITEM_WAND:
				case ITEM_STAFF:
					min_val = 1;
					max_val = NUM_SPELLS - 1;
					break;
				case ITEM_WEAPON:
					min_val = 0;
					max_val = NUM_ATTACK_TYPES - 1;
					break;
				case ITEM_KEY:
					min_val = 0;
					max_val = 999999;
					break;
				default:
					min_val = -32000;
					max_val = 32000;
					break;
			}
			GET_OBJ_VAL ( OLC_OBJ ( d ), 3 ) = LIMIT ( num, min_val, max_val );
			oedit_disp_val5_menu ( d );
			return;
		case OEDIT_VALUE_5:
			num = atoi ( arg );
			switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
			{
				case ITEM_TREE:

					min_val = NOTHING;
					max_val = 999999;
					break;
				case ITEM_DRINKCON:
				case ITEM_FOUNTAIN:
				case ITEM_FOOD:
					if ( num == 0 || num == -1 )
					{
						GET_OBJ_VAL ( OLC_OBJ ( d ), 4 ) = -1;
						oedit_disp_menu ( d );
						return;
					}
					min_val = 1;
					max_val = NUM_SPELLS - 1;
					break;
				case ITEM_LIGHTSABRE_HILT:
				case ITEM_WEAPON:
					min_val = 10;
					max_val = 200;
					break;
				default:
					min_val = -32000;
					max_val = 32000;
					break;
			}
			GET_OBJ_VAL ( OLC_OBJ ( d ), 4 ) = LIMIT ( num, min_val, max_val );
			oedit_disp_val6_menu ( d );
			return;
		case OEDIT_VALUE_6:
			num = atoi ( arg );
			switch ( GET_OBJ_TYPE ( OLC_OBJ ( d ) ) )
			{
				case ITEM_TREE:
					min_val = 1;
					max_val = 5;
					break;
				default:
					min_val = -32000;
					max_val = 32000;
					break;
			}
			GET_OBJ_VAL ( OLC_OBJ ( d ), 5 ) = LIMIT ( num, min_val, max_val );
			oedit_disp_menu ( d );
			return;
		case OEDIT_PROMPT_APPLY:
			if ( ( num = atoi ( arg ) ) == 0 )
				break;
			else if ( num < 0 || num > MAX_OBJ_AFFECT )
			{
				oedit_disp_prompt_apply_menu ( d );
				return;
			}
			OLC_VAL ( d ) = num - 1;
			OLC_MODE ( d ) = OEDIT_APPLY;
			oedit_disp_apply_menu ( d );
			return;

		case OEDIT_APPLY:
			if ( ( num = atoi ( arg ) ) == 0 )
			{
				OLC_OBJ ( d )->affected[OLC_VAL ( d ) ].location = 0;
				OLC_OBJ ( d )->affected[OLC_VAL ( d ) ].modifier = 0;
				oedit_disp_prompt_apply_menu ( d );
			}
			else if ( num < 0 || num >= NUM_APPLIES )
				oedit_disp_apply_menu ( d );
			else
			{
				int counter;

				/* add in check here if already applied.. deny builders another */
				if ( GET_LEVEL ( d->character ) < LVL_IMPL )
				{
					for ( counter = 0; counter < MAX_OBJ_AFFECT; counter++ )
					{
						if ( OLC_OBJ ( d )->affected[counter].location == num )
						{
							d->Output ( "Object already has that apply." );
							return;
						}
					}
				}

				OLC_OBJ ( d )->affected[OLC_VAL ( d ) ].location = num;
				d->Output ( "Modifier : " );
				OLC_MODE ( d ) = OEDIT_APPLYMOD;
			}
			return;

		case OEDIT_APPLYMOD:
			OLC_OBJ ( d )->affected[OLC_VAL ( d ) ].modifier = atoi ( arg );
			oedit_disp_prompt_apply_menu ( d );
			return;

		case OEDIT_EXTRADESC_KEY:
			if ( genolc_checkstring ( d, arg ) )
			{
				if ( OLC_DESC ( d )->keyword )
					free ( OLC_DESC ( d )->keyword );
				OLC_DESC ( d )->keyword = str_udup ( arg );
			}
			oedit_disp_extradesc_menu ( d );
			return;

                case OEDIT_ATTACHMENT_MENU:
                        switch ((num = atoi(arg)))
                        {
                               case 0:
                                   if (OLC_ATTACHMENT(d)->type == 0)
                                   {
                                       struct vehicle_attachment_data *temp;
                                       REMOVE_FROM_LIST(OLC_ATTACHMENT(d), OLC_OBJ(d)->attachment, next);
                                       free(OLC_ATTACHMENT(d));
                                       OLC_ATTACHMENT(d) = NULL;
                                   }
                                   oedit_disp_menu(d);
                                   break;
                               case 1:
                                   OLC_MODE(d) = OEDIT_ATTACHMENT_TYPE;
                                   oedit_disp_attachment_type(d);
                                   break;
                               case 2:
                                   d->Output("Enter value : ");
                                   OLC_MODE(d) = OEDIT_ATTACHMENT_VALUE;
                                   break;
                               case 3:
                                   d->Output("Enter value : ");
                                   OLC_MODE(d) = OEDIT_ATTACHMENT_MAX_VALUE;
                                   break;
                               case 4:
                                   if (OLC_ATTACHMENT(d)->type != 0) {
                                       if (OLC_ATTACHMENT(d)->next)
                                           OLC_ATTACHMENT(d) = OLC_ATTACHMENT(d)->next;
                                       else {
                                           struct vehicle_attachment_data *nv;
                                           CREATE(nv, struct vehicle_attachment_data, 1);
                                           OLC_ATTACHMENT(d)->next = nv;
                                           OLC_ATTACHMENT(d) = OLC_ATTACHMENT(d)->next;
                                       }
                                    }
                                      /* No break required here */ 
                               default:
                                   oedit_disp_attachment_menu(d);
                                   break;
                        }
                        return;
                        break;
                case OEDIT_ATTACHMENT_TYPE:
                    int i;
                    for (i = 0; ; i++)
                        if (attachment_types[i][0] == '\n') break;
                    num = atoi(arg);
                    if (num < 0 || num > i) {
                        d->Output("Wrong choice.\r\n");
                        oedit_disp_attachment_type(d);
                        return;
                    }
                    if (num) 
                        OLC_ATTACHMENT(d)->type = num;
                    OLC_VAL(d) = 1;
                    oedit_disp_attachment_menu(d); 
                    return;

                case OEDIT_ATTACHMENT_VALUE:
                    OLC_ATTACHMENT(d)->value = atoi(arg);
                    OLC_VAL(d) = 1;
                    oedit_disp_attachment_menu(d);
                    return;
                    
                case OEDIT_ATTACHMENT_MAX_VALUE:
                    OLC_ATTACHMENT(d)->max_value = atoi(arg);
                    OLC_VAL(d) = 1;
                    oedit_disp_attachment_menu(d);
                    return;

		case OEDIT_EXTRADESC_MENU:
			switch ( ( num = atoi ( arg ) ) )
			{
				case 0:
					if ( !OLC_DESC ( d )->keyword || !OLC_DESC ( d )->description )
					{
						struct extra_descr_data *temp;

						if ( OLC_DESC ( d )->keyword )
							free ( OLC_DESC ( d )->keyword );
						if ( OLC_DESC ( d )->description )
							free ( OLC_DESC ( d )->description );

						/*
						 * Clean up pointers
						 */
						REMOVE_FROM_LIST ( OLC_DESC ( d ), OLC_OBJ ( d )->ex_description, next );
						free ( OLC_DESC ( d ) );
						OLC_DESC ( d ) = NULL;
					}
					break;

				case 1:
					OLC_MODE ( d ) = OEDIT_EXTRADESC_KEY;
					d->Output ( "Enter keywords, separated by spaces :-\r\n| " );
					return;

				case 2:
					OLC_MODE ( d ) = OEDIT_EXTRADESC_DESCRIPTION;
					send_editor_help ( d );
					d->Output ( "Enter the extra description:\r\n\r\n" );
					if ( OLC_DESC ( d )->description )
					{
						d->Output ( "%s", OLC_DESC ( d )->description );
						oldtext = strdup ( OLC_DESC ( d )->description );
					}
					string_write ( d, &OLC_DESC ( d )->description, MAX_MESSAGE_LENGTH, 0, oldtext );
					OLC_VAL ( d ) = 1;
					return;

				case 3:
					/*
					 * Only go to the next description if this one is finished.
					 */
					if ( OLC_DESC ( d )->keyword && OLC_DESC ( d )->description )
					{
						struct extra_descr_data *new_extra;

						if ( OLC_DESC ( d )->next )
							OLC_DESC ( d ) = OLC_DESC ( d )->next;
						else  	/* Make new extra description and attach at end. */
						{
							CREATE ( new_extra, struct extra_descr_data, 1 );
							OLC_DESC ( d )->next = new_extra;
							OLC_DESC ( d ) = OLC_DESC ( d )->next;
						}
					}
					/*
					 * No break - drop into default case.
					 */
				default:
					oedit_disp_extradesc_menu ( d );
					return;
			}
			break;
		default:
			new_mudlog ( BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: Reached default case in oedit_parse()!" );
			d->Output ( "Oops...\r\n" );
			break;
	}

	/*
	 * If we get here, we have changed something.
	 */
	OLC_VAL ( d ) = 1;
	oedit_disp_menu ( d );
}

void oedit_string_cleanup ( Descriptor *d, int terminator )
{
	switch ( OLC_MODE ( d ) )
	{
		case OEDIT_FEEL_DESCRIPTION:
		case OEDIT_TASTE_DESCRIPTION:
		case OEDIT_SMELL_DESCRIPTION:
		case OEDIT_ACTDESC:
			oedit_disp_menu ( d );
			break;
		case OEDIT_EXTRADESC_DESCRIPTION:
			oedit_disp_extradesc_menu ( d );
			break;
	}
}
