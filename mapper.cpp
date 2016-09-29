/**************************************************************************
*  Original Diku Mud copyright (C) 1990, 1991 by Sebastian Hammer,        *
*  Michael Seifert, Hans Henrik St{rfeldt, Tom Madsen, and Katja Nyboe.   *
*                                                                         *
*  Merc Diku Mud improvments copyright (C) 1992, 1993 by Michael          *
*  Chastain, Michael Quan, and Mitchell Tse.                              *
*                                                                         *
*  In order to use any part of this Merc Diku Mud, you must comply with   *
*  both the original Diku license in 'license.doc' as well the Merc       *
*  license in 'license.txt'.  In particular, you may not remove either of *
*  these copyright notices.                                               *
*                                                                         *
*  Dystopia Mud improvements copyright (C) 2000, 2001 by Brian Graversen  *
*                                                                         *
*  Much time and thought has gone into this software and you are          *
*  benefitting.  We hope that you share your changes too.  What goes      *
*  around, comes around.                                                  *
***************************************************************************
*  Converted for AFKMud 1.64 by Zarius (jeff@mindcloud.com)               *
*  Downloaded from http://www.mindcloud.com                               *
*  If you like the snippet let me know                                    *
***************************************************************************/
/**************************************************************************
 *                          Version History                                *
 **************************************************************************
 *  (v1.0) - Converted Automapper to AFKMud 1.64 and added additional     *
 *           directions and removed room desc code into a sep func        *
 **************************************************************************/

/*
      TO DO
   1. Add a way of displaying up and down directions effectively
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "spells.h"
#include "house.h"
#include "screen.h"
#include "constants.h"
#include "dg_scripts.h"
#include "arena.h"
#include "clan.h"
#include "oasis.h"
#include "mapper.h"

void MapArea ( room_rnum room, Character *ch, int x, int y, int min, int max, bool show_vehicles, bool two_way_connection );
room_rnum visible_room ( room_rnum r, int door, bool *two_way_connection );

/* The map itself */
struct map_type amap[MAPX + 1][MAPY + 1];

/* Take care of some repetitive code for later */
void get_exit_dir( int dir, int *x, int *y, int xorig, int yorig )
{
  /* Get the next coord based on direction */
  switch( dir )
  {
  case 0: /* North */
    *x = xorig;
    *y = yorig - 1;
    break;
  case 1: /* East */
    *x = xorig + 1;
    *y = yorig;
    break;
  case 2: /* South */
    *x = xorig;
    *y = yorig + 1;
    break;
  case 3: /* West */
    *x = xorig - 1;
    *y = yorig;
    break;
  case 4: /* UP */
    break;
  case 5: /* DOWN */
    break;
  case 6: /* NE */
    *x = xorig + 1;
    *y = yorig - 1;
    break;
  case 7: /* NW */
    *x = xorig - 1;
    *y = yorig - 1;
    break;
  case 8: /* SE */
    *x = xorig + 1;
    *y = yorig + 1;
    break;
  case 9: /* SW */
    *x = xorig - 1;
    *y = yorig + 1;
    break;
  default:
    *x = -1;
    *y = -1;
    break;
  }
}


/* Clear one map coord */
void clear_coord( int x, int y )
{
  amap[x][y].tegn[0] = ' ';
  amap[x][y].tegn[1] = '\0';
  amap[x][y].vnum = 0;
  amap[x][y].depth = 0;
  amap[x][y].can_see = TRUE;
}

/* Clear all exits for one room */
void clear_room( int x, int y )
{
  int dir, exitx, exity;

  /* Cycle through the four directions */
  for( dir = 0; dir < 4; dir++ )
  {
    /* Find next coord in this direction */
    get_exit_dir( dir, &exitx, &exity, x, y );

    /* If coord is valid, clear it */
    if ( !BOUNDARY( exitx, exity ) )
      clear_coord( exitx, exity );
  }
}

struct room_direction_data *get_exit(room_rnum  pRoom,int door )
{
  if (door < 0)
    return NULL;
  if (door > DOWN)
    return NULL;
  if (!pRoom->dir_option)
    return NULL;
  if (!pRoom->dir_option[door])
    return NULL;
  else
    return pRoom->dir_option[door];
}

/* This function is recursive, ie it calls itself */
void map_exits(Character *ch, room_rnum pRoom, int x, int y, int depth, bool two_way_connection)
{
  static char map_chars [11] = "|-|-UD/\\\\/";
  int door;
  int exitx = 0, exity = 0;
  int roomx = 0, roomy = 0;
  room_rnum room_to;
  bool two_way;

  if ( x < 0 || y < 0 || x > MAPX || y > MAPY )
     return;

  /* Setup this coord as a room */
  snprintf(amap[x][y].tegn, 3, "%d", SECTOR(pRoom));
  amap[x][y].vnum = pRoom->number;
  amap[x][y].depth = depth;
  amap[x][y].can_see = room_is_dark( pRoom ) && affected_by_spell(ch, SPELL_INFRAVISION);

  /* Don't go beyond one-way connections */
  if ( two_way_connection == FALSE )
    return;

  /* Limit recursion */
  if ( depth > MAXDEPTH )
    return;

  /* This room is done, deal with it's exits */
  for( door = 0; door < NUM_OF_DIRS; door++ )
  {
    /* Skip up and down until I can figure out a good way to display it */
    if (door == UP || door == DOWN)
      continue;

    room_to = visible_room ( pRoom, door, &two_way );

    if ( room_to == NULL )
      continue;

    /* Get the coords for the next exit and room in this direction */
    get_exit_dir( door, &exitx, &exity, x, y );
    get_exit_dir( door, &roomx, &roomy, exitx, exity );

    /* Skip if coords fall outside map */
    if ( BOUNDARY( exitx, exity ) || BOUNDARY( roomx, roomy ))
      continue;

    /* No exits at MAXDEPTH */
    if ( depth == MAXDEPTH )
      continue;

    /* No need for exits that are already mapped */
    if ( amap[exitx][exity].depth > 0 )
      continue;

    /* Fill in exit */
    amap[exitx][exity].depth = depth;
    amap[exitx][exity].vnum = room_to->number;
    amap[exitx][exity].tegn[0] = map_chars[door];
    amap[exitx][exity].tegn[1] = '\0';

    /* More to do? If so we recurse */
    if ( amap[roomx][roomy].vnum == 0 )
    {
      /* Depth increases by one each time */
      map_exits( ch, room_to, roomx, roomy, depth + 1, two_way );
    }
  }
}

/* Reformat room descriptions to exclude undesirable characters */
void reformat_desc( char *desc , size_t len)
{
  /* Index variables to keep track of array/pointer elements */
  unsigned int i;
  int j;
  char buf[MAX_STRING_LENGTH], *p;

  i = 0;
  j = 0;
  buf[0] = '\0';

  if ( !desc  ) return;

  /* Replace all "\n" and "\r" with spaces */
  for( i = 0; i <= strlen( desc ); i++ )
  {
    if ( ( desc[i] == '\n' ) || ( desc[i] == '\r' ) ) desc[i] = ' ';
  }

  /* Remove multiple spaces */
  for( p = desc; *p != '\0'; p++ )
  {
    buf[j] = *p;
    j++;

    /* Two or more consecutive spaces? */
    if ( ( *p == ' ' ) && ( *( p + 1 ) == ' ' ) )
    {
      do
      {
        p++;
      }
      while( *(p + 1) == ' ' );
    }
  }

  buf[j] = '\0';

  /* Copy to desc */
  strlcpy(desc, buf, len);
}
#if 0
int get_line( char *desc, int max_len )
{
  int i, j = 0;

  /* Return if it's short enough for one line */
  if ( (int)strlen( desc ) <= max_len ) return 0;

  /* Calculate end point in string without colour */
  for( i = 0; i <= (int)strlen( desc ); i++ )
  {
    /* Here you need to skip your colour sequences */
    j++;

    if ( j > max_len ) break;
  }

  /* End point is now in i, find the nearest space */
  for( j = i; j > 0; j-- )
  {
    if ( desc[j] == ' ' ) break;
  }

  /* There could be a problem if there are no spaces on the line */
  return j + 1;
}
#endif
/* Display the map to the player */
void show_map( Character *ch, int mxp)
{
  char buf[MAX_STRING_LENGTH * 2];
  int x, y, sec, sect = 0;

  buf[0] = '\0';
  if (mxp)
    ch->Send( "%s", MXPTAG("FRAME Map REDIRECT"));

  ch->Send( "\n\r {cy+-----------+{cw\n\r");

  /* Write out the main map area with text */
  for( y = 0; y <= MAPY; y++ )
  {

    strlcpy( buf, " {cy|{c0", sizeof(buf) );

    for( x = 0; x <= MAPX; x++ )
    {
      switch(*amap[x][y].tegn)
      {
      case '-':
      case '|':
      case '\\':
      case '/':
        sprintf(buf + strlen(buf), "{cg%c{c0", *amap[x][y].tegn);
        break;
      case ' ':
        sprintf(buf + strlen(buf), " ");
        break;
      case 'X':
        sprintf(buf + strlen(buf), "{cR*");
        break;
      default:
        sec = atoi(amap[x][y].tegn);
        switch (sec)
        {
        case NUM_ROOM_SECTORS:
        case SECT_INSIDE:
        case SECT_CITY:
        case SECT_FIELD:
        case SECT_FOREST:
        case SECT_HILLS:
        case SECT_MOUNTAIN:
        case SECT_WATER_SWIM:
        case SECT_WATER_NOSWIM:
        case SECT_UNDERWATER:
        case SECT_FLYING:
        case SECT_DESERT:
        case SECT_SPACE:
        case SECT_ROAD:
        case SECT_ENTRANCE:
        case SECT_ATMOSPHERE:
        case SECT_SUN:
        case SECT_BLACKHOLE:
        case SECT_VEHICLE:
        case SECT_SWAMP:
        case SECT_REEF:
        case SECT_DEATHTRAP:
        case SECT_SNOW:
        case SECT_ICE:
        case SECT_PRAIRIE:
        case SECT_BADLANDS:
        case SECT_RAIL:

          sprintf(buf + strlen(buf), "%s%s", map_bit[sec].colour, map_bit[sec].bit);
          break;
        default:
          sprintf(buf + strlen(buf), " ");
          break;
        }
      }
    }
    //strlcat( buf, "\r\n ", sizeof(buf) );

    if (!mxp)
    {
      ch->Send( "%s", buf);
      ch->Send( "{cy|{c0  %s%1s{cx%c%-10s   %s%1s{cx%c%-10s   \r\n",
                       MDIS(0) ? map_bit[sect].colour : "", MDIS(0) ? map_bit[sect].bit : "", MDIS(0) ? ' ' : ' ', MDIS(0) ? map_bit[sect].name : "",
                       MDIS(1) ? map_bit[sect+1].colour : "", MDIS(1) ? map_bit[sect+1].bit : "", MDIS(1) ? ' ' : ' ', MDIS(1) ? map_bit[sect+1].name : "");
      sect+=2;

    }
    else
    {
      ch->Send( "%s {cy|{c0\r\n", buf);
    }

    //if (y == 0 && IS_PLR_FLAG( ch, PLR_AUTOEXIT))  /* the autoexits */
    //{
    //    sprintf(buf + strlen( buf ), "%s", get_exits(ch));
    //    continue;
    //}

    /* Add the text, if necessary */
    /*if ( !alldesc )
    {
     pos = get_line( p, 63 );
     if ( pos > 0 )
     {
          mudstrlcat( buf, colour_str(AT_RMDESC, ch), MSL);
          strncat( buf, p, pos );
          p += pos;
     }
     else
     {
          mudstrlcat( buf, colour_str(AT_RMDESC, ch), MSL);
          mudstrlcat( buf, p, MSL );
          alldesc = TRUE;
     }
    }
    mudstrlcat( buf, "\n\r", MSL );*/
  }

  /* Finish off map area */
  ch->Send( " {cy+-----------+{c0\r\n");
  if (mxp)
    ch->Send( "%s", MXPTAG("FRAME _previous REDIRECT"));
  /*if ( !alldesc )
  {
     pos = get_line( p, 63 );
     if ( pos > 0 )
     {
          mudstrlcat( buf, colour_str(AT_RMDESC, ch), MSL);
          strncat( buf, p, pos );
          p += pos;
     }
     else
     {
          mudstrlcat( buf, colour_str(AT_RMDESC, ch), MSL);
          mudstrlcat( buf, p, MSL );
          alldesc = TRUE;
     }
  }
  */

  /* Deal with any leftover text */
#if 0
  if ( !alldesc )
  {
    do
    {
      /* Note the number - no map to detract from width */
      pos = get_line( p, 78 );
      if ( pos > 0 )
      {
        mudstrlcat( buf, colour_str(AT_RMDESC, ch), MSL);
        strncat( buf, p, pos );
        p += pos;
      }
      else
      {
        mudstrlcat( buf, colour_str(AT_RMDESC, ch), MSL);
        mudstrlcat( buf, p, MSL );
        alldesc = TRUE;
      }
    }
    while( !alldesc );
  }
#endif

}

/* Clear, generate and display the map */
void draw_map( Character *ch)
{
  int x, y;
  static char buf[MAX_STRING_LENGTH];
  *buf = 0;
  //strlcpy( buf, desc, len);
  /* Remove undesirable characters */
  //reformat_desc( buf );

  /* Clear map */
  for( y = 0; y <= MAPY; y++ )
  {
    for( x = 0; x <= MAPX; x++ )
    {
      clear_coord( x, y );
    }
  }

  /* Start with players pos at centre of map */
  x = MAPX / 2;
  y = MAPY / 2;

  amap[x][y].vnum = ch->in_room->number;
  amap[x][y].depth = 0;

  /* Generate the map */
  map_exits( ch, ch->in_room, x, y, 0, TRUE );

  /* Current position should be a "X" */
  amap[x][y].tegn[0] = 'X';
  amap[x][y].tegn[1] = '\0';

  /* Make sure the symbols in the cardinal directions are correct */
  Room *r;
  int exitx, exity, roomx, roomy;
  bool two_way;
  for( int door = 0; door < NUM_OF_DIRS; door++ )
  {
    if (door == UP || door == DOWN)
      continue;

    r = IN_ROOM ( ch );
    x = MAPX / 2;
    y = MAPY / 2;

    while ( TRUE )
    {
      r = visible_room ( r, door, &two_way );

      if ( r == NULL )
        break;

      /* Get the coords for the next exit and room in this direction */
      get_exit_dir ( door, &exitx, &exity, x, y );
      get_exit_dir ( door, &roomx, &roomy, exitx, exity );

      /* Skip if coords fall outside map */
      if ( BOUNDARY ( exitx, exity ) || BOUNDARY ( roomx, roomy ) )
        break;

      snprintf ( amap[roomx][roomy].tegn, 3, "%d", SECTOR ( r ) );
      x = roomx;
      y = roomy;
    }
  }

  /* Send the map */
  show_map(ch, FALSE);
}



//@TODO:KAVIR: This should be rewritten, it's not very nice.  It's cobbled
//together from the old map code and it works, but the whole thing could really
//use a redesign and cleanup.

/* Clear, generate and store the map in an MSDP variable */
char *msdp_map( Character *ch )
{
  int x, y;
  static char buf[MAX_STRING_LENGTH];
  buf[0] = '\0';
  room_rnum was_in = IN_ROOM(ch);

  #define MAX_MAP 72

  if ( ROOM_FLAGGED ( IN_ROOM(ch), ROOM_WILDERNESS ) )
  {
    int size = 10; // URANGE ( 10, 0, MAX_MAP );
    int center = MAX_MAP/2;
    int min = MAX_MAP / 2 - size / 2;
    int max = MAX_MAP / 2 + size / 2;
    extern int mapgrid[MAX_MAP][MAX_MAP];

    for ( x = 0; x < MAX_MAP; ++x )
      for ( y = 0; y < MAX_MAP; ++y )
        mapgrid[x][y] = NUM_ROOM_SECTORS;

    /* starts the mapping with the center room */
    MapArea ( IN_ROOM ( ch ), ch, center, center, min - 1, max - 1, FALSE, TRUE );

    for( x = min; x <= max; x++ )
    {
      for( y = min; y <= max; y++ )
      {
        char num_buf[64];
        sprintf( num_buf, "%d", mapgrid[x][y] );

        if ( buf[0] != '\0' )
           strcat( buf, " " );

        strcat( buf, num_buf );
      }
    }
    IN_ROOM(ch) = was_in;
    return buf;
  }

  /* Clear map */
  for( y = 0; y <= MAPY; y++ )
  {
    for( x = 0; x <= MAPX; x++ )
      clear_coord( x, y );
  }

  /* Start with players pos at centre of map */
  x = MAPX / 2;
  y = MAPY / 2;

  amap[x][y].vnum = ch->in_room->number;
  amap[x][y].depth = 0;

  /* Generate the map */
  map_exits( ch, ch->in_room, x, y, 0, TRUE );

  /* Store the map */
  strcat(buf, "X X X X X X X X X X X");
  for( y = 2; y <= MAPY-2; y++ )
  {
    for( x = 0; x <= MAPX; x++ )
    {
      if ( buf[0] != '\0' )
         strcat( buf, " " );

      if ( amap[x][y].tegn[0] == ' ' )
         strcat(buf, "X");
      else
         strcat(buf, amap[x][y].tegn);
    }
  }
  strcat(buf, " X X X X X X X X X X X");

  IN_ROOM(ch) = was_in;
  return buf;
}
void update_mxp_map(Character *ch)
{
  int x, y;
  static char buf[MAX_STRING_LENGTH];
  *buf = 0;

  /** need to add support to detect if the client supports this **/
  return;
  //strlcpy( buf, desc, len);
  /* Remove undesirable characters */
  //reformat_desc( buf );

  /* Clear map */
  for( y = 0; y <= MAPY; y++ )
  {
    for( x = 0; x <= MAPX; x++ )
    {
      clear_coord( x, y );
    }
  }

  /* Start with players pos at centre of map */
  x = MAPX / 2;
  y = MAPY / 2;

  amap[x][y].vnum = ch->in_room->number;
  amap[x][y].depth = 0;

  /* Generate the map */
  map_exits( ch, ch->in_room, x, y, 0, TRUE );

  /* Current position should be a "X" */
  amap[x][y].tegn[0] = 'X';
  amap[x][y].tegn[1] = '\0';

  /* Send the map */

  show_map(ch, TRUE);
}
/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXX|XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXX*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/


