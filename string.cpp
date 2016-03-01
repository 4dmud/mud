#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "constants.h"
#include "oasis.h"

/* external functions */
char *str_udup(const char *txt);
void copy_ex_descriptions ( struct extra_descr_data **to, struct extra_descr_data *from );

ACMD(do_string)
{
  struct obj_data *obj;
  char buf[MAX_STRING_LENGTH];
  char buf1[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];

  half_chop(argument, buf, argument);
  half_chop(argument, buf1, buf2);


  if (!*buf || !*buf1 || !*buf2)
  {
    send_to_char("Usage: string <obj> <field> <value>\r\n", ch);
    return;
  }

  

  if (!(obj = get_obj_in_list_vis(ch, buf, NULL,ch->carrying)))
  {
    send_to_char("No such object around.\r\n", ch);
    return;
  }

  obj_rnum rn = GET_OBJ_RNUM ( obj );
  if (!strcmp("name", buf1))
  {
    SET_BIT_AR ( GET_OBJ_EXTRA ( obj ), ITEM_UNIQUE_SAVE );
    if ( obj->name && ( rn == NOTHING || obj->name != obj_proto[rn].name ) )
      free ( obj->name );
    obj->name = str_udup ( buf2 );
  }
  else if (!str_cmp("short", buf1))
  {
    SET_BIT_AR ( GET_OBJ_EXTRA ( obj ), ITEM_UNIQUE_SAVE );
    if ( obj->short_description && ( rn == NOTHING || obj->short_description != obj_proto[rn].short_description ) )
      free ( obj->short_description );
    obj->short_description = str_udup ( buf2 );
  }
  else if (!str_cmp("long", buf1))
  {
    SET_BIT_AR ( GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE );
    if ( obj->description && ( rn == NOTHING || obj->description != obj_proto[rn].description ) )
      free ( obj->description );
    obj->description = str_udup ( buf2 );
  }
  else if (!str_cmp("extra", buf1))
  {
	half_chop ( buf2, buf1, buf2 );
	SET_BIT_AR ( GET_OBJ_EXTRA ( obj ), ITEM_UNIQUE_SAVE );
	bool ex_was_proto = FALSE;
	if ( rn != NOTHING && obj->ex_description == obj_proto[rn].ex_description )
	{
		copy_ex_descriptions ( &obj->ex_description, obj_proto[rn].ex_description );
		ex_was_proto = TRUE;
	}

	extra_descr_data *ex_desc = obj->ex_description;
	if ( ex_desc == NULL )
	{
		CREATE ( ex_desc, extra_descr_data, 1 );
		ex_desc->keyword = str_udup ( buf1 );
		ex_desc->description = str_udup ( buf2 );
		ex_desc->next = NULL;
	}
	else if ( ex_desc->keyword == NULL )
	{
		ex_desc->keyword = str_udup ( buf1 );
		ex_desc->description = str_udup ( buf2 );
		ex_desc->next = NULL;
	}
	else for ( ; ex_desc; ex_desc = ex_desc->next )
	{
		if ( strstr ( ex_desc->keyword, buf1 ) )
		{
			if ( !ex_was_proto )
				free_string ( &ex_desc->description );
			ex_desc->description = str_udup ( buf2 );
			break;
		}
		else if ( ex_desc->next == NULL )
		{
			CREATE ( ex_desc->next, extra_descr_data, 1 );
			ex_desc->next->keyword = str_udup ( buf1 );
			ex_desc->next->description = str_udup ( buf2 );
			ex_desc->next->next = NULL;
			break;
		}
	}
  }
  else if (!str_cmp("weight", buf1))
  {
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
    GET_OBJ_WEIGHT(obj) = atoi(buf2);
  }
  else if (!str_cmp("cost", buf1))
  {
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
    GET_OBJ_COST(obj) = atoi(buf2);
  }
  else if (!str_cmp("rent", buf1))
  {
    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
    GET_OBJ_RENT(obj) = atoi(buf2);
  }
  else if (!str_cmp("timer", buf1))
  {
    GET_OBJ_TIMER(obj) = atoi(buf2);
  }
  else if (!str_cmp("v0", buf1))
  {
    GET_OBJ_VAL(obj, 0) = atoi(buf2);
  }
  else if (!str_cmp("v1", buf1))
  {
    GET_OBJ_VAL(obj, 1) = atoi(buf2);
  }
  else if (!str_cmp("v2", buf1))
  {
    GET_OBJ_VAL(obj, 2) = atoi(buf2);
  }
  else if (!str_cmp("v3", buf1))
  {
    GET_OBJ_VAL(obj, 3) = atoi(buf2);
  }
  else
    send_to_char("You can't set that field.\r\n", ch);

  send_to_char("Ok.\r\n", ch);
  return;
}

/* buf queda con la linea sin \n\r */
char *getline( char *str, char *buf , size_t len)
{
  size_t tmp = 0;
  bool found = FALSE;

  while ( *str  && tmp < len)
  {
    if ( *str == '\n' )
    {
      found = TRUE;
      break;
    }

    buf[tmp++] = *(str++);
  }

  if ( found )
  {
    if ( *(str + 1) == '\r' )
      str += 2;
    else
      str += 1;
  } /* para que quedemos en el inicio de la prox linea */

  buf[tmp] = '\0';

  return str;
}


char *numlineas( char *string )
{
  int cnt = 1;
  static char buf[MAX_STRING_LENGTH*2];
  char buf2[MAX_STRING_LENGTH], tmpb[MAX_STRING_LENGTH];

  buf[0] = '\0';

  while ( *string )
  {
    string = getline( string, tmpb , sizeof(tmpb));
    sprintf( buf2, "%2d. %s\r\n", cnt++, tmpb );
    strcat( buf, buf2 );
  }

  return buf;
}



