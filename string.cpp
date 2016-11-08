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
char *find_exdesc ( const char *word, struct extra_descr_data *list );

int edit_extra_desc ( obj_data *obj, const char *keyword, const char *description )
{
    /*
     * Returns 0: success
     *         1: a word in the keyword already exists
     *         2: the keyword and description already exist
     *
     * If there was an edit, it will be put in the head because read_one_item
     * compares the head with the prototype.
     */

    // If the keyword matches, replace the description if it's different
    int rn = real_object ( GET_OBJ_VNUM ( obj ) );
    for ( extra_descr_data *ed = obj->ex_description, *prev = nullptr; ed; prev = ed, ed = ed->next )
        if ( !strcmp ( keyword, ed->keyword ) )
        {
            if ( !strcmp ( description, ed->description ) )
                return 2;

            if ( obj->ex_description == obj_proto[rn].ex_description )
                copy_ex_descriptions ( &obj->ex_description, obj_proto[rn].ex_description );

            // Edit the ex_desc and move it to the head
            free ( ed->description );
            ed->description = str_udup ( description );
            if ( prev )
            {
                prev->next = ed->next;
                ed->next = obj->ex_description;
                obj->ex_description = ed;
            }
            return 0;
        }

    // Check if one of the words in keyword abbreviates an existing keyword
    string key = string ( keyword ), word;
    istringstream iss ( key );
    while ( iss >> word )
        if ( find_exdesc ( word.c_str(), obj->ex_description ) )
            return 1;

    // Add the new ex_desc at the head
    extra_descr_data *ex_new;
    CREATE ( ex_new, struct extra_descr_data, 1 );
    ex_new->keyword = str_udup ( keyword );
    ex_new->description = str_udup ( description );
    ex_new->next = obj->ex_description;
    obj->ex_description = ex_new;
    return 0;
}

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
    ch->Send ( "Usage: string <obj> <field> <value>\r\n"
               "       string <obj> extra '<keywords>' <description>\r\n" );
    return;
  }

  obj = get_obj_in_list_vis(ch, buf, NULL,ch->carrying);
  if ( !obj )
  {
    ch->Send ( "No such object around.\r\n" );
    return;
  }

  obj_rnum rn = GET_OBJ_RNUM ( obj );
  if (!strcmp("name", buf1))
  {
    if ( obj->name && ( rn == NOTHING || obj->name != obj_proto[rn].name ) )
      free ( obj->name );
    obj->name = str_udup ( buf2 );
  }
  else if (!str_cmp("short", buf1))
  {
    SET_BIT_AR ( GET_OBJ_EXTRA ( obj ), ITEM_UNIQUE_SHORTDESC );
    if ( obj->short_description && ( rn == NOTHING || obj->short_description != obj_proto[rn].short_description ) )
      free ( obj->short_description );
    obj->short_description = str_udup ( buf2 );
  }
  else if (!str_cmp("long", buf1))
  {
    if ( obj->description && ( rn == NOTHING || obj->description != obj_proto[rn].description ) )
      free ( obj->description );
    obj->description = str_udup ( buf2 );
  }
  else if (!str_cmp("extra", buf1))
  {
    string arg = string ( buf2 );
    size_t pos = arg.find ( "'", 1 );
    if ( arg[0] != '\'' || pos == string::npos )
    {
        ch->Send ( "Usage: string <obj> extra '<keywords>' <description>\r\n" );
        return;
    }

    string keywords = arg.substr ( 1, pos-1 );
    string desc = arg.substr ( pos+2 );
    int result = edit_extra_desc ( obj, keywords.c_str(), desc.c_str() );
    if ( result == 1 )
    {
        ch->Send ( "One of the keywords already exists.\r\n" );
        return;
    }
    else if ( result == 2 )
    {
        ch->Send ( "This extra description already exists.\r\n" );
        return;
    }
  }
  else if (!str_cmp("weight", buf1))
  {
    GET_OBJ_WEIGHT(obj) = atoi(buf2);
  }
  else if (!str_cmp("cost", buf1))
  {
    GET_OBJ_COST(obj) = atoi(buf2);
  }
  else if (!str_cmp("rent", buf1))
  {
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
    ch->Send ( "You can't set that field.\r\n" );

  ch->Send ( "Ok.\r\n" );
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



