#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "constants.h"
#include "oasis.h"
//TODO: this string function has a memory leak if reused on a strung object
ACMD(do_string)
{
    struct obj_data *obj;
    char buf[MAX_STRING_LENGTH];
    char buf1[MAX_STRING_LENGTH];
    char buf2[MAX_STRING_LENGTH];

    half_chop(argument, buf, argument);
    half_chop(argument, buf1, buf2);


    if (!*buf || !*buf1 || !*buf2) {
	send_to_char("Usage: string <obj> <field> <value>\r\n", ch);
	return;
    }

    if (!(obj = get_obj_vis(ch, buf, NULL))) {
	send_to_char("No such object around.\r\n", ch);
	return;
    }

    if (!strcmp("name", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	obj->name = strdup(buf2);
    } else if (!str_cmp("short", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	obj->short_description = strdup(buf2);
    } else if (!str_cmp("long", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	obj->description = strdup(buf2);
    } else if (!str_cmp("weight", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	GET_OBJ_WEIGHT(obj) = atoi(buf2);
    } else if (!str_cmp("cost", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	GET_OBJ_COST(obj) = atoi(buf2);
    } else if (!str_cmp("rent", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	GET_OBJ_RENT(obj) = atoi(buf2);
    } else if (!str_cmp("timer", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	GET_OBJ_RENT(obj) = atoi(buf2);
    } else if (!str_cmp("v0", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	GET_OBJ_VAL(obj, 0) = atoi(buf2);
    } else if (!str_cmp("v1", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	GET_OBJ_VAL(obj, 1) = atoi(buf2);
    } else if (!str_cmp("v2", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	GET_OBJ_VAL(obj, 2) = atoi(buf2);
    } else if (!str_cmp("v3", buf1)) {
	if (!IS_SET_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE))
	    SET_BIT_AR(GET_OBJ_EXTRA(obj), ITEM_UNIQUE_SAVE);
	GET_OBJ_VAL(obj, 3) = atoi(buf2);
    } else
	send_to_char("You can't set that field.\r\n", ch);

    send_to_char("Ok.\r\n", ch);
    return;
}

/* buf queda con la linea sin \n\r */
char *getline( char *str, char *buf )
{
        int tmp = 0;
        bool found = FALSE;

        while ( *str )
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
                string = getline( string, tmpb );
                sprintf( buf2, "%2d. %s\n\r", cnt++, tmpb );
                strcat( buf, buf2 );
        }

        return buf;
}



