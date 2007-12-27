/* ******************************************************************** *
 * FILE : assedit.c                     Copyright (C) 1999 Del Minturn  *
 * USAGE: Olc for assembly engine by Geoff Davis.                       *
 *        Oasis OLC by George Greer.
 * -------------------------------------------------------------------- *
 * 1999 July 25 caminturn@earthlink.net                                *
 * ******************************************************************** */

#include "conf.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"

#include "db.h"
#include "comm.h"
#include "handler.h"
#include "interpreter.h"
/* #include "olc.h" */
#include "oasis.h"
#include "assemblies.h"

/*-------------------------------------------------------------------*
 * External data structures.
 *-------------------------------------------------------------------*/
extern struct descriptor_data *descriptor_list;
extern struct obj_data *obj_proto;

extern const char *AssemblyTypes[];

/*-------------------------------------------------------------------*
 * Function prototypes.
 *-------------------------------------------------------------------*/
void assedit_setup(struct descriptor_data *d, int number);
void assedit_disp_menu(struct descriptor_data *d);
void assedit_delete(struct descriptor_data *d);
void assedit_edit_extract(struct descriptor_data *d);
void assedit_edit_inroom(struct descriptor_data *d);
void nodigit(struct descriptor_data *d);


/*-------------------------------------------------------------------*
 * Nasty internal macros to clean up the code.
 *-------------------------------------------------------------------*/
long lRnum = 0;

/*-------------------------------------------------------------------*
 * Assedit command
 *-------------------------------------------------------------------*/

ACMD (do_assedit)
{
  struct descriptor_data *d = ch->desc;
  char buf[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];

 *buf = '\0';  /* If I run into problems then take this sucker out */
 *buf2 = '\0';

 if (IS_NPC(ch))
      return;
 if (GET_LEVEL(ch) < LVL_IMPL)
     new_send_to_char(ch, "You do not have permission to do that.\r\n");

 for (d = descriptor_list; d; d = d->next) {
     if (d->connected == CON_ASSEDIT) {
     new_send_to_char(ch, "Assemblies are already being editted by someone.\r\n");
     return;
     }
   }

 two_arguments(argument, buf, buf2);

 d= ch->desc;

 if(!*buf) {
    nodigit(d);
    return;
    }

 if (!isdigit(*buf)) {
    if (strn_cmp("new", buf, 3) == 0) {
         if(!isdigit(*buf2))
            nodigit(d);
         else {
            assemblyCreate(atoi(buf2), 0);
            new_send_to_char(d->character, "Assembly Created.\r\n");
            assemblySaveAssemblies();
            return;
            }
       }
    else
    if (strn_cmp("delete", buf, 6) == 0) {
         if (!isdigit(*buf2))
            nodigit(d);
         else {
             assemblyDestroy(atoi(buf2));
             new_send_to_char(d->character, "Assembly Deleted.\r\n");
             assemblySaveAssemblies();
             return;
             }
      }
    else {
     nodigit(d);
     return;
     }
 } else
   if (isdigit(*buf)) {
     d = ch->desc;
     CREATE (d->olc, struct oasis_olc_data, 1);
     assedit_setup(d, atoi(buf));

     }
return;
}

/*-------------------------------------------------------------------*
 * Assedit Functions
 *-------------------------------------------------------------------*/

void assedit_setup(struct descriptor_data *d, int number)
{

    ASSEMBLY    *pOldAssembly = NULL;
    CREATE(OLC_ASSEDIT(d), ASSEMBLY, 1 );


    if( (pOldAssembly = assemblyGetAssemblyPtr( number )) == NULL ) {
      new_send_to_char(d->character, "That assembly does not exist\r\n");
      cleanup_olc(d, CLEANUP_ALL);
      return;
    } else {
        /* Copy the old assembly. */
        OLC_ASSEDIT(d)->lVnum = pOldAssembly->lVnum;
        OLC_ASSEDIT(d)->uchAssemblyType = pOldAssembly->uchAssemblyType;
        OLC_ASSEDIT(d)->lNumComponents = pOldAssembly->lNumComponents;

        if( OLC_ASSEDIT(d)->lNumComponents > 0 )  {
            CREATE(OLC_ASSEDIT(d)->pComponents, COMPONENT, OLC_ASSEDIT(d)->lNumComponents);
            memmove(OLC_ASSEDIT(d)->pComponents, pOldAssembly->pComponents,
            OLC_ASSEDIT(d)->lNumComponents * sizeof( COMPONENT ) );
           }

    }

    /*
     * At this point, pNewAssembly is now the address of a freshly allocated copy of all
     * the data contained in the original assembly structure.
     */


    if ( (lRnum = real_object( OLC_ASSEDIT(d)->lVnum ) ) < 0)
      {
       new_send_to_char(d->character, "Assembled item may not exist, check the vnum and assembles (show assemblies). \r\n");
       cleanup_olc(d, CLEANUP_ALL);    /* for right now we just get out! */
       return;
      }

 STATE(d) = CON_ASSEDIT;
 act("$n starts using OLC.", TRUE, d->character, 0, 0, TO_ROOM);
 SET_BIT_AR(PLR_FLAGS(d->character), PLR_WRITING);
 assedit_disp_menu(d);

}

void assedit_disp_menu(struct descriptor_data *d)
{
 int i = 0;
 extern const char *AssemblyTypes[];
 char buf[MAX_STRING_LENGTH];
 char szAssmType[MAX_INPUT_LENGTH] = { '\0' };

 get_char_colors(d->character);

 sprinttype(OLC_ASSEDIT(d)->uchAssemblyType, AssemblyTypes, szAssmType, sizeof(szAssmType));

#if defined(CLEAR_SCREEN)
sprintf(buf, "%c[H%c[J", 27, 27);
new_send_to_char(d->character, buf);
#endif

  sprintf(buf,
      "Assembly Number:%s %ld %s\r\n"
      "Assembly Name  :%s %s %s \r\n"
      "Assembly Type  :%s %s %s\r\n"
      "Components:\r\n",
        yel,  OLC_ASSEDIT(d)->lVnum, nrm,
        yel,  obj_proto[ real_object(OLC_ASSEDIT(d)->lVnum) ].short_description, nrm,
        yel,  szAssmType, nrm
       );
    new_send_to_char(d->character, buf);

  if(OLC_ASSEDIT(d)->lNumComponents <= 0)
    new_send_to_char(d->character, "   < NONE > \r\n");
  else {
   for( i = 0; i < OLC_ASSEDIT(d)->lNumComponents; i++ ) {
     if ( (lRnum = real_object(OLC_ASSEDIT(d)->pComponents[i].lVnum)) < 0)
       {
        sprintf(buf,
          "%s %d%s) %s ERROR --- Contact an Implimentor %s\r\n ",
           grn, i+1, nrm, yel, nrm
            );
       }  else   {
        sprintf(buf,
          "%s %d%s) [%5ld] %-20.20s  In room:%s %-3.3s %s   Extract:%s %-3.3s%s \r\n",
           grn,  i+1, nrm,
           OLC_ASSEDIT(d)->pComponents[i].lVnum,
           obj_proto[ lRnum ].short_description,
           yel, (OLC_ASSEDIT(d)->pComponents[ i ].bInRoom  ? "Yes" : "No"), nrm,
           yel, (OLC_ASSEDIT(d)->pComponents[ i ].bExtract ? "Yes" : "No"), nrm           );
       }
      new_send_to_char(d->character, buf);
      }
     }
  sprintf(buf,
       "%sA%s) Add a new component.\r\n"
       "%sE%s) Edit a component.\r\n"
       "%sD%s) Delete a component.\r\n"
       "%sT%s) Change Assembly Type.\r\n"
       "%sQ%s) Quit.\r\n"
       "\r\nEnter your choice : ",
       grn, nrm, grn, nrm, grn, nrm, grn, nrm, grn, nrm
         );
  new_send_to_char(d->character, buf);

  OLC_MODE(d) = ASSEDIT_MAIN_MENU;

return;
}

/***************************************************
   Command Parse
 ***************************************************/

void assedit_parse(struct descriptor_data *d, char *arg)
{
 int pos = 0, i = 0,  counter, columns = 0;
 char buf[MAX_STRING_LENGTH];

   COMPONENT   *pTComponents = NULL;

 switch (OLC_MODE(d)) {

   case ASSEDIT_MAIN_MENU:
     switch (*arg) {
     case 'q':
     case 'Q':                /* do the quit stuff */
       /* Ok, Time to save it back to the original stuff and get out */
       /* due to the infrequent use of this code and restricted use  */
       /* I decided to copy over changes regarless.                  */
       assemblyDestroy(OLC_ASSEDIT(d)->lVnum);
       assemblyCreate(OLC_ASSEDIT(d)->lVnum, OLC_ASSEDIT(d)->uchAssemblyType);
       for( i = 0; i < OLC_ASSEDIT(d)->lNumComponents; i++) {
           assemblyAddComponent(OLC_ASSEDIT(d)->lVnum,
                                OLC_ASSEDIT(d)->pComponents[i].lVnum,
                                OLC_ASSEDIT(d)->pComponents[i].bExtract,
                                OLC_ASSEDIT(d)->pComponents[i].bInRoom
                                );
            }
       new_send_to_char(d->character, "\r\nSaving all assemblies\r\n");
       assemblySaveAssemblies();

/*       free(pTComponents);
       free(OLC_ASSEDIT(d));
*/
       cleanup_olc(d, CLEANUP_ALL);    /* for right now we just get out! */
      break;

     case 't':
     case 'T':
         get_char_colors(d->character);
#if defined(CLEAR_SCREEN)
sprintf(buf, "%c[H%c[J", 27, 27);
new_send_to_char(d->character, buf);
#endif
      for (counter = 0; counter < MAX_ASSM; counter++) {
           sprintf(buf, "%s%2d%s) %-20.20s %s", grn, counter + 1, nrm,
                AssemblyTypes[counter], !(++columns % 2) ? "\r\n" : "");
           new_send_to_char(d->character, buf);
           }
           new_send_to_char(d->character, "Enter the assembly type : ");
     OLC_MODE(d) = ASSEDIT_EDIT_TYPES;

     break;
     case 'a':
     case 'A':                /* add a new component */
       new_send_to_char(d->character, "\r\nWhat is the vnum of the new component?");
       OLC_MODE(d) = ASSEDIT_ADD_COMPONENT;
      break;

     case 'e':
     case 'E':                /* edit a component */
       new_send_to_char(d->character, "\r\nEdit which component? ");
       OLC_MODE(d) = ASSEDIT_EDIT_COMPONENT;
      break;
     case 'd':
     case 'D':                /* delete a component */
       if ((pos < 0) || pos > OLC_ASSEDIT(d)->lNumComponents) {
           new_send_to_char(d->character, "\r\nWhich component do you wish to remove?");
           assedit_disp_menu(d);
       } else {
           new_send_to_char(d->character, "\r\nWhich component do you wish to remove?");
           OLC_MODE(d) = ASSEDIT_DELETE_COMPONENT;
       }
      break;

     default:
       assedit_disp_menu(d);
    }
   break;

 case ASSEDIT_EDIT_TYPES:
   if (isdigit(*arg)){
    pos = atoi(arg) - 1;
     if( (pos >= 0) || (pos < MAX_ASSM)) {
           OLC_ASSEDIT(d)->uchAssemblyType = pos;
     assedit_disp_menu(d);
     break;
    }
   }
   else
   assedit_disp_menu(d);

 break;
 case ASSEDIT_ADD_COMPONENT:              /* add a new component */
   if (isdigit(*arg)){
      pos = atoi(arg);
#if CIRCLE_UNSIGNED_INDEX
     if ((real_object(pos)) == NOTHING)    /* does the object exist? */
         break;
#else
     if ((real_object(pos)) <= NOTHING)    /* does the object exist? */
         break;
#endif

     for ( i = 0; i < OLC_ASSEDIT(d)->lNumComponents; i++) {
        if(OLC_ASSEDIT(d)->pComponents[i].lVnum == pos)
          break;
       }

     CREATE( pTComponents, COMPONENT, OLC_ASSEDIT(d)->lNumComponents + 1);

     if(OLC_ASSEDIT(d)->pComponents != NULL) {          /* Copy from olc to temp */
        memmove(pTComponents, OLC_ASSEDIT(d)->pComponents,
                  OLC_ASSEDIT(d)->lNumComponents * sizeof(COMPONENT) );
/*        free(OLC_ASSEDIT(d)->pComponents); */
       }

     OLC_ASSEDIT(d)->pComponents = pTComponents;
     OLC_ASSEDIT(d)->pComponents[ OLC_ASSEDIT(d)->lNumComponents ].lVnum = pos;
     OLC_ASSEDIT(d)->pComponents[ OLC_ASSEDIT(d)->lNumComponents ].bExtract = YES;
     OLC_ASSEDIT(d)->pComponents[ OLC_ASSEDIT(d)->lNumComponents ].bInRoom = NO;
     OLC_ASSEDIT(d)->lNumComponents += 1;

     assedit_disp_menu(d);

   } else {
     new_send_to_char(d->character, "That object does not exist. Please try again\r\n");
     assedit_disp_menu(d);
     }
  break;

 case ASSEDIT_EDIT_COMPONENT:
   pos = atoi(arg);
   if (isdigit(*arg)) {
      pos--;
      OLC_VAL(d) = pos;
      assedit_edit_extract(d);
      break;
    }
   else
      assedit_disp_menu(d);
   break;

 case ASSEDIT_DELETE_COMPONENT:

  if (isdigit(*arg)) {
    pos = atoi(arg);
    pos -= 1;

    CREATE( pTComponents, COMPONENT, OLC_ASSEDIT(d)->lNumComponents -1);

    if( pos > 0 )
      memmove( pTComponents, OLC_ASSEDIT(d)->pComponents, pos * sizeof( COMPONENT ) );

    if( pos < OLC_ASSEDIT(d)->lNumComponents - 1 )
       memmove( pTComponents + pos, OLC_ASSEDIT(d)->pComponents + pos + 1,
            (OLC_ASSEDIT(d)->lNumComponents - pos - 1) * sizeof(COMPONENT) );

    free(OLC_ASSEDIT(d)->pComponents );
    OLC_ASSEDIT(d)->pComponents = pTComponents;
    OLC_ASSEDIT(d)->lNumComponents -= 1;

    assedit_disp_menu(d);
    break;
  } else
    assedit_disp_menu(d);
  break;

 case ASSEDIT_EDIT_EXTRACT:
  switch (*arg) {
    case 'y':
    case 'Y':
      OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bExtract = TRUE;
      assedit_edit_inroom(d);
    break;

    case 'n':
    case 'N':
      OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bExtract = FALSE;
      assedit_edit_inroom(d);
    break;

    default:
      new_send_to_char(d->character, "Is the item to be extracted when the assembly is created? (Y/N)");
    break;
    }
   break;
 case ASSEDIT_EDIT_INROOM:
  switch (*arg) {
    case 'y':
    case 'Y':
      OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bInRoom = TRUE;
      assedit_disp_menu(d);
    break;

    case 'n':
    case 'N':
      OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bInRoom = FALSE;
      assedit_disp_menu(d);
    break;

    default:
      new_send_to_char(d->character, "Object in the room when assembly is created? (n =  in inventory):");
    break;
    }
 break;

 default:                        /* default for whole assedit parse function */
                                 /* we should never get here */
 new_mudlog(BRF, LVL_GOD, TRUE, "SYSERR: OLC assedit_parse(): Reached default case!");
 new_send_to_char(d->character, "Opps...\r\n");
 STATE(d) = CON_PLAYING;
 break;
 }
}
/* End of Assedit Parse */

void assedit_delete(struct descriptor_data *d)
{
 new_send_to_char(d->character, "Which item number do you wish to delete from this assembly?");
 OLC_MODE(d) = ASSEDIT_DELETE_COMPONENT;
 return;
}


void assedit_edit_extract(struct descriptor_data *d)
{
 new_send_to_char(d->character, "Is the item to be extracted when the assembly is created? (Y/N):");
 OLC_MODE(d) = ASSEDIT_EDIT_EXTRACT;
 return;
}

void assedit_edit_inroom(struct descriptor_data *d)
{
 new_send_to_char(d->character, "Should the object be in the room when assembly is created (n = in inventory)?");
 OLC_MODE(d) = ASSEDIT_EDIT_INROOM;
 return;
}

void nodigit(struct descriptor_data *d)
{
  new_send_to_char(d->character, "Usage: assedit <vnum>\r\n");
  new_send_to_char(d->character, "     : assedit new <vnum>\r\n");
  new_send_to_char(d->character, "     : assedit delete <vnum>\r\n");
  return;
}


