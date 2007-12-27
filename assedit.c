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
#include "descriptor.h"

/*-------------------------------------------------------------------*
 * External data structures.
 *-------------------------------------------------------------------*/
extern const char *AssemblyTypes[];

/*-------------------------------------------------------------------*
 * Function prototypes.
 *-------------------------------------------------------------------*/
void assedit_setup(Descriptor *d, int number);
void assedit_disp_menu(Descriptor *d);
void assedit_delete(Descriptor *d);
void assedit_edit_extract(Descriptor *d);
void assedit_edit_inroom(Descriptor *d);
void nodigit(Descriptor *d);
const char *compIn(int i);


/*-------------------------------------------------------------------*
 * Nasty internal macros to clean up the code.
 *-------------------------------------------------------------------*/
long lRnum = 0;

/*-------------------------------------------------------------------*
 * Assedit command
 *-------------------------------------------------------------------*/

ACMD (do_assedit)
{
  Descriptor *d = ch->desc;
  char buf[MAX_STRING_LENGTH];
  char buf2[MAX_STRING_LENGTH];

  *buf = '\0';  /* If I run into problems then take this sucker out */
  *buf2 = '\0';

  if (IS_NPC(ch))
    return;
  if (GET_LEVEL(ch) < LVL_IMPL)
    ch->Send( "You do not have permission to do that.\r\n");

  for (d = descriptor_list; d; d = d->next)
  {
    if (d->connected == CON_ASSEDIT)
    {
      ch->Send( "Assemblies are already being editted by someone.\r\n");
      return;
    }
  }

  two_arguments(argument, buf, buf2);

  d= ch->desc;

  if(!*buf)
  {
    nodigit(d);
    return;
  }

  if (!isdigit(*buf))
  {
    if (strn_cmp("new", buf, 3) == 0)
    {
      if(!isdigit(*buf2))
        nodigit(d);
      else
      {
        assemblyCreate(atoi(buf2), 0);
        d->Output("Assembly Created.\r\n");
        assemblySaveAssemblies();
        return;
      }
    }
    else
      if (strn_cmp("delete", buf, 6) == 0)
      {
        if (!isdigit(*buf2))
          nodigit(d);
        else
        {
          assemblyDestroy(atoi(buf2));
          d->Output("Assembly Deleted.\r\n");
          assemblySaveAssemblies();
          return;
        }
      }
      else
      {
        nodigit(d);
        return;
      }
  }
  else
    if (isdigit(*buf))
    {
      d = ch->desc;
      CREATE (d->olc, struct oasis_olc_data, 1);
      assedit_setup(d, atoi(buf));

    }
  return;
}

/*-------------------------------------------------------------------*
 * Assedit Functions
 *-------------------------------------------------------------------*/

void assedit_setup(Descriptor *d, int num)
{

  ASSEMBLY    *pOldAssembly = NULL;
  CREATE(OLC_ASSEDIT(d), ASSEMBLY, 1 );


  if( (pOldAssembly = assemblyGetAssemblyPtr( num )) == NULL )
  {
    d->Output("That assembly does not exist\r\n");
    cleanup_olc(d, CLEANUP_ALL);
    return;
  }
  else
  {
    /* Copy the old assembly. */
    OLC_ASSEDIT(d)->lVnum = pOldAssembly->lVnum;
    OLC_ASSEDIT(d)->uchAssemblyType = pOldAssembly->uchAssemblyType;
    OLC_ASSEDIT(d)->lNumComponents = pOldAssembly->lNumComponents;

    if( OLC_ASSEDIT(d)->lNumComponents > 0 )
    {
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
    d->Output("Assembled item may not exist, check the vnum and assembles (show assemblies). \r\n");
    cleanup_olc(d, CLEANUP_ALL);    /* for right now we just get out! */
    return;
  }

  STATE(d) = CON_ASSEDIT;
  act("$n starts editing assemblies.", TRUE, d->character, 0, 0, TO_ROOM);
  SET_BIT_AR(PLR_FLAGS(d->character), PLR_WRITING);
  assedit_disp_menu(d);

}

void assedit_disp_menu(Descriptor *d)
{
  int i = 0;
  extern const char *AssemblyTypes[];
  char szAssmType[MAX_INPUT_LENGTH] = { '\0' };

  get_char_colors(d->character);

  sprinttype(OLC_ASSEDIT(d)->uchAssemblyType, AssemblyTypes, szAssmType, sizeof(szAssmType));

#if defined(CLEAR_SCREEN)
  d->Output( "%c[H%c[J", 27, 27);
#endif

  d->Output(
                  "   Assembly Number:%s %ld %s\r\n"
                  "   Assembly Name  :%s %s %s \r\n"
                  "   ---------------------------\r\n"
                  "%sT%s) Assembly Type  :%s %s %s\r\n\r\n"
                  "Components:\r\n",
                  yel,  OLC_ASSEDIT(d)->lVnum, nrm,
                  yel,  obj_proto[ real_object(OLC_ASSEDIT(d)->lVnum) ].short_description, nrm,
                  grn, nrm,yel,  szAssmType, nrm);

  if(OLC_ASSEDIT(d)->lNumComponents <= 0)
    d->Output( "   < NONE > \r\n");
  else
  {
    for( i = 0; i < OLC_ASSEDIT(d)->lNumComponents; i++ )
    {
      if ( (lRnum = real_object(OLC_ASSEDIT(d)->pComponents[i].lVnum)) < 0)
      {
        d->Output(
                        "%s %d%s) %s ERROR --- Contact an Implimentor %s\r\n ",
                        grn, i+1, nrm, yel, nrm
                       );
      }
      else
      {
        d->Output(
                        "%s %d%s) [%5ld] %-20.20s  In:%s %-3.3s %s   Extract:%s %-3.3s%s \r\n",
                        grn,  i+1, nrm,
                        OLC_ASSEDIT(d)->pComponents[i].lVnum,
                        obj_proto[ lRnum ].short_description,
                        yel, (compIn(OLC_ASSEDIT(d)->pComponents[ i ].bInRoom)), nrm,
                        yel, (OLC_ASSEDIT(d)->pComponents[ i ].bExtract ? "Yes" : "No"), nrm           );
      }
    }
  }
  d->Output(
                  "%sA%s) Add a new component.\r\n"
                  "%sE%s) Edit a component.\r\n"
                  "%sD%s) Delete a component.\r\n"
                  "%sQ%s) Quit.\r\n"
                  "\r\nEnter your choice : ",
                  grn, nrm, grn, nrm, grn, nrm, grn, nrm
                 );
  OLC_MODE(d) = ASSEDIT_MAIN_MENU;

  return;
}

/***************************************************
   Command Parse
 ***************************************************/

void assedit_parse(Descriptor *d, char *arg)
{
  int pos = 0, i = 0,  counter, columns = 0;

  COMPONENT   *pTComponents = NULL;

  switch (OLC_MODE(d))
  {

  case ASSEDIT_MAIN_MENU:
    switch (*arg)
    {
    case 'q':
    case 'Q':                /* do the quit stuff */
      /* Ok, Time to save it back to the original stuff and get out */
      /* due to the infrequent use of this code and restricted use  */
      /* I decided to copy over changes regardless.                 */
      assemblyDestroy(OLC_ASSEDIT(d)->lVnum);
      assemblyCreate(OLC_ASSEDIT(d)->lVnum, OLC_ASSEDIT(d)->uchAssemblyType);
      for( i = 0; i < OLC_ASSEDIT(d)->lNumComponents; i++)
      {
        assemblyAddComponent(OLC_ASSEDIT(d)->lVnum,
                             OLC_ASSEDIT(d)->pComponents[i].lVnum,
                             OLC_ASSEDIT(d)->pComponents[i].bExtract,
                             OLC_ASSEDIT(d)->pComponents[i].bInRoom
                            );
      }

      d->Output( "\r\nSaving all assemblies\r\n");
      assemblySaveAssemblies();
      if (pTComponents)
        free(pTComponents);
      if (OLC_ASSEDIT(d)->pComponents)
        free(OLC_ASSEDIT(d)->pComponents);
      free(OLC_ASSEDIT(d));
      OLC_ASSEDIT(d) = NULL;

      cleanup_olc(d, CLEANUP_ALL);    /* for right now we just get out! */
      break;

    case 't':
    case 'T':
      get_char_colors(d->character);
#if defined(CLEAR_SCREEN)
      d->Output( "%c[H%c[J", 27, 27);
#endif
      for (counter = 0; counter < MAX_ASSM; counter++)
      {
        d->Output( "%s%2d%s) %-20.20s %s", grn, counter + 1, nrm,
                        AssemblyTypes[counter], !(++columns % 2) ? "\r\n" : "");
      }
      d->Output( "Enter the assembly type : ");
      OLC_MODE(d) = ASSEDIT_EDIT_TYPES;

      break;
    case 'a':
    case 'A':                /* add a new component */
      d->Output("\r\nWhat is the vnum of the new component?");
      OLC_MODE(d) = ASSEDIT_ADD_COMPONENT;
      break;

    case 'e':
    case 'E':                /* edit a component */
      d->Output( "\r\nEdit which component? ");
      OLC_MODE(d) = ASSEDIT_EDIT_COMPONENT;
      break;
    case 'd':
    case 'D':                /* delete a component */
      if ((pos < 0) || pos > OLC_ASSEDIT(d)->lNumComponents)
      {
        d->Output( "\r\nWhich component do you wish to remove?");
        assedit_disp_menu(d);
      }
      else
      {
        d->Output( "\r\nWhich component do you wish to remove?");
        OLC_MODE(d) = ASSEDIT_DELETE_COMPONENT;
      }
      break;

    default:
      assedit_disp_menu(d);
    }
    break;

  case ASSEDIT_EDIT_TYPES:
    if (isdigit(*arg))
    {
      pos = atoi(arg) - 1;
      if( (pos >= 0) || (pos < MAX_ASSM))
      {
        OLC_ASSEDIT(d)->uchAssemblyType = pos;
        assedit_disp_menu(d);
        break;
      }
    }
    else
      assedit_disp_menu(d);

    break;

  case ASSEDIT_ADD_COMPONENT:              /* add a new component */
    if (isdigit(*arg))
    {
      pos = atoi(arg);
#if CIRCLE_UNSIGNED_INDEX
      if ((real_object(pos)) == NOTHING)    /* does the object exist? */
        break;
#else
      if ((real_object(pos)) <= NOTHING)    /* does the object exist? */
        break;
#endif

      for ( i = 0; i < OLC_ASSEDIT(d)->lNumComponents; i++)
      {
        if(OLC_ASSEDIT(d)->pComponents[i].lVnum == pos)
          break;
      }

      CREATE( pTComponents, COMPONENT, OLC_ASSEDIT(d)->lNumComponents + 1);

      if(OLC_ASSEDIT(d)->pComponents != NULL)
      {          /* Copy from olc to temp */
        memmove(pTComponents, OLC_ASSEDIT(d)->pComponents,
                OLC_ASSEDIT(d)->lNumComponents * sizeof(COMPONENT) );
        /*        free(OLC_ASSEDIT(d)->pComponents); */
      }

      OLC_ASSEDIT(d)->pComponents = pTComponents;
      OLC_ASSEDIT(d)->pComponents[ OLC_ASSEDIT(d)->lNumComponents ].lVnum = pos;
      OLC_ASSEDIT(d)->pComponents[ OLC_ASSEDIT(d)->lNumComponents ].bExtract = YES;
      OLC_ASSEDIT(d)->pComponents[ OLC_ASSEDIT(d)->lNumComponents ].bInRoom = 0;
      OLC_ASSEDIT(d)->lNumComponents += 1;

      assedit_disp_menu(d);

    }
    else
    {
      d->Output("That object does not exist. Please try again\r\n");
      assedit_disp_menu(d);
    }
    break;

  case ASSEDIT_EDIT_COMPONENT:
    pos = atoi(arg);
    if (isdigit(*arg))
    {
      pos--;
      OLC_VAL(d) = pos;
      assedit_edit_extract(d);
      break;
    }
    else
      assedit_disp_menu(d);
    break;

  case ASSEDIT_DELETE_COMPONENT:

    if (isdigit(*arg))
    {
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
    }
    else
      assedit_disp_menu(d);
    break;

  case ASSEDIT_EDIT_EXTRACT:
    switch (*arg)
    {
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
      d->Output( "Is the item to be extracted when the assembly is created? (Y/N)");
      break;
    }
    break;
  case ASSEDIT_EDIT_INROOM:
      OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bInRoom = atoi(arg);
      if (OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bInRoom > 3) {
      OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bInRoom = 0;
      d->Output( "Location of item:\r\n0 :Inventory\r\n1 :Room\r\n2 :Hold\r\n3 :Wield\r\nWhat do you select:");
      } else if (OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bInRoom < 0) {
      OLC_ASSEDIT(d)->pComponents[ OLC_VAL(d) ].bInRoom = 0;
      d->Output( "Location of item:\r\n0 :Inventory\r\n1 :Room\r\n2 :Hold\r\n3 :Wield\r\nWhat do you select:");
      } else
      assedit_disp_menu(d);
    break;

  default:                        /* default for whole assedit parse function */
    /* we should never get here */
    new_mudlog(BRF, LVL_GOD, TRUE, "SYSERR: OLC assedit_parse(): Reached default case!");
    d->Output( "Opps...\r\n");
    STATE(d) = CON_PLAYING;
    break;
  }
}
/* End of Assedit Parse */

void assedit_delete(Descriptor *d)
{
  d->Output( "Which item number do you wish to delete from this assembly?");
  OLC_MODE(d) = ASSEDIT_DELETE_COMPONENT;
  return;
}


void assedit_edit_extract(Descriptor *d)
{
  d->Output( "Is the item to be extracted when the assembly is created? (Y/N):");
  OLC_MODE(d) = ASSEDIT_EDIT_EXTRACT;
  return;
}

void assedit_edit_inroom(Descriptor *d)
{
  d->Output( "Location of item:\r\n0 :Inventory\r\n1 :Room\r\n2 :Hold\r\n3 :Wield\r\nWhat do you select:");
  OLC_MODE(d) = ASSEDIT_EDIT_INROOM;
  return;
}

void nodigit(Descriptor *d)
{
  d->Output( "Usage: assedit <vnum>\r\n");
  d->Output( "     : assedit new <vnum>\r\n");
  d->Output( "     : assedit delete <vnum>\r\n");
  return;
}


