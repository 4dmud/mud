/************************************************************************
 *  OasisOLC - InGame OLC Deletion				v2.0	*
 *  Original author: Levork						*
 *  Copyright 1996 Harvey Gilpin					*
 *  Copyright 1997-2001 George Greer (greerga@circlemud.org)		*
 *  Copyright 2002 Kip Potter [Mythran] (kip_potter@hotmail.com)	*
 ************************************************************************/
 
/* 
   +-----------------------------------------------------------------------+
   | As of right now, all I have made is the ability to delete rooms.      |
   | Deleting the rest of the area (objects, zones, mobiles) will be       |
   | a little more difficult.  This is because they are broader and        |
   | deleting one is more tedious (but not impossible).  I will (hopefully)|
   | be adding more deletion code after this patch.                        |
   |   -- Mythran                                                          |
   +-----------------------------------------------------------------------+
*/                                                                       



#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "genolc.h"
#include "oasis.h"
#include "improved-edit.h"
#include "genwld.h"

/* Internal Functions */
int free_strings(void *data, int type);

/*
 * remove_room_from_memory has been removed itself :P
 * It was a duplication of delete_room() in genwld.c
 * -- Welcor
 */
 
/************************************************************************\
 ** Description :                                                      **
 **   Free's strings from any object, room, mobiles, or player.        **
 **                                                                    **
 ** Return Value:                                                      **
 **   TRUE if successful, otherwise, it returns FALSE.                 **
 **                                                                    **
 ** Parameters  :                                                      **
 **   type - The OLC type constant relating to the data type of data.  **
\************************************************************************/
int free_strings(void *data, int type)
{
  Room *room;
  struct config_data *config;
  int i;
  
  switch (type) {
    case OASIS_WLD:
      room = (Room *) data;
      
      /* Free Descriptions */
      return room->free_room_strings();
    case OASIS_EXI:
      room = (Room *) data;
      
      for (i = 0; i < NUM_OF_DIRS; i++) {
        if (room->dir_option[i]) {
          if (room->dir_option[i]->general_description)
            free(room->dir_option[i]->general_description);
            
          if (room->dir_option[i]->keyword)
            free(room->dir_option[i]->keyword);
          
          free(room->dir_option[i]);
        }
      }
      
      return (TRUE);
    
    case OASIS_MOB:
    case OASIS_OBJ:
      return (FALSE);		/* For now... */
    
    case OASIS_CFG:
      config = (struct config_data *) data;
      
      if (config->play.OK)
        free(config->play.OK);
        
      if (config->play.NOPERSON)
        free(config->play.NOPERSON);
        
      if (config->play.NOEFFECT)
        free(config->play.NOEFFECT);
      
      if (config->operation.DFLT_IP)
        free(config->operation.DFLT_IP);
        
      if (config->operation.DFLT_DIR)
        free(config->operation.DFLT_DIR);
        
      if (config->operation.LOGNAME)
        free(config->operation.LOGNAME);
        
      if (config->operation.MENU)
        free(config->operation.MENU);
        
      if (config->operation.WELC_MESSG)
        free(config->operation.WELC_MESSG);
        
      if (config->operation.START_MESSG)
        free(config->operation.START_MESSG);
      
      return (TRUE);
    
    default:
      new_mudlog(BRF, LVL_GOD, TRUE, "SYSERR: oasis_delete.c: free_strings: Invalid type handled (Type %d).", type);
      return (FALSE);
  }
}
