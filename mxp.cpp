//
// C++ Implementation: descriptor
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "conf.h"
#include "sysdep.h"

#if CIRCLE_GNU_LIBC_MEMORY_TRACK
# include <mcheck.h>
#endif

#ifdef HAVE_ARPA_TELNET_H
#include <arpa/telnet.h>
#else
#include "telnet.h"
#endif

#ifdef CIRCLE_MACINTOSH       /* Includes for the Macintosh */
# define SIGPIPE 13
# define SIGALRM 14
/* GUSI headers */
# include <sys/ioctl.h>
/* Codewarrior dependant */
# include <SIOUX.h>
# include <console.h>
#endif

#ifdef CIRCLE_WINDOWS         /* Includes for Win32 */
# ifdef __BORLANDC__
#  include <dir.h>
# else                   /* MSVC */
#  include <direct.h>
# endif
# include <mmsystem.h>
#endif                   /* CIRCLE_WINDOWS */

#ifdef CIRCLE_AMIGA      /* Includes for the Amiga */
# include <sys/ioctl.h>
# include <clib/socket_protos.h>
#endif                   /* CIRCLE_AMIGA */

#ifdef CIRCLE_ACORN      /* Includes for the Acorn (RiscOS) */
# include <socklib.h>
# include <inetlib.h>
# include <sys/ioctl.h>
#endif


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "house.h"
#include "dg_scripts.h"
#include "screen.h"
#include "descriptor.h"
#include "mxp.h"


int Descriptor::count_mxp_tags (const int bMXP, const char *txt, int length)
{
  char c = '\0';
  const char * p;
  int count = 0;
  int bInTag = FALSE;
  int bInEntity = FALSE;
  
  for (p = txt, count = 0;
       length > 0;
       p++, length--)
  {
    c = *p;
    
    if (bInTag)  /* in a tag, eg. <send> */
    {
      if (!bMXP)
        count--;     /* not output if not MXP */
      if (c == MXP_ENDc)
        bInTag = FALSE;
    } /* end of being inside a tag */
    else if (bInEntity)  /* in a tag, eg. <send> */
    {
      if (!bMXP)
        count--;     /* not output if not MXP */
      if (c == ';')
        bInEntity = FALSE;
    } /* end of being inside a tag */
    else switch (c)
    {
      
    case MXP_BEGc:
      bInTag = TRUE;
      if (!bMXP)
        count--;     /* not output if not MXP */
      else
        count += 4;  /* allow for ESC [1z */
      break;
      
    case MXP_ENDc:   /* shouldn't get this case */
      if (!bMXP)
        count--;     /* not output if not MXP */
      break;
      
    case MXP_AMPc:
      bInEntity = TRUE;
      if (!bMXP)
        count--;     /* not output if not MXP */
      break;
      
    default:
      if (bMXP)
      {
        switch (c)
        {
        case '<':       /* < becomes &lt; */
        case '>':       /* > becomes &gt; */
          count += 3;
          break;
          
        case '&':
          count += 4;    /* & becomes &amp; */
          break;
          
        case '"':        /* " becomes &quot; */
          count += 5;
          break;
          
        } /* end of inner switch */
      }   /* end of MXP enabled */
    } /* end of switch on character */
    
  }   /* end of counting special characters */
  
  return count;
} /* end of count_mxp_tags */

/* size gets pre checked by count mxp tags */
string Descriptor::convert_mxp_tags (const int bMXP, string src)
{
  char c;
  int bInTag = FALSE;
  int bInEntity = FALSE;
  int srclen = src.size();
  string dest("");
  size_t ps;
  
  if (srclen == 0)
    return src;
  
  
  for (ps = 0; srclen > 0; ps++, srclen--)
  {
    c = src[ps];
    if (bInTag)  /* in a tag, eg. <send> */
    {
      if (c == MXP_ENDc)
      {
        bInTag = FALSE;
        if (bMXP)
          dest += ">";
      }
      else if (bMXP)
        dest += c;  /* copy tag only in MXP mode */
      /*else
        *pd = '\0';*/
    } /* end of being inside a tag */
    else if (bInEntity)  /* in a tag, eg. <send> */
    {
      if (bMXP)
        dest += c;  /* copy tag only in MXP mode */
      if (c == ';')
        bInEntity = FALSE;
    } /* end of being inside a tag */
    else
    {
      switch (c)
      {
      case MXP_BEGc:
        bInTag = TRUE;
        if (bMXP)
        {
        dest += MXPMODE(1);
          //memcpy (pd, MXPMODE (1), 4);
          //pd += 4;
          dest += "<";
        }
        break;
        
      case MXP_ENDc:    /* shouldn't get this case */
        if (bMXP)
          dest += ">";
        break;
        
      case MXP_AMPc:
        bInEntity = TRUE;
        if (bMXP)
          dest += "&";
        break;
        
      default:
        if (bMXP)
        {
          switch (c)
          {
          case '<':
            //memcpy (pd, "&lt;", 4);
            //pd += 4;
            dest += "&lt;";
            break;
            
          case '>':
            //memcpy (pd, "&gt;", 4);
            //pd += 4;
            dest += "&gt;";
            break;
            
          case '&':
            //memcpy (pd, "&amp;", 5);
            //pd += 5;
            dest += "&amp;";
            break;
            
          case '"':
            //memcpy (pd, "&quot;", 6);
            //pd += 6;
            dest += "&quot;";
            break;
            
          default:
            //*pd++ = c;
            dest += c;
            break;  /* end of default */
            
          } /* end of inner switch */
          
        }
        else
          //*pd++ = c;  /* not MXP - just copy character */
          dest += c;
        break;
        
      } /* end of switch on character */
    } /*end of else */
    
  }   /* end of converting special characters */
  //*pd = 0;

  return dest;
} /* end of convert_mxp_tags */



/* set up MXP */
void Descriptor::turn_on_mxp ()
{
  this->mxp = TRUE;  /* turn it on now */
  //mccp_off(d);
  this->Output( "%s", start_mxp_str);
  this->Output( "%s", MXPTAG("support"));
  this->Output( "%s", MXPMODE(6) );   // permanent secure mode
  this->Output( "%s", MXPTAG("FRAME Name=\"Map\" Left=\"-16c\" Top=\"0\" Width=\"16c\" Height=\"15c\")"));
  this->Output( "%s", MXPTAG("!ELEMENT Ex '<send href=\"&text;\">' ATT=\"text\"   FLAG=RoomExit"));
  this->Output( "%s", MXPTAG("!ELEMENT VEx '<send href=\"drive &text;\">' ATT=\"text\"  FLAG=RoomExit"));
  this->Output( "%s", MXPTAG("!ELEMENT Player \"<send href='tell &name; |ignore &name;' "
                          "hint='Tell &name; something or ignore them|Tell &name; |Ignore &name;'  prompt>\" "
                          "ATT=\"name\""));
  this->Output( "%s", MXPTAG("!ELEMENT affect '<COLOR &col;>' ATT='col=whitesmoke'"));
  this->Output( "%s", MXPTAG("!ELEMENT LookAt '<send href=\"look at &at;\">'"));
  this->Output( "%s", MXPTAG("!ELEMENT Read '<send href=\"read at &at;\">'"));
  
#if 0
  /* Room description tag */
  this->Output( "%s", MXPTAG ("!ELEMENT rdesc '<p>' FLAG=RoomDesc"));
  
  /* Get an item tag (for things on the ground) */
  this->Output( "%s",
                   MXPTAG("!ELEMENT GroundItem \"<send href='"
    "get &name;|"
    "examine &name;|"
    "drink &name;"
    "' "
    "hint='RH mouse click to use this object|"
    "Get &desc;|"
    "Examine &desc;|"
    "Drink from &desc;"
    "'>\" ATT='name desc'"));
  /* Drop an item tag (for things in the inventory) */
  this->Output( "%s",  MXPTAG
                   ("!ELEMENT InvenItem \"<send href='"
    "drop &name;|"
    "examine &name;|"
    "wear &name;|"
    "eat &name;|"
    "drink &name;"
    "' "
    "hint='RH mouse click to use this object|"
    "Drop &desc;|"
    "Examine &desc;|"
    "Wear &desc;|"
    "Eat &desc;|"
    "Drink &desc;"
    "'>\" ATT='name desc'"));
  this->Output( "%s", MXPTAG
                   ("!ELEMENT List \"<send href='"
    "buy &name;| "
    "id &name;"
    "' "
    "hint='Buy &desc;|Id &desc;'>\" "
    "ATT='name desc'"));
#endif
  /* Player tag (for who lists, tells etc.) */
  
  /* List an item tag (for things in a shop) */
  //this->Output( "%s", MXPTAG("GAUGE HP Max=XHP Caption='Hp:' Color='red'"));
  //this->Output( "%s", MXPTAG("GAUGE MANA Max=XMANA Caption='Mn:' Color='cyan'"));
  //this->Output( "%s", MXPTAG("GAUGE MOVE Max=XMOVE Caption='Mv:' Color='green'"));
  //this->Output( "%s", MXPTAG("GAUGE STAM Max=XSTAM Caption='St:' Color='white'"));
  //this->Output( "%s", MXPTAG("FRAME Name='Map' Left='-20c' Top='0' Width='20c' Height='20c'"));
  
  
  
} /* end of turn_on_mxp */


char * Descriptor::send_mxp_status()
{
  static char mxpstat[MAX_MXP_STATUS];
  size_t len = 0;
  int hp = 0, mhp = 0;
  int mana = 0, mmana = 0;
  int move = 0, mmove = 0;
  int stam = 0, mstam = 0;
  if (this->character)
  {
    hp = GET_HIT(this->character);
    mana = GET_MANA(this->character);
    move = GET_MOVE(this->character);
    stam = GET_STAMINA(this->character);
    
    mhp = GET_MAX_HIT(this->character);
    mmana = GET_MAX_MANA(this->character);
    mmove = GET_MAX_MOVE(this->character);
    mstam = GET_MAX_STAMINA(this->character);
  }
  
  len = snprintf(mxpstat, sizeof(mxpstat), "%s", MXPMODE(6));
  len = snprintf(mxpstat + len, sizeof(mxpstat) - len, "<!ENTITY hp '%d'>" "<!ENTITY xhp '%d'>",hp,mhp);
  len = snprintf(mxpstat + len, sizeof(mxpstat) - len, "<!ENTITY mana '%d'>" "<!ENTITY xmana '%d'>",mana,mmana);
  len = snprintf(mxpstat + len, sizeof(mxpstat) - len, "<!ENTITY move '%d'>" "<!ENTITY xmove '%d'>",move,mmove);
  len = snprintf(mxpstat + len, sizeof(mxpstat) - len, "<!ENTITY stam '%d'>" "<!ENTITY xstam '%d'>",stam,mstam);
  /*this->Output(  "%s", MXPMODE(6));
  this->Output( MXP_BEG "!ENTITY hp '%d'" MXP_END MXP_BEG "!ENTITY xhp '%d'" MXP_END,hp,mhp);
  this->Output( MXP_BEG "!ENTITY mana '%d'" MXP_END MXP_BEG "!ENTITY xmana '%d'" MXP_END,mana,mmana);
  this->Output( MXP_BEG "!ENTITY move '%d'" MXP_END MXP_BEG "!ENTITY xmove '%d'" MXP_END,move,mmove);
  this->Output( MXP_BEG "!ENTITY stam '%d'" MXP_END MXP_BEG "!ENTITY xstam '%d'" MXP_END,stam,mstam);*/
  return mxpstat;
}
