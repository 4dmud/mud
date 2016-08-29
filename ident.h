#ifndef IDENT_H
#define IDENT_H
/********************************************************************
 *
 *  File    : ident.h
 *  Location: /home/asterix/pt94/pt94jpi/mud/raven/src/ident.h
 *
 *  Created : 97/03/31
 *  Author  : Joachim Pileborg
 *  E-Mail  : pt94jpi@student.hk-r.se
 *
 *  Comments:
 *
 *  Copyright (C) 1997 by Joachim Pileborg
 *  All rights reserved.
 *
 * $Id: ident.h,v 1.2 2006/05/22 10:50:49 w4dimenscor Exp $
 */

#include <netinet/in.h>

/*******************************************************************/

struct message
{
  int                type;
  struct sockaddr_in addr;
  int                fd;
  char               host[256];
  char               user[256];
};

#define MSG_ERROR -1
#define MSG_NOP    0  /* don't do anything */
#define MSG_IDENT  1  /* lookup hostname and username for a user */
#define MSG_QUIT   2  /* kill the lookup process */
#define MSG_IDREP  3  /* reply from the lookup process */

/*******************************************************************/

void id_lookup(Descriptor *d);
int  id_init(void);
void id_kill(void);

extern int id_serv_socket;

/*******************************************************************/

#endif
