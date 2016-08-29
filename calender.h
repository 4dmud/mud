//
// C++ Interface: calender
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//

#ifndef CALENDER_H
#define CALENDER_H

const char *date_event_type[] = {
    "Double Exp Day",
    "RP Event",
    "K.O.T.M"
};

struct date_event_data {
  int type;
  char desc[1024];
  int day;
  int month;
  int year;
  struct date_event_data *next;
};
#endif
