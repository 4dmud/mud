/* ************************************************************************
*   File: weather.c                                     Part of CircleMUD *
*  Usage: functions handling time and the weather                         *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *4d
************************************************************************ */

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "handler.h"
#include "interpreter.h"
#include "db.h"

extern struct time_info_data time_info;

void weather_and_time(int mode);
void another_hour(int mode);
void weather_change(void);
int find_month(void);
int get_temperature(room_rnum room);
/*
static struct sect_temperature_data {
int min;
int max;
int stdev;
int average;
} sect_temp = {
{ 20, 30, 1, 24},//SECT_INSIDE         
{ 15, 30, 2, 21},//SECT_CITY           
{  0,  0, 0,  0},//SECT_FIELD          
{  0,  0, 0,  0},//SECT_FOREST         
{  0,  0, 0,  0},//SECT_HILLS          
{  0,  0, 0,  0},//SECT_MOUNTAIN       
{  0,  0, 0,  0},//SECT_WATER_SWIM     
{  0,  0, 0,  0},//SECT_WATER_NOSWIM   
{  0,  0, 0,  0},//SECT_UNDERWATER	    
{  0,  0, 0,  0},//SECT_FLYING         
{  0,  0, 0,  0},//SECT_DESERT         
{  0,  0, 0,  0},//SECT_SPACE	
{  0,  0, 0,  0},//SECT_ROAD	
{  0,  0, 0,  0},//SECT_ENTRANCE	
{  0,  0, 0,  0},//SECT_ATMOSPHERE 
{  0,  0, 0,  0},//SECT_SUN	
{  0,  0, 0,  0},//SECT_BLACKHOLE	
{  0,  0, 0,  0},//SECT_VEHICLE	
{  0,  0, 0,  0},//SECT_SWAMP
{  0,  0, 0,  0},//SECT_REEF  
{  0,  0, 0,  0},//SECT_TUNDRA
{  0,  0, 0,  0},//SECT_SNOW
{  0,  0, 0,  0},//SECT_ICE
};
*/
int get_temperature(room_rnum room) {
int temp = 24;
if (!VALID_ROOM_RNUM(room))
return 0;

switch (SECT(room)) {

}

return temp;
}

int temperature_affect(int temp) {
if (temp < -12)
return TAFF_DEATHLY_COLD;
else if (temp < 0)
return TAFF_DIRE_FREEZE;
else if (temp < 12)
return TAFF_FREEZE;
else if (temp < 24)
return TAFF_CHILL;
else if (temp <= 30)
return TAFF_OPTIMAL;
else if (temp < 39)
return TAFF_WARM;
else if (temp < 45)
return TAFF_HOT;
else if (temp < 60)
return  TAFF_DIRE_HEAT;
else 
return TAFF_DEATHLY_HOT;
}

void weather_and_time(int mode)
{
    another_hour(mode);
    if (mode)
	weather_change();
}

int find_month(void) {
return time_info.month;
}

void another_hour(int mode)
{
    time_info.hours++;

    if (mode) {
	switch (time_info.hours) {
	case 5:
	    sunlight = SUN_RISE;
	    send_to_outdoor("The sun rises in the east.\r\n");
	    break;
	case 6:
	    sunlight = SUN_LIGHT;
	    send_to_outdoor("The day has begun.\r\n");
	    break;
	case 21:
	    sunlight = SUN_SET;
	    send_to_outdoor("The sun slowly disappears in the west.\r\n");
	    break;
	case 22:
	    sunlight = SUN_DARK;
	    send_to_outdoor("The night has begun.\r\n");
	    break;
	default:
	    break;
	}
    }
    if (time_info.hours > 23) {	/* Changed by HHS due to bug ??? */
	time_info.hours -= 24;
	time_info.day++;

	if (time_info.day > 34) {
	    time_info.day = 0;
	    time_info.month++;

	    if (time_info.month > 16) {
		time_info.month = 0;
		time_info.year++;
	    }
	}
    }
}


void weather_change(void)
{
    int diff, change, i;
    struct descriptor_data *j;
    char buf[MAX_STRING_LENGTH];

    for (i = 0; i <= top_of_zone_table; i++) {

	if ((time_info.month >= 9) && (time_info.month <= 16))
	    diff = (zone_table[i].pressure > 985 ? -2 : 2);
	else
	    diff = (zone_table[i].pressure > 1015 ? -2 : 2);

	zone_table[i].change +=
	    (dice(1, 4) * diff + dice(2, 6) - dice(2, 6));
	zone_table[i].change = MIN(zone_table[i].change, 12);
	zone_table[i].change = MAX(zone_table[i].change, -12);

	zone_table[i].pressure += zone_table[i].change;
	zone_table[i].pressure = MIN(zone_table[i].pressure, 1040);
	zone_table[i].pressure = MAX(zone_table[i].pressure, 960);

	change = 0;

	switch (zone_table[i].sky) {
	case SKY_CLOUDLESS:
	    if (zone_table[i].pressure < 990)
		change = 1;
	    else if (zone_table[i].pressure < 1010)
		if (dice(1, 4) == 1)
		    change = 1;
	    break;
	case SKY_CLOUDY:
	    if (zone_table[i].pressure < 970)
		change = 2;
	    else if (zone_table[i].pressure < 990)
		if (dice(1, 4) == 1)
		    change = 2;
		else
		    change = 0;
	    else if (zone_table[i].pressure > 1030)
		if (dice(1, 4) == 1)
		    change = 3;

	    break;
	case SKY_RAINING:
	    if (zone_table[i].pressure < 970)
		if (dice(1, 4) == 1)
		    change = 4;
		else
		    change = 0;
	    else if (zone_table[i].pressure > 1030)
		change = 5;
	    else if (zone_table[i].pressure > 1010)
		if (dice(1, 4) == 1)
		    change = 5;

	    break;
	case SKY_LIGHTNING:
	    if (zone_table[i].pressure > 1010)
		change = 6;
	    else if (zone_table[i].pressure > 990)
		if (dice(1, 4) == 1)
		    change = 6;

	    break;
	default:
	    change = 0;
	    zone_table[i].sky = SKY_CLOUDLESS;
	    break;
	}

	switch (change) {
	case 0:
	    break;
	case 1:
	    sprintf(buf, "The sky starts to get cloudy.\r\n");
	    zone_table[i].sky = SKY_CLOUDY;
	    break;
	case 2:
	    sprintf(buf, "It starts to rain.\r\n");
	    zone_table[i].sky = SKY_RAINING;
	    break;
	case 3:
	    sprintf(buf, "The clouds disappear.\r\n");
	    zone_table[i].sky = SKY_CLOUDLESS;
	    break;
	case 4:
	    sprintf(buf, "Lightning starts to show in the sky.\r\n");
	    zone_table[i].sky = SKY_LIGHTNING;
	    break;
	case 5:
	    sprintf(buf, "The rain stops.\r\n");
	    zone_table[i].sky = SKY_CLOUDY;
	    break;
	case 6:
	    sprintf(buf, "The lightning stops.\r\n");
	    zone_table[i].sky = SKY_RAINING;
	    break;
	default:
	    break;
	}

	if ((change >= 1) && (change <= 6)) {
	    for (j = descriptor_list; j; j = j->next)
		if (!j->connected && j->character && AWAKE(j->character) &&
		    OUTSIDE(j->character) &&
		    (zone_table[j->character->in_room->zone].number == i))
		    SEND_TO_Q(buf, j);
	}

    }
}

char *print_weather(room_rnum room, char *buf, size_t len) {
  if (room==NULL) {
     *buf = '\0';
     return buf;
     }
  if ((ROOM_FLAGGED(room, ROOM_INDOORS)||SECT(room)==SECT_INSIDE))
  {
    snprintf(buf, len, "Inside");
    return buf;
  }

  switch (zone_table[room->zone].sky) {
  case SKY_CLOUDLESS:
  if (IS_DAY)
  snprintf(buf, len, "Sunny");
  else
  snprintf(buf, len, "Cloudless");
  break;
  case SKY_CLOUDY:
  snprintf(buf, len, "Cloudy");
  break;
  case SKY_RAINING:
  snprintf(buf, len, "Raining");
  break;
  case SKY_LIGHTNING:
  snprintf(buf, len, "Stormy");
  break;
  default:
  *buf = '\0';
  break;
  }
  return buf;
}
