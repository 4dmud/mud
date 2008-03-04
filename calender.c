/*
*  C Implementation: calendar
*
* Description:
*
*
* Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
*
* Copyright: See COPYING file that comes with this distribution
*
*/
#include "conf.h"
#include "sysdep.h"

#include "structs.h"
#include "comm.h"
#include "interpreter.h"
#include "calender.h"
#include "db.h"
#include "utils.h"


#define EVENT_FILE "blah"

#define DAYS_IN_A_WEEK   7
#define MAX_COLS         6
#define BASE_YEAR   1990      /* Year from which all calculations are done.     */

/* Function declatations */
int calender_month ( Character *ch, int month, int year, int theday );
int start_day ( const int month, const int year );
int leap ( int );
void load_current_events();
int save_current_events();
void free_current_events();
void free_current_event ( struct date_event_data *evt );


extern struct time_info_data time_info;

/* Global variables.          */
struct date_event_data *calender_events = NULL;
char event_day_colour[10];
int days_in_month[]={0,31,28,31,30,31,30,31,31,30,31,30,31};


char * event_day ( int day, int month, int year )
{
	/** check through a database of dates for events. - mord **/
	return "";
}

ACMD ( do_calender )
{
	int i;                 /* General purpose var        */
	int year;                   /* year to start display */
	int num_of_months;               /* Number of months      */
	int start_month;            /* Start month           */
	int month;                  /* Current month         */
	int theday;
	time_t timer1 = time ( 0 );
	struct tm *tme = localtime ( &timer1 );

	/* Put command line parms into
	   meaningfull variable names */
	start_month   = tme->tm_mon+1;
	year          = tme->tm_year + 1900;
	theday        = tme->tm_mday;
	num_of_months = 3;

	/* AND OFF WE GO...      */
	month = start_month;
	for ( i=start_month; i < start_month + num_of_months ; i++ )
	{
		calender_month ( ch, month, year, i == start_month ? theday : -1 );   /* O/P one month.        */
		if ( month++ >= 12 )           /* Q. End of year?       */
		{
			/* Yes.                  */
			month = 1;
			year++;
		}
	}
}

/************************************************************************/
/*                    Calender_month                                    */
/*        Display one calender month.                  */
/************************************************************************/

int calender_month ( Character *ch, int month, int year, int theday )
{
	int i,j,count;              /* General Purpose variables. */

	/* dont forget - arrays start
	   at zero               */
	char *days[]=
	{
		" ",
		"Mon",
		"Tue",
		"Wed",
		"Thr",
		"Fri",
		"Sat",
		"Sun"
	};

	char *months[]=
	{
		" ",
		"January",
		"Febuary",
		"March",
		"April",
		"May",
		"June",
		"July",
		"August",
		"September",
		"October",
		"November",
		"December"
	};


	int month_map[8][MAX_COLS+1]={0};     /* init array with zeros */

	j=start_day ( month, year );     /* Get the day the month starts */
	/* Build the calender values  */
	i=1;count=0;
	while ( days_in_month[month] > count )
	{
		month_map[j++][i]=++count;          /* Build the table.      */
		/* Start a new week.          */
		if ( j > DAYS_IN_A_WEEK )
		{
			j=1;
			i++;
		}
	}
	/* O/P Title bar.        */
	ch->Send ( "\r\n%s %d\r\n",months[month], year );
	/* O/P the calender      */
	for ( j=1; j<=DAYS_IN_A_WEEK; j++ )
	{
		ch->Send ( "%5s ",days[j] );    /* Day names             */

		for ( i=1; i<=MAX_COLS; i++ )
		{
			if ( theday != -1 && month_map[j][i] == theday )
				ch->Send ( "{cV" );
			ch->Send ( "%s", event_day ( month_map[j][i], month, year ) );
			if ( month_map[j][i] == 0 )  /* dates            */
				{ch->Send ( "   " );}
			else
				{ch->Send ( "%2d ", month_map[j][i] );}

			if ( theday != -1 && month_map[j][i] == theday )
				ch->Send ( "{c0" );
		}

		ch->Send ( "\r\n" );             /* puts supplies a Newline    */
	}
}

/************************************************************************/
/*                    Start_day                                    */
/*        Work out which day the month starts on.           */
/************************************************************************/

int start_day ( const int month, const int year )
{
	int day=1;                  /* 1/1/1990 was a Monday.       */
	int i,j;                    /* GP work variable.          */

	/* Count days in the year.    */
	i = BASE_YEAR;

	while ( i < year )
	{
		leap ( i );               /* Check for leap years       */
		for ( j=1; j<=12; j++ ) day = day + days_in_month[j];
		i++;
	}

	/* Count upto the month required */
	i=1;
	leap ( year );              /* Check for leap years       */
	while ( i < month )
	{
		day = day + days_in_month[i++];
	}
	/* Get modulo and return it as
	   the start day for this month    */
	if ( ( day = day%7 ) == 0 ) day = 7;  /* correct 0 to 7        */
	return ( day );
}

/************************************************************************/
/*                    leap                                         */
/*        Find leap years.                                  */
/*             Returns 0 - Not a leap year.                 */
/*                     1 - leap year.                  */
/************************************************************************/

int leap ( int year )
{

	/*
	A leap year follows the following rules:
	  if divisable by 4 and not 100 its a leap year.
	  if divisable by 400 it is a leap year.
	*/

	if ( ( year%4 == FALSE && year%100 != FALSE ) || year%400 == FALSE )
	{
		days_in_month[2] = 29;
		return ( 1 );             /* leap year        */
	}
	else
	{
		days_in_month[2] = 28;
		return ( 0 );             /* NOT a leap year  */
	}
}

void free_current_events() {}
void free_current_event ( struct date_event_data *evt ) {}

int save_current_events()
{
	FILE *bgtime;

	if ( ( bgtime = fopen ( EVENT_FILE, "w" ) ) == NULL )
		log ( "SYSERR: Can't write to '%s' event file.", EVENT_FILE );
	else
	{
		fprintf ( bgtime, "%ld\n", 2222 );
		fclose ( bgtime );
	}
}
void load_current_events()
{
	time_t beginning_of_time = 0;
	FILE *bgtime;

	if ( ( bgtime = fopen ( TIME_FILE, "r" ) ) == NULL )
		log ( "SYSERR: Can't read from '%s' time file.", TIME_FILE );
	else
	{
		fscanf ( bgtime, "%ld\n", &beginning_of_time );
		fclose ( bgtime );
	}

	if ( beginning_of_time == 0 )
		beginning_of_time = 650336715;


	time_info = *mud_time_passed ( time ( 0 ), beginning_of_time );

	if ( time_info.hours <= 4 )
		sunlight = SUN_DARK;
	else if ( time_info.hours == 5 )
		sunlight = SUN_RISE;
	else if ( time_info.hours <= 20 )
		sunlight = SUN_LIGHT;
	else if ( time_info.hours == 21 )
		sunlight = SUN_SET;
	else
		sunlight = SUN_DARK;

	log ( "   Current Gametime: %dH %dD %dM %dY.", time_info.hours,
	      time_info.day, time_info.month, time_info.year );
}
