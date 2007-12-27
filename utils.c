/* ************************************************************************
*   File: utils.c                                       Part of CircleMUD *
*  Usage: various internal functions of a utility nature                  *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "db.h"
#include "comm.h"
#include "screen.h"
#include "spells.h"
#include "handler.h"
#include "interpreter.h"

extern struct descriptor_data *descriptor_list;
extern struct time_data time_info;
extern struct room_data *world;


/* local functions */
struct time_info_data *real_time_passed(time_t t2, time_t t1);
struct time_info_data *mud_time_passed(time_t t2, time_t t1);
void die_follower(struct char_data *ch);
void add_follower(struct char_data *ch, struct char_data *leader);
char *stripcr(char *dest, const char *src);
void prune_crlf(char *txt);


// m0rd
char* print_gold(char* result, gold_int gold)
{
  int len = sprintf(result, "%lld", gold);
  int num_commas = ((result[0] == '-') ? (len - 1) / 3 : len / 3);
  int num_digits = 0;

  if (num_commas > 1 && (len%3))
    num_commas++;

  if (gold <0)
    num_commas--;

  if (num_commas>0)
    num_commas--;
  result[len-- + num_commas] = 0;
  while(num_commas > 0)
  {
    *(result + len + num_commas) = *(result + len);
    num_digits++;
    len--;
    if(num_digits == 3)
    {
      *(result + len + num_commas) = ',';
      num_digits = 0;
      num_commas--;
    }
  }

  return result;
}

char *center_align(char *str, int width)
{
  static char retbuf[MAX_STRING_LENGTH];
  char statbuf[MAX_STRING_LENGTH], *stptr, *eptr;
  int len;
  if (!str)
    return NULL;
  len = strlen(str);
  snprintf(statbuf, sizeof(statbuf), "%s", str);
  stptr = statbuf;
  eptr = (statbuf + strlen(statbuf) - 1);
  for (;isspace(*eptr);*eptr--) *eptr = '\0';
  if (*stptr) skip_spaces(&stptr);

  len = strlen(stptr);
  if (len == 0)
    return NULL;

  if (len > width)
    return stptr;
  else
  {
    int y = 0, dif = width - len;

    y = (dif/2);

    snprintf(retbuf, sizeof(retbuf), "%*s%s%*s",y, "", stptr, y, "");
    return retbuf;
  }

}

/* ch = player to change the gold values of
   amount = the amount to add to their gold, or subtract
   type = for a transaction type, taxed untaxted.. or whatever.
   
*/
gold_int char_gold(struct char_data *ch, gold_int amount, short type)
{

  gold_int temp = 0;

  switch (type)
  {
  case GOLD_HAND:
    GET_GOLD(ch) += amount;
    temp = (GET_GOLD(ch) + (amount));
    return temp;
    break;
  case GOLD_BANK:
    GET_BANK_GOLD(ch) += amount;
    temp = (GET_BANK_GOLD(ch) + (amount));
    return temp;
    break;
  case GOLD_ALL:
    if (amount < 0)
    {
      temp = (abs(amount));
      if (temp <= (GET_GOLD(ch) + GET_BANK_GOLD(ch)))
      { // No Transaction. This needs watching



        if (temp > GET_GOLD(ch))
        {
          temp -= GET_GOLD(ch);
          GET_GOLD(ch) = 0;
          GET_BANK_GOLD(ch) -= temp;
        }
        else
          GET_GOLD(ch) -= temp;

      }
      else //positive amount -- add to bank
      {
        GET_BANK_GOLD(ch) += amount;
      }
    }
    else
      GET_BANK_GOLD(ch) += amount;
    return (GET_GOLD(ch) + GET_BANK_GOLD(ch));
    break;
  default:
    new_mudlog(CMP, LVL_GOD, TRUE, "%s invalid transaction type #%d (amount: %lld)", GET_NAME(ch), type, amount);
    break;
    return 0;
  }
  return 0;

}


/*
 * Compares two strings, and returns TRUE
 * if they match 100% (not case sensetive).
 */
bool compares(const char *aStr, const char *bStr)
{
  int i = 0;

  /* NULL strings never compares */
  if (aStr == NULL || bStr == NULL) return FALSE;

  while (aStr[i] != '\0' && bStr[i] != '\0' && toupper(aStr[i]) == toupper(bStr[i]))
    i++;

  /* if we terminated for any reason except the end of both strings return FALSE */
  if (aStr[i] != '\0' || bStr[i] != '\0')
    return FALSE;

  /* success */
  return TRUE;
}

/*
 * Checks if aStr is a prefix of bStr.
 */
bool is_prefix(const char *aStr, const char *bStr)
{
  /* NULL strings never compares */
  if (aStr == NULL || bStr == NULL) return FALSE;

  /* empty strings never compares */
  if (aStr[0] == '\0' || bStr[0] == '\0') return FALSE;

  /* check if aStr is a prefix of bStr */
  while (*aStr)
  {
    if (tolower(*aStr++) != tolower(*bStr++))
      return FALSE;
  }

  /* success */
  return TRUE;
}

/* creates a random number in interval [from;to] */
int number(int from, int to)
{
  /* error checking in case people call number() incorrectly */
  if (from > to)
  {
    int tmp = from;
    from = to;
    to = tmp;
    //log("SYSERR: number() should be called with lowest, then highest. number(%d, %d), not number(%d, %d).", from, to, to, from);
  }

  return ((circle_random() % (to - from + 1)) + from);
}


/* simulates dice roll */
int dice(int number, int size)
{
  int sum = 0;


  if (size <= 0 || number <= 0)
    return (0);

  while (number-- > 0)
    sum += ((circle_random() % size) + 1);

  return (sum);
}


int MIN(int a, int b)
{
  return (a < b ? a : b);
}


int MAX(int a, int b)
{
  return (a > b ? a : b);
}


char *CAP(char *txt)
{
  *txt = UPPER(*txt);
  return (txt);
}



/* Create a duplicate of a string */
char *str_dup(const char *source)
{
  char *new_z = NULL;

  CREATE(new_z, char, strlen(source) + 1);
  return (strcpy(new_z, source));
}


//#ifndef (HAVE_STRLCPY)
/*
 * A 'strlcpy' function in the same fashion as 'strdup' below.
 *
 * This copies up to totalsize - 1 bytes from the source string, placing
 * them and a trailing NUL into the destination string.
 *
 * Returns the total length of the string it tried to copy, not including
 * the trailing NUL.  So a '>= totalsize' test says it was truncated.
 * (Note that you may have _expected_ truncation because you only wanted
 * a few characters from the source string.)
 */
#if 0
size_t strlcpy(char *dest, const char *src, size_t copylen)
{
  strncpy(dest, src, copylen - 1);	/* strncpy: OK (we must assume 'totalsize' is correct) */
  dest[copylen - 1] = '\0';
  return strlen(src);
}
#else
size_t strlcpy(char *dest, const char *src, size_t copylen)
{
  register char *d = dest;
  register const char *s = src;
  register size_t n = copylen;

  if (dest == NULL || src == NULL)
    return 0;

  /* Copy as many bytes as will fit */
  if (n != 0 && --n != 0)
  {
    register char c;
    do
    {
      c = *d++ = *s++;
      if (c == '\0')
        break;
    }
    while (--n != 0);
  }

  /* If not enough room in dest, add NUL and traverse rest of src */
  if (n == 0)
  {
    if (copylen != 0)
      *d = '\0';    /* NUL-terminate dst */
    while (*s++)
      ;
  }

  return s - src - 1;  /* count does not include NUL */
}
#endif

/*
 * Strips \r\n from end of string.
 */
void prune_crlf(char *txt)
{
  int i = strlen(txt) - 1;

  while (txt[i] == '\n' || txt[i] == '\r')
    txt[i--] = '\0';
}


/*
 * str_cmp: a case-insensitive version of strcmp().
 * Returns: 0 if equal, > 0 if arg1 > arg2, or < 0 if arg1 < arg2.
 *
 * Scan until strings are found different or we reach the end of both.
 */
int str_cmp(const char *arg1, const char *arg2)
{
  int chk, i;

  if (arg1 == NULL || arg2 == NULL)
  {
    log("SYSERR: str_cmp() passed a NULL pointer, %p or %p.", arg1,
        arg2);
    return (0);
  }

  for (i = 0; arg1[i] || arg2[i]; i++)
    if ((chk = LOWER(arg1[i]) - LOWER(arg2[i])) != 0)
      return (chk);	/* not equal */

  return (0);
}

int first_word_is_name(struct char_data *ch, char * argument)
{
  char *first_arg, *name_p;
  int chk, i;
  if (argument == NULL || IS_NPC(ch))
    return 0;
  name_p = GET_NAME(ch);
  first_arg = argument;

  for (i = 0; (name_p[i] != ' ' || first_arg[i] != ' ') && (name_p[i] && first_arg[i]); i++)
    if ((chk = LOWER(name_p[i]) - LOWER(first_arg[i])) != 0)
      return (!chk);


  return 1;
}

/*
 * strn_cmp: a case-insensitive version of strncmp().
 * Returns: 0 if equal, > 0 if arg1 > arg2, or < 0 if arg1 < arg2.
 *
 * Scan until strings are found different, the end of both, or n is reached.
 */
#if 0
int strn_cmp(const char *arg1, const char *arg2, int n)
{
  int chk, i;

  if (arg1 == NULL || arg2 == NULL)
  {
    log("SYSERR: strn_cmp() passed a NULL pointer, %p or %p.", arg1,
        arg2);
    return (0);
  }

  for (i = 0; (arg1[i] || arg2[i]) && (n > 0); i++, n--)
    if ((chk = LOWER(arg1[i]) - LOWER(arg2[i])) != 0)
      return (chk);	/* not equal */

  return (0);
}
#endif

/* log a death trap hit */
void log_death_trap(struct char_data *ch)
{
  new_mudlog(BRF, LVL_GOD, TRUE, "%s hit death trap #%d (%s)", GET_NAME(ch),
             GET_ROOM_VNUM(IN_ROOM(ch)), IN_ROOM(ch)->name);
}

/*
 * New variable argument log() function.  Works the same as the old for
 * previously written code but is very nice for new code.
 */
void basic_mud_vlog(const char *format, va_list args)
{
  time_t ct = time(0);
  char *time_s = asctime(localtime(&ct));

  if (logfile == NULL)
  {
    puts("SYSERR: Using log() before stream was initialized!");
    return;
  }

  if (format == NULL)
    format = "SYSERR: log() received a NULL format.";

  time_s[strlen(time_s) - 1] = '\0';

  fprintf(logfile, "%-15.15s :: ", time_s + 4);
  vfprintf(logfile, format, args);
  fputc('\n', logfile);
  fflush(logfile);
}

void free_commlist(struct comm_data *c)
{
  if (!c)
    return;
  if (c->next)
    free_commlist(c->next);
  if (c->type)
    free(c->type);
  if (c->text)
    free(c->text);
  free(c);

}

void add_to_comm(const char *type, const char *text)
{
  struct comm_data *com, *tmp;
  int i;
  if (type && text)
  {
    CREATE(com, struct comm_data, 1);
    com->type = strdup(type);
    com->text = strdup(text);
    com->next = comlist;
    comlist = com;
    for (i = 0, tmp = comlist;tmp;i++, tmp = tmp->next)
    {
      if (i > 50)
      {
        free_commlist(tmp->next);
        tmp->next = NULL;
        return;
      }
    }
  }
}



/*
 * New variable argument log() function.  Works the same as the old for
 * previously written code but is very nice for new code.
 */
/* So mudlog() can use the same function. */
void basic_mud_log(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  basic_mud_vlog(format, args);
  va_end(args);
}


/*
 * New variable argument log() function.  Works the same as the old for
 * previously written code but is very nice for new code.
 */
void basic_mud_clog( const char *format, va_list args)
{
  time_t ct = time(0);
  char *time_s = asctime(localtime(&ct));

  if (comfile == NULL)
  {
    puts("SYSERR: Using log() before stream was initialized!");
    return;
  }

  if (format == NULL)
    format = "SYSERR: log() received a NULL format.";

  time_s[strlen(time_s) - 1] = '\0';

  fprintf(comfile, "%-15.15s :: ", time_s + 4);
  vfprintf(comfile, format, args);
  fputc('\n', comfile);
  fflush(comfile);



}


/*
 * New variable argument log() function.  Works the same as the old for
 * previously written code but is very nice for new code.
 */
/* So mudlog() can use the same function. */
void comlog(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  basic_mud_clog(format, args);
  va_end(args);
}
/*
void basic_mud_log(const char *format, ...)
{
    va_list args;
    time_t ct = time(0);
    char *time_s = asctime(localtime(&ct));
 
    if (logfile == NULL)
	puts("SYSERR: Using log() before stream was initialized!");
    if (format == NULL)
	format = "SYSERR: log() received a NULL format.";
 
    time_s[strlen(time_s) - 1] = '\0';
 
    fprintf(logfile, "%-15.15s :: ", time_s + 4);
 
    va_start(args, format);
    vfprintf(logfile, format, args);
    va_end(args);
 
    fprintf(logfile, "\n");
    fflush(logfile);
}
*/

/* the "touch" command, essentially. */
int touch(const char *path)
{
  FILE *fl;

  if (!(fl = fopen(path, "a")))
  {
    log("SYSERR: %s: %s", path, strerror(errno));
    return (-1);
  }
  else
  {
    fclose(fl);
    return (0);
  }
}

/*
 * mudlog -- log mud messages to a file & to online imm's syslogs
 * based on syslog by Fen Jul 3, 1992
 */
void new_mudlog(int type, int level, int file, const char *str, ...)
{
  char buf[MAX_STRING_LENGTH];
  struct descriptor_data *i;
  va_list args;

  if (str == NULL)
    return;	/* eh, oh well. */

  if (file)
  {
    va_start(args, str);
    basic_mud_vlog(str, args);
    va_end(args);
  }

  if (level < 0)
    return;

  strcpy(buf, "[ ");	/* strcpy: OK */
  va_start(args, str);
  vsnprintf(buf + 2, sizeof(buf) - 6, str, args);
  va_end(args);
  strcat(buf, " ]\r\n");	/* strcat: OK */

  for (i = descriptor_list; i; i = i->next)
  {
    if (STATE(i) != CON_PLAYING || IS_NPC(i->character)) /* switch */
      continue;
    if (GET_LEVEL(i->character) < level)
      continue;
    if (PLR_FLAGGED(i->character, PLR_WRITING))
      continue;
    if (type > (PRF_FLAGGED(i->character, PRF_LOG1) ? 1 : 0) + (PRF_FLAGGED(i->character, PRF_LOG2) ? 2 : 0))
      continue;

    new_send_to_char(i->character, "%s%s%s", CCGRN(i->character, C_NRM), buf, CCNRM(i->character, C_NRM));
  }
}

/*
 * mudlog -- log mud messages to a file & to online imm's syslogs
 * based on syslog by Fen Jul 3, 1992
 */
void mudlog(const char *str, int type, int level, int file)
{
  char buf[MAX_STRING_LENGTH], tp;
  struct descriptor_data *i;

  if (str == NULL)
    return;			/* eh, oh well. */
  if (file)
    log(str);
  if (level < 0)
    return;

  sprintf(buf, "[ %s ]\r\n", str);

  for (i = descriptor_list; i; i = i->next)
  {
    if (STATE(i) != CON_PLAYING || IS_NPC(i->character))	/* switch */
      continue;
    if (GET_ORIG_LEV(i->character) ? GET_ORIG_LEV(i->character) < level : GET_LEVEL(i->character) < level )
      continue;
    if (PLR_FLAGGED(i->character, PLR_WRITING))
      continue;
    tp = ((PRF_FLAGGED(i->character, PRF_LOG1) ? 1 : 0) +
          (PRF_FLAGGED(i->character, PRF_LOG2) ? 2 : 0));
    if (tp < type)
      continue;

    send_to_char(CCGRN(i->character, C_NRM), i->character);
    send_to_char(buf, i->character);
    send_to_char(CCNRM(i->character, C_NRM), i->character);
  }

}

/*
 * If you don't have a 'const' array, just cast it as such.  It's safer
 * to cast a non-const array as const than to cast a const one as non-const.
 * Doesn't really matter since this function doesn't change the array though.
 */
void sprintbit(bitvector_t bitvector, const char *names[], char *result, size_t r_len)
{
  long nr;

  *result = '\0';

  for (nr = 0; bitvector; bitvector >>= 1)
  {
    if (IS_SET(bitvector, 1))
    {
      if (*names[nr] != '\n')
      {
        strlcat(result, names[nr], r_len);
        strlcat(result, " ", r_len);
      }
      else
        strlcat(result, "UNDEFINED ", r_len);
    }
    if (*names[nr] != '\n')
      nr++;
  }

  if (!*result)
    strlcpy(result, "NOBITS ", r_len);
}


void sprintbitarray(int bitvector[], const char *names[], int maxar,
                    char *result, size_t r_len)
{
  int nr, teller, found = FALSE;

  *result = '\0';

  for (teller = 0; teller < maxar && !found; teller++)
    for (nr = 0; nr < 32 && !found; nr++)
    {
      if (IS_SET_AR(bitvector, (teller * 32) + nr))
      {
        if (*names[(teller * 32) + nr] != '\n')
        {
          if (*names[(teller * 32) + nr] != '\0')
          {
            strlcat(result, names[(teller * 32) + nr], r_len);
            strlcat(result, " ", r_len);
          }
        }
        else
        {
          strlcat(result, "UNDEFINED ", r_len);
        }
        if (*names[(teller * 32) + nr] == '\n')
          found = TRUE;
      }
    }

  if (!*result)
    strlcpy(result, "NOBITS ", r_len);
}

void sprinttype(int type, const char *names[], char *result, size_t r_len)
{
  int nr = 0;

  while (type && *names[nr] != '\n')
  {
    type--;
    nr++;
  }

  if (*names[nr] != '\n')
    strlcpy(result, names[nr], r_len);
  else
    strlcpy(result, "UNDEFINED", r_len);
}

/*
 * If you don't have a 'const' array, just cast it as such.  It's safer
 * to cast a non-const array as const than to cast a const one as non-const.
 * Doesn't really matter since this function doesn't change the array though.
 */
size_t new_sprintbit(bitvector_t bitvector, const char *names[],
                     char *result, size_t reslen)
{
  size_t len = 0; //, nlen;
  long nr;

  *result = '\0';

  for (nr = 0; bitvector /* && len < reslen */; bitvector >>= 1)
  {
    if (IS_SET(bitvector, 1))
    {
    strlcat(result, *names[nr] != '\n' ? names[nr] : "UNDEFINED", reslen);
    /*
      nlen =
        snprintf(result + len, reslen - len, "%s ",
                 *names[nr] != '\n' ? names[nr] : "UNDEFINED");
      if (len + nlen >= reslen || nlen < 0)
        break;
      len += nlen;
      */
    }

    if (*names[nr] != '\n')
      nr++;
  }

  if (!*result)
    len = strlcpy(result, "NOBITS ", reslen);

  return (len);
}


size_t new_sprinttype(int type, const char *names[], char *result,
                      size_t reslen)
{
  int nr = 0;

  while (type && *names[nr] != '\n')
  {
    type--;
    nr++;
  }

  return strlcpy(result, *names[nr] != '\n' ? names[nr] : "UNDEFINED",
                 reslen);
}


/* Calculate the REAL time passed over the last t2-t1 centuries (secs) */
struct time_info_data *real_time_passed(time_t t2, time_t t1)
{
  long secs;
  static struct time_info_data now;

  secs = (long) (t2 - t1);

  now.hours = (secs / SECS_PER_REAL_HOUR) % 24;	/* 0..23 hours */
  secs -= SECS_PER_REAL_HOUR * now.hours;

  now.day = (secs / SECS_PER_REAL_DAY);	/* 0..34 days  */
  /* secs -= SECS_PER_REAL_DAY * now.day; - Not used. */

  now.month = -1;
  now.year = -1;

  return (&now);
}



/* Calculate the MUD time passed over the last t2-t1 centuries (secs) */
struct time_info_data *mud_time_passed(time_t t2, time_t t1)
{
  long secs;
  static struct time_info_data now;

  secs = (long) (t2 - t1);

  now.hours = (secs / SECS_PER_MUD_HOUR) % 24;	/* 0..23 hours */
  secs -= SECS_PER_MUD_HOUR * now.hours;

  now.day = (secs / SECS_PER_MUD_DAY) % 35;	/* 0..34 days  */
  secs -= SECS_PER_MUD_DAY * now.day;

  now.month = (secs / SECS_PER_MUD_MONTH) % 17;	/* 0..16 months */
  secs -= SECS_PER_MUD_MONTH * now.month;

  now.year = (secs / SECS_PER_MUD_YEAR);	/* 0..XX? years */

  return (&now);
}

time_t mud_time_to_secs(struct time_info_data * now)
{
  time_t when = 0;

  when += now->year * SECS_PER_MUD_YEAR;
  when += now->month * SECS_PER_MUD_MONTH;
  when += now->day * SECS_PER_MUD_DAY;
  when += now->hours * SECS_PER_MUD_HOUR;

  return (time(NULL) - when);
}

struct time_info_data *age(struct char_data *ch)
{
  static struct time_info_data player_age;

  player_age = *mud_time_passed(time(0), ch->player.time.birth);

  player_age.year += 17;	/* All players start at 17 */

  return (&player_age);
}


/* Check if making CH follow VICTIM will create an illegal */
/* Follow "Loop/circle"                                    */
bool circle_follow(struct char_data * ch, struct char_data * victim)
{
  struct char_data *k;

  for (k = victim; k; k = k->master)
  {
    if (k == ch)
      return (TRUE);
  }

  return (FALSE);
}



/* Called when stop following persons, or stopping charm */
/* This will NOT do if a character quits/dies!!          */
void stop_follower(struct char_data *ch)
{
  struct follow_type *j, *k;

  if (ch->master == NULL || ch->master->followers == NULL)
  {
    core_dump();
    return;
  }

  if (AFF_FLAGGED(ch, AFF_CHARM))
  {
    act("$n hates your guts!", FALSE, ch, 0, ch->master, TO_VICT);
    if (affected_by_spell(ch, SPELL_CHARM))
      affect_from_char(ch, SPELL_CHARM);
  }
  else
  {
    act("You stop following $N.", FALSE, ch, 0, ch->master, TO_CHAR);
    act("$n stops following $N.", TRUE, ch, 0, ch->master, TO_NOTVICT);
    act("$n stops following you.", TRUE, ch, 0, ch->master, TO_VICT);
  }

  if (ch->master->followers->follower == ch)
  {	/* Head of follower-list? */
    k = ch->master->followers;
    ch->master->followers = k->next;
    free(k);

  }
  else
  {			/* locate follower who is not head of list */


    for (k = ch->master->followers; k && k->next ; k = (k ? k->next : NULL))
    {
      if (!k)
        continue;
      if (!k->next)
        continue;
      if (!k->next->follower)
        continue;
      if (k->next->follower != ch)
        continue;
      j = k->next;
      k->next = j->next;
      if (j)
        free(j);
      break;
    }
  }
  if (!IS_NPC(ch) && (!IS_NPC(ch->master)) && ch->master)
  {
    GET_PERC(ch->master) += GET_PERC(ch);
    GET_PERC(ch) = 0.0;
    total_perc(ch->master);
  }
  ch->master = NULL;
  total_perc(ch);

  REMOVE_BIT_AR(AFF_FLAGS(ch), AFF_CHARM);
  //REMOVE_BIT_AR(AFF_FLAGS(ch), AFF_GROUP);
}



/* Called when a character that follows/is followed dies */
void die_follower(struct char_data *ch)
{
  struct follow_type *j, *k;

  if (ch->master)
    stop_follower(ch);

  for (k = ch->followers; k; k = j)
  {
    j = k->next;
    stop_follower(k->follower);
  }
}



/* Do NOT call this before having checked if a circle of followers */
/* will arise. CH will follow leader                               */
void add_follower(struct char_data *ch, struct char_data *leader)
{
  struct follow_type *k;

  if (ch->master)
  {
    core_dump();
    return;
  }

  ch->master = leader;

  CREATE(k, struct follow_type, 1);

  k->follower = ch;
  k->next = leader->followers;
  leader->followers = k;
  GET_PERC(ch) = 0.0;

  act("You now follow $N.", FALSE, ch, 0, leader, TO_CHAR);
  if (CAN_SEE(leader, ch))
    act("$n starts following you.", TRUE, ch, 0, leader, TO_VICT);
  act("$n starts to follow $N.", TRUE, ch, 0, leader, TO_NOTVICT);
}

/*
 * get_line reads the next non-blank line off of the input stream.
 * The newline character is removed from the input.  Lines which begin
 * with '*' are considered to be comments.
 *
 * Returns the number of lines advanced in the file.
 */
int get_line(FILE *fl, char *buf)
{
  char temp[READ_SIZE];
  int lines = 0;
  int sl;

  do
  {
    if (!fgets(temp, READ_SIZE, fl))
      return (0);
    lines++;
  }
  while (*temp == '*' || *temp == '\n' || *temp == '\r');

  /* Last line of file doesn't always have a \n, but it should. */
  sl = strlen(temp);
  while (sl > 0 && (temp[sl - 1] == '\n' || temp[sl - 1] == '\r'))
    temp[--sl] = '\0';

  strcpy(buf, temp); /* strcpy: OK, if buf >= READ_SIZE (256) */
  return (lines);
}

int get_filename(const char *orig_name, char *filename, int mode)
{
  const char *prefix, *middle, *suffix;
  char name[64], *ptr;

  if (orig_name == NULL || *orig_name == '\0' || filename == NULL)
  {
    log("SYSERR: NULL pointer or empty string passed to get_filename(), %p or %p.", orig_name, filename);
    return (0);
  }

  switch (mode)
  {
  case CRASH_FILE:
    prefix = LIB_PLROBJS;
    suffix = SUF_OBJS;
    break;
  case ETEXT_FILE:
    prefix = LIB_PLRTEXT;
    suffix = SUF_TEXT;
    break;
  case ALIAS_FILE:
    prefix = LIB_PLRALIAS;
    suffix = SUF_ALIAS;
    break;
  case POOF_FILE:
    prefix = LIB_PLRPOOFS;
    suffix = SUF_POOFS;
    break;
  case NEW_OBJ_FILES:
    prefix = LIB_PLROBJS;
    suffix = "new";
    break;
  case SCRIPT_VARS_FILE:
    prefix = LIB_PLRVARS;
    suffix = SUF_MEM;
    break;
  case ASCII_OBJ_FILES:
    prefix = LIB_PLROBJS;
    suffix = "anew";
    break;
  case LOCKER_FILES:
    prefix = LIB_PLROBJS;
    suffix = "locker";
    break;
  default:
    return (0);
  }

  strcpy(name, orig_name);
  for (ptr = name; *ptr; ptr++)
    *ptr = LOWER(*ptr);

  switch (LOWER(*name))
  {
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
    middle = "A-E";
    break;
  case 'f':
  case 'g':
  case 'h':
  case 'i':
  case 'j':
    middle = "F-J";
    break;
  case 'k':
  case 'l':
  case 'm':
  case 'n':
  case 'o':
    middle = "K-O";
    break;
  case 'p':
  case 'q':
  case 'r':
  case 's':
  case 't':
    middle = "P-T";
    break;
  case 'u':
  case 'v':
  case 'w':
  case 'x':
  case 'y':
  case 'z':
    middle = "U-Z";
    break;
  default:
    middle = "ZZZ";
    break;
  }

  sprintf(filename,  "%s%s" SLASH "%s.%s", prefix, middle, name, suffix);
  //filename[strlen(prefix)+strlen(middle)+strlen(name)+strlen(suffix)+1] = 0;
  return (1);
}


int num_pc_in_room(struct room_data *room)
{
  int i = 0;
  struct char_data *ch;

  for (ch = room->people; ch != NULL; ch = ch->next_in_room)
    if (!IS_NPC(ch))
      i++;

  return (i);
}

/*
 * This function (derived from basic fork(); abort(); idea by Erwin S.
 * Andreasen) causes your MUD to dump core (assuming you can) but
 * continue running.  The core dump will allow post-mortem debugging
 * that is less severe than assert();  Don't call this directly as
 * core_dump_unix() but as simply 'core_dump()' so that it will be
 * excluded from systems not supporting them. (e.g. Windows '95).
 *
 * You still want to call abort() or exit(1) for
 * non-recoverable errors, of course...
 *
 * XXX: Wonder if flushing streams includes sockets?
 */
void core_dump_real(const char *who, int line)
{

  log("SYSERR: Assertion failed at %s:%d!", who, line);
#if 0
#if 1
  int pid;
  int i;

  if ((pid=fork()) < 0)
    return;
  else if (!pid)
    return;
  for (i=0; i<MAX_FDS; i++)
    if (i != syslogfd)
      close(i);

  dup2(syslogfd, STDOUT_FILENO);
  dup2(syslogfd, STDERR_FILENO); /* point stdout/stderr to the syslog file descriptor */
  if ((pid=fork()) < 0)
    exit(0);  /* can't return, since we're a child.  No core for you. */
  if (!pid)
  { /* child... */
    abort(); /* dumps core */
  }
  waitpid(pid, NULL, 0); /* waits for the child to die */
  execlp("gdb", "gdb", "../bin/circle", "-c", "core", "-batch", "-x", "../.gdbinit", NULL);
  exit(0); /* not reached, but if exec fails... */
#else
#if defined(CIRCLE_UNIX)

  /* These would be duplicated otherwise... */
  fflush(stdout);
  fflush(stderr);
  fflush(logfile);

  /*
  * Kill the child so the debugger or script doesn't think the MUD
  * crashed.  The 'autorun' script would otherwise run it again.
  */
  if (fork() == 0)
    abort();
#endif
#endif
#endif

}
/*
 * Rules (unless overridden by ROOM_DARK):
 *
 * Inside and City rooms are always lit.
 * Outside rooms are dark at sunset and night.
 */
int room_is_dark(room_rnum room)
{
  if (!VALID_ROOM_RNUM(room))
  {
    log("room_is_dark: Invalid room rnum  (0-%d)", top_of_world);
    return (FALSE);
  }

  if (room->light)
    return (FALSE);

  if (ROOM_FLAGGED(room, ROOM_DARK))
    return (TRUE);


  if (ROOM_FLAGGED(room, ROOM_INDOORS))
    return (FALSE);

  if (SECT(room) == SECT_INSIDE || SECT(room) == SECT_CITY)
    return (FALSE);

  if (sunlight == SUN_SET || sunlight == SUN_DARK)
    return (TRUE);

  return (FALSE);
}


/* strips \r's from line */
char *stripcr(char *dest, const char *src)
{
  int i, length;
  char *temp;

  if (!dest || !src)
    return NULL;
  temp = &dest[0];
  length = strlen(src);
  for (i = 0; *src && (i < length); i++, src++)
    if (*src != '\r')
      *(temp++) = *src;
  *temp = '\0';
  return dest;
}

int get_pidx_from_name(struct char_data *ch)
{
  return GET_IDNUM(ch);
}




// Called like so from somewhere that you want to ask a question:
// line_input(ch->desc, "Do you want to multi-class to warrior (Yes/No)? ",
//                       parse_multiclass);
// ONLY USE FROM CON_PLAYING STATE()'s!

void line_input(struct descriptor_data *d, const char *prompt,
                C_FUNC(*callback), void *info)
{
  d->callback = callback;
  d->callback_depth++;	// Increase depth of possible recursiveness.
  d->c_data = (char *) info;
  STATE(d) = CON_LINE_INPUT;
  SEND_TO_Q(prompt, d);
}


int attack_type(char chclass)
{
  switch (chclass)
  {
  case CLASS_MAGE:
    return ATTACK_MAGIC;
    break;
  case CLASS_PRIEST:
    return ATTACK_MAGIC;
    break;
  case CLASS_ESPER:
    return ATTACK_MAGIC;
    break;
  case CLASS_GYPSY:
    return ATTACK_SKILL;
    break;
  case CLASS_THIEF:
    return ATTACK_SKILL;
    break;
  case CLASS_RANGER:
    return ATTACK_SKILL;
    break;
  case CLASS_HUNTER:
    return ATTACK_MELEE;
    break;
  case CLASS_WARRIOR:
    return ATTACK_MELEE;
    break;
  default:
    return 0;
    break;
  }
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
//#include <math.h>
#define NUL '\0'

/*
** strrepl: Replace OldStr by NewStr in string Str contained in buffer
**          of size BufSiz.
**
** Str should have enough allocated space for the replacement - if not,
** NULL is returned. Str and OldStr/NewStr should not overlap.
**
** The empty string ("") is found at the beginning of every string.
**
** Returns: pointer to first location behind where NewStr was inserted.
**          Str if OldStr was not found.
**          NULL if replacement would overflow Str's buffer
**
** This is useful for multiple replacements, see example in main() below
** (be careful not to replace the empty string this way !)
**
**  NOTE: The name of this funtion violates ANSI/ISO 9899:1990 sec. 7.1.3,
**        but this violation seems preferable to either violating sec. 7.13.8
**        or coming up with some hideous mixed-case or underscore infested
**        naming. Also, many SNIPPETS str---() functions duplicate existing
**        functions which are supported by various vendors, so the naming
**        violation may be required for portability.
*/
#if 0
#if defined(__cplusplus) && __cplusplus
extern "C"
{
#endif

  char *strrepl(char *Str, size_t BufSiz, char *OldStr, char *NewStr)
  {
    int OldLen, NewLen;
    char *p, *q;

    if(NULL == (p = strstr(Str, OldStr)))
      return Str;
    OldLen = strlen(OldStr);
    NewLen = strlen(NewStr);
    if ((strlen(Str) + NewLen - OldLen + 1) > BufSiz)
      return NULL;
    memmove(q = p+NewLen, p+OldLen, strlen(p+OldLen)+1);
    memcpy(p, NewStr, NewLen);
    return q;
  }

#if defined(__cplusplus) && __cplusplus

}
#endif

/*
**  XSTRCAT.C - String concatenation function
**
**  Notes: 1st argument must be a buffer large enough to contain the
**         concatenated strings.
**
**         2nd thru nth arguments are the string to concatenate.
**
**         (n+1)th argument must be NULL to terminate the list.
*/

#if defined(__cplusplus) && __cplusplus
extern "C"
{
#endif

  char *xstrcat(char *des, char *src, ...)
  {
    char *destination = des;
    va_list v;

    va_start(v, src);

    while (src != 0)
    {
      while (*src != 0)
        *des++ = *src++;
      src = va_arg(v, char *);
    }
    *des = 0;

    va_end(v);

    return destination;
  }

#if defined(__cplusplus) && __cplusplus

}
#endif
#endif

/*
    This is a quicksort routine to be used to sort linked-lists
    by Jon Guthrie.
*/

void    *sortl(void *list, void *(*getnext)(void *),
               void (*setnext)(void *, void *),
               int (*compare)(void *, void *))
{
  void    *low_list, *high_list, *current, *pivot, *temp;
  int     result;

  /*
      Test for empty list.
  */
  if(NULL == list)
    return(NULL);

  /*
      Find the first element that doesn't have the same value as the first
      element.
  */
  current = list;
  do
  {
    current = getnext(current);
    if(NULL == current)
      return(list);
  }
  while(0 == (result = compare(list, current)));

  /*
      My pivot value is the lower of the two.  This insures that the sort
      will always terminate by guaranteeing that there will be at least one
      member of both of the sublists.
  */
  if(result > 0)
    pivot = current;
  else
    pivot = list;

  /* Initialize the sublist pointers */
  low_list = high_list = NULL;

  /*
      Now, separate the items into the two sublists
  */
  current = list;
  while(NULL != current)
  {
    temp = getnext(current);
    if(compare(pivot, current) < 0)
    {
      /* add one to the high list */
      setnext(current, high_list);
      high_list = current;
    }
    else
    {
      /* add one to the low list */
      setnext(current, low_list);
      low_list = current;
    }
    current = temp;
  }

  /*
      And, recursively call the sort for each of the two sublists.
  */
  low_list  = sortl(low_list, getnext, setnext, compare);
  high_list = sortl(high_list, getnext, setnext, compare);

  /*
      Now, I have to put the "high" list after the end of the "low" list.  
      To do that, I first have to find the end of the "low" list...
  */
  current = temp = low_list;
  while(1)
  {
    current = getnext(current);
    if(NULL == current)
      break;
    temp = current;
  }

  /*
      Then, I put the "high" list at the end of the low list
  */
  setnext(temp, high_list);
  return(low_list);
}
size_t commafmt(char   *buf,            /* Buffer for formatted string  */
                int     bufsize,        /* Size of buffer               */
                gold_int    N)              /* Number to convert            */
{
  int len = 1, posn = 1, sign = 1;
  char *ptr = buf + bufsize - 1;

  if (2 > bufsize)
  {
  ABORT:      *buf = NUL;
    return 0;
  }

  *ptr-- = NUL;
  --bufsize;
  if (0L > N)
  {
    sign = -1;
    N = -N;
  }

  for ( ; len <= bufsize; ++len, ++posn)
  {
    *ptr-- = (char)((N % 10L) + '0');
    if (0L == (N /= 10L))
      break;
    if (0 == (posn % 3))
    {
      *ptr-- = ',';
      ++len;
    }
    if (len >= bufsize)
      goto ABORT;
  }

  if (0 > sign)
  {
    if (len >= bufsize)
      goto ABORT;
    *ptr-- = '-';
    ++len;
  }

  memmove(buf, ++ptr, len + 1);
  return (size_t)len;
}

static char *text[] = {"th", "st", "nd", "rd"};

char *ordinal_text(int number)
{
  if (((number %= 100) > 9 && number < 20) || (number %= 10) > 3)
    number = 0;
  return text[number];
}

/* wiz functions */
#include <signal.h>

#define IMM_LMARG "   "
#define IMM_NSIZE  16
#define LINE_LEN   64
#define MIN_LEVEL LVL_HERO

/* max level that should be in columns instead of centered */
#define COL_LEVEL LVL_HERO

struct name_rec
{
  char name[25];
  struct name_rec *next;
};

struct control_rec
{
  int level;
  char *level_name;
};

struct level_rec
{
  struct control_rec *params;
  struct level_rec *next;
  struct name_rec *names;
};

struct control_rec level_params[] =
  {
    {LVL_HERO, "Heros"},
    {LVL_GOD, "Gods"},
    {LVL_GRGOD, "Greater Gods"},
    {LVL_IMPL, "Implementors"},
    {0, ""}
  };


struct level_rec *levels = 0;

void wiz_initialize(void)
{
  struct level_rec *tmp;
  int i = 0;

  while (level_params[i].level > 0)
  {
    tmp = (struct level_rec *) malloc(sizeof(struct level_rec));
    tmp->names = 0;
    tmp->params = &(level_params[i++]);
    tmp->next = levels;
    levels = tmp;
  }
}


void wiz_read_file(void)
{
  void wiz_add_name(byte level, char *name);
  struct char_data *vict;
  int i;
  extern struct player_index_element *player_table;	/* index to plr file     */
  extern int top_of_p_table;

  for (i = 0; i <= top_of_p_table; i++)
    if (*player_table[i].name)
    {



      CREATE(vict, struct char_data, 1);
      clear_char(vict);
      TEMP_LOAD_CHAR = TRUE;
      //CREATE(vict->player_specials, struct player_special_data, 1);
      if (store_to_char(player_table[i].name, vict) > -1)
      {
        free_char(vict);
        TEMP_LOAD_CHAR = TRUE;
        continue;
      }


      if (( GET_LEVEL(vict) >= MIN_LEVEL &&
            !(PLR_FLAGGED(vict, PLR_FROZEN)) &&
            !PLR_FLAGGED(vict, PLR_NOWIZLIST)) &&
          !PLR_FLAGGED(vict, PLR_DELETED))
      {
        log("Update Wizlist: %d - %s", GET_LEVEL(vict), GET_NAME(vict));
        wiz_add_name(GET_LEVEL(vict), GET_NAME(vict));
      }
      TEMP_LOAD_CHAR = TRUE;

      free_char(vict);
    }


}


void wiz_add_name(byte level, char *name)
{
  struct name_rec *tmp;
  struct level_rec *curr_level;
  char *ptr;

  if (!*name)
    return;

  for (ptr = name; *ptr; ptr++)
    if (!isalpha(*ptr))
      return;

  tmp = (struct name_rec *) malloc(sizeof(struct name_rec));
  strcpy(tmp->name, name);
  tmp->next = 0;

  curr_level = levels;
  while (curr_level->params->level > level)
    curr_level = curr_level->next;

  tmp->next = curr_level->names;
  curr_level->names = tmp;
}


void wiz_sort_names(void)
{
  struct level_rec *curr_level;
  struct name_rec *a, *b;
  char temp[100];

  for (curr_level = levels; curr_level; curr_level = curr_level->next)
  {
    for (a = curr_level->names; a && a->next; a = a->next)
    {
      for (b = a->next; b; b = b->next)
      {
        if (strcmp(a->name, b->name) > 0)
        {
          strcpy(temp, a->name);
          strcpy(a->name, b->name);
          strcpy(b->name, temp);
        }
      }
    }
  }
}


void write_wizlist(FILE * out, int minlev, int maxlev)
{
  char buf[100];
  struct level_rec *curr_level;
  struct name_rec *curr_name;
  unsigned int i;
  unsigned int j;

  fprintf(out,
          "****************************************************************************\n"
          "* The following people have reached immortality on CircleMUD.  They are to *\n"
          "* treated with respect and awe.  Occasional prayers to them are advisable. *\n"
          "* Annoying them is not recommended.  Stealing from them is punishable by   *\n"
          "* immediate death.                                                         *\n"
          "****************************************************************************\n\n");

  for (curr_level = levels; curr_level; curr_level = curr_level->next)
  {
    if (curr_level->params->level < minlev ||
        curr_level->params->level > maxlev)
      continue;
    i = 39 - (strlen(curr_level->params->level_name) >> 1);
    for (j = 1; j <= i; j++)
      fputc(' ', out);
    fprintf(out, "%s\n", curr_level->params->level_name);
    for (j = 1; j <= i; j++)
      fputc(' ', out);
    for (j = 1; j <= strlen(curr_level->params->level_name); j++)
      fputc('~', out);
    fprintf(out, "\n");

    strcpy(buf, "");
    curr_name = curr_level->names;
    while (curr_name)
    {
      strlcat(buf, curr_name->name, sizeof(buf));
      if (strlen(buf) > LINE_LEN)
      {
        if (curr_level->params->level <= COL_LEVEL)
          fprintf(out, IMM_LMARG);
        else
        {
          i = 40 - (strlen(buf) >> 1);
          for (j = 1; j <= i; j++)
            fputc(' ', out);
        }
        fprintf(out, "%s\n", buf);
        strlcpy(buf, "", sizeof(buf));
      }
      else
      {
        if (curr_level->params->level <= COL_LEVEL)
        {
          for (j = 1; j <= (IMM_NSIZE - strlen(curr_name->name)); j++)
            strlcat(buf, " ", sizeof(buf));
        }
        if (curr_level->params->level > COL_LEVEL)
          strlcat(buf, "   ", sizeof(buf));
      }
      curr_name = curr_name->next;
    }

    if (*buf)
    {
      if (curr_level->params->level <= COL_LEVEL)
        fprintf(out, "%s%s\n", IMM_LMARG, buf);
      else
      {
        i = 40 - (strlen(buf) >> 1);
        for (j = 1; j <= i; j++)
          fputc(' ', out);
        fprintf(out, "%s\n", buf);
      }
    }
    fprintf(out, "\n");
  }
}




int create_wizlist(int wizlevel,char *file1, int immlevel, char *file2)
{
  FILE *fl;
  int ret = 1;
  log("Starting wizlist creation");
  wiz_initialize();
  wiz_read_file();
  wiz_sort_names();

  if (!(fl = fopen(file1, "w")))
    ret = 0;
  else
  {
    log("Writing.. Wiz");
    write_wizlist(fl, wizlevel, LVL_IMPL);
    fclose(fl);
  }

  if (!(fl = fopen(file2, "w")))
    ret = 0;
  else
  {
    log("Writing.. Imm");
    write_wizlist(fl, immlevel, wizlevel - 1);
    fclose(fl);

  }
  log("Wizlist Done -- %s", YESNO(ret)); /* and here i thought it said wish list?? */
  return (ret);
}


long time_to_sec(time_t timeCheck)
{
  long secs;

  secs = (long) (timeCheck - time(0));

  return (secs);
}

time_t sec_to_time(int sec)
{

  time_t timeN;

  timeN = (time_t) (sec + time(0));

  return timeN;

}

char *stristr(const char *String, const char *Pattern)
{
  char *pptr, *sptr, *start;
  if (String == NULL)
    return NULL;

  for (start = (char *)String; *start != NUL; start++)
  {
    /* find start of pattern in string */
    for ( ; ((*start!=NUL) && (toupper(*start) != toupper(*Pattern))); start++)
      ;
    if (NUL == *start)
      return NULL;

    pptr = (char *)Pattern;
    sptr = (char *)start;

    while (toupper(*sptr) == toupper(*pptr))
    {
      sptr++;
      pptr++;

      /* if end of pattern then pattern was found */

      if (NUL == *pptr)
        return (start);
    }
  }
  return NULL;
}


#if 0

size_t strlcpy(char *dest, const char *src, size_t copylen)
{
  size_t len = strlen (src);

  if (len < copylen)
  {
    memcpy (dest, src, len + 1);
  }
  else
  {
    if (copylen)
    {
      memcpy (dest, src, copylen - 1);

      dest [copylen - 1] = 0;
    }
  }

  return len;
}
#endif

#ifndef HAVE_STRLCAT

/* sometimes strlen() is faster than using pointers ..
 * In this case, uncomment the following
 */
/*#define STRLEN_FASTER*/


/* append src to dst, guaranteeing a null terminator.
 * If dst+src is too big, truncate it.
 * Return strlen(old dst)+dstrlen(src).
 */
size_t strlcat(char *dest, const char *src, size_t copylen)
{
  size_t n=0;

  /* find the end of string in dst */
#ifdef STRLEN_FASTER
  if (!copylen)
    return strlen(src);
  n = strlen(dest);
  dst += n;
#else
  while (n < copylen && *dest++)
    ++n;

  if (n >= copylen)
    return copylen + strlen(src);
  /* back up over the '\0' */
  --dest;
#endif

  /* copy bytes from src to dst.
   * If there's no space left, stop copying
   * if we copy a '\0', stop copying
   */
  while (n < copylen)
  {
    if (!(*dest++ = *src++))
      return n;
    ++n;
  }

  if (n == copylen)
  {
    /* overflow, so truncate the string, and ... */
    if (copylen)
      dest[-1] = '\0';
    /* ... work out what the length would have been had there been
     * enough space in the buffer
     */
    n += strlen(dest);
  }

  return n;
}

#endif /* HAVE_STRLCAT */



