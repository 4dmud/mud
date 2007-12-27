/* ************************************************************************
*   File: boards.c                                      Part of CircleMUD *
*  Usage: handling of multiple bulletin boards                            *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */


/* FEATURES & INSTALLATION INSTRUCTIONS ***********************************
 
This board code has many improvements over the infamously buggy standard
Diku board code.  Features include:
 
- Arbitrary number of boards handled by one set of generalized routines.
  Adding a new board is as easy as adding another entry to an array.
- Safe removal of messages while other messages are being written.
- Does not allow messages to be removed by someone of a level less than
  the poster's level.
 
 
TO ADD A NEW BOARD, simply follow our easy 4-step program:
 
1 - Create a new board object in the object files
 
2 - Increase the NUM_OF_BOARDS constant in boards.h
 
3 - Add a new line to the board_info array below.  The fields, in order, are:
 
	Board's virtual number.
	Min level one must be to look at this board or read messages on it.
	Min level one must be to post a message to the board.
	Min level one must be to remove other people's messages from this
		board (but you can always remove your own message).
	Filename of this board, in quotes.
	Last field must always be 0.
 
4 - In spec_assign.c, find the section which assigns the special procedure
    gen_board to the other bulletin boards, and add your new one in a
    similar fashion.
 
*/


#include "conf.h"
#include "sysdep.h"


#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "db.h"
#include "boards.h"
 #include "interpreter.h"
 #include "handler.h"
#include "improved-edit.h"
#include "clan.h"

/* Board appearance order. */
#define	NEWEST_AT_TOP	FALSE

#define IMMORTAL_POST_COLOR "{cC"

/*
format:	vnum, read lvl, write lvl, remove lvl, filename, 0 at end
Be sure to also change NUM_OF_BOARDS in board.h
*/
struct board_info_type board_info[NUM_OF_BOARDS] =
  {
    {590, 0, 0, LVL_GOD, LIB_ETC "board.newbie", 0},
    {1315, 0, 0, LVL_GOD, LIB_ETC "board.sicilian", 0},
    {2914, 0, 0, LVL_GOD, LIB_ETC "board.table_round", 0},
    {2917, 0, 0, LVL_GOD, LIB_ETC "board.viking", 0},
    {2999, 0, 0, LVL_GOD, LIB_ETC "board.rt", 0},
    {3079, 0, 0, LVL_GOD, LIB_ETC "board.rp5", 0},
    {3080, 0, LVL_GOD, LVL_GOD, LIB_ETC "board.mort", 0},
    {3089, 0, 0, LVL_GOD, LIB_ETC "board.ideas", 0},
    {3098, LVL_GOD, LVL_GOD, LVL_GRGOD, LIB_ETC "board.immort", 0},
    {3399, 0, 0, LVL_GOD, LIB_ETC "board.mort1", 0},
    {3201, 0, LVL_GOD, LVL_SEN, LIB_ETC "board.announce", 0},
    {8400, 0, 0, LVL_GOD, LIB_ETC "board.rp1", 0},
    {8401, 0, 0, LVL_GOD, LIB_ETC "board.rp2", 0},
    {8402, 0, 0, LVL_GOD, LIB_ETC "board.rp3", 0},
    {8403, 0, 0, LVL_GOD, LIB_ETC "board.rp4", 0},
    {8449, 0, 0, LVL_GOD, LIB_ETC "board.spy", 0},
    {8470, 0, 0, LVL_GOD, LIB_ETC "board.bitch", 0},
    {9421, 0, 0, LVL_GOD, LIB_ETC "board.chaos", 0},
    {9435, 0, 0, LVL_GOD, LIB_ETC "board.chaos2", 0},
    {10026, 0, 0, LVL_GOD, LIB_ETC "board.dragon", 0},    
    {10031, 0, 0, LVL_GOD, LIB_ETC "board.dragon2", 0},
    {10402, 0, 0, LVL_GOD, LIB_ETC "board.dj", 0},
    {12000, 0, 0, LVL_GOD, LIB_ETC "board.zaade", 0},
    {12422, 0, 0, LVL_GOD, LIB_ETC "board.saints", 0},
    {12902, 0, 0, LVL_GOD, LIB_ETC "board.seekers", 0},
    {17101, 0, 0, LVL_GOD, LIB_ETC "board.house", 0},
    {17807, 0, 0, LVL_GOD, LIB_ETC "board.hero", 0},
    {28705, 0, 0, LVL_GOD, LIB_ETC "board.vampire", 0},
    {10233, 0, 0, LVL_GOD, LIB_ETC "board.qual", 0}


  };

/* local functions */
SPECIAL(gen_board);
int find_slot(void);
int find_board(struct char_data *ch);
void init_boards(void);

char *msg_storage[INDEX_SIZE];
int msg_storage_taken[INDEX_SIZE];
int num_of_msgs[NUM_OF_BOARDS];
int ACMD_READ, ACMD_LOOK, ACMD_EXAMINE, ACMD_WRITE, ACMD_REMOVE;
struct board_msginfo msg_index[NUM_OF_BOARDS][MAX_BOARD_MESSAGES];


int find_slot(void)
{
  int i;

  for (i = 0; i < INDEX_SIZE; i++)
    if (!msg_storage_taken[i])
    {
      msg_storage_taken[i] = 1;
      return (i);
    }
  return (-1);
}


/* search the room ch is standing in to find which board he's looking at */
int find_board(struct char_data *ch)
{
  struct obj_data *obj;
  int i;

  for (obj = ch->in_room->contents; obj; obj = obj->next_content)
    for (i = 0; i < NUM_OF_BOARDS; i++)
      if (BOARD_RNUM(i) == GET_OBJ_RNUM(obj))
        return (i);

  if (GET_LEVEL(ch) >= LVL_IMMORT)
    for (obj = ch->carrying; obj; obj = obj->next_content)
      for (i = 0; i < NUM_OF_BOARDS; i++)
        if (BOARD_RNUM(i) == GET_OBJ_RNUM(obj))
          return (i);

  return (-1);
}


void init_boards(void)
{
  int i, j, fatal_error = 0;

  for (i = 0; i < INDEX_SIZE; i++)
  {
    msg_storage[i] = 0;
    msg_storage_taken[i] = 0;
  }

  for (i = 0; i < NUM_OF_BOARDS; i++)
  {
    if ((BOARD_RNUM(i) = real_object(BOARD_VNUM(i))) == NOTHING)
    {
      log("SYSERR: Fatal board error: board vnum %d does not exist!",
          BOARD_VNUM(i));
      continue;
      //fatal_error = 1; /*stopped so that the buildport can run*/
    }
    num_of_msgs[i] = 0;
    for (j = 0; j < MAX_BOARD_MESSAGES; j++)
    {
      memset((char *) &(msg_index[i][j]), 0, sizeof(struct board_msginfo));
      msg_index[i][j].slot_num = -1;
    }
    Board_load_board(i);
  }

  if (fatal_error)
    exit(1);
}


SPECIAL(gen_board)
{
  int board_type;
  static int loaded = 0;
  struct obj_data *board = (struct obj_data *) me;

  if (!loaded)
  {
    init_boards();
    loaded = 1;
  }
  if (!ch->desc)
    return (0);

  ACMD_READ = find_command("read");
  ACMD_WRITE = find_command("write");
  ACMD_REMOVE = find_command("remove");
  ACMD_LOOK = find_command("look");
  ACMD_EXAMINE = find_command("examine");



  if (cmd != ACMD_WRITE && cmd != ACMD_LOOK && cmd != ACMD_EXAMINE &&
      cmd != ACMD_READ && cmd != ACMD_REMOVE)
    return (0);

  if ((board_type = find_board(ch)) == -1)
  {
    log("SYSERR:  degenerate board!  (what the hell...)");
    return (0);
  }
  if (cmd == ACMD_WRITE)
    return (Board_write_message(board_type, ch, argument, board));
  else if (cmd == ACMD_LOOK || cmd == ACMD_EXAMINE)
    return (Board_show_board(board_type, ch, argument, board));
  else if (cmd == ACMD_READ)
    return (Board_display_msg(board_type, ch, argument, board));
  else if (cmd == ACMD_REMOVE)
    return (Board_remove_msg(board_type, ch, argument, board));
  else
    return (0);
}


int Board_write_message(int board_type, struct char_data *ch, char *arg,
                        struct obj_data *board)
{
  char *tmstr;
  time_t ct;
  char buf[MAX_INPUT_LENGTH], buf2[MAX_INPUT_LENGTH];

  if (GET_LEVEL(ch) < WRITE_LVL(board_type))
  {
    send_to_char("You are not holy enough to write on this board.\r\n",
                 ch);
    return (1);
  }
  if (num_of_msgs[board_type] >= MAX_BOARD_MESSAGES)
  {
    send_to_char("The board is full.\r\n", ch);
    new_mudlog( NRM, LVL_SEN, TRUE, "Board vnum: %d, is full.", GET_OBJ_VNUM(board));
    return (1);
  }
  if ((NEW_MSG_INDEX(board_type).slot_num = find_slot()) == -1)
  {
    send_to_char("The board is malfunctioning - sorry.\r\n", ch);
    log("SYSERR: Board: failed to find empty slot on write.");
    return (1);
  }
  /* skip blanks */
  skip_spaces(&arg);
  delete_doubledollar(arg);

  /* JE 27 Oct 95 - Truncate headline at 80 chars if it's longer than that */
  arg[80] = '\0';

  if (!*arg)
  {
    send_to_char("We must have a headline!\r\n", ch);
    return (1);
  }
  ct = time(0);
  tmstr = (char *) asctime(localtime(&ct));
  *(tmstr + strlen(tmstr) - 1) = '\0';

  snprintf(buf2, sizeof(buf2), "(%s)", GET_NAME(ch));
  snprintf(buf, sizeof(buf), "%6.10s %-12s :: %s", tmstr, buf2, arg);
  NEW_MSG_INDEX(board_type).heading = strdup(buf);
  NEW_MSG_INDEX(board_type).level = GET_LEVEL(ch);

  new_send_to_char(ch, "Write your message.\r\n");
  send_editor_help(ch->desc);
  act("$n starts to write a message.", TRUE, ch, 0, 0, TO_ROOM);

  string_write(ch->desc,
               &(msg_storage[NEW_MSG_INDEX(board_type).slot_num]),
               MAX_MESSAGE_LENGTH, board_type + BOARD_MAGIC, NULL);

  num_of_msgs[board_type]++;
  return (1);
}


int Board_show_board(int board_type, struct char_data *ch, char *arg,
                     struct obj_data *board)
{
  int i;
  char tmp[MAX_STRING_LENGTH], buf[MAX_STRING_LENGTH];

  if (!ch->desc)
    return (0);

  if (BOARD_RNUM(board_type) == NOTHING)
    return 0;
  one_argument(arg, tmp);

  if (!*tmp || !isname(tmp, board->name))
    return (0);

  if (GET_LEVEL(ch) < READ_LVL(board_type))
  {
    send_to_char("You try but fail to understand the holy words.\r\n",
                 ch);
    return (1);
  }
  act("$n studies the board.", TRUE, ch, 0, 0, TO_ROOM);


  if (!num_of_msgs[board_type])
    new_send_to_char(ch, "This is a bulletin board.  Usage: READ/REMOVE <messg #>, WRITE <header>.\r\nThe board is empty.\r\n");
  else
  {
    size_t len = 0;
    int nlen;

    len = snprintf(buf, sizeof(buf),
                   "This is a bulletin board.  Usage: READ/REMOVE <messg #>, WRITE <header>.\r\n"
                   "You will need to look at the board to save your message.\r\n"
                   "There are %d messages on the board.\r\n",
                   num_of_msgs[board_type]);
#if NEWEST_AT_TOP
    for (i = num_of_msgs[board_type] - 1; i >= 0; i--)
    {
      if (!MSG_HEADING(board_type, i))
        goto fubar;

      nlen = snprintf(buf + len, sizeof(buf) - len, "%-2d : %s%s{c0\r\n", num_of_msgs[board_type] - i, (MSG_LEVEL(board_type, i) > LVL_IMMORT ? IMMORTAL_POST_COLOR: ""), MSG_HEADING(board_type, i));
      if (len + nlen >= sizeof(buf) || nlen < 0)
        break;
      len += nlen;
    }
#else
    for (i = 0; i < num_of_msgs[board_type]; i++)
    {
      if (!MSG_HEADING(board_type, i))
        goto fubar;

      nlen = snprintf(buf + len, sizeof(buf) - len, "%-2d : %s%s{c0\r\n", i + 1, (MSG_LEVEL(board_type, i) > LVL_IMMORT ? IMMORTAL_POST_COLOR: ""), MSG_HEADING(board_type, i));
      if (len + nlen >= sizeof(buf) || nlen < 0)
        break;
      len += nlen;
    }
#endif
    page_string(ch->desc, buf, 1);
  }
  return (1);

fubar:
  log("SYSERR: Board %d is fubar'd.", board_type);
  new_send_to_char(ch, "Sorry, the board isn't working.\r\n");
  return (1);
}


int Board_display_msg(int board_type, struct char_data *ch, char *arg,
                      struct obj_data *board)
{
  char number[MAX_STRING_LENGTH], buffer[MAX_STRING_LENGTH];
  int msg, ind;

  if (BOARD_RNUM(board_type) == NOTHING)
    return 0;

  one_argument(arg, number);
  if (!*number)
    return (0);
  if (isname(number, board->name))	/* so "read board" works */
    return (Board_show_board(board_type, ch, arg, board));
  if (!is_number(number))	/* read 2.mail, look 2.sword */
    return (0);
  if (!(msg = atoi(number)))
    return (0);

  if (GET_LEVEL(ch) < READ_LVL(board_type))
  {
    send_to_char("You try but fail to understand the holy words.\r\n",
                 ch);
    return (1);
  }
  if (!num_of_msgs[board_type])
  {
    send_to_char("The board is empty!\r\n", ch);
    return (1);
  }
  if (msg < 1 || msg > num_of_msgs[board_type])
  {
    send_to_char("That message exists only in your imagination.\r\n",
                 ch);
    return (1);
  }
#if NEWEST_AT_TOP
  ind = num_of_msgs[board_type] - msg;
#else
  ind = msg - 1;
#endif
  if (MSG_SLOTNUM(board_type, ind) < 0 ||
      MSG_SLOTNUM(board_type, ind) >= INDEX_SIZE)
  {
    send_to_char("Sorry, the board is not working.\r\n", ch);
    log("SYSERR: Board is screwed up. (Room #%d)",
        GET_ROOM_VNUM(IN_ROOM(ch)));
    return (1);
  }
  if (!(MSG_HEADING(board_type, ind)))
  {
    send_to_char("That message appears to be screwed up.\r\n", ch);
    return (1);
  }
  if (!(msg_storage[MSG_SLOTNUM(board_type, ind)]))
  {
    send_to_char("That message seems to be empty.\r\n", ch);
    return (1);
  }
  snprintf(buffer, sizeof(buffer), "Message %d : %s\r\n\r\n%s\r\n", msg,
           MSG_HEADING(board_type, ind),
           msg_storage[MSG_SLOTNUM(board_type, ind)]);

  page_string(ch->desc, buffer, TRUE);

  return (1);
}


int Board_remove_msg(int board_type, struct char_data *ch, char *arg,
                     struct obj_data *board)
{
  int ind, msg, slot_num, id;
  char number[MAX_INPUT_LENGTH], buf[MAX_INPUT_LENGTH];
  struct descriptor_data *d;

  one_argument(arg, number);

  if (!*number || !is_number(number))
    return (0);
  if (!(msg = atoi(number)))
    return (0);

  if (!num_of_msgs[board_type])
  {
    send_to_char("The board is empty!\r\n", ch);
    return (1);
  }
  if (msg < 1 || msg > num_of_msgs[board_type])
  {
    send_to_char("That message exists only in your imagination.\r\n",
                 ch);
    return (1);
  }
#if NEWEST_AT_TOP
  ind = num_of_msgs[board_type] - msg;
#else
  ind = msg - 1;
#endif
  if (!MSG_HEADING(board_type, ind))
  {
    send_to_char("That message appears to be screwed up.\r\n", ch);
    return (1);
  }
  snprintf(buf, sizeof(buf), "(%s)", GET_NAME(ch));
  if (GET_LEVEL(ch) < LVL_IMMORT) {
  if (!(GET_CLAN(ch) > 0 &&
        (id = find_clan_by_id(GET_CLAN(ch))) >= 0 &&
        GET_CLAN_RANK(ch) == clan[id].ranks &&
        GET_OBJ_VNUM(board) == clan[id].board) &&
      !(strstr(MSG_HEADING(board_type, ind), buf)))
  {
    send_to_char
    ("You are not holy enough to remove other people's messages.\r\n",
     ch);
    return (1);
  }
  }
  if (GET_LEVEL(ch) < MSG_LEVEL(board_type, ind))
  {
    send_to_char
    ("You can't remove a message holier than yourself.\r\n", ch);
    return (1);
  }
  slot_num = MSG_SLOTNUM(board_type, ind);
  if (slot_num < 0 || slot_num >= INDEX_SIZE)
  {
    send_to_char("That message is majorly screwed up.\r\n", ch);
    log("SYSERR: The board is seriously screwed up. (Room #%d)",
        GET_ROOM_VNUM(IN_ROOM(ch)));
    return (1);
  }
  for (d = descriptor_list; d; d = d->next)
    if (STATE(d) == CON_PLAYING && d->str == &(msg_storage[slot_num]))
    {
      send_to_char
      ("At least wait until the author is finished before removing it!\r\n",
       ch);
      return (1);
    }
  if (msg_storage[slot_num])
    free(msg_storage[slot_num]);
  msg_storage[slot_num] = 0;
  msg_storage_taken[slot_num] = 0;
  if (MSG_HEADING(board_type, ind))
    free(MSG_HEADING(board_type, ind));

  for (; ind < num_of_msgs[board_type] - 1; ind++)
  {
    MSG_HEADING(board_type, ind) = MSG_HEADING(board_type, ind + 1);
    MSG_SLOTNUM(board_type, ind) = MSG_SLOTNUM(board_type, ind + 1);
    MSG_LEVEL(board_type, ind) = MSG_LEVEL(board_type, ind + 1);
  }
  num_of_msgs[board_type]--;
  send_to_char("Message removed.\r\n", ch);
  snprintf(buf, sizeof(buf), "$n just removed message %d.", msg);
  act(buf, FALSE, ch, 0, 0, TO_ROOM);
  Board_save_board(board_type);

  return (1);
}


void Board_save_board(int board_type)
{
  FILE *fl;
  int i;
  char *tmp1, *tmp2 = NULL;

  if (!num_of_msgs[board_type])
  {
    remove(FILENAME(board_type));
    return;
  }
  if (!(fl = fopen(FILENAME(board_type), "wb")))
  {
    perror("SYSERR: Error writing board");
    return;
  }
  fwrite(&(num_of_msgs[board_type]), sizeof(int), 1, fl);

  for (i = 0; i < num_of_msgs[board_type]; i++)
  {
    if ((tmp1 = MSG_HEADING(board_type, i)) != NULL)
      msg_index[board_type][i].heading_len = strlen(tmp1) + 1;
    else
      msg_index[board_type][i].heading_len = 0;

    if (MSG_SLOTNUM(board_type, i) < 0 ||
        MSG_SLOTNUM(board_type, i) >= INDEX_SIZE ||
        (!(tmp2 = msg_storage[MSG_SLOTNUM(board_type, i)])))
      msg_index[board_type][i].message_len = 0;
    else
      msg_index[board_type][i].message_len = strlen(tmp2) + 1;

    fwrite(&(msg_index[board_type][i]), sizeof(struct board_msginfo), 1, fl);
    if (tmp1)
      fwrite(tmp1, sizeof(char),
             msg_index[board_type][i].heading_len, fl);
    if (tmp2)
      fwrite(tmp2, sizeof(char),
             msg_index[board_type][i].message_len, fl);
  }

  fclose(fl);
}


void Board_load_board(int board_type)
{
  FILE *fl;
  int i, len1, len2;
  char *tmp1, *tmp2;
  
  if (!fileExists(FILENAME(board_type))) {
  log("Board file %s doesn't exist, creating now.", FILENAME(board_type));
  fl = fopen(FILENAME(board_type), "wb");
  fprintf(fl, "%d\n", 0);
  fclose(fl);
  chmod(FILENAME(board_type), 666);
  }

  if (!(fl = fopen(FILENAME(board_type), "rb")))
  {
    if (errno != ENOENT)
      perror("SYSERR: Error reading board");
    return;
  }
  fread(&(num_of_msgs[board_type]), sizeof(int), 1, fl);
  if (num_of_msgs[board_type] < 1
      || num_of_msgs[board_type] > MAX_BOARD_MESSAGES)
  {
    log("SYSERR: Board file %d corrupt.  Resetting.", board_type);
    Board_reset_board(board_type);
    return;
  }
  for (i = 0; i < num_of_msgs[board_type]; i++)
  {
    fread(&(msg_index[board_type][i]), sizeof(struct board_msginfo), 1, fl);
    if ((len1 = msg_index[board_type][i].heading_len) <= 0)
    {
      log("SYSERR: Board file %d corrupt!  Resetting.", board_type);
      Board_reset_board(board_type);
      return;
    }
    CREATE(tmp1, char, len1);
    fread(tmp1, sizeof(char), len1, fl);
    MSG_HEADING(board_type, i) = tmp1;

    if ((MSG_SLOTNUM(board_type, i) = find_slot()) == -1)
    {
      log("SYSERR: Out of slots booting board %d!  Resetting...",
          board_type);
      Board_reset_board(board_type);
      return;
    }
    if ((len2 = msg_index[board_type][i].message_len) > 0)
    {
      CREATE(tmp2, char, len2);
      fread(tmp2, sizeof(char), len2, fl);
      msg_storage[MSG_SLOTNUM(board_type, i)] = tmp2;
    }
    else
      msg_storage[MSG_SLOTNUM(board_type, i)] = NULL;
  }

  fclose(fl);
}

/* When shutting down, clear all boards. */
void Board_clear_all(void)
{
  int i;

  for (i = 0; i < NUM_OF_BOARDS; i++)
    Board_clear_board(i);
}

/* Clear the in-memory structures. */
void Board_clear_board(int board_type)
{
  int i;
  if (BOARD_RNUM(board_type) == NOTHING)
    return;

  for (i = 0; i < MAX_BOARD_MESSAGES; i++)
  {
    if (MSG_HEADING(board_type, i))
      free(MSG_HEADING(board_type, i));
    if (msg_storage[MSG_SLOTNUM(board_type, i)])
      free(msg_storage[MSG_SLOTNUM(board_type, i)]);
    msg_storage_taken[MSG_SLOTNUM(board_type, i)] = 0;
    memset((char *)&(msg_index[board_type][i]),0,sizeof(struct board_msginfo));
    msg_index[board_type][i].slot_num = -1;
  }
  num_of_msgs[board_type] = 0;
}

/* Destroy the on-disk and in-memory board. */
void Board_reset_board(int board_type)
{
  Board_clear_board(board_type);
  remove(FILENAME(board_type));
}
/*
void Board_reset_board(int board_type)
{
    int i;
 
    for (i = 0; i < MAX_BOARD_MESSAGES; i++) {
	if (MSG_HEADING(board_type, i))
	    free(MSG_HEADING(board_type, i));
	if (msg_storage[MSG_SLOTNUM(board_type, i)])
	    free(msg_storage[MSG_SLOTNUM(board_type, i)]);
	msg_storage_taken[MSG_SLOTNUM(board_type, i)] = 0;
	memset((char *) &(msg_index[board_type][i]), 0, sizeof(struct board_msginfo));
	msg_index[board_type][i].slot_num = -1;
    }
    num_of_msgs[board_type] = 0;
    remove(FILENAME(board_type));
}
*/

