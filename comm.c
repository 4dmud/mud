/* ************************************************************************
*   File: comm.c                                        Part of CircleMUD *
*  Usage: Communication, socket handling, main(), central game loop       *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */
/*
 * Compression support.  Currently could be used with:
 *
 *   MUD Client for Linux, by Erwin S. Andreasen
 *     http://www.andreasen.org/mcl/
 *
 *   mcclient, by Oliver 'Nemon@AR' Jowett
 *     http://homepages.ihug.co.nz/~icecube/compress/
 *
 * Contact them for help with the clients. Contact greerga@circlemud.org
 * for problems with the server end of the connection.  If you think you
 * have found a bug, please test another MUD for the same problem to see
 * if it is a client or server problem.
 *
 * Version 2 support added.
 */
#define __COMM_C__

#include "conf.h"
#include "sysdep.h"

#if CIRCLE_GNU_LIBC_MEMORY_TRACK
# include <mcheck.h>
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

#define RUNNING_IDENT 0

#ifdef HAVE_ARPA_TELNET_H
#include <arpa/telnet.h>
#else
#include "telnet.h"
#endif
/*
 * Note, most includes for all platforms are in sysdep.h.  The list of
 * files that is included is controlled by conf.h for that platform.
 */

#include "structs.h"
#include "utils.h"
#include "comm.h"
#include "interpreter.h"
#include "handler.h"
#include "db.h"
#include "house.h"
#include "dg_scripts.h"
#include "screen.h"
#include "arena.h"
#include "mail.h"
#include "dg_event.h"
#include "clan.h"
#include "oasis.h"
#include "genolc.h"
#include "constants.h"
#include "ident.h"
#include "auction.h"
#include "mxp.h"


#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1
#endif

/* externs */
extern struct ban_list_element *ban_list;
extern int num_invalid;
extern char *GREETINGS;
extern const char *circlemud_version;
extern const char *oasisolc_version;
extern int circle_restrict;
extern int mini_mud;
extern int no_rent_check;
extern FILE *qic_fl;
extern ush_int DFLT_PORT;
extern int id_serv_socket;

extern int *cmd_sort_info;

extern struct room_data *world_vnum[];  /* In db.c */
extern struct time_info_data time_info; /* In db.c */
extern char help[];

extern int top_of_zone_table;
extern struct zone_data *zone_table;
extern const char *save_info_msg[];     /* In olc.c */
extern char *race_abbrevs[];

const char *ACTNULL = "<NULL>";

/* local globals */
struct comm_data * comlist = NULL;
int syslogfd = -1;
struct meta_host_data *host_list = NULL;
int message_type = NOTHING;
void string_format(BYTE * cmd, LWORD space);
Descriptor *descriptor_list = NULL;   /* master desc list */
struct txt_block *bufpool = NULL;  /* pool of large output buffers */
int buf_largecount = 0;       /* # of large buffers which exist */
int buf_overflows = 0;        /* # of overflows of output */
int buf_switches = 0;         /* # of switches from small to large buf */
int circle_shutdown = 0; /* clean shutdown */
int circle_reboot = 0;        /* reboot the game after a shutdown */
int no_specials = 0;          /* Suppress ass. of special routines */
int max_players = 0;          /* max descriptors available */
int tics_passed = 0;          /* for extern checkpointing */
int scheck = 0;               /* for syntax checking mode */
int dg_act_check = 0;         /* toggle for act_trigger */
unsigned long pulse = 0; /* number of pulses since game start */
extern const int xap_objs;    /* ascii objects. */
struct timeval null_time;     /* zero-valued time structure */
FILE *logfile = NULL;         /* Where to send the log messages. */
FILE *comfile = NULL; /* where to send communications */
struct comm_data *commlist = NULL;
int intermud_desc;
int connected_to_intermud;
byte reread_wizlist;          /* signal: SIGUSR1 */
byte emergency_unban;         /* signal: SIGUSR2 */

void free_host_list(struct meta_host_data *thi);
void add_ip_to_host_list(char *host_ip, char *host, time_t date);

int no_ext_processes = 0;       /* shall we use external processes? */
extern int use_external_lookup_process; /* see config.c */

static bool fCopyOver;        /* Are we booting in copyover mode? */
socket_t mother_desc;         /* Now a global */
ush_int port = 0;
char last_command[MAX_STRING_LENGTH] = "Empty";

/* functions in this file */

void send_out_signals(Descriptor *d);
char * parse_prompt(Character *ch, char *str, size_t lenn);
void clear_char_q(Descriptor *t);
RETSIGTYPE reread_wizlists(int sig);
RETSIGTYPE unrestrict_game(int sig);
RETSIGTYPE reap(int sig);
RETSIGTYPE checkpointing(int sig);
RETSIGTYPE hupsig(int sig);
ssize_t perform_socket_read(socket_t desc, char *read_point, size_t space_left);
ssize_t perform_socket_write(socket_t desc, const char *txt, size_t length, struct compr *comp);
void echo_off(Descriptor *d);
void echo_on(Descriptor *d);
void circle_sleep(struct timeval *timeout);
void signal_setup(void);
void game_loop(socket_t s_mother_desc);
void timediff(struct timeval *diff, struct timeval *a, struct timeval *b);
void timeadd(struct timeval *sum, struct timeval *a, struct timeval *b);
void flush_queues(Descriptor *d);
void nonblock(socket_t s);
int perform_subst(Descriptor *t, char *orig, char *subst);
void record_usage(void);
void check_idle_passwords(void);
void heartbeat(int heart_pulse);
void make_who2html(void);
void init_descriptor(Descriptor *newd, int desc);
void tick_grenade(void);
void setup_log(const char *filename, int fd);
void setup_com(const char *filename, int fd);
socket_t init_socket(ush_int port);
void init_game(ush_int port);
int get_from_q(struct txt_q *queue, char *dest, int *aliased);
Descriptor *new_descriptor(socket_t s, int copyover);
int get_max_players(void);
int process_output(Descriptor *t);
int process_input(Descriptor *t);
int perform_alias(Descriptor *d, char *orig, size_t maxlen);
int parse_ip(const char *addr, struct in_addr *inaddr);
int set_sendbuf(socket_t s);
void free_bufpool(void);
int open_logfile(const char *filename, FILE * stderr_fp);
int open_comlogfile(const char *filename, FILE * stderr_fp);
char *make_prompt(Descriptor *point);
struct in_addr *get_bind_addr(void);
#if defined(POSIX)
sigfunc *my_signal(int signo, sigfunc * func);
#endif
#if defined(HAVE_ZLIB)
void *zlib_alloc(void *opaque, unsigned int items, unsigned int size);
void zlib_free(void *opaque, void *address);
#endif

void free_all_notes(void);
void check_all_trees(void);
void clear_free_list(void);
void free_messages(void);
void Board_clear_all(void);
void free_social_messages(void);
void free_mail_index(void);
void Free_Invalid_List(void);
extern struct obj_data *dead_obj;
void free_pending_objects(OBJ_DATA *obj);
void free_command_list(void);
//void free_help(struct help_index_element *help);

/* extern fcnts */
void check_for_dead(void);
void extract_pending_chars(void);
#if 0
void extract_pending_objects(void);
#endif
void free_mine_shafts(void);
void clear_free_list(void);
void reboot_wizlists(void);
void boot_world(void);
void regen_update(void);
void affect_update(void);     /* In spells.c */
void mobile_activity(void);
void perform_violence(void);
void sector_update(void);
void mobile_mating(void);
void load_config(void);
void free_mail_index(void);
void show_string(Descriptor *d, char *input);
void weather_and_time(int mode);
int id_init(void);
void id_kill(void);
void init_intermud_socket(void);
size_t proc_color(char *inbuf, int color_lvl, size_t len);
void redit_save_to_disk(int zone_num);
void oedit_save_to_disk(int zone_num);
void medit_save_to_disk(int zone_num);
void sedit_save_to_disk(int zone_num);
void zedit_save_to_disk(int zone_num);
void start_arena(void);
void run_events2(void);
void extract_delayed_mobs(void);
void extract_delayed_objs(void);
void check_auction(void);
int real_zone(int number);
int isbanned(char *hostname);
void free_messages(void);
void update_spell_wait(void);
int restrict_check(const Character *ch);
void Board_clear_all(void);
int has_note(Character *ch, int type);
void thefree_social_messages(void);
void free_ban_list(void);
void free_vehicles(void);
int enter_player_game(Descriptor *d);
#if RUNNING_IDENT
static void get_lookup_reply(void);
#endif
void send_compress_offer(Descriptor *d);

#ifdef HAVE_ZLIB_H
/* zlib helper functions */
void *z_alloc(void *opaque, uInt items, uInt size)
{
  return calloc(items, size);
}

void z_free(void *opaque, void *address)
{
  return free(address);
}
#endif /* HAVE_ZLIB_H */

#ifdef __CXREF__
#undef FD_ZERO
#undef FD_SET
#undef FD_ISSET
#undef FD_CLR
#define FD_ZERO(x)
#define FD_SET(x, y) 0
#define FD_ISSET(x, y) 0
#define FD_CLR(x, y)
#endif

#if defined(HAVE_ZLIB)
/*
 * MUD Client for Linux and mcclient compression support.
 * "The COMPRESS option (unofficial and completely arbitary) is
 * option 85." -- mcclient documentation as of Dec '98.
 *
 * [ Compression protocol documentation below, from Compress.c ]
 *
 * Server sends  IAC WILL COMPRESS
 * We reply with IAC DO COMPRESS
 *
 * Later the server sends IAC SB COMPRESS WILL SE, and immediately following
 * that, begins compressing
 *
 * Compression ends on a Z_STREAM_END, no other marker is used
 *
 * 2001/02/08 - Version 2 support. - mike
 * Version one uses an improper subnegotiation sequence to indicate start of compression
 * This represents the changes in v1 and v2:
 *  It uses the equally arbitrary COMPRESS2 option 86,
 *  It properly terminates the subnegotiation sequence.
 */
#define TELOPT_COMPRESS        85
#define TELOPT_COMPRESS2       86
#define COMPRESS        85
#define COMPRESS2       86

/* first compression neg. string */
static const char compress_offer[] =
  {
    (char) IAC,
    (char) WILL,
    (char) TELOPT_COMPRESS2,
    (char) 0,
  };

static const char will_sig[] = { IAC, WILL, TELOPT_COMPRESS, 0 };
static const char do_sig[] = { IAC, DO, TELOPT_COMPRESS, 0 };
static const char dont_sig[] = { IAC, DONT, TELOPT_COMPRESS, 0 };
static const char on_sig[] = { IAC, SB, TELOPT_COMPRESS, WILL, SE, 0 };
static const char will_sig2[] = { IAC, WILL, TELOPT_COMPRESS2, 0 };
static const char do_sig2[] = { IAC, DO, TELOPT_COMPRESS2, 0 };
static const char dont_sig2[] = { IAC, DONT, TELOPT_COMPRESS2, 0 };
static const char on_sig2[] = { IAC, SB, TELOPT_COMPRESS2, IAC, SE, 0 };

#endif



/***********************************************************************
*  main game loop and related stuff                                    *
***********************************************************************/

#if defined(CIRCLE_WINDOWS) || defined(CIRCLE_MACINTOSH)

/*
 * Windows doesn't have gettimeofday, so we'll simulate it.
 * The Mac doesn't have gettimeofday either.
 * Borland C++ warns: "Undefined structure 'timezone'"
 */
void gettimeofday(struct timeval *t, struct timezone *dummy)
{
#if defined(CIRCLE_WINDOWS)
  DWORD millisec = GetTickCount();
#elif defined(CIRCLE_MACINTOSH)
  unsigned long int millisec;
  millisec = (int) ((float) TickCount() * 1000.0 / 60.0);
#endif

  t->tv_sec = (int) (millisec / 1000);
  t->tv_usec = (millisec % 1000) * 1000;
}

#endif                   /* CIRCLE_WINDOWS || CIRCLE_MACINTOSH */

#define plant_magic(x)   do { (x)[sizeof(x) - 1] = MAGIC_NUMBER; } while (0)
#define test_magic(x)    ((x)[sizeof(x) - 1])

int main(int argc, char **argv)
{
  //ush_int port;
  int pos = 1;
  const char *dir;

#ifdef CIRCLE_UNIX
  if (!getuid())
  {
    puts("Please, *DO*NOT* run CircleMUD as root! Someone may get hurt!\r\n");
    exit(1);
  }
#endif

#if CIRCLE_GNU_LIBC_MEMORY_TRACK
  mtrace();    /* This must come before any use of malloc(). */
#endif

#ifdef CIRCLE_MACINTOSH
  /*
   * ccommand() calls the command line/io redirection dialog box from
   * Codewarriors's SIOUX library
   */
  argc = ccommand(&argv);
  /* Initialize the GUSI library calls.  */
  GUSIDefaultSetup();
#endif

  /****************************************************************************/
  /** Load the game configuration.                                           **/
  /** We must load BEFORE we use any of the constants stored in constants.c. **/
  /** Otherwise, there will be no variables set to set the rest of the vars  **/
  /** to, which will mean trouble --> Mythran                                **/
  /****************************************************************************/
  CONFIG_CONFFILE = NULL;
  while ((pos < argc) && (*(argv[pos]) == '-'))
  {
    if (*(argv[pos] + 1) == 'f')
    {
      if (*(argv[pos] + 2))
        CONFIG_CONFFILE = argv[pos] + 2;
      else if (++pos < argc)
        CONFIG_CONFFILE = argv[pos];
      else
      {
        puts("SYSERR: File name to read from expected after option -f.");
        exit(1);
      }
    }
    pos++;
  }
  pos = 0;

  /* i dunno why here but...*/
  while (pos < HIGHEST_VNUM)
    world_vnum[pos++] = NULL;

  pos = 1;

  if (!CONFIG_CONFFILE)
    CONFIG_CONFFILE = strdup(CONFIG_FILE);

  load_config();

  port = CONFIG_DFLT_PORT;
  dir = CONFIG_DFLT_DIR;

  while ((pos < argc) && (*(argv[pos]) == '-'))
  {
    switch (*(argv[pos] + 1))
    {
    case 'f':
      if (! *(argv[pos] + 2))
        ++pos;
      break;
    case 'x':
      no_ext_processes = 1;
      log("Running without external processes.");
      break;
    case 'o':
      if (*(argv[pos] + 2))
        CONFIG_LOGNAME = argv[pos] + 2;
      else if (++pos < argc)
        CONFIG_LOGNAME = argv[pos];
      else
      {
        puts("SYSERR: File name to log to expected after option -o.");
        exit(1);
      }
      break;
    case 'C':       /* -C<socket number> - recover from copyover, this is the control socket */
      fCopyOver = TRUE;
      mother_desc = atoi(argv[pos] + 2);
      break;
    case 'd':
      if (*(argv[pos] + 2))
        dir = argv[pos] + 2;
      else if (++pos < argc)
        dir = argv[pos];
      else
      {
        puts("SYSERR: Directory arg expected after option -d.");
        exit(1);
      }
      break;
    case 'm':
      mini_mud = 1;
      no_rent_check = 1;
      puts("Running in minimized mode & with no rent check.");
      break;
    case 'c':
      scheck = 1;
      puts("Syntax check mode enabled.");
      break;
    case 'q':
      no_rent_check = 1;
      puts("Quick boot mode -- rent check supressed.");
      break;
    case 'r':
      circle_restrict = 1;
      puts("Restricting game -- no new players allowed.");
      break;
    case 's':
      no_specials = 1;
      puts("Suppressing assignment of special routines.");
      break;
    case 'h':
      /* From: Anil Mahajan <amahajan@proxicom.com> */
      printf
      ("Usage: %s [-c] [-m] [-q] [-r] [-s] [-x] [-d pathname] [port #]\n"
       "  -c             Enable syntax check mode.\n"
       "  -d <directory> Specify library directory (defaults to 'lib').\n"
       "  -h             Print this command line argument help.\n"
       "  -m             Start in mini-MUD mode.\n"
       "  -f<file>       Use <file> for configuration.\n"
       "  -o <file>      Write log to <file> instead of stderr.\n"
       "  -q             Quick boot (doesn't scan rent for object limits)\n"
       "  -r             Restrict MUD -- no new players allowed.\n"
       "  -s             Suppress special procedure assignments.\n"
       " Note:      These arguments are 'CaSe SeNsItIvE!!!'\n",
       argv[0]
      );
      exit(1);
    default:
      printf("SYSERR: Unknown option -%c in argument string.",
             *(argv[pos] + 1));
      break;
    }
    pos++;
  }

  if (pos < argc)
  {
    if (!isdigit(*argv[pos]))
    {
      printf
      ("Usage: %s [-c] [-m] [-q] [-r] [-s] [-x] [-d pathname] [port #]\n",
       argv[0]);
      exit(1);
    }
    else if ((port = atoi(argv[pos])) <= 1024)
    {
      printf("SYSERR: Illegal port number %d.\n", port);
      exit(1);
    }
  }



  /* All arguments have been parsed, try to open log file. */
  setup_log(CONFIG_LOGNAME, STDERR_FILENO);
  setup_com("log/comlog", -1);

  /*
   * Moved here to distinguish command line options and to show up
   * in the log if stderr is redirected to a file.
   */
  log("Using %s for configuration.", CONFIG_CONFFILE);
  log("%s", circlemud_version);
  log("%s", oasisolc_version);
  log("%s", fourdimensions_version);

  if (id_init() < 0)
    exit(2);

  if (chdir(dir) < 0)
  {
    perror("SYSERR: Fatal error changing to data directory");
    exit(1);
  }
  log("Using %s as data directory.", dir);

  if (scheck)
  {
    boot_world();
    log("Done.");
  }
  else
  {
    log("Running game on port %d.", port);
    init_game(port);
  }

  log("Clearing game world.");
  destroy_db();

  if (!scheck)
  {
    log("Clearing other memory.");
    free_bufpool();             /* comm.c */
    free_all_notes();           /*note.c*/
    free_social_messages();     /* act.social.c */
    free_player_index(); /* db.c */
    free_messages();          /* fight.c */

    clear_free_list();        /* mail.c */
    free_mail_index();          /* mail.c */
    free_text_files();        /* db.c */
    Board_clear_all();        /* boards.c */
    free(cmd_sort_info); /* act.informative.c */
    free_command_list();        /* act.informative.c */
    the_free_help();          /* db.c */
    Free_Invalid_List(); /* ban.c */
    free_ban_list();          /* ban.c*/
    free_host_list(host_list);
    free_strings(&config_info, OASIS_CFG); /* oasis_delete.c */
    free_vehicles();
    free_commlist(comlist);
    free_mine_shafts();
    if (comfile)
      fclose(comfile);


  }
  /* probably should free the entire config here.. */
  free(CONFIG_CONFFILE);

  log("Done.");
  return (0);
}

void circle_exit(int retval)
{
  id_kill();
#undef exit
  exit(retval);
#define exit(n) circle_exit(n)

}


/* Reload players after a copyover */
void copyover_recover(void)
{
  Descriptor *d;
  FILE *fp;
  char host[1024];
  int desc, player_i, mxpon;
  bool fOld;
  char name[MAX_INPUT_LENGTH];

  int saved_loadroom = NOWHERE, varcnt = 0;

  log("Copyover recovery initiated");

  fp = fopen(COPYOVER_FILE, "r");

  if (!fp)
  {            /* there are some descriptors open which will hang forever then ? */
    perror("copyover_recover:fopen");
    log("Copyover file not found. Exitting.\r\n");
    exit(1);
  }

  unlink(COPYOVER_FILE); /* In case something crashes - doesn't prevent reading  */

  for (;;)
  {
    fOld = TRUE;
    varcnt = fscanf(fp, "%d %s %s %d %d\n", &desc, name, host, &saved_loadroom, &mxpon);
    if (desc == -1)
      break;
    if (varcnt !=5)
      continue;

    /* Write something, and check if it goes error-free */
    if (write_to_descriptor(desc,"\n\r...the hourglass turns over and the sand starts flowing again...\r\n", NULL) == 0)
    {
      close(desc);  /* nope */
      continue;
    }
#if 0
    if ((d = new_descriptor(desc, TRUE)) == NULL)
      continue;
#else
    /* create a new descriptor */
    CREATE(d, Descriptor, 1);
    memset((char *) d, 0, sizeof(Descriptor));
    init_descriptor(d, desc); /* set up various stuff */

    d->next = descriptor_list;
    descriptor_list = d;
#endif
    d->mxp = mxpon;
    strcpy(d->host, host);


    d->connected = CON_CLOSE;

    /* Now, find the pfile */

    CREATE(d->character, Character, 1);
    CREATE(d->character->player_specials, struct player_special_data,        1);
    clear_char(d->character);
    d->character->desc = d;

    if ((player_i = load_char(name, d->character)) >= 0)
    {
      GET_PFILEPOS(d->character) = player_i;
      if (!PLR_FLAGGED(d->character, PLR_DELETED))
      {
        REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_WRITING);
        REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_MAILING);
        REMOVE_BIT_AR(PLR_FLAGS(d->character), PLR_CRYO);
      }
      else
        fOld = FALSE;
    }
    else
      fOld = FALSE;

    if (!fOld)
    {          /* Player file not found?! */
      write_to_descriptor(desc,"\n\rSomehow, your character was lost in the copyover. Sorry.\r\n", NULL);
      close_socket(d);
    }
    else
    {          /* ok! */

      write_to_descriptor (desc, "\n\rColor floods back into the world.\r\n", NULL);
#ifdef HAVE_ZLIB_H
      if (!PRF_FLAGGED(d->character, PRF_NOCOMPRESS) && !d->mxp)
      {
        d->comp->state = 1; /* indicates waiting for comp negotiation */
        send_compress_offer(d);
      }
#endif /* HAVE_ZLIB_H */
      send_out_signals(d);
      d->connected = CON_PLAYING;
      GET_LOADROOM(d->character) = saved_loadroom;
      enter_player_game(d);



    }

  }

  fclose(fp);
}

/* Init sockets, run game, and cleanup sockets */
void init_game(ush_int s_port)
{

  /* We don't want to restart if we crash before we get up. */
  touch(KILLSCRIPT_FILE);

  circle_srandom(time(0));

  log("Finding player limit.");
  max_players = get_max_players();

  if (!fCopyOver)
  {       /* If copyover mother_desc is already set up */
    log("Opening mother connection.");
    mother_desc = init_socket(s_port);
  }
  log("Initiating events.");
  event_init();
  log("Setting hash table");
  /* set up hash table for find_char() */
  init_lookup_table();
  log("Booting database.");
  boot_db();

#if defined(CIRCLE_UNIX) || defined(CIRCLE_MACINTOSH)
  log("Signal trapping.");
  signal_setup();
#endif

  /* If we made it this far, we will be able to restart without problem. */
  remove(KILLSCRIPT_FILE);

  if (fCopyOver)         /* reload players */
    copyover_recover();

  log("Entering game loop.");

  game_loop(mother_desc);
  log("Crash saving all.");
  Crash_save_all();

  log("Closing all sockets.");
  while (descriptor_list)
    close_socket(descriptor_list);

  CLOSE_SOCKET(mother_desc);
  CLOSE_SOCKET(intermud_desc);
  fclose(qic_fl);        /* close qic database */

  if (circle_reboot != 2)
    save_all();

  log("Saving current MUD time.");
  save_mud_time(&time_info);

  if (circle_reboot)
  {
    log("Rebooting.");
    exit(52);       /* what's so great about HHGTTG, anyhow? */
  }
  log("Normal termination of game.");
}



/*
 * init_socket sets up the mother descriptor - creates the socket, sets
 * its options up, binds it, and listens.
 */
socket_t init_socket(ush_int s_port)
{
  socket_t s;
  struct sockaddr_in sa;
  int opt;

#ifdef CIRCLE_WINDOWS
  {
    WORD wVersionRequested;
    WSADATA wsaData;

    wVersionRequested = MAKEWORD(1, 1);

    if (WSAStartup(wVersionRequested, &wsaData) != 0)
    {
      log("SYSERR: WinSock not available!");
      exit(1);
    }
    if ((wsaData.iMaxSockets - 4) < max_players)
    {
      max_players = wsaData.iMaxSockets - 4;
    }
    log("Max players set to %d", max_players);

    if ((s = socket(PF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
    {
      log("SYSERR: Error opening network connection: Winsock error #%d", WSAGetLastError());
      exit(1);
    }
  }
#else
  /*
  * Should the first argument to socket() be AF_INET or PF_INET?  I don't
  * know, take your pick.  PF_INET seems to be more widely adopted, and
  * Comer (_Internetworking with TCP/IP_) even makes a point to say that
  * people erroneously use AF_INET with socket() when they should be using
  * PF_INET.  However, the man pages of some systems indicate that AF_INET
  * is correct; some such as ConvexOS even say that you can use either one.
  * All implementations I've seen define AF_INET and PF_INET to be the same
  * number anyway, so the point is (hopefully) moot.
  */

  if ((s = socket(PF_INET, SOCK_STREAM, 0)) < 0)
  {
    perror("SYSERR: Error creating socket");
    exit(1);
  }
#endif                   /* CIRCLE_WINDOWS */

#if defined(SO_REUSEADDR) && !defined(CIRCLE_MACINTOSH)
  opt = 1;
  if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *) &opt, sizeof(opt))
      < 0)
  {
    perror("SYSERR: setsockopt REUSEADDR");
    exit(1);
  }
#endif
#ifdef SO_SNDBUF
  set_sendbuf(s);
#endif

  /*
   * The GUSI sockets library is derived from BSD, so it defines
   * SO_LINGER, even though setsockopt() is unimplimented.
   * (from Dean Takemori <dean@UHHEPH.PHYS.HAWAII.EDU>)
   */
#if defined(SO_LINGER) && !defined(CIRCLE_MACINTOSH)

  {
    struct linger ld;

    ld.l_onoff = 0;
    ld.l_linger = 0;
    if (setsockopt(s, SOL_SOCKET, SO_LINGER, (char *) &ld, sizeof(ld))
        < 0)
      perror("SYSERR: setsockopt SO_LINGER");     /* Not fatal I suppose. */
  }
#endif

  /* Clear the structure */
  memset((char *) &sa, 0, sizeof(sa));

  sa.sin_family = AF_INET;
  sa.sin_port = htons(s_port);
  sa.sin_addr = *(get_bind_addr());

  if (bind(s, (struct sockaddr *) &sa, sizeof(sa)) < 0)
  {
    perror("SYSERR: bind");
    CLOSE_SOCKET(s);
    exit(1);
  }
  nonblock(s);
  listen(s, 5);
  return (s);
}


int get_max_players(void)
{
#ifndef CIRCLE_UNIX
  return (CONFIG_MAX_PLAYING);
#else

  int max_descs = 0;
  const char *method;

  /*
  * First, we'll try using getrlimit/setrlimit.  This will probably work
  * on most systems.  HAS_RLIMIT is defined in sysdep.h.
  */
#ifdef HAS_RLIMIT
  {
    struct rlimit limit;

    /* find the limit of file descs */
    method = "rlimit";
    if (getrlimit(RLIMIT_NOFILE, &limit) < 0)
    {
      perror("SYSERR: calling getrlimit");
      exit(1);
    }

    /* set the current to the maximum */
    limit.rlim_cur = limit.rlim_max;
    if (setrlimit(RLIMIT_NOFILE, &limit) < 0)
    {
      perror("SYSERR: calling setrlimit");
      exit(1);
    }
#ifdef RLIM_INFINITY
    if (limit.rlim_max == RLIM_INFINITY)
      max_descs = CONFIG_MAX_PLAYING + NUM_RESERVED_DESCS;
    else
      max_descs = MIN(CONFIG_MAX_PLAYING + NUM_RESERVED_DESCS, limit.rlim_max);
#else
  max_descs = MIN(CONFIG_MAX_PLAYING + NUM_RESERVED_DESCS, limit.rlim_max);
#endif
  }

#elif defined (OPEN_MAX) || defined(FOPEN_MAX)
#if !defined(OPEN_MAX)
#define OPEN_MAX FOPEN_MAX
#endif
  method = "OPEN_MAX";
  max_descs = OPEN_MAX;  /* Uh oh.. rlimit didn't work, but we have
  * OPEN_MAX */
#elif defined (_SC_OPEN_MAX)
  /*
  * Okay, you don't have getrlimit() and you don't have OPEN_MAX.  Time to
  * try the POSIX sysconf() function.  (See Stevens' _Advanced Programming
  * in the UNIX Environment_).
  */
  method = "POSIX sysconf";
  errno = 0;
  if ((max_descs = sysconf(_SC_OPEN_MAX)) < 0)
  {
    if (errno == 0)
      max_descs = CONFIG_MAX_PLAYING + NUM_RESERVED_DESCS;
    else
    {
      perror("SYSERR: Error calling sysconf");
      exit(1);
    }
  }
#else
  /* if everything has failed, we'll just take a guess */
  method = "random guess";
  max_descs = CONFIG_MAX_PLAYING + NUM_RESERVED_DESCS;
#endif

  /* now calculate max _players_ based on max descs */
  max_descs = MIN(CONFIG_MAX_PLAYING, max_descs - NUM_RESERVED_DESCS);

  if (max_descs <= 0)
  {
    log("SYSERR: Non-positive max player limit!  (Set at %d using %s).", max_descs, method);
    exit(1);
  }
  log("   Setting player limit to %d using %s.", max_descs, method);
  return (max_descs);
#endif                   /* CIRCLE_UNIX */
}
#if 0
void *process_io(void *loopstate)
{
  Descriptor *d, *next_d;
  char comm[MAX_INPUT_LENGTH];
  int aliased;

  if (loopstate == NULL)
  { /* we're a thread */
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
  }

  do
  {
    if (dlock++ > 3600)
      dlock = 0;

    /* Process commands we just read from process_input */
    for (d = descriptor_list; d; d = next_d)
    {
      
      next_d = d->next;
      if (is_locked(d))
        continue;
      if (d->close_me)
        continue;

      /* FIXME: d->wait will be off. */
      if ((--(d->wait) <= 0) && get_from_q(&d->input, comm, &aliased))
      {
        if (d->character)
        {
          /* Reset the idle timer & pull char back from void if necessary */
          d->character->char_specials.timer = 0;
          if (!d->connected && GET_WAS_IN(d->character) != NULL)
          {
            if (d->character->in_room != NULL)
              char_from_room(d->character);
            char_to_room(d->character, GET_WAS_IN(d->character));
            GET_WAS_IN(d->character) = NULL;
            act("$n fades in to view.", TRUE, d->character, 0, 0, TO_ROOM);
          }
        }
        d->wait = 1;
        d->has_prompt = 0;

        if (d->str)         /* Writing boards, mail, etc. */
          string_add(d, comm);
        else if (d->showstr_count) /* Reading something w/ pager */
          show_string(d, comm);
        else if (d->connected != CON_PLAYING) /* In menus, etc. */
          nanny(d, comm);
        else
        {              /* else: we're playing normally. */
          if (aliased)      /* To prevent recursive aliases. */
            d->has_prompt = 1;   /* To get newline before next cmd output. */
          else
          {
            if (perform_alias(d, comm, sizeof(comm) -1))     /* Run it through aliasing system */
              get_from_q(&d->input, comm, &aliased);
          }
          dlock++;     /* Deadlock counter. */
#if 0 /* debugging */
          sleep(number(1,12));
#endif
          command_interpreter(d->character, comm); /* Send it to interpreter */
          dlock++;
        }
      }
    }

    /* Now we pause here if we are a thread to let input accumulate. */
    if (loopstate == NULL)
    {
      usleep(1); /* FIXME: Sleep for 1/10 of second like game_loop */
      pthread_testcancel();
    }

  }
  while (loopstate == NULL);    /* We're a thread if loopstate == NULL */

  return NULL;
}
#endif
/*
 * game_loop contains the main loop which drives the entire MUD.  It
 * cycles once every 0.10 seconds and is responsible for accepting new
 * new connections, polling existing connections for input, dequeueing
 * output and sending it out to players, and calling "heartbeat" functions
 * such as mobile_activity().
 */
void game_loop(socket_t s_mother_desc)
{
  fd_set input_set, output_set, exc_set, null_set;
  struct timeval last_time, opt_time, process_time, temp_time;
  struct timeval before_sleep, now, timeout;
  char comm[MAX_STRING_LENGTH];
  Descriptor *d, *next_d;
  int missed_pulses = 0, maxdesc, aliased;

  /* initialize various time values */
  null_time.tv_sec = 0;
  null_time.tv_usec = 0;
  opt_time.tv_usec = OPT_USEC;
  opt_time.tv_sec = 0;
  FD_ZERO(&null_set);

  gettimeofday(&last_time, (struct timezone *) 0);

  /* The Main Loop.  The Big Cheese.  The Top Dog.  The Head Honcho.  The.. */
  while (!circle_shutdown)
  {

    /* Sleep if we don't have any connections */
    if (descriptor_list == NULL)
    {
      log("No connections.  Going to sleep.");
      make_who2html();
      FD_ZERO(&input_set);
      FD_SET(s_mother_desc, &input_set);
      if (select(s_mother_desc + 1, &input_set, (fd_set *) 0, (fd_set *) 0, NULL) < 0)
      {
        if (errno == EINTR)
          log("Waking up to process signal.");
        else
          perror("SYSERR: Select coma");
      }
      else
        log("New connection.  Waking up.");
      gettimeofday(&last_time, (struct timezone *) 0);
    }
    /* Set up the input, output, and exception sets for select(). */
    FD_ZERO(&input_set);
    FD_ZERO(&output_set);
    FD_ZERO(&exc_set);
    FD_SET(s_mother_desc, &input_set);

    maxdesc = mother_desc;

    if (id_serv_socket > 0)
    {
      FD_SET(id_serv_socket, &input_set);
      maxdesc = MAX(maxdesc, id_serv_socket);
    }

    for (d = descriptor_list; d; d = next_d)
    {
#ifndef CIRCLE_WINDOWS
      if (d->descriptor > maxdesc)
        maxdesc = d->descriptor;
#endif
      next_d = d->next;
      FD_SET(d->descriptor, &input_set);
      FD_SET(d->descriptor, &output_set);
      FD_SET(d->descriptor, &exc_set);
    }

    /*
     * At this point, we have completed all input, output and heartbeat
     * activity from the previous iteration, so we have to put ourselves
     * to sleep until the next 0.1 second tick.  The first step is to
     * calculate how long we took processing the previous iteration.
     */

    gettimeofday(&before_sleep, (struct timezone *) 0);     /* current time */
    timediff(&process_time, &before_sleep, &last_time);

    /*
     * If we were asleep for more than one pass, count missed pulses and sleep
     * until we're resynchronized with the next upcoming pulse.
     */
    if (process_time.tv_sec == 0 && process_time.tv_usec < OPT_USEC)
    {
      missed_pulses = 0;
    }
    else
    {
      missed_pulses = process_time.tv_sec * PASSES_PER_SEC;
      missed_pulses += process_time.tv_usec / OPT_USEC;
      process_time.tv_sec = 0;
      process_time.tv_usec = process_time.tv_usec % OPT_USEC;
    }

    /* Calculate the time we should wake up */
    timediff(&temp_time, &opt_time, &process_time);
    timeadd(&last_time, &before_sleep, &temp_time);

    /* Now keep sleeping until that time has come */
    gettimeofday(&now, (struct timezone *) 0);
    timediff(&timeout, &last_time, &now);

    /* Go to sleep */
    do
    {
      circle_sleep(&timeout);
      gettimeofday(&now, (struct timezone *) 0);
      timediff(&timeout, &last_time, &now);
    }
    while (timeout.tv_usec || timeout.tv_sec);

    /* Poll (without blocking) for new input, output, and exceptions */
    if (select
        (maxdesc + 1, &input_set, &output_set, &exc_set,
         &null_time) < 0)
    {
      perror("SYSERR: Select poll");
      return;
    }
    /* If there are new connections waiting, accept them. */
    if (FD_ISSET(s_mother_desc, &input_set))
      new_descriptor(s_mother_desc, FALSE);
#if RUNNING_IDENT
    /* check for a reply from the ident process */
    if ((id_serv_socket > 0) && FD_ISSET(id_serv_socket, &input_set))
      get_lookup_reply();
#endif

    /* Kick out the freaky folks in the exception set and marked for close */
    for (d = descriptor_list; d; d = next_d)
    {
      next_d = d->next;
      if (d->close_me || STATE(d) == CON_CLOSE || FD_ISSET(d->descriptor, &exc_set))
      {
        {
          FD_CLR(d->descriptor, &input_set);
          FD_CLR(d->descriptor, &output_set);
          close_socket(d);
        }
      }

      /* Process descriptors with input pending */
      for (d = descriptor_list; d; d = next_d)
      {
        next_d = d->next;
        if (FD_ISSET(d->descriptor, &input_set))
          if (process_input(d) < 0)
            close_socket(d);
      }

      
    /* Process commands we just read from process_input */
      for (d = descriptor_list; d; d = next_d)
      {
        next_d = d->next;
        
      /*
       * Not combined to retain --(d->wait) behavior. -gg 2/20/98
       * If no wait state, no subtraction.  If there is a wait
       * state then 1 is subtracted. Therefore we don't go less
       * than 0 ever and don't require an 'if' bracket. -gg 2/27/99
       */
        if (d->character)
        {
          GET_WAIT_STATE(d->character) -= (GET_WAIT_STATE(d->character) > 0);
          
          if (GET_WAIT_STATE(d->character) > 0)
            continue;
        }
        
        if (!get_from_q(&d->input, comm, &aliased))
          continue;
        
        if (d->character)
        {
        /* Reset the idle timer & pull char back from void if necessary */
          d->character->char_specials.timer = 0;
          if (STATE(d) == CON_PLAYING
              && GET_WAS_IN(d->character) != NULL)
          {
            if (IN_ROOM(d->character) != NULL)
              char_from_room(d->character);
            char_to_room(d->character, GET_WAS_IN(d->character));
            GET_WAS_IN(d->character) = NULL;
            act("You fade into view.", TRUE, d->character, 0, 0, TO_CHAR);
            act("$n fades into view.", TRUE, d->character, 0, 0, TO_ROOM);
          }
          GET_WAIT_STATE(d->character) = 1;
        }
        d->has_prompt = FALSE;
        
        if (d->showstr_count) /* Reading something w/ pager */
          show_string(d, comm);
        else if (d->str)        /* Writing boards, mail, etc. */
          string_add(d, comm);
        else if (STATE(d) != CON_PLAYING) /* In menus, etc. */
          nanny(d, comm);
        else
        {        /* else: we're playing normally. */
          if (aliased)     /* To prevent recursive aliases. */
            d->has_prompt = TRUE;    /* To get newline before next cmd output. */
          else if (perform_alias(d, comm, sizeof(comm))) /* Run it through aliasing system */
            get_from_q(&d->input, comm, &aliased);
          command_interpreter(d->character, comm);  /* Send it to interpreter */
        }
      }
  }

    /* Send queued output out to the operating system (ultimately to user). */
      for (d = descriptor_list; d; d = next_d)
      {
        next_d = d->next;
        if (*(d->output) && FD_ISSET(d->descriptor, &output_set))
        {
        /* Output for this player is ready */
          if (process_output(d) < 0)
            close_socket(d);
          else
            d->has_prompt = 1;
        }
      }

      
    /* Print prompts for other descriptors who had no other output */
      for (d = descriptor_list; d; d = d->next)
      {
        if (!d->has_prompt)
        {
          write_to_descriptor(d->descriptor, make_prompt(d), d->comp);
          d->has_prompt = TRUE;
        }
      }
    /* Kick out folks in the CON_CLOSE or CON_DISCONNECT state */
      for (d = descriptor_list; d; d = next_d)
      {
        next_d = d->next;
        if (STATE(d) == CON_CLOSE || STATE(d) == CON_DISCONNECT)
          close_socket(d);
      }
      

    /**
     * Now, we execute as many pulses as necessary--just one if we haven't
     * missed any pulses, or make up for lost time if we missed a few
     * pulses by sleeping for too long.
     */
    missed_pulses++;

    if (missed_pulses <= 0)
    {
      log("SYSERR: **BAD** MISSED_PULSES NONPOSITIVE (%d), TIME GOING BACKWARDS!!", missed_pulses);
      missed_pulses = 1;
    }

    /* If we missed more than 5 seconds worth of pulses, just do 5 secs */
    if (missed_pulses > (10 RL_SEC))
    {
      log("SYSERR: Missed %d seconds worth of pulses.",
          missed_pulses / PASSES_PER_SEC);
      missed_pulses = 5 RL_SEC;
    }

    /* Now execute the  functions */
    while (missed_pulses--)
      heartbeat(++pulse);

    /* Check for any signals we may have received. */
    if (reread_wizlist)
    {
      reread_wizlist = FALSE;
      new_mudlog(CMP, LVL_IMMORT, TRUE, "Signal received - rereading wizlists.");
      reboot_wizlists();
    }
    if (emergency_unban)
    {
      emergency_unban = FALSE;
      new_mudlog(BRF, LVL_IMMORT, TRUE, "Received SIGUSR2 - completely unrestricting game (emergent)");
      ban_list = NULL;
      circle_restrict = 0;
      num_invalid = 0;
    }

#ifdef CIRCLE_UNIX
    /* Update tics for deadlock protection (UNIX only) */
    tics_passed++;
#endif

  }
}


void heartbeat(int heart_pulse)
{
  static int mins_since_crashsave = 0;
  //void process_events(void);
  event_process();



  if (CONFIG_AUTO_SAVE && !(heart_pulse % PULSE_AUTOSAVE))
  {  /* 1 minute */
    if (++mins_since_crashsave >= CONFIG_AUTOSAVE_TIME)
    {
      mins_since_crashsave = 0;
      Crash_save_all();
      House_save_all();
    }
  }

  if (!(heart_pulse % PULSE_DG_SCRIPT))
  {
    //log("Pulse for: trigger check");
    script_trigger_check();
  }

  if (!(heart_pulse % PULSE_ZONE))
  {
    //log("Pulse for: zone update");
    zone_update();
  }

  if (!(heart_pulse % (15 * PASSES_PER_SEC)))
    check_idle_passwords();

  if (!(heart_pulse % (PULSE_MOBILE)))
    mobile_activity();

  if (!(heart_pulse % PULSE_AUCTION))
    auction_update();

  if (!(heart_pulse % (5 * PASSES_PER_SEC)))      // kalten
    sector_update();     // kalten

  if (!(heart_pulse % (PASSES_PER_SEC)))
  {
    update_spell_wait(); /* this includes a check for the tasks */
    check_for_dead();
  }

  if (!(heart_pulse % (30 * PASSES_PER_SEC)))
    make_who2html();

  if (!(heart_pulse % PULSE_EVENT))
    run_events2();

  if (in_arena == ARENA_START)
    if (!(heart_pulse % PULSE_ARENA))
      start_arena();
  if (in_arena == ARENA_RUNNING)
    if (!(heart_pulse % PULSE_ARENA))
      do_game();

  if (!(heart_pulse % (SECS_PER_MUD_HOUR * PASSES_PER_SEC)))
  {
    //log("Pulse for: weather and time, affect, point, time trigger check");
    weather_and_time(1);
    check_time_triggers();
    affect_update();
    point_update();
    save_mud_time(&time_info);
  }


  if (!(heart_pulse % (5 * 60 * PASSES_PER_SEC)))
    record_usage();

  if (!(heart_pulse % (SECS_PER_MUD_DAY * PASSES_PER_SEC)))
    check_all_trees();


  extract_pending_chars();
#if 0
  extract_pending_objects();
#else
  free_pending_objects(dead_obj);
  dead_obj = NULL;
#endif
}



//Mordecais altered send_to_char
size_t new_send_to_char(Character *ch, const char *messg, ...)
{
  if (ch && ch->desc && messg && *messg)
  {
    size_t left;
    va_list args;

    va_start(args, messg);
    left = ch->desc->vwrite_to_output(messg, args);
    va_end(args);
    return left;
  }
  return 0;
}

size_t send_to_fusion(Character *ch, const char *messg, ...)
{
  if (FUSED_TO(ch) && messg && *messg)
  {
    size_t left = 0;
    va_list args;
    int i = 0;
    Character *fuse, *tmpfuse;

    fuse = (FUSED_TO(ch));
    tmpfuse = fuse;
    for (i = 0; i < TOP_FUSE_LOCATION; i++)
    {
      tmpfuse = FUSE_LOC(fuse, i);
      if (tmpfuse && tmpfuse->desc)
      {
        va_start(args, messg);
        left = tmpfuse->desc->vwrite_to_output(messg, args);
        va_end(args);
      }


    }
    return left;
  }
  return 0;
}





/* ******************************************************************
*  general utility stuff (for local use)                            *
****************************************************************** */

/*
 *  new code to calculate time differences, which works on systems
 *  for which tv_usec is unsigned (and thus comparisons for something
 *  being < 0 fail).  Based on code submitted by ss@sirocco.cup.hp.com.
 */

/*
 * code to return the time difference between a and b (a-b).
 * always returns a nonnegative value (floors at 0).
 */
void timediff(struct timeval *rslt, struct timeval *a, struct timeval *b)
{
  if (a->tv_sec < b->tv_sec)
    *rslt = null_time;
  else if (a->tv_sec == b->tv_sec)
  {
    if (a->tv_usec < b->tv_usec)
      *rslt = null_time;
    else
    {
      rslt->tv_sec = 0;
      rslt->tv_usec = a->tv_usec - b->tv_usec;
    }
  }
  else
  {            /* a->tv_sec > b->tv_sec */
    rslt->tv_sec = a->tv_sec - b->tv_sec;
    if (a->tv_usec < b->tv_usec)
    {
      rslt->tv_usec = a->tv_usec + 1000000 - b->tv_usec;
      rslt->tv_sec--;
    }
    else
      rslt->tv_usec = a->tv_usec - b->tv_usec;
  }
}

/*
 * Add 2 time values.
 *
 * Patch sent by "d. hall" <dhall@OOI.NET> to fix 'static' usage.
 */
void timeadd(struct timeval *rslt, struct timeval *a, struct timeval *b)
{
  rslt->tv_sec = a->tv_sec + b->tv_sec;
  rslt->tv_usec = a->tv_usec + b->tv_usec;

  while (rslt->tv_usec >= 1000000)
  {
    rslt->tv_usec -= 1000000;
    rslt->tv_sec++;
  }
}


void record_usage(void)
{
  int sockets_connected = 0, sockets_playing = 0;
  Descriptor *d;

  for (d = descriptor_list; d; d = d->next)
  {
    sockets_connected++;
    if (IS_PLAYING(d))
      sockets_playing++;
  }

  log("nusage: %-3d sockets connected, %-3d sockets playing",
      sockets_connected, sockets_playing);

#ifdef RUSAGE            /* Not RUSAGE_SELF because it doesn't guarantee prototype. */
  {
    struct rusage ru;

    getrusage(RUSAGE_SELF, &ru);
    log("rusage: user time: %ld sec, system time: %ld sec, max res size: %ld", ru.ru_utime.tv_sec, ru.ru_stime.tv_sec, ru.ru_maxrss);
  }
#endif

}



/*
 * Turn off echoing (specific to telnet client)
 */
void echo_off(Descriptor *d)
{
  char off_string[] = {
                        (char) IAC,
                        (char) WILL,
                        (char) TELOPT_ECHO,
                        (char) 0,
                      };

  d->Output( "%s", off_string);
}


/*
 * Turn on echoing (specific to telnet client)
 */
void echo_on(Descriptor *d)
{
  char on_string[] = {
                       (char) IAC,
                       (char) WONT,
                       (char) TELOPT_ECHO,
                       (char) 0
                     };

  d->Output( "%s", on_string);
}

const char *end_prompt(Descriptor *d)
{
  static const char eor_prompt[] =
    {
      (char) IAC,
      (char) EOR,
      (char) 0
    };
  static const char ga_prompt[] =
    {
      (char) IAC,
      (char) GA,
      (char) 0
    };
  if (!d)
    return "";
  if (d->eor == 1)
    return eor_prompt;
  /** edited out for the moment - needs to check for compatability first - mord**/
  else if (d->telnet_capable == 1)
    return ga_prompt;
  else
    return "";
}

char *make_prompt(Descriptor *d)
{
  static char prompt[MAX_PROMPT_LENGTH + 1] = "";
  /* Note, prompt is truncated at MAX_PROMPT_LENGTH chars (structs.h )
     (This is no longer true, prompt has expanding buffer when called from process_output)*/
  strcpy(prompt, "");
  if (d->showstr_count)
    snprintf(prompt, sizeof(prompt),
             "[ Return to continue, (q)uit, (r)efresh, (b)ack, or page number (%d/%d) ]",
             d->showstr_page, d->showstr_count);
  else if (d->str)
    strcpy(prompt, "] ");     /* strcpy: OK (for 'MAX_PROMPT_LENGTH >= 3') */
  else if (STATE(d) == CON_PLAYING && !IS_NPC(d->character))
#if 1
    return parse_prompt(d->character, prompt, sizeof(prompt));
#else
  {
    int count = 0;
    size_t len = 0;
    int percent;


    if (PRF_FLAGGED(d->character, PRF_AFK)&& len < sizeof(prompt))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "\x1B[32m[AFK]\x1B[0m ");
      if (count >= 0)
        len += count;
    }
    if (PRF_FLAGGED(d->character, PRF_RP)&& len < sizeof(prompt))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "\x1B[32m[RP]\x1B[0m ");
      if (count >= 0)
        len += count;
    }

    if ((check_mail(d->character) && len < sizeof(prompt))
        && (PRF_FLAGGED(d->character, PRF_MAIL)))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "[MAIL] ");
      if (count >= 0)
        len += count;
    }

    if ((has_note(d->character, NOTE_NOTE) && len < sizeof(prompt))
        && (PRF_FLAGGED(d->character, PRF_MAIL)))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "[NOTE] ");
      if (count >= 0)
        len += count;
    }
    if ((has_note(d->character, NOTE_NEWS) && len < sizeof(prompt))
        && (PRF_FLAGGED(d->character, PRF_MAIL)))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "[NEWS] ");
      if (count >= 0)
        len += count;
    }
    if ((has_note(d->character, NOTE_CHANGES) && len < sizeof(prompt))
        && (PRF_FLAGGED(d->character, PRF_MAIL)))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "[CHANGES] ");
      if (count >= 0)
        len += count;
    }
    if ((has_note(d->character, NOTE_IDEA) && len < sizeof(prompt))
        && (PRF_FLAGGED(d->character, PRF_MAIL)))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "[IDEA] ");
      if (count >= 0)
        len += count;
    }



    if (PLR_FLAGGED(d->character, PLR_KILLER)&& len < sizeof(prompt))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "[\x1B[1;31mKILLER\x1B[0;0m] ");
      if (count >= 0)
        len += count;
    }

    if (GET_INVIS_LEV(d->character)&& len < sizeof(prompt))
    {
      count = snprintf(prompt + len, sizeof(prompt) - len, "(i%d) ", GET_INVIS_LEV(d->character));
      if (count >= 0)
        len += count;
    }

    if (PRF_FLAGGED(d->character, PRF_DISPHP)&& len < sizeof(prompt))
    {
      if (COLOR_LEV(d->character) >= C_CMP)
      {
        if (GET_MAX_HIT(d->character) > 0)
          percent =
            (100 * GET_HIT(d->character)) /
            GET_MAX_HIT(d->character);
        else
          percent = -1;
        if (percent >= 100)
          count = snprintf(prompt + len, sizeof(prompt) - len, "\x1B[0;32m%d\x1B[0;0mH ", GET_HIT(d->character));
        else if (percent >= 90)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[0;32m%d\x1B[0;0mH\x1B[0;0m ", GET_HIT(d->character));
        else if (percent >= 75)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[0;32m%d\x1B[0;0mH\x1B[0;0m ", GET_HIT(d->character));
        else if (percent >= 50)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[0;0m%d\x1B[0;0mH\x1B[0;0m ", GET_HIT(d->character));
        else if (percent >= 30)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[0;0m%d\x1B[0;0mH\x1B[0;0m ", GET_HIT(d->character));
        else if (percent >= 15)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[1;31m%d\x1B[0;0mH\x1B[0;0m ",GET_HIT(d->character));
        else if (percent >= 0)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[1;31m%d\x1B[0;0mH\x1B[0;0m ",GET_HIT(d->character));
        else
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[1;31m%d\x1B[0;0mH\x1B[0;0m ",GET_HIT(d->character));
      }
      else
        count = snprintf(prompt + len, sizeof(prompt) - len, "%dH ", GET_HIT(d->character));

      if (count >= 0)
        len += count;
    }

    if (PRF_FLAGGED(d->character, PRF_DISPMANA)&& len < sizeof(prompt))
    {
      if (COLOR_LEV(d->character) >= C_CMP)
      {
        if (GET_MAX_MANA(d->character) > 0)
          percent =
            (100 * GET_MANA(d->character)) /
            GET_MAX_MANA(d->character);
        else
          percent = -1;
        if (percent >= 30)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[36m%d\x1B[0mM\x1B[0m ", GET_MANA(d->character));
        else if (percent >= 15)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[31m%d\x1B[0mM\x1B[0m ", GET_MANA(d->character));
        else if (percent >= 0)
          count = snprintf(prompt + len, sizeof(prompt) - len, "\x1B[0;31m%d\x1B[0mM\x1B[0;0m ", GET_MANA(d->character));
        else
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[1;31m%d\x1B[0mM\x1B[0;0m ", GET_MANA(d->character));
      }
      else
        count = snprintf(prompt + len, sizeof(prompt) - len, "%dM ", GET_MANA(d->character));
      if (count >= 0)
        len += count;
    }

    if (PRF_FLAGGED(d->character, PRF_DISPMOVE)&& len < sizeof(prompt))
    {
      if (COLOR_LEV(d->character) >= C_CMP)
      {
        if (GET_MAX_MOVE(d->character) > 0)
          percent =
            (100 * GET_MOVE(d->character)) /
            GET_MAX_MOVE(d->character);
        else
          percent = -1;
        if (percent >= 50)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[33m%d\x1B[0mV\x1B[0m ", GET_MOVE(d->character));
        else if (percent >= 30)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[33m%d\x1B[0mV\x1B[0m ", GET_MOVE(d->character));
        else if (percent >= 15)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[1;33m%d\x1B[0;0mV\x1B[0;0m ",GET_MOVE(d->character));
        else if (percent >= 0)
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[31m%d\x1B[0mV\x1B[0;0m ",GET_MOVE(d->character));
        else
          count = snprintf(prompt + len, sizeof(prompt) - len,"\x1B[1;31m%d\x1B[0;0mV\x1B[0;0m ",GET_MOVE(d->character));
      }
      else
        count = snprintf(prompt + len, sizeof(prompt) - len, "%dV ",GET_MOVE(d->character));
      if (count >= 0)
        len += count;
    }

    if (((PRF_FLAGGED(d->character, PRF_DISPMOVE))
         || (PRF_FLAGGED(d->character, PRF_DISPHP))
         || (PRF_FLAGGED(d->character, PRF_DISPMANA))) && len < sizeof(prompt))
    {
      strncat(prompt, ">\x1B[0m ",sizeof(prompt) - len - 1);
    }

  }
#endif
  else if (STATE(d) == CON_PLAYING && IS_NPC(d->character))
    snprintf(prompt, sizeof(prompt), "%s> ", GET_NAME(d->character));
  else
    *prompt = '\0';

  strlcat(prompt, end_prompt(d), sizeof(prompt));

  return (prompt);
}


void write_to_q(const char *txt, struct txt_q *queue, int aliased)
{
  struct txt_block *newt;

  CREATE(newt, struct txt_block, 1);
  newt->text = str_dup(txt);
  newt->aliased = aliased;

  /* queue empty? */
  if (!queue->head)
  {
    newt->next = NULL;
    queue->head = queue->tail = newt;
  }
  else
  {
    queue->tail->next = newt;
    queue->tail = newt;
    newt->next = NULL;
  }
}



int get_from_q(struct txt_q *queue, char *dest, int *aliased)
{
  struct txt_block *tmp;

  /* queue empty? */
  if (!queue->head)
    return (0);

  strcpy(dest, queue->head->text); /* strcpy: OK (mutual MAX_INPUT_LENGTH) */
  *aliased = queue->head->aliased;

  tmp = queue->head;
  queue->head = queue->head->next;

  if (tmp->text)
    free(tmp->text);
  if (tmp)
    free(tmp);

  return (1);
}

/* Initialize a descriptor */
void init_descriptor(Descriptor *newd, int desc)
{
  static int last_desc = 0;   /* last descriptor number */

  /* initialize descriptor data */

  *newd->small_outbuf = 0;
  newd->large_outbuf = NULL;
  newd->descriptor = desc;
  newd->character = NULL;
  newd->idle_tics = 0;
  newd->output = newd->small_outbuf;
  newd->bufspace = SMALL_BUFSIZE - 1;
  newd->login_time = time(0);
  *newd->output = '\0';
  newd->bufptr = 0;
  newd->has_prompt = TRUE;  /* prompt is part of greetings */
  /*
   * This isn't exactly optimal but allows us to make a design choice.
   * Do we embed the history in descriptor_data or keep it dynamically
   * allocated and allow a user defined history size?
   */
  CREATE(newd->history, char *, HISTORY_SIZE);

  if (++last_desc == 1000)
    last_desc = 1;
  newd->desc_num = last_desc;


  CREATE(newd->comp, struct compr, 1);
  newd->comp->state = 0; /* we start in normal mode */
#ifdef HAVE_ZLIB_H
  newd->comp->stream = NULL;
#endif /* HAVE_ZLIB_H */
  newd->eor = 0;
  newd->mxp = FALSE;

}


/* Empty the queues before closing connection */
void flush_queues(Descriptor *d)
{
  if (d->large_outbuf)
  {
    d->large_outbuf->next = bufpool;
    bufpool = d->large_outbuf;
  }
  while (d->input.head)
  {
    struct txt_block *tmp = d->input.head;
    d->input.head = d->input.head->next;
    free(tmp->text);
    free(tmp);
  }
}



void free_bufpool_recu(struct txt_block *k)
{
  if (!k)
    return;

  if (k->next && k->next != k)
    free_bufpool_recu(k->next);

  if (k->text)
    free(k->text);

  free(k);
}

void free_bufpool(void)
{
  free_bufpool_recu(bufpool);
  bufpool=NULL;
}



/* ******************************************************************
*  socket handling                                                  *
****************************************************************** */


/*
 * get_bind_addr: Return a struct in_addr that should be used in our
 * call to bind().  If the user has specified a desired binding
 * address, we try to bind to it; otherwise, we bind to INADDR_ANY.
 * Note that inet_aton() is preferred over inet_addr() so we use it if
 * we can.  If neither is available, we always bind to INADDR_ANY.
 */

struct in_addr *get_bind_addr()
{
  static struct in_addr bind_addr;

  /* Clear the structure */
  memset((char *) &bind_addr, 0, sizeof(bind_addr));

  /* If DLFT_IP is unspecified, use INADDR_ANY */
  if (CONFIG_DFLT_IP == NULL)
  {
    bind_addr.s_addr = htonl(INADDR_ANY);
  }
  else
  {
    /* If the parsing fails, use INADDR_ANY */
    if (!parse_ip(CONFIG_DFLT_IP, &bind_addr))
    {
      log("SYSERR: DFLT_IP of %s appears to be an invalid IP address",
          CONFIG_DFLT_IP);
      bind_addr.s_addr = htonl(INADDR_ANY);
      return &bind_addr;
    }
  }

  /* Put the address that we've finally decided on into the logs */
  if (bind_addr.s_addr == htonl(INADDR_ANY))
    log("Binding to all IP interfaces on this host.");
  else
    log("Binding only to IP address %s", inet_ntoa(bind_addr));

  return (&bind_addr);
}

#ifdef HAVE_INET_ATON

/*
 * inet_aton's interface is the same as parse_ip's: 0 on failure, non-0 if
 * successful
 */
int parse_ip(const char *addr, struct in_addr *inaddr)
{
  return (inet_aton(addr, inaddr));
}

#elif HAVE_INET_ADDR

/* inet_addr has a different interface, so we emulate inet_aton's */
int parse_ip(const char *addr, struct in_addr *inaddr)
{
  long ip;

  if ((ip = inet_addr(addr)) == -1)
  {
    return (0);
  }
  else
  {
    inaddr->s_addr = (unsigned long) ip;
    return (1);
  }
}

#else

/* If you have neither function - sorry, you can't do specific binding. */
int parse_ip(const char *addr, struct in_addr *inaddr)
{
  log("SYSERR: warning: you're trying to set DFLT_IP but your system has no\n"
      "functions to parse IP addresses (how bizarre!)");
  return (0);
}

#endif                   /* INET_ATON and INET_ADDR */



/* Sets the kernel's send buffer size for the descriptor */
int set_sendbuf(socket_t s)
{
#if defined(SO_SNDBUF) && !defined(CIRCLE_MACINTOSH)
  int opt = MAX_SOCK_BUF;

  if (setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *) &opt, sizeof(opt)) < 0)
  {
    perror("SYSERR: setsockopt SNDBUF");
    return (-1);
  }
#if 0
  if (setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char *) &opt, sizeof(opt)) <
      0)
  {
    perror("SYSERR: setsockopt RCVBUF");
    return (-1);
  }
#endif

#endif

  return (0);
}



#if RUNNING_IDENT
/********************************************************************
 * Function : get_lookup_reply
 * Created  : 97/04/05
 * Author   : Joachim Pileborg
 * E-Mail   : pt94jpi@student.hk-r.se
 *
 */
static void get_lookup_reply(void)
{
  register Descriptor *d;
  struct message msg;

  /* get the reply */
  read(id_serv_socket, &msg, sizeof(msg));
  if (msg.type == MSG_NOP)
    ;
  else if (msg.type == MSG_ERROR)
    ;
  else if (msg.type == MSG_IDREP)
  {
    for (d = descriptor_list; d; d = d->next)
    {
      if (d->descriptor == msg.fd)
      {
        *d->host = 0;
        if (*msg.user)
        {
          strcpy(d->user_name, msg.user);
          strcat(d->host     , msg.user);
          strcat(d->host     , "@");
        }
        if (*msg.host)
        {
          strcpy(d->host_name, msg.host);
          strcat(d->host     , msg.host);
        }
        else
          strcat(d->host, d->host_ip);
        // Maybe attempt to make the async just let the character in
        // and then sort out its host.
        /* STATE(d) = CON_GET_NAME;


        //Log new connections - probably unnecessary, but you may want it 
        new_mudlog( CMP, LVL_GOD, FALSE,"New connection from [%s]", d->host);
        */

        return;
      }
    }

    log("SYSERR: Ident lookup for filedecriptor not connected: %d",
        msg.fd);
  }
  else
  {
    log( "SYSERR: get_lookup_reply(): msg.type = %d\n", msg.type);
  }
}

#endif
void free_host_list(struct meta_host_data *thi)
{
  if (!thi)
    return;

  if (thi->next)
    free_host_list(thi->next);

  free(thi);
}
void save_host_list(void)
{
  FILE *fl;
  struct meta_host_data *thi;

  if ((fl = fopen(HOST_LIST_FILE, "wb")) == NULL)
    return;
  for (thi = host_list;thi;thi=thi->next)
  {
    fprintf(fl, "%s %s %ld\n", thi->host_ip, thi->host, thi->date);
  }
  fprintf(fl, "~\n");
  fclose(fl);

}
void load_host_list(void)
{
  FILE *fl;
  char host[HOST_LENGTH+1];
  char host_ip[HOST_LENGTH+1];
  char line[HOST_LENGTH * 3];
  time_t date;
  int retval = 0;
  time_t expire_date;

  if ((fl = fopen(HOST_LIST_FILE, "rb")) == NULL)
  {
    return;
  }
  expire_date = time(0) - (SECS_PER_REAL_DAY * 3);
  get_line(fl, line);
  while (line && *line && *line != '~')
  {
    if (*line != '\n')
    {
      if ((retval = sscanf(line, "%s %s %ld", host_ip, host, &date)) != 3)
      {
        log("Host list error: %s", line);
        fclose(fl);
        return;
      }
      if (date > expire_date)
        add_ip_to_host_list(host_ip, host, date);

    }
    get_line(fl, line);
  }
  fclose(fl);

}

void add_ip_to_host_list(char *host_ip, char *host, time_t date)
{
  struct meta_host_data *thi;
  if (!host_ip || !*host_ip || !host || !*host)
    return;
  CREATE(thi, struct meta_host_data, 1);
  thi->next = host_list;
  snprintf(thi->host_ip, HOST_LENGTH, host_ip);
  snprintf(thi->host, HOST_LENGTH, host);
  thi->date = date;
  host_list = thi;
}
int check_for_ip(char *ip_add, char *host)
{
  struct meta_host_data *thi;
  for (thi = host_list; thi; thi = thi->next)
  {
    if (!strcmp(thi->host_ip, ip_add))
    {
      strncpy(host, thi->host, HOST_LENGTH);
      *(host + HOST_LENGTH) = '\0';
      if (strcmp(thi->host, ip_add))
      {
        thi->date = time(0);
        save_host_list();
      }
      return 1;
    }
  }
  return 0;
}

Descriptor *new_descriptor(socket_t s, int copyover)
{
  socket_t desc = 0;
  int sockets_connected = 0;
  socklen_t i;
  Descriptor *newd;
  struct sockaddr_in peer;
  struct hostent *from = NULL;

  if (!copyover)
  {
    /* accept the new connection */
    i = sizeof(peer);
    if ((desc = accept(s, (struct sockaddr *) &peer, &i)) == INVALID_SOCKET)
    {
      perror("SYSERR: accept");
      return (NULL);
    }

    /* keep it from blocking */
    nonblock(desc);

#ifdef SO_SNDBUF
    /* set the send buffer size */
    if (set_sendbuf(desc) < 0)
    {
      CLOSE_SOCKET(desc);
      return (NULL);
    }
#endif

    /* make sure we have room for it */
    for (newd = descriptor_list; newd; newd = newd->next)
      sockets_connected++;

    if (sockets_connected >= CONFIG_MAX_PLAYING)
    {
      write_to_descriptor(desc,"Sorry, 4Dimensions is full right now... \r\n"
                          "Please try again later!\r\n", NULL);
      CLOSE_SOCKET(desc);
      return (NULL);
    }
  }
  /* create a new descriptor */
  CREATE(newd, Descriptor, 1);
  if (!copyover)
  {
    /* find the numeric site address */
    strncpy(newd->host_ip, (char *)inet_ntoa(peer.sin_addr), HOST_LENGTH); /* strncpy: OK (n->host:HOST_LENGTH+1) */
    *(newd->host_ip + HOST_LENGTH) = '\0';
    /* find the sitename */
    if (!check_for_ip((char *)inet_ntoa(peer.sin_addr), newd->host))
    {

      if (!(from = gethostbyaddr((char *) &peer.sin_addr,
                                 sizeof(peer.sin_addr), AF_INET)))
      {
        perror("SYSERR: gethostbyaddr");

        /* find the numeric site address */
        strncpy(newd->host, newd->host_ip, HOST_LENGTH);    /* strncpy: OK (n->host:HOST_LENGTH+1) */
        *(newd->host + HOST_LENGTH) = '\0';
      }
      else
      {
        strncpy(newd->host, from->h_name, HOST_LENGTH);     /* strncpy: OK (n->host:HOST_LENGTH+1) */
        *(newd->host + HOST_LENGTH) = '\0';
      }
      add_ip_to_host_list(newd->host_ip, newd->host, time(0));
      save_host_list();

    }
    else
    { //check_for_ip
      log("Meta Host Used: %s found.", newd->host);
    }

    if (isbanned(newd->host) == BAN_ALL)
    {
      CLOSE_SOCKET(desc);
      new_mudlog(CMP, LVL_GOD, TRUE, "Connection attempt denied from [%s]", newd->host);
      free(newd);
      return (NULL);
    }
  } /*copyover*/
  if (!copyover)
  {

    /* Write something, and check if it goes error-free */
    if (write_to_descriptor(desc,GREETINGS, NULL) == 0)
    {
      close(desc);  /* nope */
      return NULL;
    }
  }
  init_descriptor(newd, desc);

  /* prepend to list */
  newd->next = descriptor_list;
  descriptor_list = newd;
  if (!copyover)
  {
    newd->Output( "%s%s", "\r\nBy what name do you wish to be known? ", end_prompt(newd));
  }
  STATE(newd) = CON_GET_NAME;
  if (!copyover)
  {
#if RUNNING_IDENT
    newd->saddr = peer;
    strcpy(newd->host_ip, inet_ntoa(newd->saddr.sin_addr));
    strcpy(newd->host   , newd->host_ip);
    /* now lookup the hostname and username */
    id_lookup(newd);
#endif

  }
  return (newd);
}
void send_compress_offer(Descriptor *d)
{
#ifdef HAVE_ZLIB_H
  d->Output( "%s", will_sig);
  d->Output( "%s", will_sig2);
#endif
  return;
}
void send_out_signals(Descriptor *d)
{
  d->Output( "%s", eor_offer);
  d->Output( "%s", ga_offer);
  d->Output( "%s", will_mxp_str);
}

/*
 * Send all of the output that we've accumulated for a player out to
 * the player's descriptor.
 *
 * 32 byte GARBAGE_SPACE in MAX_SOCK_BUF used for:
 *    2 bytes: prepended \r\n
 *   14 bytes: overflow message
 *    2 bytes: extra \r\n for non-comapct
 *      14 bytes: unused
 */


int process_output(Descriptor *t)
{
  char i[MAX_SOCK_BUF], *osb = i + 2;
  int result = 0;
  /* we may need this \r\n for later -- see below */
  strlcpy(i, "\r\n", sizeof(i));   /* strcpy: OK (for 'MAX_SOCK_BUF >= 3') */

  if (strlen(t->output)-1 > MAX_SOCK_BUF)
  {
    log("output bigger then sock buf");
    strcpy(osb, "##OVERFLOW##\r\n");
  }
  else
    /* now, append the 'real' output */
    strcpy(osb, t->output);     /* strcpy: OK (t->output:LARGE_BUFSIZE < osb:MAX_SOCK_BUF-2) */


  /* if we're in the overflow state, notify the user */
  if (t->bufspace == 0)
    strlcat(i, "**OVERFLOW**\r\n", sizeof(i));

  /* add the extra CRLF if the person isn't in compact mode */
  if (STATE(t) == CON_PLAYING && t->character && !IS_NPC(t->character) && !PRF_FLAGGED(t->character, PRF_COMPACT))
    strcat(osb, "\r\n");

  /* add a prompt */
  if (t->mxp && t->character && IS_PLAYING(t))
    t->send_mxp_status();
  strlcat(i, make_prompt(t), sizeof(i));

  /**TODO: need to in the future, check this against the fact that we don't
  want to have tags cut off half way in any place, so like, no buffer overflows.
  size should always be less then lenn - Mord**/

  /*
   * now, send the output.  If this is an 'interruption', use the prepended
   * CRLF, otherwise send the straight output sans CRLF.
   */
  if (t->has_prompt)
  {
    t->has_prompt = 0;
    result = write_to_descriptor(t->descriptor, i, t->comp);
    if (result >= 2)
      result -= 2;
  }
  else
  {
    result = write_to_descriptor(t->descriptor, osb, t->comp);
  }

  if (result < 0)
  {  /* Oops, fatal error. Bye! */
    close_socket(t);

    return (-1);
  }
  else if (result == 0)
  {
    /* Socket buffer full. Try later. */
    return (0);
  }

  /* Handle snooping: prepend "% " and send to snooper. */
  if (t->snoop_by)
    t->snoop_by->Output("%% %*s%%%%", result, t->output);

  /**
  This needs to not return the altered buffer size!
  **/
  /* The common case: all saved output was handed off to the kernel buffer. */
  if (result >= (int)t->bufptr )
  {
    /*
     * if we were using a large buffer, put the large buffer on the buffer pool
     * and switch back to the small one
     */
    if (t->large_outbuf)
    {
      t->large_outbuf->next = bufpool;
      bufpool = t->large_outbuf;
      t->large_outbuf = NULL;
      t->output = t->small_outbuf;
    }
    /* reset total bufspace back to that of a small buffer */
    t->bufspace = SMALL_BUFSIZE - 1;
    t->bufptr = 0;
    *(t->output) = '\0';

    /*
     * If the overflow message or prompt or mxp or wordwrapping were partially written, try to save
     * them. There will be enough space for them if this is true.  'result'
     * is effectively unsigned here anyway.
     */
    if ((unsigned int)result < (strlen(osb)))
    {
      size_t savetextlen = strlen(osb + result);

      strcat(t->output, osb + result);
      t->bufptr   -= savetextlen;
      t->bufspace += savetextlen;
    }

  }
  else
  {
    /* Not all data in buffer sent.  result < output buffersize. */
    //size_t outlen = strlen(t->output) - result;
    // if (outlen > 0)
    //  memmove(t->output, t->output + result, outlen);
    strcpy(t->output, t->output + result);
    t->bufptr   -= result;
    t->bufspace += result;
  }

  return (result);
}


/*
 * perform_socket_write: takes a descriptor, a pointer to text, and a
 * text length, and tries once to send that text to the OS.  This is
 * where we stuff all the platform-dependent stuff that used to be
 * ugly #ifdef's in write_to_descriptor().
 *
 * This function must return:
 *
 * -1  If a fatal error was encountered in writing to the descriptor.
 *  0  If a transient failure was encountered (e.g. socket buffer full).
 * >0  To indicate the number of bytes successfully written, possibly
 *     fewer than the number the caller requested be written.
 *
 * Right now there are two versions of this function: one for Windows,
 * and one for all other platforms.
 */

#if defined(CIRCLE_WINDOWS)

ssize_t perform_socket_write(socket_t desc, const char *txt, size_t length, struct compr *comp)
{
  ssize_t result;

  result = send(desc, txt, length, 0);

  if (result > 0)
  {
    /* Write was sucessful */
    return (result);
  }

  if (result == 0)
  {
    /* This should never happen! */
    log("SYSERR: Huh??  write() returned 0???  Please report this!");
    return (-1);
  }

  /* result < 0: An error was encountered. */

  /* Transient error? */
  if (WSAGetLastError() == WSAEWOULDBLOCK
      || WSAGetLastError() == WSAEINTR)
    return (0);

  /* Must be a fatal error. */
  return (-1);
}

#else

#if defined(CIRCLE_ACORN)
#define write  socketwrite
#endif

/* perform_socket_write for all Non-Windows platforms */
ssize_t perform_socket_write(socket_t desc, const char *txt, size_t length, struct compr *comp)
{
  ssize_t result = 0;

#ifdef HAVE_ZLIB_H


  int compr_result, tmp, cnt, bytes_copied;

  /* MCCP! this is where the zlib compression is handled */
  if (comp && comp->state >= 2)
  { /* compress2 on */
    /* copy data to input buffer */
    /* first check that overflow won't happen */
    /* if it will, we only copy over so much text */
    if ((size_t)comp->size_in + length > (size_t)comp->total_in)
      bytes_copied = comp->total_in - comp->size_in;
    else
      bytes_copied = (int)length;

    // now copy what will fit into the buffer
    strncpy((char *)comp->buff_in + comp->size_in, txt, bytes_copied);
    comp->size_in += bytes_copied;

    /* set up stream input */
    comp->stream->avail_in = comp->size_in;
    comp->stream->next_in = comp->buff_in;

    /* lets do it */
    /* deflate all the input - this means flushing our output buffer when it fills */
    do
    {
      /* set up stream output - the size_out bit is somewhat unnecessary, but makes things safer */
      comp->stream->avail_out = comp->total_out - comp->size_out;
      comp->stream->next_out = comp->buff_out + comp->size_out;

      compr_result = deflate(comp->stream, comp->state == 3 ? Z_FINISH : Z_SYNC_FLUSH);

      if (compr_result == Z_STREAM_END)
        compr_result = 0;
      else if (compr_result == Z_OK && !(comp->stream->avail_out))
        compr_result = 1;
      else if (compr_result < 0)
      {  /* uh oh, fatal zlib error */
        result = 0;
        break;
      }
      else
        compr_result = 0;

      /* adjust output state value */
      comp->size_out = comp->total_out - comp->stream->avail_out;

      /* write out compressed data - flush buff_out */
      /* if problems encountered, resort to resending all data by breaking and returning < 1.. */
      tmp = 0;
      while (comp->size_out > 0)
      {
        result = write(desc, comp->buff_out + tmp, comp->size_out);
        if (result < 1) /* unsuccessful write or socket error */
          goto exitzlibdo; /* yummy, goto. faster than two breaks ! */
        comp->size_out -= result;
        tmp += result;
      }
    }
    while (compr_result);
  exitzlibdo:

    /* adjust buffers - is this necessary? not with Z_SYNC_FLUSH I think - but just to be safe */
    /* input loses size_in - avail_in bytes */
    tmp = comp->size_in - comp->stream->avail_in;
    for (cnt = tmp; cnt < comp->size_in; cnt++)
      *(comp->buff_in + (cnt - tmp)) = *(comp->buff_in + cnt);

    /* adjust input state value - it is important that this is done after the previous step */
    comp->size_in = comp->stream->avail_in;
    /* the above as taken out because I don't think its necessary.. this is faster too */
    /*comp->size_in = 0;*/

    if (result > 0)
      result = bytes_copied;
  }
  else
#endif /* HAVE_ZLIB_H */

    result = write(desc, txt, length);

  if (result > 0)
  {
    /* Write was successful. */
    return (result);
  }

  if (result == 0)
  {
    /* This should never happen! */
    log("SYSERR: Huh??  write() returned 0???  Please report this!");
    return (-1);
  }

  /*
  * result < 0, so an error was encountered - is it transient?
  * Unfortunately, different systems use different constants to
  * indicate this.
  */

#ifdef EAGAIN            /* POSIX */
  if (errno == EAGAIN)
    return (0);
#endif

#ifdef EWOULDBLOCK       /* BSD */
  if (errno == EWOULDBLOCK)
    return (0);
#endif

#ifdef EDEADLK           /* Macintosh */
  if (errno == EDEADLK)
    return (0);
#endif

  /* Looks like the error was fatal.  Too bad. */
  return (-1);
}

#endif                   /* CIRCLE_WINDOWS */


/*
 * write_to_descriptor takes a descriptor, and text to write to the
 * descriptor.  It keeps calling the system-level write() until all
 * the text has been delivered to the OS, or until an error is
 * encountered.
 *
 * Returns:
 * >=0  If all is well and good.
 *  -1  If an error was encountered, so that the player should be cut off.
 */
int write_to_descriptor(socket_t desc, const char *txt, struct compr *comp)
{
  ssize_t bytes_written;
  size_t total = strlen(txt), write_total = 0;

  while (total > 0)
  {
    bytes_written = perform_socket_write(desc, txt, total, comp);

    if (bytes_written < 0)
    {
      /* Fatal error.  Disconnect the player. */
      perror("SYSERR: Write to socket");
      return (-1);
    }
    else if (bytes_written == 0)
    {
      /* Temporary failure -- socket buffer full. */
      return (write_total);
    }
    else
    {
      txt += bytes_written;
      total -= bytes_written;
      write_total += bytes_written;
    }
  }

  return (write_total);
}





/*
 * Same information about perform_socket_write applies here. I like
 * standards, there are so many of them. -gg 6/30/98
 */
ssize_t perform_socket_read(socket_t desc, char *read_point,
                            size_t space_left)
{
  ssize_t ret;

#if defined(CIRCLE_ACORN)
  ret = recv(desc, read_point, space_left, MSG_DONTWAIT);
#elif defined(CIRCLE_WINDOWS)
  ret = recv(desc, read_point, space_left, 0);
#else
  ret = read(desc, read_point, space_left);
#endif

  /* Read was successful. */
  if (ret > 0)
    return (ret);

  /* read() returned 0, meaning we got an EOF. */
  if (ret == 0)
  {
    log("WARNING: EOF on socket read (connection broken by peer)");
    return (-1);
  }

  /*
   * read returned a value < 0: there was an error
   */

#if defined(CIRCLE_WINDOWS)   /* Windows */
  if (WSAGetLastError() == WSAEWOULDBLOCK || WSAGetLastError() == WSAEINTR)
    return (0);
#else

#ifdef EINTR        /* Interrupted system call - various platforms */
  if (errno == EINTR)
    return (0);
#endif

#ifdef EAGAIN       /* POSIX */
  if (errno == EAGAIN)
    return (0);
#endif

#ifdef EWOULDBLOCK  /* BSD */
  if (errno == EWOULDBLOCK)
    return (0);
#endif /* EWOULDBLOCK */

#ifdef EDEADLK      /* Macintosh */
  if (errno == EDEADLK)
    return (0);
#endif

#ifdef ECONNRESET
  if (errno == ECONNRESET)
    return (-1);
#endif

#endif /* CIRCLE_WINDOWS */


  /*
   * We don't know what happened, cut them off. This qualifies for
   * a SYSERR because we have no idea what happened at this point.
   */
  perror("SYSERR: perform_socket_read: about to lose connection");
  return (-1);
}
int toggle_compression(Descriptor *t)
{
#if defined(HAVE_ZLIB)
  int derr;
  if (!t->comp)
    return 0;


  /* Notify client. */
  switch(t->comp->compression)
  {
  case 1:
    write_to_descriptor(t->descriptor, on_sig, NULL);
    break;
  case 2:
    write_to_descriptor(t->descriptor, on_sig2, NULL);
    break;
  }

  CREATE(t->comp->stream, z_stream, 1);
  t->comp->stream->zalloc = z_alloc;
  t->comp->stream->zfree = z_free;
  t->comp->stream->opaque = Z_NULL;
  if ((derr = deflateInit(t->comp->stream, Z_DEFAULT_COMPRESSION)) != 0)
    log("SYSERR: deflateEnd returned %d.", derr);

  /* init the state structure */
  /* first the output component */
  CREATE(t->comp->buff_out, Bytef, SMALL_BUFSIZE);
  t->comp->total_out = SMALL_BUFSIZE;
  t->comp->size_out = 0;
  /* now the input component */
  CREATE(t->comp->buff_in, Bytef, SMALL_BUFSIZE);
  t->comp->total_in = SMALL_BUFSIZE;
  t->comp->size_in = 0;

#endif
  return 0;
}


/*
 * ASSUMPTION: There will be no newlines in the raw input buffer when this
 * function is called.  We must maintain that before returning.
 *
 * Ever wonder why 'tmp' had '+8' on it?  The crusty old code could write
 * MAX_INPUT_LENGTH+1 bytes to 'tmp' if there was a '$' as the final
 * character in the input buffer.  This would also cause 'space_left' to
 * drop to -1, which wasn't very happy in an unsigned variable.  Argh.
 * So to fix the above, 'tmp' lost the '+8' since it doesn't need it
 * and the code has been changed to reserve space by accepting one less
 * character. (Do you really need 256 characters on a line?)
 * -gg 1/21/2000
 */
int process_input(Descriptor *t)
{
  int buf_length, failed_subst;
  ssize_t bytes_read;
  size_t space_left;
  char  *read_point, *write_point, *nl_pos = NULL;
  char tmp[MAX_INPUT_LENGTH];
  char *ptr;

  const unsigned char do_eor[] = {IAC,DO,EOR,0};
  const unsigned char do_ga[] = {IAC,DO,GA,0};
  const unsigned char do_ech[] = {IAC,DO,TELOPT_ECHO,0};
#define  TELOPT_MXP        '\x5B'



  /* first, find the point where we left off reading data */
  buf_length = strlen(t->inbuf);
  read_point = t->inbuf + buf_length;
  space_left = MAX_RAW_INPUT_LENGTH - buf_length - 1;

  do
  {
    if (space_left <= 0)
    {
      log("WARNING: process_input: about to close connection: input overflow");
      return (-1);
    }

    bytes_read = perform_socket_read(t->descriptor, read_point, space_left);

    if (bytes_read < 0)  /* Error, disconnect them. */
      return (-1);
    else if (bytes_read == 0) /* Just blocking, no problems. */
      return (0);

    /* check for compression response, if still expecting something */
    /* note: this will bork if the user is giving lots of input when he first connects */
    /* he shouldn't be doing this, and for the sake of efficiency, the read buffer isn't searched */
    /* (ie. it assumes that read_point[0] will be IAC, etc.) */
    /* at this point, we know we got some data from the read */

    read_point[bytes_read] = '\0'; /* terminate the string */

    /* search for a newline in the data we just read
    for (ptr = read_point; *ptr && !nl_pos; ptr++)
      if (ISNEWL(*ptr))
        nl_pos = ptr;
    */

    for (ptr = read_point, nl_pos = NULL; *ptr && !nl_pos; ptr++)
    {
      /*
       * Search for an "Interpret As Command" marker. Note that we still
       * have an ostrich attitude to all other IAC markers.  At least now
       * they won't show up in the users' input streams. -gg 2/28/99
       */
      if (*(unsigned char *)ptr == IAC)
      {

        //log("IAC found");
        //if (*(unsigned char *)(ptr+1) == DO)
        //log("DO found");
        //else if (*(unsigned char *)(ptr+1) == DONT)
        //log("DONT found");

        //if (*(unsigned char *)(ptr+2) == COMPRESS2)
        //log("COMPRESS found");
        //else if (*(unsigned char *)(ptr+2) == EOR)
        //log("END-OF-RECORD found");

#if defined(HAVE_ZLIB)

        if (memcmp (ptr, do_sig2, strlen ((const char *)do_sig2)) == 0)
        {
          t->telnet_capable = 1;
          //log("MCCP found on");
          toggle_compression(t);
          t->comp->state = 2;
          t->comp->compression = 2;
          memmove (ptr, &ptr [strlen (do_sig2)], strlen (&ptr [strlen (do_sig2)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        }
        if (memcmp (ptr, do_sig, strlen (do_sig)) == 0)
        {
          t->telnet_capable = 1;
          //log("MCCP found on");
          toggle_compression(t);
          t->comp->state = 2;
          t->comp->compression = 1;
          memmove (ptr, &ptr [strlen (do_sig)], strlen (&ptr [strlen (do_sig)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        }
        if (memcmp (ptr, dont_sig, strlen (dont_sig)) == 0)
        {
          t->telnet_capable = 1;
          //log("MCCP found off");
          t->comp->state = 0;
          t->comp->compression = 1;
          memmove (ptr, &ptr [strlen (dont_sig)], strlen (&ptr [strlen (dont_sig)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        }
        if (memcmp (ptr, dont_sig2, strlen (dont_sig2)) == 0)
        {
          t->telnet_capable = 1;
          //log("MCCP found off");
          t->comp->state = 0;
          t->comp->compression = 2;
          memmove (ptr, &ptr [strlen (dont_sig2)], strlen (&ptr [strlen (dont_sig2)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        }
#endif
        if (memcmp (ptr, do_eor, strlen ((const char *)do_eor)) == 0)
        {
          t->telnet_capable = 0;
          t->eor = 1;
          //log("eor found");
          memmove (ptr, &ptr [strlen ((const char *)do_eor)], strlen (&ptr [strlen ((const char *)do_eor)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        }
        if (memcmp (ptr, do_ga, strlen ((const char *)do_ga)) == 0)
        {
          t->telnet_capable = 1;
          //log("eor found");
          memmove (ptr, &ptr [strlen ((const char *)do_ga)], strlen (&ptr [strlen ((const char *)do_ga)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        }
        if (memcmp (ptr, do_ech, strlen ((const char *)do_ech)) == 0)
        {
          t->telnet_capable = 1;
          /** wanna do something with this? **/
          //log("do echo found");
          memmove (ptr, &ptr [strlen ((const char *)do_ech)], strlen (&ptr [strlen ((const char *)do_ech)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        }
        if (memcmp (ptr, do_mxp_str, strlen ((const char *)do_mxp_str)) == 0)
        {
          t->telnet_capable = 1;
          t->turn_on_mxp ();
          /* remove string from input buffer */
          memmove (ptr, &ptr [strlen ((const char *)do_mxp_str)], strlen (&ptr [strlen ((const char *)do_mxp_str)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        } /* end of turning on MXP */
        else if (memcmp (ptr, dont_mxp_str, strlen ((const char *)dont_mxp_str)) == 0)
        {
          t->telnet_capable = 1;
          t->mxp = FALSE;
          /* remove string from input buffer */
          memmove (ptr, &ptr [strlen ((const char *)dont_mxp_str)], strlen (&ptr [strlen ((const char *)dont_mxp_str)]) + 1);
          ptr--; /* adjust to allow for discarded bytes */
        } /* end of turning off MXP */



        /*
         * Convert the IAC string so that it is filtered out. This
         * should, in theory, leave the 'U' from the compression
         * handshake but in my testing it didn't show up... -gg 3/21/99
         *
        while (*ptr < 0)
          *ptr++ = '\0';

         //If this was everything, pretend we had nothing. 
        if (ISNEWL(*ptr))
          *ptr++ = '\0';
        */
      }
      else if (ISNEWL(*ptr))
        nl_pos = ptr;
    }
    read_point += bytes_read;
    space_left -= bytes_read;

    /*
     * on some systems such as AIX, POSIX-standard nonblocking I/O is broken,
     * causing the MUD to hang when it encounters input not terminated by a
     * newline.  This was causing hangs at the Password: prompt, for example.
     * I attempt to compensate by always returning after the _first_ read, instead
     * of looping forever until a read returns -1.  This simulates non-blocking
     * I/O because the result is we never call read unless we know from select()
     * that data is ready (process_input is only called if select indicates that
     * this descriptor is in the read set).  JE 2/23/95.
     */
#if !defined(POSIX_NONBLOCK_BROKEN)

  }
  while (nl_pos == NULL);
#else

  }
  while (0);

  if (nl_pos == NULL)
    return (0);
#endif /* POSIX_NONBLOCK_BROKEN */

  /*
   * okay, at this point we have at least one newline in the string; now we
   * can copy the formatted data to a new array for further processing.
   */

  read_point = t->inbuf;

  while (nl_pos != NULL)
  {
    write_point = tmp;
    space_left = MAX_INPUT_LENGTH - 1;

    /* The '> 1' reserves room for a '$ => $$' expansion. */
    for (ptr = read_point; (space_left > 1) && (ptr < nl_pos); ptr++)
    {
      if (*ptr == '\b' || *ptr == 127)
      { /* handle backspacing or delete key */
        if (write_point > tmp)
        {
          if (*(--write_point) == '$')
          {
            write_point--;
            space_left += 2;
          }
          else
            space_left++;
        }
      }
      else if (isascii(*ptr) && isprint(*ptr))
      {
        if ((*(write_point++) = *ptr) == '$')
        {      /* copy one character */
          *(write_point++) = '$';  /* if it's a $, double it */
          space_left -= 2;
        }
        else
          space_left--;
      }
    }

    *write_point = '\0';

    if ((space_left <= 0) && (ptr < nl_pos))
    {
      char buffer[MAX_INPUT_LENGTH + 64];

      snprintf(buffer, sizeof(buffer), "Line too long.  Truncated to:\r\n%s\r\n", tmp);
      if (write_to_descriptor(t->descriptor, buffer, t->comp) < 0)
        return (-1);
    }
    if (t->snoop_by)
      t->snoop_by->Output("%% %s\r\n", tmp);
    failed_subst = 0;
    if (*tmp == '-' && *(tmp + 1) == '-')
      flush_queues(t);
    else if (*tmp == '!' && !(*(tmp + 1)))   /* Redo last command. */
      strcpy(tmp, t->last_input);  /* strcpy: OK (by mutual MAX_INPUT_LENGTH) */
    else if (*tmp == '!' && *(tmp + 1))
    {
      char *commandln = (tmp + 1);
      int starting_pos = t->history_pos,
                         cnt = (t->history_pos == 0 ? HISTORY_SIZE - 1 : t->history_pos - 1);

      skip_spaces(&commandln);
      for (; cnt != starting_pos; cnt--)
      {
        if (t->history[cnt] && is_abbrev(commandln, t->history[cnt]))
        {
          strcpy(tmp, t->history[cnt]); /* strcpy: OK (by mutual MAX_INPUT_LENGTH) */
          strcpy(t->last_input, tmp);   /* strcpy: OK (by mutual MAX_INPUT_LENGTH) */
          t->Output( "%s\r\n", tmp);
          break;
        }
        if (cnt == 0)    /* At top, loop to bottom. */
          cnt = HISTORY_SIZE;
      }
    }
    else if (*tmp == '^')
    {
      if (!(failed_subst = perform_subst(t, t->last_input, tmp)))
        strcpy(t->last_input, tmp);     /* strcpy: OK (by mutual MAX_INPUT_LENGTH) */
    }
    else
    {
      strcpy(t->last_input, tmp);  /* strcpy: OK (by mutual MAX_INPUT_LENGTH) */
      if (t->history[t->history_pos])
        free(t->history[t->history_pos]);    /* Clear the old line. */
      t->history[t->history_pos] = strdup(tmp);   /* Save the new. */
      if (++t->history_pos >= HISTORY_SIZE)  /* Wrap to top. */
        t->history_pos = 0;
    }

    if (!failed_subst)
      write_to_q(tmp, &t->input, 0);

    /* find the end of this line */
    while (ISNEWL(*nl_pos))
      nl_pos++;

    /* see if there's another newline in the input buffer */
    read_point = ptr = nl_pos;
    for (nl_pos = NULL; *ptr && !nl_pos; ptr++)
      if (ISNEWL(*ptr))
        nl_pos = ptr;
  }

  /* now move the rest of the buffer up to the beginning for the next pass */
  write_point = t->inbuf;
  while (*read_point)
    *(write_point++) = *(read_point++);
  *write_point = '\0';

  return (1);
}

ACMD(do_clear_buffer)
{

  ch->Send( "Commands cleared.\r\n");
}

void clear_char_q(Descriptor *t)
{

  flush_queues(t);

}

/* perform substitution for the '^..^' csh-esque syntax orig is the
 * orig string, i.e. the one being modified.  subst contains the
 * substition string, i.e. "^telm^tell"
 */
int perform_subst(Descriptor *t, char *orig, char *subst)
{
  char newsub[MAX_INPUT_LENGTH + 5];

  char *first, *second, *strpos;

  /*
   * first is the position of the beginning of the first string (the one
   * to be replaced
   */
  first = subst + 1;

  /* now find the second '^' */
  if (!(second = strchr(first, '^')))
  {
    t->Output( "Invalid substitution.\r\n");
    return (1);
  }
  /* terminate "first" at the position of the '^' and make 'second' point
   * to the beginning of the second string */
  *(second++) = '\0';

  /* now, see if the contents of the first string appear in the original */
  if (!(strpos = strstr(orig, first)))
  {
    t->Output( "Invalid substitution.\r\n");
    return (1);
  }
  /* now, we construct the new string for output. */

  /* first, everything in the original, up to the string to be replaced */
  strncpy(newsub, orig, strpos - orig); /* strncpy: OK (newsub:MAX_INPUT_LENGTH+5 > orig:MAX_INPUT_LENGTH) */
  newsub[strpos - orig] = '\0';

  /* now, the replacement string */
  strncat(newsub, second, MAX_INPUT_LENGTH - strlen(newsub) - 1);     /* strncpy: OK */

  /* now, if there's anything left in the original after the string to
   * replaced, copy that too. */
  if (((strpos - orig) + strlen(first)) < strlen(orig))
    strncat(newsub, strpos + strlen(first), MAX_INPUT_LENGTH - strlen(newsub) - 1);  /* strncpy: OK */

  /* terminate the string in case of an overflow from strncat */
  newsub[MAX_INPUT_LENGTH - 1] = '\0';
  strcpy(subst, newsub); /* strcpy: OK (by mutual MAX_INPUT_LENGTH) */

  return (0);
}


void close_socket(Descriptor *d)
{
  Descriptor *temp;

  REMOVE_FROM_LIST(d, descriptor_list, next);
  CLOSE_SOCKET(d->descriptor);
  flush_queues(d);

  /* Forget snooping */
  if (d->snooping)
    d->snooping->snoop_by = NULL;

  if (d->snoop_by)
  {
    d->snoop_by->Output("Your victim is no longer among us.\r\n");
    d->snoop_by->snooping = NULL;
    d->snoop_by = NULL;
  }

  if (d->character)
  {
    /* If we're switched, this resets the mobile taken. */
    d->character->desc = NULL;

    /* Plug memory leak, from Eric Green. */
    if (!IS_NPC(d->character) && PLR_FLAGGED(d->character, PLR_MAILING) && d->str)
    {
      if (*(d->str))
        free(*(d->str));
      free(d->str);
      d->str = NULL;
    }
    else if (d->backstr && !IS_NPC(d->character) && !PLR_FLAGGED(d->character, PLR_WRITING))
    {
      free(d->backstr);      /* editing description ... not olc */
      d->backstr = NULL;
    }
    if (IS_PLAYING(d) || STATE(d) == CON_DISCONNECT)
    {
      Character *link_challenged = d->original ? d->original : d->character;

      /* We are guaranteed to have a person. */
      act("$n has lost $s link.", TRUE, link_challenged, 0, 0, TO_ROOM);
      //save_char(link_challenged);
      new_mudlog(NRM, MAX(LVL_IMMORT, GET_INVIS_LEV(link_challenged)), TRUE, "Closing link to: %s.", GET_NAME(link_challenged));
    }
    else
    {
      new_mudlog(CMP, LVL_IMMORT, TRUE, "Losing player: %s.", GET_NAME(d->character) ? GET_NAME(d->character) : "<null>");
      free_char(d->character);
    }
  }
  else
    new_mudlog(CMP, LVL_IMMORT, TRUE, "Losing descriptor without char.");

  /* JE 2/22/95 -- part of my unending quest to make switch stable */
  if (d->original && d->original->desc)
    d->original->desc = NULL;

  /* Clear the command history. */
  if (d->history)
  {
    int cnt;
    for (cnt = 0; cnt < HISTORY_SIZE; cnt++)
      if (d->history[cnt])
        free(d->history[cnt]);
    free(d->history);
    d->history = NULL;
  }

  if (d->showstr_head)
    free(d->showstr_head);
  if (d->showstr_count)
    free(d->showstr_vector);

  /*. Kill any OLC stuff .*/
  switch (d->connected)
  {
  case CON_OEDIT:
  case CON_REDIT:
  case CON_ZEDIT:
  case CON_MEDIT:
  case CON_SEDIT:
  case CON_TEDIT:
  case CON_AEDIT:
  case CON_TRIGEDIT:
    cleanup_olc(d, CLEANUP_ALL);
    break;
  default:
    break;
  }

  /* free compression structures */
#ifdef HAVE_ZLIB_H
  if (d->comp->stream)
  {
    deflateEnd(d->comp->stream);
    free(d->comp->stream);
    free(d->comp->buff_out);
    free(d->comp->buff_in);

  }
#endif /* HAVE_ZLIB_H */
  /* d->comp was still created even if there is no zlib, for comp->state) */
  if (d->comp)
    free(d->comp);
  if (d);
  free(d);
}




void check_idle_passwords(void)
{
  Descriptor *d, *next_d;

  for (d = descriptor_list; d; d = next_d)
  {
    next_d = d->next;
    if (STATE(d) != CON_PASSWORD && STATE(d) != CON_GET_NAME)
      continue;
    if (!d->idle_tics)
    {
      d->idle_tics++;
      continue;
    }
    else
    {
      echo_on(d);
      d->Output( "\r\nTimed out... goodbye.\r\n");
      STATE(d) = CON_CLOSE;
    }
  }
}


/*
 * I tried to universally convert Circle over to POSIX compliance, but
 * alas, some systems are still straggling behind and don't have all the
 * appropriate defines.  In particular, NeXT 2.x defines O_NDELAY but not
 * O_NONBLOCK.  Krusty old NeXT machines!  (Thanks to Michael Jones for
 * this and various other NeXT fixes.)
 */

#if defined(CIRCLE_WINDOWS)

void nonblock(socket_t s)
{
  unsigned long val = 1;
  ioctlsocket(s, FIONBIO, &val);
}

#elif defined(CIRCLE_AMIGA)

void nonblock(socket_t s)
{
  long val = 1;
  IoctlSocket(s, FIONBIO, &val);
}

#elif defined(CIRCLE_ACORN)

void nonblock(socket_t s)
{
  int val = 1;
  socket_ioctl(s, FIONBIO, &val);
}

#elif defined(CIRCLE_VMS)

void nonblock(socket_t s)
{
  int val = 1;

  if (ioctl(s, FIONBIO, &val) < 0)
  {
    perror("SYSERR: Fatal error executing nonblock (comm.c)");
    exit(1);
  }
}

#elif defined(CIRCLE_UNIX) || defined(CIRCLE_OS2) || defined(CIRCLE_MACINTOSH)

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif

void nonblock(socket_t s)
{
  int flags;

  flags = fcntl(s, F_GETFL, 0);
  flags |= O_NONBLOCK;
  if (fcntl(s, F_SETFL, flags) < 0)
  {
    perror("SYSERR: Fatal error executing nonblock (comm.c)");
    exit(1);
  }
}

#endif                   /* CIRCLE_UNIX || CIRCLE_OS2 || CIRCLE_MACINTOSH */


/* ******************************************************************
*  signal-handling functions (formerly signals.c).  UNIX only.      *
****************************************************************** */

#if defined(CIRCLE_UNIX) || defined(CIRCLE_MACINTOSH)

RETSIGTYPE reread_wizlists(int sig)
{
  reread_wizlist = TRUE;
}


RETSIGTYPE unrestrict_game(int sig)
{
  emergency_unban = TRUE;
}

#ifdef CIRCLE_UNIX

/* clean up our zombie kids to avoid defunct processes */
RETSIGTYPE reap(int sig)
{
  while (waitpid(-1, NULL, WNOHANG) > 0);

  my_signal(SIGCHLD, reap);
}

RETSIGTYPE checkpointing(int sig)
{
  if (!tics_passed)
  {
    log("SYSERR: CHECKPOINT shutdown: tics not updated. (Infinite loop suspected)");
    abort();
  }
  else
    tics_passed = 0;
}

RETSIGTYPE hupsig(int sig)
{
  log("SYSERR: Received SIGHUP, SIGINT, or SIGTERM.  Shutting down...");
  exit(0);               /* perhaps something more elegant should
                                                       * substituted */
}

RETSIGTYPE chldsig(void)
{
  struct rusage ru;

  //  pid_t wait3(int *, int, struct rusage *);

  wait3(NULL, WNOHANG, &ru);
}

RETSIGTYPE core_dump_on_crash(void)
{
  log("Last command typed: %s", last_command);
  exit(0);
  //core_dump();
}

#endif                   /* CIRCLE_UNIX */

/*
 * This is an implementation of signal() using sigaction() for portability.
 * (sigaction() is POSIX; signal() is not.)  Taken from Stevens' _Advanced
 * Programming in the UNIX Environment_.  We are specifying that all system
 * calls _not_ be automatically restarted for uniformity, because BSD systems
 * do not restart select(), even if SA_RESTART is used.
 *
 * Note that NeXT 2.x is not POSIX and does not have sigaction; therefore,
 * I just define it to be the old signal.  If your system doesn't have
 * sigaction either, you can use the same fix.
 *
 * SunOS Release 4.0.2 (sun386) needs this too, according to Tim Aldric.
 */

#ifndef POSIX
#define my_signal(signo, func) signal(signo, func)
#else
sigfunc *my_signal(int signo, sigfunc * func)
{
  struct sigaction acti, oact;

  acti.sa_handler = func;
  sigemptyset(&acti.sa_mask);
  acti.sa_flags = 0;
#ifdef SA_INTERRUPT
  acti.sa_flags |= SA_INTERRUPT;    /* SunOS */
#endif

  if (sigaction(signo, &acti, &oact) < 0)
    return (SIG_ERR);

  return (oact.sa_handler);
}
#endif                   /* POSIX */


void signal_setup(void)
{
#ifndef CIRCLE_MACINTOSH
  struct itimerval itime;
  struct timeval interval;

  my_signal(SIGSEGV, (sigfunc *)core_dump_on_crash);
  /* user signal 1: reread wizlists.  Used by autowiz system. */
  my_signal(SIGUSR1, (sigfunc *)reread_wizlists);


  /*
   * user signal 2: unrestrict game.  Used for emergencies if you lock
   * yourself out of the MUD somehow.  (Duh...)
   */
  my_signal(SIGUSR2, (sigfunc *)unrestrict_game);

  /*
   * set up the deadlock-protection so that the MUD aborts itself if it gets
   * caught in an infinite loop for more than 2 minutes.
   */
  interval.tv_sec = 120;
  interval.tv_usec = 0;
  itime.it_interval = interval;
  itime.it_value = interval;
  setitimer(ITIMER_VIRTUAL, &itime, NULL);
  my_signal(SIGVTALRM, (sigfunc *)checkpointing);

  /* just to be on the safe side: */
  my_signal(SIGHUP, (sigfunc *)hupsig);
  my_signal(SIGCHLD, (sigfunc *)reap);
#endif                   /* CIRCLE_MACINTOSH */
  my_signal(SIGINT, (sigfunc *)hupsig);
  my_signal(SIGTERM, (sigfunc *)hupsig);
  my_signal(SIGPIPE, (sigfunc *)SIG_IGN);
  my_signal(SIGALRM, (sigfunc *)SIG_IGN);

#ifdef SIGCLD  /* only on SYSV */
  my_signal(SIGCLD, (sigfunc *)SIG_IGN);  /* "automagically" delete zombies */
#else
 # ifdef SIGCHLD
  my_signal(SIGCHLD, (sigfunc *)sigchld);  /* remove zombies */
# endif /* SIGCHLD */
 #endif /* SIGCLD */
}

#endif                   /* CIRCLE_UNIX || CIRCLE_MACINTOSH */

/* ****************************************************************
*       Public routines for system-to-player-communication        *
**************************************************************** */

void send_to_char(const char *messg, Character *ch)
{
  if (ch && ch->desc && messg)
    ch->Send( "%s", messg);
}

void send_to_arena(const char *messg, ...)
{
  Descriptor *d;
  va_list args;

  for (d = descriptor_list; d; d = d->next)
  {
    //      if (d->descriptor && messg && PRF_FLAGGED(d->character, PRF_ARENA))
    if (STATE(d) == CON_PLAYING && d->character && messg &&
        PRF_FLAGGED(d->character, PRF_ARENA) &&
        !PLR_FLAGGED(d->character, PLR_WRITING))
    {
      va_start(args, messg);
      d->vwrite_to_output(messg, args);
      va_end(args);
    }
  }
}

void send_to_all(const char *messg, ...)
{
  Descriptor *i;
  va_list args;

  if (messg == NULL)
    return;

  for (i = descriptor_list; i; i = i->next)
  {
    if (STATE(i) != CON_PLAYING)
      continue;

    va_start(args, messg);
    i->vwrite_to_output(messg, args);
    va_end(args);
  }
}




void send_to_outdoor(const char *messg, ...)
{
  Descriptor *i;

  if (!messg || !*messg)
    return;

  for (i = descriptor_list; i; i = i->next)
  {
    va_list args;

    if (STATE(i) != CON_PLAYING || i->character == NULL)
      continue;
    if (!AWAKE(i->character) || !OUTSIDE(i->character))
      continue;

    va_start(args, messg);
    i->vwrite_to_output( messg, args);
    va_end(args);
  }
}



void string_format(BYTE * cmd, LWORD space)
{



  BYTE *buf;
  BYTE shortbuffer[1024];
  LWORD srcOS;
  LWORD dstOS;
  LWORD cntOS;
  LWORD spacer = 0;
  //if (strlen(cmd)<=80)
  return;
  buf = shortbuffer;
  srcOS = 0;
  dstOS = (space == 2) ? 0 : space;
  cntOS = 0;

  while (cmd[srcOS])
  {

    buf[cntOS] = cmd[srcOS];
    dstOS++;
    srcOS++;
    cntOS++;

    /* break up the speech into multiple lines */

    if (dstOS >= (78 - spacer))
    {


      while (dstOS > (65 - spacer))
      {

        if (buf[cntOS] == ' ')
        {
          srcOS++;  // skip space
          buf[cntOS] = '\'';
          dstOS++;
          cntOS++;
          buf[cntOS] = '\n';
          dstOS++;
          cntOS++;
          break;
        }

        dstOS -= 1;
        srcOS -= 1;
        cntOS -= 1;

      }

      // couldnt find a space - hyphenate
      if (dstOS == (65 - spacer))
      {
        dstOS += 11;
        srcOS += 11;
        cntOS += 11;

        buf[cntOS] = '-';
        dstOS++;
        cntOS++;
        buf[cntOS] = '\'';
        dstOS++;
        cntOS++;
        buf[cntOS] = '\n';
        dstOS++;
        cntOS++;

      }

      spacer = 11;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = ' ';
      dstOS++;
      cntOS++;
      buf[cntOS] = '\'';
      dstOS++;
      cntOS++;
      dstOS = 0;

    }               //end of first if
    buf[cntOS] = '\0';

  }

  //lbuf[counter] = '\0';

  strcpy((char *)cmd,(const char *) buf);
}


void send_to_room(room_rnum room, const char *messg, ...)
{
  Character *i;
  va_list args;

  if (messg == NULL)
    return;
  if (!VALID_ROOM_RNUM(room))
    return;

  for (i = room->people; i; i = i->next_in_room)
  {
    if (!i->desc)
      continue;

    va_start(args, messg);
    i->desc->vwrite_to_output(messg, args);
    va_end(args);
  }
}

void send_to_range(room_vnum start, room_vnum finish, const char *messg, ...)
{
  Character *i;
  va_list args;
  room_rnum rm;
  room_vnum r;

  if (start > finish)
  {
    log("send_to_range passed start value greater then finish.");
    return;
  }
  if (messg == NULL)
    return;

  for (r = 0;r < top_of_world; r++)
  {
    if ((rm = world_vnum[r]) == NULL)
      continue;
    if (rm->number >= start && rm->number <= finish)
    {
      for (i = rm->people; i; i = i->next_in_room)
      {
        if (!i->desc)
          continue;

        va_start(args, messg);
        i->desc->vwrite_to_output(messg, args);
        va_end(args);
      }
    }
  }
}





/* higher-level communication: the act() function */
void perform_act(const char *orig, Character *ch,
                 struct obj_data *obj, const void *vict_obj,
                 const Character *to)
{
  const char *i = NULL;
  char lbuf[MAX_STRING_LENGTH], *buf, *j;
  Character *dg_victim = NULL;
  struct obj_data *dg_target = NULL;
  char *dg_arg = NULL;
  bool uppercasenext = FALSE;

  if (!orig || !*orig)
  {
    log("No string passed to perform_act!");
    return;
  }



  buf = lbuf;

  for (;;)
  {
    if (*orig == '$')
    {
      switch (*(++orig))
      {
      case 'n':
        i = PERS(ch, to);
        break;
      case 'N':
        CHECK_NULL(vict_obj, PERS((Character *) vict_obj, to));
        dg_victim = (Character *) vict_obj;
        break;
      case 'm':
        i = HMHR(ch);
        break;
      case 'M':
        CHECK_NULL(vict_obj, HMHR((const Character *) vict_obj));
        dg_victim = (Character *) vict_obj;
        break;
      case 's':
        i = HSHR(ch);
        break;
      case 'S':
        CHECK_NULL(vict_obj, HSHR((const Character *) vict_obj));
        dg_victim = (Character *) vict_obj;
        break;
      case 'e':
        i = HSSH(ch);
        break;
      case 'E':
        CHECK_NULL(vict_obj,  HSSH((const Character *) vict_obj));
        dg_victim = (Character *) vict_obj;
        break;
      case 'o':
        CHECK_NULL(obj, OBJN(obj, to));
        break;
      case 'O':
        CHECK_NULL(vict_obj, OBJN((const struct obj_data *) vict_obj, to));
        dg_target = (struct obj_data *) vict_obj;
        break;
      case 'p':
        CHECK_NULL(obj, OBJS(obj, to));
        break;
      case 'P':
        CHECK_NULL(vict_obj, OBJS((const struct obj_data *) vict_obj, to));
        dg_target = (struct obj_data *) vict_obj;
        break;
      case 'a':
        CHECK_NULL(obj, SANA(obj));
        break;
      case 'A':
        CHECK_NULL(vict_obj, SANA((const struct obj_data *) vict_obj));
        dg_target = (struct obj_data *) vict_obj;
        break;
      case 'T':
        CHECK_NULL(vict_obj, (const char *) vict_obj);
        dg_arg = (char *) vict_obj;
        break;
      case 't':
        CHECK_NULL(obj, (char *) obj);
        break;
      case 'F':
        CHECK_NULL(vict_obj, fname((const char *) vict_obj));
        break;
        /* uppercase previous word */
      case 'u':
        for (j = buf; j > lbuf && !isspace((int) *(j - 1)); j--);
        if (j != buf)
          *j = UPPER(*j);
        i = "";
        break;
        /* uppercase next word */
      case 'U':
        uppercasenext = TRUE;
        i = "";
        break;
      case '$':
        i = "$";
        break;
      default:
        log("SYSERR: Illegal $-code to act(): %c", *orig);
        log("SYSERR: %s", orig);
        i = "";
        break;
      }
      while ((*buf = *(i++)))
      {
        if (uppercasenext && !isspace((int) *buf))
        {
          *buf = UPPER(*buf);
          uppercasenext = FALSE;
        }
        buf++;
      }
      orig++;
    }
    else if (!(*(buf++) = *(orig++)))
    {
      break;
    }
    else if (uppercasenext && !isspace((int) *(buf - 1)))
    {
      *(buf - 1) = UPPER(*(buf - 1));
      uppercasenext = FALSE;
    }
  }
  *(--buf) = '\r';
  *(++buf) = '\n';
  *(++buf) = '\0';

  if (to->desc)
    to->desc->Output("%s", CAP(lbuf));

  if ((IS_NPC(to) && dg_act_check) && (to != ch))
    act_mtrigger(to, lbuf, ch, dg_victim, obj, dg_target, dg_arg);
}


int restrict_check(const Character *ch)
{

  if (ch == NULL)
    return 1;

  switch (message_type)
  {
  case REST_MOVE:
    if (PRF_FLAGGED(ch, PRF_MOVEMSG))
      return 0;
    break;
  }

  return 1;
}

/* moved this to utils.h --- mah
#ifndef SENDOK
#define SENDOK(ch)  ((ch)->desc && (to_sleeping || AWAKE(ch)) && \
               (IS_NPC(ch) || !PLR_FLAGGED((ch), PLR_WRITING)))
#endif
*/

void act(const char *str, int hide_invisible, Character *ch,
         struct obj_data *obj, const void *vict_obj, int type)
{
  const Character *to = NULL;
  int to_sleeping;

  if (!str || !*str)
    return;

  if (!(dg_act_check = !(type & DG_NO_TRIG)))
    type &= ~DG_NO_TRIG;

  /*
   * Warning: the following TO_SLEEP code is a hack.
   * 
   * I wanted to be able to tell act to deliver a message regardless of sleep
   * without adding an additional argument.  TO_SLEEP is 128 (a single bit
   * high up).  It's ONLY legal to combine TO_SLEEP with one other TO_x
   * command.  It's not legal to combine TO_x's with each other otherwise.
   * TO_SLEEP only works because its value "happens to be" a single bit;
   * do not change it to something else.  In short, it is a hack.
   */

  /* check if TO_SLEEP is there, and remove it if it is. */
  if ((to_sleeping = (type & TO_SLEEP)))
    type &= ~TO_SLEEP;

  if (type == TO_CHAR)
  {
    if (ch && SENDOK(ch) && restrict_check(ch))
      perform_act(str, ch, obj, vict_obj, ch);
    return;
  }

  if (type == TO_VICT)
  {
    if ((to = (const Character *) vict_obj)
        && ((SENDOK(to) && restrict_check((const Character *)to)) || dg_act_check))
    {
      if (!is_ignoring((Character *)to, ch))
        perform_act(str, ch, obj, vict_obj, to);
    }
    return;
  }
  /* ASSUMPTION: at this point we know type must be TO_NOTVICT or TO_ROOM */

  if (ch && ch->in_room != NULL)
    to = ch->in_room->people;
  else if (obj && obj->in_room != NULL)
    to = obj->in_room->people;
  else
  {
    log("SYSERR: no valid target to act()!");
    return;
  }

  for (; to; to = to->next_in_room)
  {
    if (!SENDOK(to) || (to == ch))
      continue;
    if (hide_invisible && ch && !CAN_SEE(to, ch))
      continue;
    if (type != TO_ROOM && to == vict_obj)
      continue;
    if (!restrict_check((const Character *)to))
      continue;
    if (!is_ignoring((Character *)to, ch))
      perform_act(str, ch, obj, vict_obj, to);
  }
}


/* Prefer the file over the descriptor. */
void setup_log(const char *filename, int fd)
{
  FILE *s_fp;

#if defined(__MWERKS__) || defined(__GNUC__)
  s_fp = stderr;
#else
  if ((s_fp = fdopen(STDERR_FILENO, "w")) == NULL)
  {
    puts("SYSERR: Error opening stderr, trying stdout.");

    if ((s_fp = fdopen(STDOUT_FILENO, "w")) == NULL)
    {
      puts("SYSERR: Error opening stdout, trying a file.");

      /* If we don't have a file, try a default. */
      if (filename == NULL || *filename == '\0')
        filename = "log/syslog";
    }
  }
#endif

  if (filename == NULL || *filename == '\0')
  {
    /* No filename, set us up with the descriptor we just opened. */
    logfile = s_fp;
    syslogfd = fileno(s_fp);
    puts("Using file descriptor for logging.");
    return;
  }

  /* We honor the default filename first. */
  if (open_logfile(filename, s_fp))
    return;

  /* Well, that failed but we want it logged to a file so try a default. */
  if (open_logfile("log/syslog", s_fp))
    return;

  /* Ok, one last shot at a file. */
  if (open_logfile("syslog", s_fp))
    return;

  /* Erp, that didn't work either, just die. */
  puts("SYSERR: Couldn't open anything to log to, giving up.");
  exit(1);
}

int open_logfile(const char *filename, FILE * stderr_fp)
{
  if (stderr_fp)         /* freopen() the descriptor. */
    logfile = freopen(filename, "w", stderr_fp);
  else
    logfile = fopen(filename, "w");

  syslogfd = fileno(logfile);
  if (logfile)
  {
    printf("Using log file '%s'%s.\n",
           filename, stderr_fp ? " with redirection" : "");
    return (TRUE);
  }

  printf("SYSERR: Error opening file '%s': %s\n", filename,
         strerror(errno));
  return (FALSE);
}

/* Prefer the file over the descriptor. */
void setup_com(const char *filename, int fd)
{
  FILE *s_fp = NULL;


  if (filename == NULL || *filename == '\0')
  {
    return;
  }

  /* We honor the default filename first. */
  if (open_comlogfile(filename, s_fp))
    return;

  /* Well, that failed but we want it logged to a file so try a default. */
  if (open_comlogfile("log/comlog", s_fp))
    return;

  /* Ok, one last shot at a file. */
  if (open_comlogfile("comlog", s_fp))
    return;

  /* Erp, that didn't work either, just die. */
  puts("SYSERR: Couldn't open anything to log to, giving up.");
  exit(1);
}

int open_comlogfile(const char *filename, FILE * stderr_fp)
{
  if (stderr_fp)         /* freopen() the descriptor. */
    comfile = freopen(filename, "w", stderr_fp);
  else
    comfile = fopen(filename, "w");

  if (comfile)
  {
    printf("Using log file '%s'%s.\n",
           filename, stderr_fp ? " with redirection" : "");
    return (TRUE);
  }

  printf("SYSERR: Error opening file '%s': %s\n", filename,
         strerror(errno));
  return (FALSE);
}


/*
 * This may not be pretty but it keeps game_loop() neater than if it was inline.
 */
#if defined(CIRCLE_WINDOWS)

void circle_sleep(struct timeval *timeout)
{
  Sleep(timeout->tv_sec * 1000 + timeout->tv_usec / 1000);
}

#else

void circle_sleep(struct timeval *timeout)
{
  if (select(0, (fd_set *) 0, (fd_set *) 0, (fd_set *) 0, timeout) < 0)
  {
    if (errno != EINTR)
    {
      perror("SYSERR: Select sleep");
      exit(1);
    }
  }
}

#endif

#if defined(HAVE_ZLIB)

/* Compression stuff. */

void *zlib_alloc(void *opaque, unsigned int items, unsigned int size)
{
  return calloc(items, size);
}

void zlib_free(void *opaque, void *address)
{
  free(address);
}

#endif


/*
void send_to_prf(char *messg, Character *nosend, int prf_flags) {
  register Descriptor *i;
  register Character *ch;
  int to_sleeping = 1;
  
 
  for (i = descriptor_list; i; i = i->next)
    if (!i->connected &&
        (ch = (i->character ? i->character : i->original)) != nosend &&
        PRF_FLAGGED(ch, prf_flags) && SENDOK(ch))
      SEND_TO_Q(messg, i);
}
*/

void brag(Character *ch, Character *vict)
{
  /* Npc taunts slayed player characters.  Text goes out through gossip
     channel.  Muerte - Telnet://betterbox.net:4000                     */

  Descriptor *i;
  Descriptor *next;
  char buf[MAX_STRING_LENGTH];
  const char *bragmsg[] =
    {
      "$N was just too easy a kill!", //0
      "Huh? Did you die $N? Sorry I was taking a nap!",
      "Hey $N, I'm pissing on your corpse!",
      "$N stinks! Buwahaha!",
      "$N has fallen! Muahaha!",
      "$N saw the graveyard and decided to stay!",
      "$N just took a dirt nap! Muhaha!",
      "$N has kicked the bucket! *snicker*",
      "I have snuffed $N's life! Muhaha!",
      "$N greeted death warmly! Muhahaha!",
      "$N has decided to stop living for a few!",
      "$N has fallen in battle! Muhaha!",
      "$N has temporarily stepped out of this life!",
      "$N has been vanquished!",
      "$N has drawn $S last breath!",
      "$N was cut down in $S prime!",
      "$N has been extinguished!",
      "$N was mercilessly slaughtered!",
      "$N was sent packing!",
      "No $N, not best 2 of 3!",
      "$N fall down! Go boom!!",
      "$N has a chest full of holes..Muhaha!",
      "$N, a dingo ate your corpse! Muahha!",
      "$N just ran with scissors! Muhaha!",
      "$N uses standard issue! Muhaha!",
      "$N is a skinny runt. Fights like a cityguard. Buwahaha!",
      "Bleh! $N you're giving me indigestion!. Muhaha!",
      "$N your skill in death mimicry is at 100 percent!",
      "Is that ALL you can do $N? Muhaha!",
      "Damnit $N! You got my weapon all bloody!",
      "OOOooooooo more of $N's fingerpaint!",
      "$N's ugly and $S mother dresses him.. dead.",
      "$N, your tombstone's gonna read: 'Here lies the body of a pansy!'",
      "$N, may you rest in pieces!",
      "You look good in red, $N!",
      "When I said death before dishonor $N, I meant alphabetically",
      "5 heals: 10,000 coins. 1 dagger: 20,000 coins, $N's heart on a platter: Priceless!",
      "$N had such a lovely neck, but the head had to go! Muhaha!",
      "Damn it $N! These bloodstains will NEVER come off now!",
      "'I took the pain away from $N, the dead have no feeling!",
      "$N was all hype, no substance!",
      "Note to $N: Don't fight when you suck!",
      "$N was a tasty dinner, now who's for desert? Muhaha!",
      "Bahaha! $N should sit at recall!",
      "$N is now in need of some exp...Muhaha!",
      "$N needs a hospital now. Muhaha!",
      "$N's mother is a slut! Muhaha!",
      "$N is a punk, hits like a swampfly. Bah.",
      "$N, your life force has just run out...Muahaha!",
      "Bah, $N should stick to the newbie zone!",
      "$N, give me your daughter's number and I might return your corpse. Muhaha!",
      "Hey $N! Come back, you dropped your corpse! Muahaha",
      "I think $N wears pink chainmail. Fights like a girl! Muhaha!",
      "Thanks for the powerlevel, $N!  Bwahaha!"//53

    };


  if (ch == vict)
    return;
  //  if (IS_NPC(ch))
  // snprintf(buf, sizeof(buf), "Someone brags, '%s'", brag[number(0, 53)]);
  // else
  snprintf(buf, sizeof(buf), "%s brags, '%s'", GET_NAME(ch),  bragmsg[number(0, 53)]);

  for (i = descriptor_list; i; i = next)
  {
    next = i->next;
    if (!i->connected && i != ch->desc && i->character &&
        !PRF_FLAGGED(i->character, PRF_NOBRAG) &&
        !PLR_FLAGGED(i->character, PLR_WRITING) &&
        !ROOM_FLAGGED(i->character->in_room, ROOM_SOUNDPROOF))
    {

      if (COLOR_LEV(i->character) >= C_NRM)
        i->Output("%s", CCRED(i->character, C_NRM));

      act(buf, FALSE, i->character, 0, vict, TO_CHAR | TO_SLEEP);

      if (COLOR_LEV(i->character) >= C_NRM)
        i->Output( "%s", CCNRM(i->character, C_NRM));
    }
  }
}

void make_wholist(void)
{
  FILE *fl;
  Character *ch;
  Descriptor *d;
  char * the_date_now(char * buf, size_t len);
  char * the_uptime(char * buf, size_t len);
  char buf[MAX_INPUT_LENGTH];
  static int xml_log_trys = 0;

  if (port != 6000)
    return;
  if ((fl = fopen("/var/www/html/images/wholist.xml", "w")) == 0)
  {
    if (++xml_log_trys < 4)
      log("XML Who List unable to be opened (Try %d)", xml_log_trys);
    else if (xml_log_trys == 4)
      log("XML Who List unable to be opened (Try 4) [Error will no longer be reported]");

    return;              /* or log it ? *shrug* */
  }

  fprintf(fl, "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n" );
  //fprintf(fl, "<?xml-stylesheet type=\"text/xsl\" href=\"http://4dimensions.mu-host.com/who.xsl\"?>\n");
  fprintf(fl, "<wholist>\n" );

  fprintf(fl, "<info>\n"
          "<uptime>%s</uptime>\n"
          "<updated>%s</updated>\n"
          "</info>\n",
          the_uptime(buf, sizeof(buf)),
          the_date_now(buf, sizeof(buf)));
  fprintf(fl, "<players>");
  for (d = descriptor_list; d; d = d->next)
  {
    if (!IS_PLAYING(d))
      continue;

    if (d->original) ch = d->original;
    else if (!(ch = d->character))
      continue;
    if (GET_LEVEL(ch) < LVL_IMMORT
        || (GET_LEVEL(ch) >= LVL_IMMORT && !GET_INVIS_LEV(ch)))
    {
      fprintf(fl,
              "<player>\n"
              "<name>%s</name>\n"
              "<lev>%d</lev>\n"
              "<tier>%s</tier>\n"
              "<race>%s</race>\n"
              "<clan>%s</clan>\n"
              "<idle>%d</idle>\n"
              "</player>\n",

              GET_NAME(ch),
              GET_LEVEL(ch),
              class_name[(int)GET_CLASS(ch)].name[current_class_is_tier_num(ch)],
              RACE_ABBR(ch),
              clan_name(find_clan_by_id(GET_CLAN(ch)))  ,
              ch->char_specials.timer * SECS_PER_MUD_HOUR /
              SECS_PER_REAL_MIN);
    }
  }
  fprintf(fl, "</players>\n");
  fprintf(fl, "</wholist>\n" );
  fclose(fl);
}

void make_who2html(void)
{

  extern char *class_abbrevs[];
  FILE *opf;
  Descriptor *d;
  Character *ch;

  char buf[MAX_STRING_LENGTH];
  if (port != 6000)
    return;

  make_wholist();
  if ((opf = fopen("/var/www/html/mud.html", "w")) == 0)
    return;              /* or log it ? *shrug* */
  fprintf(opf, "<HTML><HEAD><TITLE>Who is on the Mud?</TITLE></HEAD>\n");
  fprintf(opf, "<BODY>");
  fprintf(opf, "<style>");
  fprintf(opf, ".cntr {\n");
  fprintf(opf, "width:382px;\n");
  fprintf(opf, "border:solid #000 2px;\n");
  fprintf(opf, "text-align:right;\n");
  fprintf(opf, "padding:0;\n");
  fprintf(opf, "background: #FFF;\n");
  fprintf(opf, "}\n");
  fprintf(opf, "body {\n");
  fprintf(opf, "margin:0;\n");
  fprintf(opf, "color: #000;\n");
  fprintf(opf, "font-family: \"Arial\";\n");
  fprintf(opf, "font-size: 11px;\n");
  fprintf(opf, "line-height: 175%%;\n");
  fprintf(opf, "}\n");
  fprintf(opf, "h1 {\n");
  fprintf(opf, "text-decoration: none;\n");
  fprintf(opf, "color: #BE7373;\n");
  fprintf(opf, "font-weight: bold;\n");
  fprintf(opf, "}\n");
  fprintf(opf, "</style>\n");

  fprintf(opf, "<H1>Who is playing right now?</H1><HR>\n");

  for (d = descriptor_list; d; d = d->next)
    if (!d->connected)
    {
      if (d->original)
        ch = d->original;
      else if (!(ch = d->character))
        continue;
      if (GET_LEVEL(ch) < LVL_IMMORT
          || (GET_LEVEL(ch) >= LVL_IMMORT && !GET_INVIS_LEV(ch)))
      {
        sprintf(buf, "[%2d %s %s] %s %s\n <BR>", GET_LEVEL(ch),
                RACE_ABBR(ch), CLASS_ABBR(ch),
                GET_LEVEL(ch) > 50 ? "(GOD)" : "     ",
                GET_NAME(ch));
        fprintf(opf, buf);
      }
    }
    else
      fprintf(opf,
              "Apparently, nobody is logged on at the moment...");

  fprintf(opf, "<HR></BODY></HTML>\n");
  fclose(opf);
}
#define PROMPT_CHAR '%'
char * parse_prompt(Character *ch, char *str, size_t lenn)
{

  char insert_text[MAX_INPUT_LENGTH] = "";
  char *inbuf;
  //static char prompt[MAX_STRING_LENGTH + 1] = "";
  char out_buf[MAX_STRING_LENGTH + 1] = "";
  char ptemp[MAX_PROMPT_LENGTH * 5] = "";
  register unsigned int inpos = 0, outpos = 0;
  Descriptor *d = NULL;
  time_t ct = time(0);
  bool def = FALSE;
  int count = 0, mhp= 0;
  gold_int expe;
  size_t len = 0, psize;
  
  if (ch && ch->desc)
    d = ch->desc;
  char **msg;

  if (!FIGHTING(ch))
    msg = &(PROMPT(ch));
  else
    msg = &(BPROMPT(ch));

  if (*msg && **msg)
  {
    strcpy(ptemp, *msg);
    proc_color(ptemp, IRANGE(0, COLOR_LEV(ch), 3), sizeof(ptemp));
    ptemp[MAX_PROMPT_LENGTH-1] = '\0';
  }
  else
    strcpy(ptemp, "");

  psize = sizeof(out_buf);

  inbuf = ptemp;
  if (PRF_FLAGGED(ch, PRF_AFK)&& len < psize)
  {
    count = snprintf(out_buf + len, psize - len, "\x1B[32m[AFK]\x1B[0m ");
    if (count >= 0)
      len += count;
  }
  if (PRF_FLAGGED(ch, PRF_RP)&& len < psize)
  {
    count = snprintf(out_buf + len, psize - len, "\x1B[32m[RP]\x1B[0m ");
    if (count >= 0)
      len += count;
  }


  if ((check_mail(ch) && len < psize)
      && (PRF_FLAGGED(ch, PRF_MAIL)))
  {
    count = snprintf(out_buf + len, psize - len, "[MAIL] ");
    if (count >= 0)
      len += count;
  }

  if ((has_note(ch, NOTE_NOTE) && len < psize)
      && (PRF_FLAGGED(ch, PRF_MAIL)))
  {
    count = snprintf(out_buf + len, psize - len, "[NOTE] ");
    if (count >= 0)
      len += count;
  }
  if ((has_note(ch, NOTE_NEWS) && len < psize)
      && (PRF_FLAGGED(ch, PRF_MAIL)))
  {
    count = snprintf(out_buf + len, psize - len, "[NEWS] ");
    if (count >= 0)
      len += count;
  }
  if ((has_note(ch, NOTE_CHANGES) && len < psize)
      && (PRF_FLAGGED(ch, PRF_MAIL)))
  {
    count = snprintf(out_buf + len, psize - len, "[CHANGES] ");
    if (count >= 0)
      len += count;
  }
  if ((has_note(ch, NOTE_IDEA) && len < psize)
      && (PRF_FLAGGED(ch, PRF_MAIL)))
  {
    count = snprintf(out_buf + len, psize - len, "[IDEA] ");
    if (count >= 0)
      len += count;
  }



  if (PLR_FLAGGED(ch, PLR_KILLER)&& len < psize)
  {
    count = snprintf(out_buf + len, psize - len, "[\x1B[1;31mKILLER\x1B[0;0m] ");
    if (count >= 0)
      len += count;
  }

  if (GET_INVIS_LEV(ch)&& len < psize)
  {
    count = snprintf(out_buf + len, psize - len, "(i%d) ", GET_INVIS_LEV(ch));
    if (count >= 0)
      len += count;
  }


  if (PRF_FLAGGED(ch, PRF_BUILDWALK) && len < psize)
  {
    count = snprintf(out_buf + len, psize - len, "BUILDWALKING ");
    if (count >= 0)
      len += count;
  }


  outpos = len;

  if ((inbuf && *inbuf))
  {
    if (str_cmp(inbuf, "none"))
    {
      mhp = ((GET_HIT(ch)*100)/GET_MAX_HIT(ch));
      while (inbuf[inpos] != '\0' && inpos < MAX_PROMPT_LENGTH)
      {
        //remaining = strlen(inbuf) - inpos;

        //if (remaining > 1) {
        if (inbuf[inpos] == PROMPT_CHAR)
        {
          *insert_text = '\0';
          def = FALSE;
          switch (inbuf[inpos + 1])
          {

          default:
            insert_text[0] = PROMPT_CHAR;
            insert_text[1] = '\0';
            inpos += 1;
            def = TRUE;
            break;
          case PROMPT_CHAR:
            insert_text[0] = PROMPT_CHAR;
            insert_text[1] = '\0';
            break;
          case '1':
            print_weather(IN_ROOM(ch), insert_text, sizeof(insert_text));
            break;
          case '2':
            snprintf(insert_text, sizeof(insert_text), "%s", IS_DAY ? "Day" : IS_NIGHT ? "Night" : "Twilight");
            break;
          case '3':
            snprintf(insert_text, sizeof(insert_text), "%s", (IS_HOT(IN_ROOM(ch)) ? "Hot" : (IS_COLD(IN_ROOM(ch)) ? "Cold" : "Warm")));
            break;
          case '4':
            snprintf(insert_text, sizeof(insert_text), "%d", MASTER(ch) && !DEAD(MASTER(ch)) ? (GET_MOVE(MASTER(ch))*100)/GET_MAX_MOVE(MASTER(ch)) : 0);
            break;
          case 'f':
            snprintf(insert_text, sizeof(insert_text), "%d", FIGHTING(ch) && !DEAD(FIGHTING(ch)) ? (GET_HIT(FIGHTING(ch))*100)/GET_MAX_HIT(FIGHTING(ch)) : 0);
            break;
          case 'd':
            commafmt(insert_text, sizeof(insert_text), (gold_int)GET_LAST_DAM_D(ch));
            break;
          case 'D':
            commafmt(insert_text, sizeof(insert_text), (gold_int)GET_LAST_DAM_T(ch));
            break;
          case 'J':
            snprintf(insert_text, sizeof(insert_text), "%d", MASTER(ch) && !DEAD(MASTER(ch)) ? (GET_HIT(MASTER(ch))*100)/GET_MAX_HIT(MASTER(ch)) : 0);
            break;
          case 'c':
            snprintf(insert_text, sizeof(insert_text), "%s", clan_name(find_clan_by_id(GET_CLAN(ch))));
            break;
          case 'j':
            snprintf(insert_text, sizeof(insert_text), "%d",GET_ALIGNMENT(ch));
            break;
          case 'b':
            snprintf(insert_text, sizeof(insert_text), "%s", RIDING(ch) && !DEAD(RIDING(ch)) ? GET_NAME(RIDING(ch)) : "unmounted");
            break;
          case 'B':
            snprintf(insert_text, sizeof(insert_text), "%s", RIDDEN_BY(ch) && !DEAD(RIDDEN_BY(ch))? GET_NAME(RIDDEN_BY(ch)) : "Unriden");
            break;
          case 'i':
            snprintf(insert_text, sizeof(insert_text), "%d",(int)GET_PERC(ch));
            break;
          case 'r':
            snprintf(insert_text, sizeof(insert_text), "%d",GET_RIP_CNT(ch));
            break;
          case 'R':
            snprintf(insert_text, sizeof(insert_text), "%d",GET_PK_RIP(ch));
            break;
          case 'k':
            commafmt(insert_text, sizeof(insert_text), GET_KILL_CNT(ch));
            break;
          case 'K':
            snprintf(insert_text, sizeof(insert_text), "%d",GET_PK_CNT(ch));
            break;
          case 'l':
            snprintf(insert_text, sizeof(insert_text), "%d",GET_LEVEL(ch));
            break;
          case 'L':
            snprintf(insert_text, sizeof(insert_text), "%d", FIGHTING(ch) && !DEAD(FIGHTING(ch)) ? GET_LEVEL(FIGHTING(ch)) : 0);
            break;
          case 'T':
            snprintf(insert_text, sizeof(insert_text), "%-2d",localtime(&ct)->tm_hour);
            break;
          case 't':
            snprintf(insert_text, sizeof(insert_text), "%-2d",localtime(&ct)->tm_min);
            break;
          case 's':
            snprintf(insert_text, sizeof(insert_text), "%-2d",localtime(&ct)->tm_sec);
            break;
          case 'S':
            snprintf(insert_text, sizeof(insert_text), "%d",(GET_STAMINA(ch) * 100) / GET_MAX_STAMINA(ch));
            break;
          case 'H':
            /*if (d && d->mxp)
            snprintf(insert_text, sizeof(insert_text), "<hp>%d</hp>", GET_MAX_HIT(ch));
            else*/
            snprintf(insert_text, sizeof(insert_text), "%d", GET_MAX_HIT(ch));
            break;
          case 'h':
            /*if (d && d->mxp)
                     snprintf(insert_text, sizeof(insert_text), "%s<xhp>%d</xhp>%s",mhp <= 30 ? "\x1B[1;31m" : "", GET_HIT(ch), mhp <= 30 ? "\x1B[1m""\x1B[0m" : "");*/
            snprintf(insert_text, sizeof(insert_text), "%s%d%s",mhp <= 30 ? "\x1B[1;31m" : "", GET_HIT(ch), mhp <= 30 ? "\x1B[1m""\x1B[0m" : "");
            break;
          case 'M':
            /* if (d && d->mxp)
                      snprintf(insert_text, sizeof(insert_text), "<xmana>%d</xmana>",GET_MAX_MANA(ch));
               else*/
            snprintf(insert_text, sizeof(insert_text), "%d",GET_MAX_MANA(ch));
            break;
          case 'm':
            /*if (d && d->mxp)
            snprintf(insert_text, sizeof(insert_text), "<mana>%d</mana>", GET_MANA(ch));
            else*/
            snprintf(insert_text, sizeof(insert_text), "%d", GET_MANA(ch));
            break;
          case 'V':
            /*if (d && d->mxp)
            snprintf(insert_text, sizeof(insert_text), "<xmove>%d</xmove>", GET_MAX_MOVE(ch));
            else*/
            snprintf(insert_text, sizeof(insert_text), "%d",GET_MAX_MOVE(ch));
            break;
          case 'v':
            /*if (d && d->mxp)
            snprintf(insert_text, sizeof(insert_text), "<move>%d</move>", GET_MOVE(ch));
            else*/
            snprintf(insert_text, sizeof(insert_text), "%d", GET_MOVE(ch));
            break;
          case 'G':
            commafmt(insert_text, sizeof(insert_text), GET_BANK_GOLD(ch));
            break;
          case 'g':
            commafmt(insert_text, sizeof(insert_text), GET_GOLD(ch));
            break;
          case 'e':
            commafmt(insert_text, sizeof(insert_text),GET_EXP(ch));
            break;
          case 'E':
            if ((expe = exp_needed(ch)) < 0)
            {
              if (GET_LEVEL(ch) == LVL_MAX_MORT)
                snprintf(insert_text, sizeof(insert_text), "Can Now Remort");
              else if (GET_LEVEL(ch) > LVL_MAX_MORT)
                snprintf(insert_text, sizeof(insert_text), "Immortal");
              else
                snprintf(insert_text, sizeof(insert_text), "Can Now Level");
            }
            else
              commafmt(insert_text, sizeof(insert_text), expe);
            break;
          case 'x':
            snprintf(insert_text, sizeof(insert_text), "%d",mhp);
            break;
          case 'y':
            snprintf(insert_text, sizeof(insert_text), "%d",(GET_MANA(ch)*100)/GET_MAX_MANA(ch));
            break;
          case 'z':
            snprintf(insert_text, sizeof(insert_text), "%d",(GET_MOVE(ch)*100)/GET_MAX_MOVE(ch));
            break;
          case 'n':
            snprintf(insert_text, sizeof(insert_text), "%s", GET_NAME(ch));
            break;
          case 'N':
            snprintf(insert_text, sizeof(insert_text), "%s", MASTER(ch) && !DEAD(MASTER(ch)) ? GET_NAME(MASTER(ch)) : GET_NAME(ch));
            break;
          case 'F':
            snprintf(insert_text, sizeof(insert_text), "%s", FIGHTING(ch) && !DEAD(FIGHTING(ch)) ? GET_NAME(FIGHTING(ch)) : "not fighting");
            break;
          case 'X':
            snprintf(insert_text, sizeof(insert_text), "%lld",
                     (gold_int)
                     ((exp_needed(ch)*100)/
                      (level_exp((int)GET_CLASS(ch), GET_LEVEL(ch) + 1,  current_class_is_tier_num(ch), REMORTS(ch)) -
                       level_exp((int)GET_CLASS(ch), GET_LEVEL(ch)    ,  current_class_is_tier_num(ch), REMORTS(ch))    )));
            break;
          case '!':
            snprintf(insert_text, sizeof(insert_text), "%s", "\r\n");
            break;

          }

          inpos += ((int)!def)*2;

          /* switch */
          if (outpos < (sizeof(out_buf)))
            out_buf[outpos] = '\0';

          /* don't overfill buffer */
          if (insert_text != NULL && (outpos < (sizeof(out_buf))))
          {
            out_buf[outpos] = '\0';     /* so strcat is not confused by whatever out_buf WAS */
            strlcat(out_buf, insert_text, sizeof(out_buf));
            outpos = strlen(out_buf);
          }


        } /* if char is '/' (PROMPT_CHAR) */
        else
        {
          if (outpos < (sizeof(out_buf)))
            out_buf[outpos] = inbuf[inpos];

          inpos++;
          outpos++;

        }

        //} /* if remaining > 2 */
        /*else {
            if (outpos < MAX_PROMPT_LENGTH) {
          out_buf[outpos] = inbuf[inpos];
          inpos++;
          outpos++;
            }
        }*/


      }                  /* while */
    } /* none */
  } /* if inbuf */

  outpos = sizeof(out_buf);
  out_buf[outpos] = '\0';
  strlcat(str, out_buf, lenn);
  strlcat(str, end_prompt(ch->desc), lenn);
  return str;
}

/* This function takes a block of text that may already have formatting in it,
   and wraps it intuitively to a certain width.
   cmd = the text to be wrapped.
   width = the width to wrap cmd at.
   maxlen = the sizeof(cmd)
   
   This function returns the modified (cmd)
   
   By Jamie Nelson
   Mordecai@xtra.co.nz
*/
/** TODO: This needs to be changed to never put newlines inside MXP tags! **/
char *wordwrap(char *cmd, char *buf, size_t width, size_t maxlen)
{
  size_t srcOS = 0;
  size_t dstOS = 0;
  size_t cntOS = 0;
  size_t skip = FALSE;
  if (!cmd || !*cmd)
  {
    *buf = '\0';
    return cmd;
  }
  /* no need to wrap the text if it is shorter then the width, just return */
  if (maxlen<=width)
  {
    strcpy(buf, cmd);
    return cmd;
  }

  /* if for some reason width is 0 AND cmd has 0 characters in it. just return */
  if (maxlen <= 0)
  {
    strcpy(buf, cmd);
    return cmd;
  }



  /*
   This switch block stops color from being split onto different lines.
   as well as if a line has a \r\n before the width of the line,
   just stop there and start on the next line.
  */
  while (cmd[srcOS] && srcOS < maxlen)
  {

    buf[cntOS] = cmd[srcOS];

    switch (buf[cntOS])
    {
    case '\x1B':
      skip = TRUE;
      break;
    case 'm':
      if (skip)
        skip = FALSE;
      else
        dstOS++;
      break;
    case '\r':
      dstOS = 0;
      break;
    case '\n':
      dstOS = 0;
      break;
    default:
      if (skip != TRUE)
        dstOS++;
      break;
    }

    if ((dstOS == width) &&  ((cmd[srcOS + 1] == '\r') || (cmd[srcOS + 1] == '\n')))
      dstOS = 0;

    srcOS++;
    cntOS++;

    /* break up the text into multiple lines */
    if (dstOS >= (width))
    {




      while (dstOS > (width - 7))
      {
        dstOS -= 1;
        srcOS -= 1;
        cntOS -= 1;

        if (buf[cntOS] == ' ')
        {
          srcOS++;  // skip space
          buf[cntOS] = '\r';
          dstOS++;
          cntOS++;
          buf[cntOS] = '\n';
          dstOS++;
          cntOS++;
          break;
        }
      }

      // couldnt find a space - hyphenate
      if (dstOS == (width - 8))
      {
        dstOS += 7;
        srcOS += 7;
        cntOS += 7;

        buf[cntOS] = '-';
        dstOS++;
        cntOS++;
        buf[cntOS] = '\r';
        dstOS++;
        cntOS++;
        buf[cntOS] = '\n';
        dstOS++;
        cntOS++;

      }


      dstOS = 0;

    }               //end of first if
    buf[cntOS] = '\0';

  }
  /* safely(?) puts the new formatted text back into the
     original buffer given */
  return cmd;
}

/* turn off mccp */
void mccp_off(Descriptor *d)
{
#ifdef HAVE_ZLIB_H
  if (d->comp->state == 2)
  {
    d->comp->state = 3; /* Code to use Z_FINISH for deflate */
  }
#endif /* HAVE_ZLIB_H */
  write_to_descriptor (d->descriptor, " ", d->comp);
  d->comp->state = 0;
#ifdef HAVE_ZLIB_H
  if (d->comp->stream)
  {
    deflateEnd(d->comp->stream);
    free(d->comp->stream);
    free(d->comp->buff_out);
    free(d->comp->buff_in);
  }
#endif /* HAVE_ZLIB_H */
}

/*
* Count number of mxp tags need converting
*    ie. < becomes &lt;
*        > becomes &gt;
*        & becomes &amp;
*/
