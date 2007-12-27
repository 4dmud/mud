//
// C++ Interface: descriptor
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
class Descriptor {
public:
  socket_t descriptor; /* file descriptor for socket           */
  char host[HOST_LENGTH + 1];    /* hostname                             */
  byte bad_pws;        /* number of bad pw attemps this login  */
  byte idle_tics;      /* tics idle at password prompt         */
  int connected;       /* mode of 'connectedness'              */
  int orig_connected;
  int sub_state;
  int desc_num;        /* unique num assigned to desc          */
  time_t login_time;        /* when the person connected            */
  char *showstr_head;       /* for keeping track of an internal str */
  char **showstr_vector;    /* for paging through texts             */
  int showstr_count;        /* number of pages to page through      */
  int showstr_page;         /* which page are we currently showing? */
  char **str;               /* for the modify-str system            */
  char pagebuf[MAX_STRING_LENGTH];    /* personal buffer space                */
  char *backstr;       /* backup string for modify-str system  */
  size_t max_str;         /* maximum size of string in modify-str   */
  long mail_to;        /* name for mail system                 */
  int has_prompt;      /* is the user at a prompt?             */
  int close_me;
  int wait;
  char inbuf[MAX_RAW_INPUT_LENGTH];   /* buffer for raw input           */
  char last_input[MAX_INPUT_LENGTH];  /* the last input                 */
  char small_outbuf[SMALL_BUFSIZE];   /* standard output buffer         */
  char **history;      /* History of commands, for ! mostly.   */
  int history_pos;          /* Circular array position.             */
  unsigned int bufptr;           /* ptr to end of current output         */
  unsigned int bufspace;         /* space left in the output buffer      */
  struct txt_block *large_outbuf;     /* ptr to large buffer, if we need it */
  struct txt_q input;       /* q of unprocessed input               */
  Character *character;     /* linked to char                       */
  Character *original; /* original char if switched            */
  Descriptor *snooping;   /* Who is this char snooping       */
  Descriptor *snoop_by;   /* And who is snooping this char   */
  Descriptor *next;  /* link to next descriptor             */
  struct oasis_olc_data *olc;   /* OLC info                            */
  struct account_data *acc;
  C_FUNC(*callback);        // Call back function for anything I want to do
  int callback_depth;       // Call back "psuedo-recursiveness" depth
  void *c_data;        // Storage for the Callback function
  int  options;         /* descriptor flags               */
  struct compr *comp;                /* compression info */
  int eor; /* End Of Record - for prompts - telnet proticol - mord*/
  int mxp;
  int telnet_capable; /* if any of the protocols are processed then set this flag */
  bool locked; /* Is this descriptor in a locked state? Multithreading - mord*/
  struct sockaddr_in saddr;
  char  host_ip[HOST_LENGTH + 1];
  char  host_name[HOST_LENGTH + 1];
  char  user_name[HOST_LENGTH + 1];
  void init_descriptor(int desc);
  Descriptor *new_descriptor(socket_t s, int copyover);
  Descriptor();
  ~Descriptor();
  size_t Output(const char *txt, ...) __attribute__ ((format (printf, 2, 3)));
  size_t Output(string *txt, ...);
  size_t vwrite_to_output(const char *format, va_list args);
  string convert_mxp_tags (const int bMXP, string src);
  int count_mxp_tags (const int bMXP, const char *txt, int length);
  void turn_on_mxp ();
  char * send_mxp_status();
  bool pending_output();
  
/*private:*/
  string output;
};
