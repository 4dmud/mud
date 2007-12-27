/* ************************************************************************
*   File: comm.h                                        Part of CircleMUD *
*  Usage: header file: prototypes of public communication functions       *
*                                                                         *
*  All rights reserved.  See license.doc for complete information.        *
*                                                                         *
*  Copyright (C) 1993, 94 by the Trustees of the Johns Hopkins University *
*  CircleMUD is based on DikuMUD, Copyright (C) 1990, 1991.               *
************************************************************************ */

#define NUM_RESERVED_DESCS	8

#define COPYOVER_FILE "copyover.dat"
/* comm.c */
size_t new_send_to_char(Character *ch, const char *messg, ...) __attribute__ ((format(printf, 2, 3)));
size_t send_to_fusion(Character *ch, const char *messg, ...) __attribute__ ((format(printf, 2, 3)));
void	send_to_all(const char *messg, ...) __attribute__ ((format (printf, 1, 2)));
void	send_to_arena(const char *messg, ...) __attribute__ ((format (printf, 1, 2)));
void send_to_char(const char *messg, Character *ch);
void	send_to_range(room_vnum start, room_vnum finish, const char *messg, ...) __attribute__ ((format (printf, 3, 4)));
void	send_to_room(room_rnum room, const char *messg, ...) __attribute__ ((format (printf, 2, 3)));
void	send_to_outdoor(const char *messg, ...) __attribute__ ((format (printf, 1, 2)));
void perform_to_all(const char *messg, Character *ch);
void close_socket(Descriptor *d);
string wordwrap(const char *cmd, size_t width, size_t maxlen);

int is_ignoring(Character *ch, Character *vict);

void perform_act(const char *orig, Character *ch,
		 struct obj_data *obj, const void *vict_obj,
		 const Character *to);

void act(const char *str, int hide_invisible, Character *ch, struct obj_data *obj, const void *vict_obj, int type);

void brag(Character *ch, Character *victim);

extern struct comm_data * comlist;

void free_commlist(struct comm_data *c);
void send_out_signals(Descriptor *d);

#define ALERT_1 send_to_all("{cRALERT: 4Dimensions has had an internal error and is shutting down now.\r\n" \
"Please Reconnect in 20 seconds.\r\n{c0");

#define ALERT_2 send_to_all("{cRALERT: 4Dimensions has encountered some instability." \
  "\r\nCrash may follow.\r\n{c0");
  

#define CHECK_NULL(pointer, expression) \
  if ((pointer) == NULL) i = ACTNULL; else i = (expression);


#define TO_ROOM		1
#define TO_VICT		2
#define TO_NOTVICT	3
#define TO_CHAR		4
#define TO_SLEEP	128	/* to char, even if sleeping */
#define DG_NO_TRIG      256	/* don't check act trigger   */

/* I/O functions */
int	write_to_descriptor(socket_t desc, const char *txt, struct compr *comp);
void write_to_q(const char *txt, struct txt_q *queue, int aliased);
size_t write_to_output(Descriptor *d, const char *txt, ...) __attribute__ ((format (printf, 2, 3)));
size_t vwrite_to_output(Descriptor *d, const char *format, va_list args);
void page_string(Descriptor *d, char *str, int keep_internal);
void string_add(Descriptor *d, char *str);
void string_write(Descriptor *d, char **txt, size_t len,
		  long mailto, void *data);
int toggle_compression(Descriptor *t);


#define SEND_TO_SOCKET(messg, desc)	write_to_descriptor((desc), (messg), strlen(messg))

/* values for keep_internal...
   FALSE  Buffer is read only and will not be modifed so it does not
          need to be copied.
   TRUE   Buffer is re-usable and needs to be copied before use. */
#define DYN_BUFFER 2		/* Buffer is dynamically allocated and does not need to be
				   copied, but does need to be freed. */

#define SEND_TO_Q(messg, desc)  write_to_output(desc,"%s", (messg))

#define USING_SMALL(d)	((d)->output == (d)->small_outbuf)
#define USING_LARGE(d)  ((d)->output == (d)->large_outbuf)

typedef RETSIGTYPE sigfunc(int);


