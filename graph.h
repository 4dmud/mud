//
// C++ Interface: graph
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2007
//
// Copyright: See COPYING file that comes with this distribution
//
//
/* Utility macros */
#define MARK(room) 	(SET_BIT_AR(ROOM_FLAGS(room), ROOM_BFS_MARK))
#define UNMARK(room) 	(REMOVE_BIT_AR(ROOM_FLAGS(room), ROOM_BFS_MARK))
#define IS_MARKED(room) (ROOM_FLAGGED(room, ROOM_BFS_MARK))
#define IS_CLOSED(x, y) (EXIT_FLAGGED((x)->dir_option[(y)], EX_CLOSED))

class Graph {
	public:
		
		/* Local functions */
		int VALID_EDGE(room_rnum x, int y,bool honour_notrack);
		void bfs_enqueue(room_rnum room, int dir);
		void bfs_dequeue(void);
		void bfs_clear_queue(void);
		int find_first_step(room_rnum src, room_rnum target,bool honour_notrack=false);
		
		Graph() {
			queue_head = NULL;
			queue_tail = NULL;
			
		}
		~Graph() {
			bfs_clear_queue();
		}
		
	private:
vector <room_vnum> tracked;

struct bfs_queue_struct {
	room_rnum room;
	char dir;
	bfs_queue_struct *next;
	
	bfs_queue_struct(room_rnum r, char d) {
		room = r;
		dir = d;
		next = NULL;
	}
};
//std::list <bfs_queue_struct *> bfs_queue;
//std::list <bfs_queue_struct *>::iterator qit;
struct bfs_queue_struct *queue_head;
struct bfs_queue_struct *queue_tail;
};

extern Graph graph;
