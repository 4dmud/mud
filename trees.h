int save_forest(void);
int load_forest(void);
void load_trees(void);
void init_trees(int num);
void create_trees(void);
int load_tree(room_rnum room, int v0, int v1, int v2, int v3);
room_rnum find_forest_rand(void);
extern struct obj_data *make_tree(void);
void parse_tree_name(struct obj_data *tree);
extern int tree_total;
extern struct forest_data *forest;
extern int forest_room;
#define MAX_TREE_TYPES 9
