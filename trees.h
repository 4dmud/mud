//Forest and Trees
int save_forest(void);
int load_forest(void);
void load_trees(void);
void check_all_trees(void);
void init_trees(int num);
void create_trees(void);
int load_tree(room_rnum room, int v0, int v1, int v2, int v3);
room_rnum find_forest_rand(void);
struct obj_data *make_tree(int v0, int v1, int v2);
void parse_tree_name(struct obj_data *tree);
extern int tree_total;
extern struct forest_data *forest;
extern int forest_room;

//Mineable veins
/*
int save_veins(void);
int load_veins(void);
void load_veins(void);
void check_all_veins(void);
void init_veins(int num);
void create_veins(void);
int load_vein(room_rnum room, int v0, int v1, int v2, int v3);
room_rnum find_vein_rand(void);
struct obj_data *make_vein(int v0, int v1, int v2);
void parse_vein_name(struct obj_data *tree);
extern int vein_total;
extern struct vein_data *vein;
extern int vein_room;
*/

#define MAX_TREE_TYPES 9
#define MAX_VEIN_TYPE 1

