
extern const char *body[];
extern const int wear_order_index[];	/* RG 6/28/98 */
extern const char *vial_types[];
extern const char *newbie_status[];
extern const char *creation_state_types[];
extern const char * dimension_types[];
extern const char *material_names[];
extern const char *fourdimensions_version;
extern const char *circlemud_version;
extern const char *oasisolc_version;
extern const char * profession_names[];
extern const char * profession_group_names[];
extern const char *dirs[];
extern const char *room_bits[];
extern const char *exit_bits[];
extern const char *sector_types[];
extern const char *genders[];
extern const char *position_types[];
extern const char *player_bits[];
extern const char *action_bits[];
extern const char *preference_bits[];
extern const char *affected_bits[];
extern const char *connected_types[];
extern const char *where[];
const char *disp_where(int pos, Character *ch);
extern const char *equipment_types[];
extern const char *item_types[];
extern const char *wear_bits[];
extern const char *extra_bits[];
extern const char *apply_types[];
extern const char *container_bits[];
extern const char *drinks[];
extern const char *drinknames[];
extern const char *colour_liquid[];
extern const char *fullness[];
extern const char *spell_wear_off_msg[];
extern const char *skill_wear_off_msg[];
extern const char *npc_class_types[];
extern const char *weekdays[];
extern const char *month_name[];
extern const char *rp_group_names[];
extern const char *pc_class_types[];
extern const char * fusion_locations[];
extern const char *weapon_type_names[];
extern const  char *random_desc[];
extern const char *age_desc_staff[];
extern const char *age_desc_tree[];
extern const char *tree_names[];
extern const char *potion_names[];
extern const char *stance_change[];
extern const char *fly_stance_change[];
extern const char *zone_bits[];
extern const char *elemental_types[];
extern const char *tunnel_msgs[];

extern const char *trig_types[];
extern const char *otrig_types[];
extern const char *wtrig_types[];
#if defined(CONFIG_OASIS_MPROG)
extern const char *mobprog_types[];
#endif

extern const struct str_app_type str_app[];
extern const struct dex_skill_type dex_app_skill[];
extern const struct dex_app_type dex_app[];
extern const struct con_app_type con_app[];
extern const struct int_app_type int_app[];
extern const struct wis_app_type wis_app[];
extern const int rev_dir[];
extern const int movement_loss[];
extern const int drink_aff[][3];
extern const char *magic_types[];
extern const char *cast_types[];
extern const char *target_types[];

extern const size_t room_bits_count;
extern const size_t action_bits_count;
extern const size_t affected_bits_count;
extern const size_t extra_bits_count;
extern const size_t wear_bits_count;

extern const struct class_name_data class_name[];
extern const char *AssemblyTypes[];
extern const char *unused_spellname;
extern const char *unused_spellmessage;

extern const char *colour_names[];
extern const char *quality_names[];
extern const char *origin_names[];
