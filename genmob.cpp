/************************************************************************
 * Generic OLC Library - Mobiles / genmob.c			v1.0	*
 * Copyright 1996 by Harvey Gilpin					*
 * Copyright 1997-2001 by George Greer (greerga@circlemud.org)		*
 ************************************************************************/

#include "config.h"
#include "sysdep.h"

#include "structs.h"
#include "utils.h"
#include "db.h"
#include "shop.h"
#include "handler.h"
#include "genolc.h"
#include "genmob.h"
#include "genzon.h"
#include "dg_olc.h"
#include "htree.h"
#include "spells.h"
#include "name.map.h"

int update_mobile_strings ( Character *t, Character *f );
void check_mobile_strings ( Character *mob );
void check_mobile_string ( mob_vnum i, char **str, const char *dscr );
int write_mobile_record ( mob_vnum mvnum, Character *mob, FILE *fd );
int write_mobile_espec ( mob_vnum mvnum, Character *mob, FILE *fd );
int write_mobile_links ( mob_vnum mvnum, Character *mob, FILE *fd );
int write_mobile_trains ( mob_vnum mvnum, Character *mob, FILE *fd );
int free_mobile_strings ( Character *mob );
int copy_mobile_strings ( Character *t, Character *f );
#if CONFIG_GENOLC_MOBPROG
int write_mobile_mobprog ( mob_vnum mvnum, Character *mob, FILE *fd );
#endif

float m_powf ( float x, float y );
struct mob_stat_table mob_stats[MAX_MOB_LEVELS];

void assign_mob_stats ( void )
{
    int s[11] =     {0, 100, 1, 8, 0, 1, 1, 0, 100, 0, 0};

    int i;

    for ( i = 1; i < MAX_MOB_LEVELS; i++ )
    {

        s[0] = i;
        /* ac needs to drop from 100 at level <1 to -100 at level > 50  */
        s[1] = ( ( i > 50 ) ? -100 : ( 100 - ( ( i< 1?1:i ) *4 ) ) );
        //Hp
        s[2] = FTOI ( i * 1.5 );
        s[3] = FTOI ( i * 1.5 );
        s[4] = MAX ( 40, ( i * i * i ) /35 ) + 150;
        //Damroll
        s[5] = FTOI ( i * 0.5 );
        s[6] = i;
        s[7] = FTOI ( m_powf ( i, ( i*0.0026 ) +1.34 ) );//(i >= 10) ? (i * i * i * (0.0036 + (i>=60?0.01:0.0))) : (i);

        s[8]  = FTOI ( ( i * i * i * 15 ) + 200 ); /* exp */
        s[9]  = FTOI ( ( i * i * i * 1.4 ) ); /* gold */
        s[10] = FTOI ( i * ( 1.0 + ( i * 0.035 ) ) ); /* hitroll */

        /*-------put it back together--------*/
        mob_stats[i].level 		= s[0];
        mob_stats[i].ac 		= s[1];
        mob_stats[i].hp_dice 	= s[2];
        mob_stats[i].hp_sides 	= s[3];
        mob_stats[i].hp_bonus 	= s[4];
        mob_stats[i].dam_dice 	= s[5];
        mob_stats[i].dam_sides 	= s[6];
        mob_stats[i].dam_bonus 	= s[7];
        mob_stats[i].exp 		= FTOI ( s[8] + ( s[8] * 0.05 ) ); /*added 5%*/
        mob_stats[i].gold 		= s[9];
        mob_stats[i].hitroll 	= s[10];
        /*log("LEV:%-2d -- HP:%-6d Avg Dam:%-4d X:%-7d G:%-d",
        s[0],(int)((s[2] * s[3]) * 0.5) + s[3] + s[4],(int)((s[5] * s[6]) * 0.5) + s[6] + s[7],s[8], s[9]);*/


    }
}

float dam_avg ( int from, int to, int lev )
{
    float ret_val = 0.0;
    return ret_val;
}

void smash_tilde ( char *str );

mob_vnum add_mobile ( Character *mob, mob_vnum vnum )
{
    int found = FALSE;
    Character *live_mob, *pmob = NULL, *nmob;
    if ( vnum != NOBODY && MobProtoExists ( vnum ) )
        pmob = GetMobProto ( vnum );
    if ( pmob != NULL )
    {
        /* Free the proto script */
        free_proto_script ( pmob, MOB_TRIGGER );

        /* Copy over the mobile and free() the old strings. */
        copy_mobile ( pmob, mob );

        /* Now re-point all existing mobile strings to here. */
        for ( live_mob = character_list; live_mob; live_mob = live_mob->next )
            if ( vnum == live_mob->vnum )
                update_mobile_strings ( live_mob, pmob );

        add_to_save_list ( zone_table[real_zone_by_thing ( vnum ) ].number, SL_MOB );
        log ( "GenOLC: add_mobile: Updated existing mobile #%d.", vnum );
        return vnum;
    }
    nmob = new Character();
    copy_mobile ( nmob, mob );
    nmob->vnum = vnum;

    //  RECREATE(mob_proto, Character, top_of_mobt + 2);
    //    RECREATE(mob_index, struct index_data, top_of_mobt + 2);


    //top_of_mobt = mob_proto.size();

    //  for (i = top_of_mobt; i > 0; i--) {
    //      if (vnum > mob_index.at(i - 1).vnum) {
    //         found = i;
    //         break;
    //     }
    // }

    SetMobIndex ( vnum, new index_data ( vnum ) );
    SetMobProto ( vnum, nmob );
    //it = mob_proto.insert(mob_proto.begin() + found, mob);
    //mob_index.insert(mob_index.begin() + found, imob);
    //(*it)->nr = found;

    //htree_add(mob_htree, mob_index[found].vnum, found);

    log ( "GenOLC: add_mobile: Added mobile %d at index #%d.", vnum, found );

#if CONFIG_GENOLC_MOBPROG

    GET_MPROG ( OLC_MOB ( d ) ) = OLC_MPROGL ( d );
    GET_MPROG_TYPE ( OLC_MOB ( d ) ) = ( OLC_MPROGL ( d ) ? OLC_MPROGL ( d )->type : 0 );
    while ( OLC_MPROGL ( d ) )
    {
        GET_MPROG_TYPE ( OLC_MOB ( d ) ) |= OLC_MPROGL ( d )->type;
        OLC_MPROGL ( d ) = OLC_MPROGL ( d )->next;
    }
#endif

    /*
     * Update live mobile rnums.
     */
    //for (live_mob = character_list; live_mob; live_mob = live_mob->next)
    //    GET_MOB_RNUM(live_mob) += (GET_MOB_RNUM(live_mob) >= found);

    /*
     * Update zone table.
     */
    //  for (zone = 0; zone <= top_of_zone_table; zone++)
    //      for (cmd_no = 0; ZCMD(zone, cmd_no).command != 'S'; cmd_no++)
    //         if (ZCMD(zone, cmd_no).command == 'M')
    //             ZCMD(zone, cmd_no).arg1 += (ZCMD(zone, cmd_no).arg1 >= found);

    /*
     * Update shop keepers.
     */
    //  if (shop_index)
    //      for (shop = 0; shop <= top_shop - top_shop_offset; shop++)
    //         SHOP_KEEPER(shop) += (SHOP_KEEPER(shop) >= found);

    add_to_save_list ( zone_table[real_zone_by_thing ( vnum ) ].number, SL_MOB );
    return found;
}

int copy_mobile ( Character *to, Character *from )
{
    to->free_char_strings();
    *to = *from;
    check_mobile_strings ( from );
    copy_mobile_strings ( to, from );
    return TRUE;
}

void extract_mobile_all ( mob_vnum vnum )
{
    Character *next, *ch;

    for ( ch = character_list; ch; ch = next )
    {
        next = ch->next;
        if ( GET_MOB_VNUM ( ch ) == vnum )
            extract_char ( ch );
    }
}

int delete_mobile ( mob_vnum refpt )
{

    if ( !MobProtoExists ( refpt ) )
    {
        log ( "SYSERR: GenOLC: delete_mobile: Invalid vnum %d.", refpt );
        return NOBODY;
    }

    add_to_save_list ( zone_table[real_zone_by_thing ( refpt ) ].number, SL_MOB );
    extract_mobile_all ( refpt );
    //  free_mobile_strings(&mob_proto[refpt]);
    DeleteMobProto ( refpt );
    DeleteMobIndex ( refpt );

    // for (counter = refpt; counter < top_of_mobt; counter++) {
    //    mob_index[counter] = mob_index[counter + 1];
    //    mob_proto[counter]->nr--;
    // }

    //top_of_mobt = mob_proto.size()+1;
//    RECREATE(mob_index, struct index_data, top_of_mobt + 1);
    //RECREATE(mob_proto, Character, top_of_mobt + 1);


    /*
     * Update live mobile rnums.
     */
    // for (live_mob = character_list; live_mob; live_mob = live_mob->next)
    //     GET_MOB_RNUM(live_mob) -= (GET_MOB_RNUM(live_mob) >= refpt);

    /*
     * Update zone table.
     */
    // for (zone = 0; zone <= top_of_zone_table; zone++)
    //     for (cmd_no = 0; ZCMD(zone, cmd_no).command != 'S'; cmd_no++)
    //        if (ZCMD(zone, cmd_no).command == 'M')
    //            ZCMD(zone, cmd_no).arg1 -= (ZCMD(zone, cmd_no).arg1 >= refpt);

    /*
     * Update shop keepers.
     */
    // if (shop_index)
    //      for (counter = 0; counter <= top_shop - top_shop_offset; counter++)
    //          SHOP_KEEPER(counter) -= (SHOP_KEEPER(counter) >= refpt);

    return refpt;
}

int copy_mobile_strings ( Character *t, Character *f )
{
    if ( f->player.name )
        t->player.name = strdup ( f->player.name );
    if ( f->player.title )
        t->player.title = strdup ( f->player.title );
    if ( f->player.short_descr )
        t->player.short_descr = strdup ( f->player.short_descr );
    if ( f->player.long_descr )
        t->player.long_descr = strdup ( f->player.long_descr );
    if ( f->player.description )
        t->player.description = strdup ( f->player.description );
    if ( f->player.custom_arrive )
            t->player.custom_arrive = strdup ( f->player.custom_arrive );
    else
            t->player.custom_arrive = NULL;
    if ( f->player.custom_leave )
            t->player.custom_leave = strdup ( f->player.custom_leave );
    else
            t->player.custom_leave = NULL;


    return TRUE;
}

int update_mobile_strings ( Character *t, Character *f )
{
    if ( f->player.name )
        t->player.name = f->player.name;
    if ( f->player.title )
        t->player.title = f->player.title;
    if ( f->player.short_descr )
        t->player.short_descr = f->player.short_descr;
    if ( f->player.long_descr )
        t->player.long_descr = f->player.long_descr;
    if ( f->player.description )
        t->player.description = f->player.description;
    if ( f->player.custom_arrive )
            t->player.custom_arrive = f->player.custom_arrive;
    else
            t->player.custom_arrive = NULL;
    if ( f->player.custom_leave )
            t->player.custom_leave = f->player.custom_leave;
    else
            t->player.custom_leave = NULL;

    return TRUE;
}

/*
 * Free a mobile structure that has been edited.
 * Take care of existing mobiles and their mob_proto!
 */
int free_mobile ( Character *mob )
{
//Character *pmob;
    if ( mob == NULL )
        return FALSE;
    /*
     * Non-prototyped mobile.  Also known as new mobiles.
     */
    if ( !MobProtoExists ( mob->vnum ) )   /** if they are the same, it is a proto! **/
    {
        /* free script proto list */
        mob->free_proto_mob();

        /* free any assigned scripts */
        if ( SCRIPT ( mob ) )
            extract_script ( mob, MOB_TRIGGER );

    }
    else  	/* Prototyped mobile. */
    {
        /* free script proto list if it's not the prototype */
//        if (mob->proto_script && mob->proto_script != GetMobProto(mob->vnum)->proto_script)
//            free_proto_script(mob, MOB_TRIGGER);
        mob->remove_all_affects();
        mob->free_non_proto_strings();
        mobNames.remNamelist(GET_ID(mob));

        /* free any assigned scripts */
        if ( SCRIPT ( mob ) )
            extract_script ( mob, MOB_TRIGGER );

    }

    delete mob;
    return TRUE;
}

int save_mobiles ( zone_rnum rznum )
{
    zone_vnum vznum;
    FILE *mobfd;
    room_vnum i;
    int written;
    char mobfname[64], usedfname[64];

#if CIRCLE_UNSIGNED_INDEX

    if ( rznum == NOWHERE || rznum > top_of_zone_table )
    {
#else
    if ( rznum < 0 || rznum > top_of_zone_table )
    {
#endif
        log ( "SYSERR: GenOLC: save_mobiles: Invalid real zone number %d. (0-%d)",
              rznum, top_of_zone_table );
        return FALSE;
    }

    vznum = zone_table[rznum].number;
    snprintf ( mobfname, sizeof ( mobfname ), "%s/%d/%d.new", LIB_WORLD, vznum, vznum );
    if ( ( mobfd = fopen ( mobfname, "w" ) ) == NULL )
    {
        new_mudlog ( BRF, LVL_GOD, TRUE, "SYSERR: GenOLC: Cannot open mob file for writing." );
        return FALSE;
    }

    for ( i = zone_table[rznum].Bot(); i <= zone_table[rznum].top; i++ )
    {
        if ( !MobProtoExists ( i ) )
            continue;
        check_mobile_strings ( GetMobProto ( i ) );
        if ( write_mobile_record ( i, GetMobProto ( i ), mobfd ) < 0 )
            log ( "SYSERR: GenOLC: Error writing mobile #%d.", i );
    }
    fputs ( "$\n", mobfd );
    written = ftell ( mobfd );
    fclose ( mobfd );
    snprintf ( usedfname, sizeof ( usedfname ), "%s/%d/%d.mob", LIB_WORLD, vznum, vznum );
    remove ( usedfname );
    rename ( mobfname, usedfname );

    if ( in_save_list ( vznum, SL_MOB ) )
        remove_from_save_list ( vznum, SL_MOB );
    log ( "GenOLC: '%s' saved, %d bytes written.", usedfname, written );
    return written;
}

#if CONFIG_GENOLC_MOBPROG
int write_mobile_mobprog ( mob_vnum mvnum, Character *mob, FILE *fd )
{
    char wmmarg[MAX_STRING_LENGTH], wmmcom[MAX_STRING_LENGTH];
    MPROG_DATA *mob_prog;

    for ( mob_prog = GET_MPROG ( mob ); mob_prog; mob_prog = mob_prog->next )
    {
        wmmarg[MAX_STRING_LENGTH - 1] = '\0';
        wmmcom[MAX_STRING_LENGTH - 1] = '\0';
        strip_cr ( strncpy ( wmmarg, mob_prog->arglist, MAX_STRING_LENGTH - 1 ) );
        strip_cr ( strncpy ( wmmcom, mob_prog->comlist, MAX_STRING_LENGTH - 1 ) );
        fprintf ( fd,	"%s %s~\n"
                  "%s%c\n",
                  medit_get_mprog_type ( mob_prog ), wmmarg,
                  wmmcom, STRING_TERMINATOR
                );
        if ( mob_prog->next == NULL )
            fputs ( "|\n", fd );
    }
    return TRUE;
}
#endif
int write_mobile_links ( mob_vnum mvnum, Character *mob, FILE *fd )
{
    struct combine_data *temp = NULL;
    if ( ( temp = mob->mob_specials.join_list ) != NULL )
    {

        /* linked mob */
        while ( temp )
        {
            fprintf ( fd, "J %d\n", temp->vnum );
            temp = temp->next;
        }
    }
    return TRUE;
}

int write_mobile_trains ( mob_vnum mvnum, Character *mob, FILE *fd )
{
    fputs("H\n", fd);
    for (int i = 0; i < mob->mob_specials.teaches_skills.size();i++)
    {
        fprintf(fd, "%s\n", skill_name(mob->mob_specials.teaches_skills[i]) );
    }
    fputs("H*END\n", fd);
    return TRUE;
}

int write_mobile_espec ( mob_vnum mvnum, Character *mob, FILE *fd )
{
    if ( GET_ATTACK ( mob ) != 0 )
        fprintf ( fd, "BareHandAttack: %d\n", GET_ATTACK ( mob ) );
    if ( GET_STR ( mob ) != 11 )
        fprintf ( fd, "Str: %d\n", GET_STR ( mob ) );
    if ( GET_ADD ( mob ) != 0 )
        fprintf ( fd, "StrAdd: %d\n", GET_ADD ( mob ) );
    if ( GET_DEX ( mob ) != 11 )
        fprintf ( fd, "Dex: %d\n", GET_DEX ( mob ) );
    if ( GET_INT ( mob ) != 11 )
        fprintf ( fd, "Int: %d\n", GET_INT ( mob ) );
    if ( GET_WIS ( mob ) != 11 )
        fprintf ( fd, "Wis: %d\n", GET_WIS ( mob ) );
    if ( GET_CON ( mob ) != 11 )
        fprintf ( fd, "Con: %d\n", GET_CON ( mob ) );
    if ( GET_CHA ( mob ) != 11 )
        fprintf ( fd, "Cha: %d\n", GET_CHA ( mob ) );
    if ( GET_CLASS ( mob ) != 0 )
        fprintf ( fd, "Class: %d\n", GET_CLASS ( mob ) );
    if ( MOB_TIER ( mob ) != 0 )
        fprintf ( fd, "Tier: %d\n", MOB_TIER ( mob ) );
    if ( MOB_SUBSKILL ( mob ) != SUB_UNDEFINED )
        fprintf ( fd, "Subskill: %d\n", MOB_SUBSKILL ( mob ) );
    if ( MOB_SKIN ( mob ) != -1 )
        fprintf ( fd, "Skin: %d\n", MOB_SKIN ( mob ) );
    if ( MOB_OWNER ( mob ) != -1 )
        fprintf ( fd, "Owner: %ld\n", MOB_OWNER ( mob ) );
    if ( GET_CUSTOM_ARRIVE_MSG (mob))
        fprintf ( fd, "Arrive: %s\n", GET_CUSTOM_ARRIVE_MSG ( mob ) );
    if ( GET_CUSTOM_LEAVE_MSG (mob))
        fprintf ( fd, "Leave: %s\n", GET_CUSTOM_LEAVE_MSG ( mob ) );
    fputs ( "E\n", fd );
    return TRUE;
}


int write_mobile_record ( mob_vnum mvnum, Character *mob, FILE *fd )
{


    char ldesc[MAX_STRING_LENGTH];
    char ddesc[MAX_STRING_LENGTH];

    ldesc[MAX_STRING_LENGTH - 1] = '\0';
    ddesc[MAX_STRING_LENGTH - 1] = '\0';
    check_mobile_strings ( mob );
    if ( GET_LDESC ( mob ) )
        strip_cr ( strncpy ( ldesc, GET_LDESC ( mob ), MAX_STRING_LENGTH - 1 ) );
    if ( GET_DDESC ( mob ) )
        strip_cr ( strncpy ( ddesc, GET_DDESC ( mob ), MAX_STRING_LENGTH - 1 ) );

    fprintf ( fd,	"#%d\n"
              "%s%c\n"
              "%s%c\n"
              "%s%c\n"
              "%s%c\n",
              mvnum,
              GET_ALIAS ( mob ), STRING_TERMINATOR,
              GET_SDESC ( mob ), STRING_TERMINATOR,
              ldesc, STRING_TERMINATOR,
              ddesc, STRING_TERMINATOR
            );



    fprintf ( fd,	"%d %d %d %d %d %d %d %d %d E\n"
              "%d %d %d %dd%d+%d %dd%d+%d\n",
              MOB_FLAGS ( mob ) [0], MOB_FLAGS ( mob ) [1],
              MOB_FLAGS ( mob ) [2], MOB_FLAGS ( mob ) [3],
              AFF_FLAGS ( mob ) [0], AFF_FLAGS ( mob ) [1],
              AFF_FLAGS ( mob ) [2], AFF_FLAGS ( mob ) [3], GET_ALIGNMENT ( mob ),

              GET_LEVEL ( mob ), 20 - GET_HITROLL ( mob ), GET_AC ( mob ) / 10, GET_HIT ( mob ),
              GET_MANA ( mob ), GET_MOVE ( mob ), GET_NDD ( mob ), GET_SDD ( mob ),
              GET_DAMROLL ( mob )
            );
    fprintf ( fd, 	"%lld %lld\n"
              "%d %d %d %d\n",
              GET_GOLD ( mob ), GET_EXP ( mob ),
              GET_POS ( mob ), GET_DEFAULT_POS ( mob ), GET_SEX ( mob ), GET_MRACE ( mob )
            );



    if ( write_mobile_espec ( mvnum, mob, fd ) < 0 )
        log ( "SYSERR: GenOLC: Error writing E-specs for mobile #%d.", mvnum );

    if ( write_mobile_links ( mvnum, mob, fd ) < 0 )
        log ( "SYSERR: GenOLC: Error writing links for mobile #%d.", mvnum );

    if ( write_mobile_trains ( mvnum, mob, fd ) < 0 )
        log ( "SYSERR: GenOLC: Error writing training for mobile #%d.", mvnum );

    script_save_to_disk ( fd, mob, MOB_TRIGGER );

#if CONFIG_GENOLC_MOBPROG

    if ( write_mobile_mobprog ( mvnum, mob, fd ) < 0 )
        log ( "SYSERR: GenOLC: Error writing MobProgs for mobile #%d.", mvnum );
#endif

    return TRUE;
}

void check_mobile_strings ( Character *mob )
{
    check_mobile_string ( mob->vnum, &GET_LDESC ( mob ), "long description" );
    check_mobile_string ( mob->vnum, &GET_DDESC ( mob ), "detailed description" );
    check_mobile_string ( mob->vnum, &GET_ALIAS ( mob ), "alias list" );
    check_mobile_string ( mob->vnum, &GET_SDESC ( mob ), "short description" );
}

void check_mobile_string ( mob_vnum i, char **str, const char *dscr )
{
    if ( !str )
    {
        new_mudlog ( BRF, LVL_GOD, TRUE, "GenOLC: Mob #%d has an invalid %s.", i, dscr );
        *str = strdup ( "An undefined string." );
    }
}



