//
// C Implementation: character
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2004
//
// Copyright: See COPYING file that comes with this distribution
//
//

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
    #include "descriptor.h"
    #include "spells.h"
    #include "regen.h"

    #ifdef HAVE_ARPA_TELNET_H
    #include <arpa/telnet.h>
    #else
    #include "telnet.h"
    #endif


    void free_join_list(struct combine_data *list);
void free_killlist(Character *ch);
long get_ptable_by_name(const char *name);

void free_note(NOTE_DATA *note, int type);
void free_mob_memory(memory_rec *k);
void free_followers(struct follow_type *k);

void damage_count_free(Character *vict);
void extract_all_in_list(OBJ_DATA *obj);
void free_alias(struct alias_data *a);
void affect_modify_ar(Character *ch, byte loc, sbyte mod, int bitv[], bool add
                         );
int find_eq_pos(Character *ch, struct obj_data *obj, char *arg);
extern long top_idnum;

int find_first_step(room_rnum src, room_rnum target,bool honour_notrack=false);

/* ================== Structure for player/non-player ===================== */



size_t Character::Send(const char *messg, ...) {
    if (this && this->desc && messg && *messg) {
        size_t left;
        va_list args;

        va_start(args, messg);
        left = this->desc->vwrite_to_output(messg, args);
        va_end(args);
        return left;
    }
    return 0;
}

size_t Character::Send(string &i) {
    if (this && this->desc)
        return this->desc->Output(i);
    else
        return 0;
}
size_t Character::Send(string *i) {
    if (this && this->desc)
        return this->desc->Output(i);
    else
        return 0;
}

size_t Character::Send(stringstream &i) {
    if (this && this->desc)
        return this->desc->Output(i);
    else
        return 0;
}

void Character::send_char_pos(int dam) {
    switch (GET_POS(this)) {
    case POS_MORTALLYW:
        act("$n is mortally wounded, and will die soon, if not aided.",
            TRUE, this, 0, 0, TO_ROOM);
        this->Send( "You are mortally wounded, and will die soon, if not aided.\r\n");
        break;
    case POS_INCAP:
        act("$n is incapacitated and will slowly die, if not aided.", TRUE,
            this, 0, 0, TO_ROOM);
        this->Send( "You are incapacitated and will slowly die, if not aided.\r\n");
        break;
    case POS_STUNNED:
        act("$n is stunned, but will probably regain consciousness again.",
            TRUE, this, 0, 0, TO_ROOM);
        this->Send( "You're stunned, but will probably regain consciousness again.\r\n");
        break;
    case POS_DEAD:
        act("$n is dead!  R.I.P.", FALSE, this, 0, 0, TO_ROOM);
        this->Send( "You are dead!  Sorry...\r\n");
        break;
    default:             /* >= POSITION SLEEPING */
        if (dam > (GET_MAX_HIT(this) >> 2))
            act("That really did HURT!", FALSE, this, 0, 0, TO_CHAR);
        if (GET_HIT(this) < (GET_MAX_HIT(this) >> 2))
            this->Send("%sYou wish that your wounds would stop BLEEDING so much!%s\r\n",
                       CCRED(this, C_SPR), CCNRM(this, C_SPR));
    }
}


/* ch = player to change the gold values of
   amount = the amount to add to their gold, or subtract
   type = for a transaction type, taxed untaxted.. or whatever.
   
*/
gold_int Character::Gold(gold_int amount, int type) {
    gold_int temp = 0;

    switch (type) {
    case GOLD_HAND:
        points.gold += amount;
        return points.gold;
        break;
    case GOLD_BANK:
        points.bank_gold += amount;
        return points.bank_gold;
        break;
    case GOLD_ALL:
        if (amount < 0) {
            temp = (abs(amount));
            if (temp <= (points.gold + points.bank_gold)) { // No Transaction. This needs watching
                if (temp > points.gold) {
                    temp -= points.gold;
                    points.gold = 0;
                    points.bank_gold -= temp;
                } else
                    points.gold -= temp;

            } else //positive amount -- add to bank
                points.bank_gold += amount;
        } else
            points.bank_gold += amount;

        return (points.gold + points.bank_gold);
        break;
    default:
        new_mudlog(CMP, LVL_GOD, TRUE, "%s invalid transaction type #%d (amount: %lld)", GET_NAME(this), type, amount);
        break;
        return 0;
    }
    return 0;
}

Character::Character(bool is_mob) {
    if (!is_mob)
        player_specials = new player_special_data();
    else
        player_specials = &dummy_mob;
    default_char();
    if (!is_mob) {
        GET_BRASS_TOKEN_COUNT(this) = 0;
        GET_BRONZE_TOKEN_COUNT(this) = 0;
        GET_SILVER_TOKEN_COUNT(this) = 0;
        GET_GOLD_TOKEN_COUNT(this) = 0;
        GET_COOLNESS(this) 		= 0;
        GET_PK_CNT(this) 		= 0;
        GET_PK_RIP(this)		= 0;
        GET_PK_POINTS(this) 		= 0;
    }
    init_char_strings();
    clear();
}

Character::~Character() {
    freeself();
}

/* release memory allocated for a char struct */
void Character::freeself() {
    int i;
    struct alias_data *a;

    if (this == NULL)
        return;

    /* fix seated players */
    char_from_chair(this);

    while (affected)
        affect_remove(affected);
    affected = NULL;

    for (i = 0; i < 4; i++)
        if (GET_POINTS_EVENT(this, i)) {
            event_cancel(GET_POINTS_EVENT(this, i));
            GET_POINTS_EVENT(this, i) = NULL;
        }
    /* cancel message updates */
    if (GET_MESSAGE_EVENT(this)) {
        event_cancel(GET_MESSAGE_EVENT(this));
        GET_MESSAGE_EVENT(this) = NULL;
    }
    if (GET_FIGHT_EVENT(this)) {
        event_cancel(GET_FIGHT_EVENT(this));
        GET_FIGHT_EVENT(this) = NULL;
    }

    free_travel_points(TRAVEL_LIST(this));
    TRAVEL_LIST(this) = NULL;
    free_mob_memory(MEMORY(this));
    MEMORY(this) = NULL;
    free_followers(followers);
    followers = NULL;
    free_note(pnote, -1);

    GET_IGNORELIST(this).clear();
    //free_ignore(SPECIALS(this)->ignorelist);

    if (player_specials != NULL && player_specials != &dummy_mob) {
        while ((a = GET_ALIASES(this)) != NULL) {
            GET_ALIASES(this) = (GET_ALIASES(this))->next;
            free_alias(a);
        }
        free_non_proto_strings();
        free_string(&BPROMPT(this));
        free_string(&PROMPT(this));
        free_string(&player_specials->poofin);
        free_string(&player_specials->poofout);
        free_string(&player_specials->afk_msg);
        free_string(&player_specials->busy_msg);
        free_string(&player_specials->host);
        free_string(&player_specials->pretitle);

        free_string(&GET_LOGOUTMSG(this));
        free_string(&GET_LOGINMSG(this));

        free_string(&GET_EMAIL(this));
        free_string(&IMMTITLE(this));

        extract_all_in_list(LOCKER(this));
        LOCKER(this) = NULL;


        if (IS_NPC(this))
            log("SYSERR: Mob %s (#%d) had player_specials allocated!", GET_NAME(this), GET_MOB_VNUM(this));

        delete player_specials;
    }
    if (!IS_NPC(this) || proto || !MobProtoExists(vnum)) {

        /* free script proto list */
        free_proto_script(this, MOB_TRIGGER);

        //        free_join_list(mob_specials.join_list);
        mob_specials.join_list = NULL;
        free_char_strings();
    } else if (!proto) {
        /* otherwise, free strings only if the string is not pointing at proto */

        /* free script proto list if it's not the prototype */
        if (proto_script && proto_script != GetMobProto(vnum)->proto_script)
            free_proto_script(this, MOB_TRIGGER);

        if (mob_specials.join_list && mob_specials.join_list != GetMobProto(vnum)->mob_specials.join_list)
            free_join_list(mob_specials.join_list);
        mob_specials.join_list = NULL;

        damage_count_free(this);
    }

    /* free any assigned scripts */
    if (SCRIPT(this))
        extract_script(this, MOB_TRIGGER);

    if (send_string != NULL)
        delete send_string;


    if (desc)
        desc->character = NULL;
    /* find_char helper */
    if (GET_ID(this) > 0)
        removeFromChLookupTable(GET_ID(this));
}

/* clear ALL the working variables of a char; do NOT free any space alloc'ed */
void Character::clear() {
    IN_ROOM(this) = NULL;
    TRAVEL_LIST(this) = NULL;
    GET_PFILEPOS(this) = -1;
    GET_IDNUM(this) = -1;
    FIGHTING(this) = NULL;
    GET_NEXT_SKILL(this) = TYPE_UNDEFINED;
    GET_NEXT_VICTIM(this) = -1;
    //GET_MOB_RNUM(this) = NOBODY;
    GET_WAS_IN(this) = NULL;
    GET_POS(this) = POS_STANDING;
    mob_specials.default_pos = POS_STANDING;
    loader = NOBODY;
    GET_SPELL_DIR(this) = NOWHERE;
    followers = NULL;
    RIDING(this) = NULL;
    RIDDEN_BY(this) = NULL;
    HUNTING(this) = NULL;
    HUNT_COUNT(this) = 0;
    hitched = NULL;
    GET_ATTACK_POS(this) = TYPE_UNDEFINED;
    has_note[0] = -1;
    has_note[1] = -1;
    has_note[2] = -1;
    has_note[3] = -1;
    has_note[4] = -1;
    GET_POINTS_EVENT(this,0) = NULL;
    GET_POINTS_EVENT(this, 1) = NULL;
    GET_POINTS_EVENT(this,2) = NULL;
    GET_POINTS_EVENT(this,3) = NULL;
    GET_FIGHT_EVENT(this)  = NULL;
    GET_MESSAGE_EVENT(this) = NULL;
    GET_TASK(this)  = NULL;
    GET_MSG_RUN(this) = 0;
    GET_AC(this) = 100;
    points.max_mana = 100;

    CMD_FLAGS2(this) = 0;

}

/* clear some of the the working variables of a char */
void Character::reset() {
    int i;

    for (i = 0; i < NUM_WEARS; i++)
        GET_EQ(this, i) = NULL;
    for (i = 0; i < TOP_FUSE_LOCATION; i++)
        FUSE_LOC(this, i) = NULL;
    DIE_TIME(this)            = 0;
    REMOVE_BIT_AR(PLR_FLAGS(this), PLR_DYING);
    ATK_CHANCE(this)          = 3;
    FUSED_TO(this)            = NULL;
    GET_SKILLMULTI(this)      = 0.0;
    followers                 = NULL;
    master                    = NULL;
    IN_ROOM(this)             = NULL;
    carrying                  = NULL;
    next                      = NULL;
    next_fighting             = NULL;
    next_in_room              = NULL;
    SITTING(this)             = NULL;
    NEXT_SITTING(this)        = NULL;
    FIGHTING(this)            = NULL;
    GET_POINTS_EVENT(this, 0) = NULL;
    GET_POINTS_EVENT(this, 1) = NULL;
    GET_POINTS_EVENT(this, 2) = NULL;
    GET_POINTS_EVENT(this, 3) = NULL;
    GET_FIGHT_EVENT(this)     = NULL;
    GET_MESSAGE_EVENT(this)   = NULL;
    GET_TASK(this)            = NULL;
    GET_MSG_RUN(this)         = 0;
    char_specials.position    = POS_STANDING;
    mob_specials.default_pos  = POS_STANDING;
    mob_specials.join_list    = NULL;
    mob_specials.head_join    = NULL;
    char_specials.carry_weight = 0;
    char_specials.carry_items = 0;
    GET_SPELL_DIR(this)       = NOWHERE;
    GET_PERC(this)            = 100.0;
    pnote                     = NULL;
    LOCKER(this)              = NULL;
    MINE_DIR(this)            = NOWHERE;
    MINE_SPEED(this)          = 0;
    MINE_STEALTH(this)        = 0;
    MINE_BONUS(this)          = 0;
    MINE_DAMAGE(this)         = 0;
    HAS_MAIL(this)            = -1;
    IS_SAVING(this)           = FALSE;
    hitched                   = NULL;
    internal_flags 			= 0;

    if (GET_HIT(this) <= 0)
        GET_HIT(this) = 1;
    if (GET_MOVE(this) <= 0)
        GET_MOVE(this) = 1;
    if (GET_MANA(this) <= 0)
        GET_MANA(this) = 1;
    if (GET_MAX_HIT(this) <= 0)
        GET_MAX_HIT(this) = 1;
    if (GET_MAX_MOVE(this) <= 0)
        GET_MAX_MOVE(this) = 1;
    if (GET_MAX_MANA(this) <= 0)
        GET_MAX_MANA(this) = 1;
    if (GET_MAX_STAMINA(this) <= 0)
        GET_MAX_STAMINA(this) = 1;
    check_regen_rates(); /* start regening points */
    GET_LAST_TELL(this) = NOBODY;


}

/*
 * Called during character creation after picking character class
 * (and then never again for that character).
 */
/* initialize a new character only if class is set */
void Character::init() {
    int i, taeller;

    /* create a player_special structure */
    if (player_specials == NULL) {
        log("Player init without player specials somehow!");
        exit(1);
    }

    /* *** if this is our first player --- he be God *** */

    if (player_table.size() == 0 || (player_table.size() == 1 && !strcasecmp(player_table[0].name,player.name))) {

        GET_LEVEL(this) = LVL_IMPL;
        GET_EXP(this) = exp_needed(this);



        /* The implementor never goes through do_start(). */
        GET_MAX_HIT(this) = 1500;
        GET_MAX_MANA(this) = 1100;
        GET_MAX_MOVE(this) = 1100;
        GET_MAX_STAMINA(this) = 5000;
        GET_HIT(this) = GET_MAX_HIT(this);
        GET_MANA(this) = GET_MAX_MANA(this);
        GET_MOVE(this) = GET_MAX_MOVE(this);
        GET_STAMINA(this) = GET_MAX_STAMINA(this);
    }
    set_title(this, NULL);

    /* Romance Initialization, initialize to no partner and single */
    player.romance = 0;
    player.partner = 0;

    player.time.birth = time(0);
    player.time.played = 0;
    player.time.logon = time(0);
    REMORTS(this) = 0;

    LOCKER_EXPIRE(this) = 0;
    LOCKER_LIMIT(this) = 0;


    /* make favors for sex, including MatingMod additions */
    if (player.sex == SEX_MALE) {
        player.weight = number(120, 180);
        player.height = number(160, 200);
        PREG(this) = MALE;
    } else {
        player.weight = number(100, 160);
        player.height = number(150, 180);
        PREG(this) = NOT_PREG; // End of MatingMod Additions
    }

    player.was_class = -1;
    player.was_class1 = -1;
    player.was_class2 = -1;

    /*mordecai */
    player_specials->saved.tier = 1; // done in nanny
    player_specials->saved.tier1 = 0;
    player_specials->saved.tier2 = 0;
    player_specials->saved.tier3 = 0;



    GET_MAX_MANA(this) = 100;
    GET_MANA(this) = GET_MAX_MANA(this);
    GET_HIT(this) = GET_MAX_HIT(this);
    GET_MAX_MOVE(this) = 82;
    GET_MOVE(this) = GET_MAX_MOVE(this);
    points.armor = 100;
    points.gold = 15000;
    GET_STAMINA(this) = 100;
    GET_GROUP_EXP(this) = 0;
    GET_LOGOUTMSG(this) = NULL;
    GET_LOGINMSG(this) = NULL;


    //TODO: check this
    if ((i = get_ptable_by_name(player.name)) != -1)
        while (valid_id_num(++top_idnum) == FALSE)
            log("Error new id %ld being assigned to %s already exists!",top_idnum, player.name);

    player_table[i].account = player_table[i].id = GET_IDNUM(this) = GET_ID(this) = top_idnum;
    addChToLookupTable(GET_ID(this), this);


    for (taeller = 0; taeller < AF_ARRAY_MAX; taeller++)
        char_specials.saved.affected_by[taeller] = 0;


    for (i = 0; i < 5; i++)
        GET_SAVE(this, i) = 0;

    if (GET_LEVEL(this) > LVL_SEN) {
        real_abils.intel = 25;
        real_abils.wis = 25;
        real_abils.dex = 25;
        real_abils.str = 25;
        real_abils.str_add = 100;
        real_abils.con = 25;
        real_abils.cha = 25;
    }

    for (i = 0; i < 2; i++)
        GET_COND(this, i) = (GET_LEVEL(this) == LVL_IMPL ? -1 : 48);
    GET_COND(this, 2) = 0;

    GET_LOADROOM(this) = NOWHERE;
    check_regen_rates();


}

void Character::default_char() {
    int i;
    time_t tme = time(0);
    if (!this)
        return;
    vnum = NOBODY;
    proto = TRUE;
    master 			= NULL;
    desc 				= NULL;
    next 				= NULL;
    script 			= NULL;
    affected 			= NULL;
    GET_SEX(this) 		= SEX_MALE;
    GET_CLASS(this) 	= CLASS_WARRIOR;
    GET_LEVEL(this) 	= 0;
    GET_HEIGHT(this) 	= 100;
    GET_WEIGHT(this) 	= 100;
    GET_ALIGNMENT(this) 	= 0;
    MEMORY(this) 		= NULL;
    proto_script 		= NULL;
    memory 			= NULL;
    CREATE_POINTS(this) 	= 0;
    IS_CARRYING_W(this) 	= 0;
    IS_CARRYING_N(this)	= 0;
    GET_IDNUM(this) = GET_ID(this) = -1;
    for (i = 0; i < 4; i++) {
        AFF_FLAGS(this)[i] = 0;
        PRF_FLAGS(this)[i] = 0;
        PLR_FLAGS(this)[i] = 0;
    }
    for (i = 0; i < NUM_CLASSES; i++)
        GET_MASTERY(this, i) 	= 0;
    for (i = 0; i < 5; i++)
        GET_SAVE(this, i) 	= 0;

    for (i = 0; i < NUM_WEARS; i++)
        GET_EQ(this, i) 		= NULL;
    for (i = 0; i < TOP_FUSE_LOCATION; i++)
        FUSE_LOC(this, i) 	= NULL;

    for (i = 0; i < AF_ARRAY_MAX; i++)
        char_specials.saved.affected_by[i] = 0;
    GET_LOADROOM(this) 		= NOWHERE;
    GET_WAIT_STATE(this) 	= 0;
    GET_INVIS_LEV(this) 		= 0;
    GET_FREEZE_LEV(this) 	= 0;
    GET_WIMP_LEV(this) 		= 5;
    GET_COND(this, FULL)		= 48;
    GET_COND(this, THIRST) 	= 48;
    GET_COND(this, DRUNK) 	= 0;
    GET_BAD_PWS(this) 		= 0;
    GET_PRACTICES(this) 		= 0;
    GET_GOLD(this) 			= 0;
    GET_BANK_GOLD(this) 		= 0;
    GET_EXP(this) 			= 0;
    GET_GROUP_EXP(this) 		= 0;
    GET_HITROLL(this) 		= 0;
    GET_DAMROLL(this) 		= 0;
    GET_AC(this) 			= 100;
    real_abils.str 			= 0;
    real_abils.str_add 		= 0;
    real_abils.dex 			= 0;
    real_abils.intel 		= 0;
    real_abils.wis 			= 0;
    real_abils.con 			= 0;
    real_abils.cha 			= 0;
    //GET_SPEED(this) = 0;
    GET_HIT(this) 			= 25;
    GET_MAX_HIT(this) 		= 25;
    GET_MANA(this) 			= 100;
    GET_MAX_MANA(this) 		= 100;
    GET_MOVE(this) 			= 50;
    GET_MAX_MOVE(this) 		= 50;
    GET_STAMINA(this) 		= 100;
    GET_MAX_STAMINA(this) 	= 100;
    if (player_specials)
        player_specials->host = NULL;
    AFF_SPEED(this) 		= 0;
    player.time.last_logon 	= tme;
    GET_PERC(this) 			= 100;
    AFK_MSG(this) 			= NULL;
    BUSY_MSG(this) 			= NULL;
    GET_PK_CNT(this) 		= 0;
    GET_PK_RIP(this) 		= 0;
    GET_PK_POINTS(this) 		= 0;
    GET_POSTS(this) 		= 0;
    GET_NAILS(this)  		= 0;
    GET_WIRE(this)   		= 0;
    GET_PERM_ACCURACY(this)  	= 0;
    GET_PERM_EVASION(this)   	= 0;
    GET_ORIG_LEV(this) 		= 0;
    PRETITLE(this) 			= NULL;
    IMMTITLE(this) 			= NULL;
    REMORTS(this) 			= 0;
    GET_FIGHT_EVENT(this) 	= NULL;
    GET_MESSAGE_EVENT(this) 	= NULL;
    GET_POINTS_EVENT(this, 0) = NULL;
    GET_POINTS_EVENT(this, 1) = NULL;
    GET_POINTS_EVENT(this, 2) = NULL;
    GET_POINTS_EVENT(this, 3) = NULL;
    GET_REGEN_HIT(this) 		= 0;
    GET_REGEN_MANA(this) 	= 0;
    GET_REGEN_MOVE(this) 	= 0;
    GET_REGEN_STAMINA(this) 	= 0;
    GET_RP_GROUP(this)  		= 0;
    GET_CONVERSIONS(this) 	= 1;
    SPECIALS(this)->last_note 	= tme;
    SPECIALS(this)->last_idea 	= tme;
    SPECIALS(this)->last_penalty 	= tme;
    SPECIALS(this)->last_news 	= tme;
    SPECIALS(this)->last_changes 	= tme;
    GET_LAST_DAM_D(this) 	= 0;
    GET_LAST_DAM_T(this) 	= 0;
    EXTRA_BODY(this) 		= 0;
    GET_RACE(this) 			= 0;
    //MOB_RACE(this) = 0;
    pnote 				= NULL;
    SPECIALS(this)->last_reward = 0;
    GET_REWARD(this) 		= 0;
    GET_AWARD(this) 		= 0;
    CONCEALMENT(this) 		= 0;
    PROMPT(this)  			= NULL;
    BPROMPT(this) 			= NULL;
    PAGEWIDTH(this) 		= 80;
    PAGEHEIGHT(this) 		= 25;
    LOCKER_EXPIRE(this) 		= 0;
    LOCKER_LIMIT(this) 		= 0;
    FIGHTING(this) 			= NULL;
    SITTING(this)			= NULL;
    GET_LOGOUTMSG(this) 		= NULL;
    GET_LOGINMSG(this) 		= NULL;
    TRADEPOINTS(this) 		= 0;
    GET_CSNP_LVL(this)		=-2;
    mob_specials.join_list 	= NULL;
    mob_specials.head_join 	= NULL;
    mob_specials.dam_list 	= NULL;
    MOB_OWNER(this) 		= -1;
    carrying				= NULL;
    LAST_MOVE(this)			= time(0);
    pet 					= -1;
    mob_specials.attack_type	= 0;
    char_specials.timer 		= 0;
    send_string 			= NULL;
    GET_PK_CNT(this) 		= 0;
    GET_CLAN(this) 			= 0;
    GET_CLAN_RANK(this) 		= 0;
    GET_SPEED(this) 		= 0;
    GET_OLC_ZONE(this) 		= 0;
    GET_MRACE(this) 		= 0;
    MOB_SKIN(this)         	= -1;
    char_specials.next_in_chair = NULL;
    last_move = 0;
    sweep_damage = 0;
    pulling = NOBODY;
    on_task = 0;
    fused_to = 0;
    init_char_strings();
}

char * mob_name_by_vnum(mob_vnum &v) {
    if (MobProtoExists(v))
        return GetMobProto(v)->player.short_descr;

    return "";
}
void Character::remove_all_affects() {
    while (affected)
        affect_remove(affected);
}
void Character::free_proto_mob() {
    free_proto_script(this, MOB_TRIGGER);
    free_join_list(mob_specials.join_list);
    free_char_strings();
    remove_all_affects();
}


/*
 * Remove an affected_type structure from a char (called when duration
 * reaches zero). Pointer *af must never be NIL!  Frees mem and calls
 * affect_location_apply
 */
void Character::affect_remove(struct affected_type *af) {
    struct affected_type *temp;

    if (affected == NULL) {
        core_dump();
        return;
    }

    affect_modify(this, af->location, af->modifier, af->bitvector, FALSE);
    REMOVE_FROM_LIST(af, affected, next);
    free(af);
    af = NULL;
    affect_total();

}

/* This updates a character by subtracting everything he is affected by */
/* restoring original abilities, and then affecting all again           */
void Character::affect_total() {
    struct affected_type *af;
    int i, j;

    for (i = 0; i < NUM_WEARS; i++) {
        if (GET_EQ(this, i))
            for (j = 0; j < MAX_OBJ_AFFECT; j++)
                affect_modify_ar(this, GET_EQ(this, i)->affected[j].location,
                                 GET_EQ(this, i)->affected[j].modifier,
                                 GET_EQ(this, i)->obj_flags.bitvector,
                                 FALSE);
    }



    for (af = affected; af; af = af->next)
        affect_modify(this, af->location, af->modifier, af->bitvector,
                      FALSE);

    aff_abils = real_abils;

    for (i = 0; i < NUM_WEARS; i++) {
        if (GET_EQ(this, i))
            for (j = 0; j < MAX_OBJ_AFFECT; j++)
                affect_modify_ar(this, GET_EQ(this, i)->affected[j].location,
                                 GET_EQ(this, i)->affected[j].modifier,
                                 GET_EQ(this, i)->obj_flags.bitvector, TRUE);
    }




    for (af = affected; af; af = af->next)
        affect_modify(this, af->location, af->modifier, af->bitvector, TRUE);

    /* Make certain values are between 0..25, not < 0 and not > 25! */

    i = (IS_NPC(this) || (!IS_NPC(this) && GET_LEVEL(this)>= LVL_IMMORT) ? MAX_IMM_BASE : MAX_MORTAL_BASE);

    GET_DEX(this) = IRANGE(0, GET_DEX(this), i);
    GET_INT(this) = IRANGE(0, GET_INT(this), i);
    GET_WIS(this) = IRANGE(0, GET_WIS(this), i);
    GET_CON(this) = IRANGE(0, GET_CON(this), i);
    GET_CHA(this) = IRANGE(0, GET_CHA(this), 100);
    GET_STR(this) = MAX(0, GET_STR(this));


    if (IS_NPC(this)) {
        GET_STR(this) = MIN((int)GET_STR(this), i);
    } else {
        if (GET_STR(this) > i) {
            i = GET_ADD(this) + ((GET_STR(this) - i) * 10);
            GET_ADD(this) = MIN(i, 100);
            GET_STR(this) = MAX_MORTAL_BASE;
        }
    }


    //thiseck_regen_rates(this);    /* update regen rates (for age) */

}


/* updates regen rates.  Use when big regen rate changes are made */
void Character::check_regen_rates() {
    struct regen_event_obj *regen;
    int type, gain = 0;
    long t;

    if (this == NULL || GET_HIT(this) <= HIT_INCAP)
        return;

    for (type = REGEN_HIT; type <= REGEN_STAMINA; type++) {

        switch (type) {
        case REGEN_HIT:
            if (GET_HIT(this) >= GET_MAX_HIT(this))
                continue;
            gain = hit_gain(this);
            break;

        case REGEN_MANA:
            if (GET_MANA(this) >= GET_MAX_MANA(this))
                continue;
            gain = mana_gain(this);
            break;

        case REGEN_MOVE:
            if (GET_MOVE(this) >= GET_MAX_MOVE(this))
                continue;
            gain = move_gain(this);
            break;
        case REGEN_STAMINA:
            if (GET_STAMINA(this) >= GET_MAX_STAMINA(this))
                continue;
            gain = stamina_gain(this);
            break;
        }

        t = PULSES_PER_MUD_HOUR / (gain > 0 ? gain : 1);

        if (GET_POINTS_EVENT(this, type) == NULL ||
                (t < event_time(GET_POINTS_EVENT(this, type)))) {

            /* take off old event, create updated event */
            if (GET_POINTS_EVENT(this, type) != NULL)
                event_cancel(GET_POINTS_EVENT(this, type));
            GET_POINTS_EVENT(this, type) = NULL;

            regen = new regen_event_obj(this, type);
            GET_POINTS_EVENT(this, type) = event_create(points_event, regen, t, EVENT_TYPE_REGEN);
        }
    }
}
void Character::save() {
    if (this == NULL) {
        log("SYSERR: save() recieved null ch!");
        return;
    }
    if (IS_NPC(this)) {
        log("SYSERR: save() called on an NPC! %s, %d", __FILE__, __LINE__);
        return;
    }
    OBJ_INNATE_MESSAGE = FALSE;
    char_to_store(this);
    SaveKillList();
    OBJ_INNATE_MESSAGE = TRUE;
    save_char_vars(this);
}

Character *Character::assign (Character *b) {
    //    struct affected_type *hjp;
    Character *mp, *chch;

    /** Character Class Values **/
    if (b == NULL)
        return this;
    mp = GetMobProto(b->vnum);
    chch = GetMobProto(vnum);
    /** free non proto strings so they can share the new mob proto's strings */
    free_non_proto_strings();
    pfilepos = b->pfilepos;
    in_room = b->in_room;         /* Location (real room number)   */
    was_in_room = b->was_in_room;     /* location for linkdead people  */
    wait = b->wait;            /* wait for how many loops       */

    if (mp && chch)
        log("mob names: %s, and %s", mp->player.name, chch->player.name);
    player = b->player;     /* Normal data                   */
    /** copy across strings from player data! */
    if (b->player.name)
        player.name = strdup(b->player.name);
    else
        player.name = NULL;

    if (b->player.short_descr)
        player.short_descr = strdup(b->player.short_descr);
    else
        player.short_descr = NULL;

    if (b->player.long_descr)
        player.long_descr = strdup(b->player.long_descr);
    else
        player.long_descr = NULL;

    if (b->player.description)
        player.description = strdup(b->player.description);
    else
        player.description = NULL;

    if (b->player.title)
        player.title = strdup(b->player.title);
    else
        player.title = NULL;

    #if 0

    if (mp != NULL) {
        if (b->player.name && b->player.name != mp->player.name)
            player.name = strdup(b->player.name);

        if (b->player.short_descr && b->player.short_descr != mp->player.short_descr)
            player.short_descr = strdup(b->player.short_descr);

        if (b->player.long_descr && b->player.long_descr != mp->player.long_descr)
            player.long_descr = strdup(b->player.long_descr);

        if (b->player.description && b->player.description != mp->player.description)
            player.description = strdup(b->player.description);

        if (b->player.title && b->player.title != mp->player.title)
            player.title = strdup(b->player.title);
    }
    #endif
    real_abils = b->real_abils;     /* Abilities without modifiers   */
    aff_abils = b->aff_abils; /* Abils with spells/stones/etc  */
    points = b->points; /* Points                        */
    char_specials = b->char_specials;  /* PC/NPC specials        */
    if (b->player_specials) {
        if (b->player_specials == &dummy_mob) {
            player_specials = &dummy_mob;  /* PC specials            */
        } else {
            memcpy(player_specials, b->player_specials, sizeof(b->player_specials));
        }
    }

    mob_specials = b->mob_specials;    /* NPC specials           */

    combatskill = b->combatskill;
    /** Dont Move the affects over, do that in calling function
        while (affected)
            affect_remove(affected);
        hjp = b->affected;
        while (hjp) {
            affect_to_char(this, hjp);
            hjp = hjp->next;
        }
        **/
    #if 0

    affected = b->affected;
    b->affected = NULL;

    /** don't duplicate equipment **/
    //struct obj_data *equipment[NUM_WEARS];   /* Equipment array               */
    /** don't duplicate inventory either **/
    //struct obj_data *carrying;     /* Head of list                  */
    carrying = b->carrying;
    for (struct obj_data *ob = carrying;ob;ob = ob->next)
        ob->carried_by = this;
    b->carrying = NULL;
    /** keep desc null for all duplicates for now, assign this in callinfg function if needed */
    //Descriptor *desc;  /* NULL for mobiles              */
    desc = b->desc;
    desc = NULL;
    /** id's MUST always be unique, so don't assign this either! */
    //long id;             /* used by DG triggers             */
    int pi;
    if ((pi = GET_MOB_RNUM(this)) != NOBODY) {
        if (b->proto_script && b->proto_script != mob_proto[pi].proto_script) {
            proto_script = b->proto_script;    /* list of default triggers      */
            b->proto_script = NULL;
        }
    }
    script = b->script;    /* script info for the object      */
    b->script = NULL;
    memory = b->memory;  /* for mob memory triggers         */
    b->memory = NULL;
    /** Don't move them into a room, you can use char_to_room later */
    //Character *next_in_room;  /* For room->people - list         */
    /** it should already be in the character list, and if not that is fine **/
    //Character *next;     /* For either monster or ppl-list  */
    /** this value isn't even used any more **/
    //Character *next_fighting; /* For fighting list               */
    /** Do not assign followers, this can be done in the calling function **/
    //struct follow_type *followers; /* List of chars followers       */
    #endif
    //master;   /* Who is char following?        */
    cmd2 = b->cmd2;
    //long cmd2;           /* These wizcmds aren't saved     */

    internal_flags = b->internal_flags; /* Flags used internally - not saved */
    /** don't assign events! do this in the calling function with checks or it may cause havok!**/
    //struct event *pts_event[4]; /* events for regening H/M/V/S     */
    //struct event *fight_event;     /*events used for fighting/defending */
    //struct event *message_event; /* events used in skill/spell messages*/
    //struct sub_task_obj *task;   /* working on a task? This will look after you!*/
    /** don't assign spell direction **/
    //int spell_dir;       /*used for casting directional spells */
    /** don't assign interact, as this is a grouping thing and we havent assigned groups **/
    //float interact;      /*used for the percentage they land hits and get hit and gain exp in battle */
    //int attack_location;
    //long loader;         /*id of player who linkloaded them */
    //struct sub_list *subs; /*list of subskills available to that person*/
    //struct skillspell_data *skills; /*list of skills and spells available to that person */
    //int msg_run;
    //int on_task;
    //struct note_data *pnote;
    //concealment = b->concealment;

    //int has_note[NUM_NOTE_TYPES];
    //Character *fuses[TOP_FUSE_LOCATION];
    //Character *fused_to;
    //struct obj_data *hitched;
    //last_move = b->last_move;
    //sweep_damage = b->sweep_damage;
    body = b->body;                   /* body positions aquired */
    atk = b->atk;
    //pulling = b->pulling;
    pet = b->pet;


    //    nr = b->nr;         /* Mob's rnum                    */
    //struct travel_point_data *travel_list;
    if (mp && chch)
        log("mob names: %s, and %s", mp->player.name, chch->player.name);
    return this;

}

void Character::free_non_proto_strings() {

    if (IS_MOB(this)) {
        Character *mp = GetMobProto(vnum);

        if (player.name && player.name != mp->player.name) {
            free_string(&player.name);
        }
        if (player.short_descr && player.short_descr != mp->player.short_descr) {
            free_string(&player.short_descr);
        }
        if (player.long_descr && player.long_descr != mp->player.long_descr) {
            free_string(&player.long_descr);
        }
        if (player.description && player.description != mp->player.description) {
            free_string(&player.description);
        }
        if (player.title) {
            free_string(&player.title);
        }
        init_char_strings();

    }
}

void Character::free_char_strings() {
    if (player.name) {
        free(player.name);               /* PC / NPC s name (kill ...  )         */
        player.name = NULL;
    }
    if (player.short_descr) {
        free(player.short_descr);        /* for NPC 'actions'                    */
        player.short_descr = NULL;
    }
    if (player.long_descr) {
        free(player.long_descr);         /* for 'look'                           */
        player.long_descr = NULL;
    }
    if (player.description) {
        free(player.description);        /* Extra descriptions                   */
        player.description = NULL;
    }
    if (player.title) {
        free(player.title);         /* PC / NPC's title                     */
        player.title = NULL;
    }
}

void Character::init_char_strings() {
    player.name = NULL;               /* PC / NPC s name (kill ...  )         */
    player.short_descr = NULL;        /* for NPC 'actions'                    */
    player.long_descr  = NULL;         /* for 'look'                           */
    player.description  = NULL;        /* Extra descriptions                   */
    player.title = NULL;         /* PC / NPC's title                     */
}
bool Character::zone_empty() {
    if (in_room == NULL)
        return TRUE;

    return zone_table[GET_ROOM_ZONE(in_room)].num_players > 0;
}
bool Character::CanMove() {
    if (AFF_FLAGGED(this, AFF_STUCK))
        return FALSE;
    if (AFF_FLAGGED(this, AFF_HOLD))
        return FALSE;
    if (get_sub_status(this, SUB_JUGGLE) == STATUS_ON)
        return FALSE;

    return TRUE;
}

/* simple function to determine if char can walk on water */
bool Character::HasBoat() {
    if (!IS_NPC(this) && GET_LEVEL(this) >= LVL_IMMORT)
        return TRUE;
    if (AFF_FLAGGED(this, AFF_WATERWALK))
        return TRUE;
    if (Flying())
        return TRUE;

    /* non-wearable boats in inventory will do it */
    for (struct obj_data *obj = carrying; obj; obj = obj->next_content)
        if (GET_OBJ_TYPE(obj) == ITEM_BOAT
                && (find_eq_pos(this, obj, NULL) < 0))
            return TRUE;

    /* and any boat you're wearing will do it too */
    for (int i = 0; i < NUM_WEARS; i++)
        if (HAS_BODY(this, i) && GET_EQ(this, i) &&
                GET_OBJ_TYPE(GET_EQ(this, i)) == ITEM_BOAT)
            return TRUE;

    return FALSE;
}


bool Character::Flying() {
    if (!IS_NPC(this) && GET_LEVEL(this) >= LVL_IMPL)
        return TRUE;

    for (sh_int i = 0; i < NUM_WEARS; i++) {
        if (HAS_BODY(this, i) && (GET_EQ(this, i)) &&
                (GET_OBJ_TYPE(GET_EQ(this, i)) == ITEM_WINGS))
            return TRUE;
    }

    if (AFF_FLAGGED(this, AFF_FLY))
        return TRUE;

    if (GET_POS(this) == POS_SITTING && char_specials.chair && GET_OBJ_TYPE(char_specials.chair) == ITEM_SPACEBIKE)
        return TRUE;

    if (MountHere() && RIDING(this)->Flying())
        return TRUE;

    return FALSE;
}

bool Character::SpaceProtected() {
    if (!IS_NPC(this) &&GET_LEVEL(this) >= LVL_IMPL)
        return TRUE;

    if (GET_RACE(this) == RACE_MARTIAN)
        return TRUE;

    for (sh_int i = 0; i < NUM_WEARS; i++)
        if (HAS_BODY(this, i) && GET_EQ(this, i) &&
                GET_OBJ_TYPE(GET_EQ(this, i)) == ITEM_SPACESUIT)
            return TRUE;

    return FALSE;
}

bool Character::SunProtected() {
    if (!IS_NPC(this) && GET_LEVEL(this) >= LVL_IMPL)
        return TRUE;

    for (sh_int i = 0; i < NUM_WEARS; i++)
        if (HAS_BODY(this, i) && GET_EQ(this, i) &&
                GET_OBJ_TYPE(GET_EQ(this, i)) == ITEM_THERMAL_PROT)
            return TRUE;

    return FALSE;
}

bool Character::WaterBreathing() {
    if (!IS_NPC(this) && GET_RACE(this) == RACE_MARTIAN)
        return TRUE;

    for (sh_int i = 0; i < NUM_WEARS; i++)
        if (HAS_BODY(this, i) && GET_EQ(this, i) &&
                GET_OBJ_TYPE(GET_EQ(this, i)) == ITEM_AQUALUNG)
            return TRUE;

    if (AFF_FLAGGED(this, AFF_GILLS))
        return TRUE;

    return FALSE;
}

Character * Character::NextFightingMe() {
    if (!in_room)
        return NULL;

    for (Character *v = in_room->people;v;v = v->next_in_room) {
        if (v == this)
            continue;
        if (FIGHTING(v) == this)
            return v;
    }
    return NULL;
}

bool Character::canHuntChar(Character *vict) {
    if(!CAN_HUNT(this) ||
            ((MOB_FLAGGED(this, MOB_STAY_ZONE) && IN_ROOM(this)->zone != IN_ROOM(vict)->zone) ||
             (MOB_FLAGGED(this, MOB_STAY_SECTOR) && IN_ROOM(this)->sector_type != IN_ROOM(vict)->sector_type) ||
             find_first_step(IN_ROOM(this), IN_ROOM(vict)) < 0))
        return false;

    //check for path things here (see notes)
    int stepcount = 0;
    int stepdir = find_first_step(IN_ROOM(this),IN_ROOM(vict));
    //	Room *steproom = IN_ROOM(this)->dir_option[find_first_step(IN_ROOM(this),IN_ROOM(vict))]->to_room;
    Room *curroom = IN_ROOM(this);
    while (curroom && stepdir>=0 && stepdir <= 5 && stepcount++ != MAX_HUNTSTEPS(this) && curroom->dir_option[stepdir]) {
        if (IS_SET(curroom->dir_option[stepdir]->exit_info, EX_CLOSED))
            return false;
        Room *steproom = curroom->dir_option[stepdir]->to_room;
        if (!steproom || (MOB_FLAGGED(this, MOB_STAY_SECTOR) && steproom->zone != IN_ROOM(vict)->zone) ||
                (MOB_FLAGGED(this, MOB_STAY_ZONE) && steproom->sector_type != IN_ROOM(vict)->sector_type) ||
                ROOM_FLAGGED(steproom, ROOM_NOMOB) ||
                ROOM_FLAGGED(steproom, ROOM_DEATH))
            return false;
        curroom = steproom;
        if (curroom == IN_ROOM(vict))
            return true;
    }

    return false;
}
