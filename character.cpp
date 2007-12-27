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

#ifdef HAVE_ARPA_TELNET_H
#include <arpa/telnet.h>
#else
#include "telnet.h"
#endif


void free_join_list(struct combine_data *list);
void free_killlist(Character *ch);
long get_ptable_by_name(char *name);

void free_note(NOTE_DATA *note, int type);
void free_mob_memory(memory_rec *k);
void subs_remove(Character *ch, struct sub_list *af);
void skills_remove(Character *ch, struct skillspell_data *af);
void free_followers(struct follow_type *k);

void damage_count_free(Character *vict);
void extract_all_in_list(OBJ_DATA *obj);
void free_alias(struct alias_data *a);
extern long top_idnum;

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

size_t Character::Send(string i) {
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

Character::Character() {
    player_specials = new struct player_special_data();
    default_char();
    clear();
}

Character::~Character() {
    freeself();
}

/* release memory allocated for a char struct */
void Character::freeself() {
    void free_ignorelist(Character *ch);
    int i;
    struct alias_data *a;

  if(is_mtransformed) {
    clear();
    return;
  }
    if (this == NULL)
        return;

    while (affected)
        affect_remove(this, this->affected);

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

    while (subs)
        subs_remove(this, subs);
    subs = NULL;
    while (skills)
        skills_remove(this, skills);
    skills = NULL;

    free_ignorelist(this);

    if (player_specials != NULL && player_specials != &dummy_mob) {
        while ((a = GET_ALIASES(this)) != NULL) {
            GET_ALIASES(this) = (GET_ALIASES(this))->next;
            free_alias(a);
        }
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
        free_killlist(this);

        if (player_specials)
            delete player_specials;

        if (IS_NPC(this))
            log("SYSERR: Mob %s (#%d) had player_specials allocated!", GET_NAME(this), GET_MOB_VNUM(this));
    }
    if (!IS_NPC(this) || (IS_NPC(this) && GET_MOB_RNUM(this) == NOBODY)) {
        /* if this is a player, or a non-prototyped non-player, free all */
        free_string(&player.name);
        free_string(&player.title);
        free_string(&player.short_descr);
        free_string(&player.long_descr);
        free_string(&player.description);

        /* free script proto list */
        free_proto_script(this, MOB_TRIGGER);

        free_join_list(mob_specials.join_list);
        mob_specials.join_list = NULL;

    } else if ((i = GET_MOB_RNUM(this)) != NOBODY) {
        /* otherwise, free strings only if the string is not pointing at proto */
        if (player.name && player.name != mob_proto[i].player.name)
            free(player.name);
        if (player.title
                && player.title != mob_proto[i].player.title)
            free(player.title);
        if (player.short_descr
                && player.short_descr != mob_proto[i].player.short_descr)
            free(player.short_descr);
        if (player.long_descr
                && player.long_descr != mob_proto[i].player.long_descr)
            free(player.long_descr);
        if (player.description
                && player.description != mob_proto[i].player.description)
            free(player.description);

        /* free script proto list if it's not the prototype */
        if (proto_script && proto_script != mob_proto[i].proto_script)
            free_proto_script(this, MOB_TRIGGER);

        if (mob_specials.join_list && mob_specials.join_list != mob_proto[i].mob_specials.join_list)
            free_join_list(mob_specials.join_list);
        mob_specials.join_list = NULL;

        damage_count_free(this);

    }

    /* free any assigned scripts */
    if (SCRIPT(this))
        extract_script(this, MOB_TRIGGER);


    if (desc)
        desc->character = NULL;
    /* find_char helper */
    if (GET_ID(this) > 0)
        remove_from_lookup_table(GET_ID(this));
}

/* clear ALL the working variables of a char; do NOT free any space alloc'ed */
void Character::clear() {
    IN_ROOM(this) = NULL;
    TRAVEL_LIST(this) = NULL;
    GET_PFILEPOS(this) = -1;
    GET_IDNUM(this) = 0;
    GET_NEXT_SKILL(this) = TYPE_UNDEFINED;
    GET_NEXT_VICTIM(this) = -1;
    GET_MOB_RNUM(this) = NOBODY;
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
    GET_IGNORELIST(this)      = NULL;
    hitched                   = NULL;
    pet                       = -1;

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
    check_regen_rates(this); /* start regening points */
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

    if (top_of_p_table == 0) {

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
    GET_KILLS(this) = NULL;
    player.short_descr = NULL;
    player.long_descr = NULL;
    player.description = NULL;


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
    pet = -1;


    //TODO: check this
    if ((i = get_ptable_by_name(GET_NAME(this))) != -1) {
        while (!valid_id_num(++top_idnum))
            log("Error new id %ld being assigned to %s already exists!",top_idnum, GET_NAME(this));
        player_table[i].id = GET_IDNUM(this) = GET_ID(this) =  top_idnum;

        player_table[i].account = GET_IDNUM(this);
        add_to_lookup_table(GET_ID(this), (void *)this);
    } else
        log("SYSERR: init_char: Character '%s' not found in player table.",
            GET_NAME(this));



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
    check_regen_rates(this);


}

void Character::default_char() {
    int i;
    time_t tme = time(0);
    if (!this)
        return;
desc = NULL;
proto_script = NULL;
next = NULL;
GET_TITLE(this) = NULL;

script = NULL;
    affected = NULL;
    subs = NULL; //yep subskills are a linked list. - mord
    skills = NULL; // and now skills and spells are a linked list
    GET_SEX(this) = SEX_MALE;
    GET_CLASS(this) = CLASS_WARRIOR;
    GET_LEVEL(this) = 0;
    GET_HEIGHT(this) = 100;
    GET_WEIGHT(this) = 100;
    GET_ALIGNMENT(this) = 0;
    player.name = NULL;
    player.short_descr = NULL;
    player.long_descr = NULL;
    MEMORY(this) = NULL;
    proto_script = NULL;
    memory = NULL;
    CREATE_POINTS(this) = 0;
    GET_IDNUM(this) = GET_ID(this) = -1;
    for (i = 0; i < 4; i++) {
        AFF_FLAGS(this)[i] = 0;
        PRF_FLAGS(this)[i] = 0;
        PLR_FLAGS(this)[i] = 0;
    }
    for (i = 0; i < NUM_CLASSES; i++)
        GET_MASTERY(this, i) = 0;
    for (i = 0; i < 5; i++)
        GET_SAVE(this, i) = 0;

    for (i = 0; i < NUM_WEARS; i++)
        GET_EQ(this, i) = NULL;
    for (i = 0; i < TOP_FUSE_LOCATION; i++)
        FUSE_LOC(this, i) = NULL;

    for (i = 0; i < AF_ARRAY_MAX; i++)
        char_specials.saved.affected_by[i] = 0;
    GET_LOADROOM(this) = NOWHERE;
    GET_WAIT_STATE(this) = 0;
    GET_INVIS_LEV(this) = 0;
    GET_FREEZE_LEV(this) = 0;
    GET_WIMP_LEV(this) = 5;
    GET_COND(this, FULL) = 48;
    GET_COND(this, THIRST) = 48;
    GET_COND(this, DRUNK) = 0;
    GET_BAD_PWS(this) = 0;
    GET_PRACTICES(this) = 0;
    GET_GOLD(this) = 0;
    GET_BANK_GOLD(this) = 0;
    GET_EXP(this) = 0;
    GET_GROUP_EXP(this) = 0;
    GET_HITROLL(this) = 0;
    GET_DAMROLL(this) = 0;
    GET_AC(this) = 100;
    real_abils.str = 0;
    real_abils.str_add = 0;
    real_abils.dex = 0;
    real_abils.intel = 0;
    real_abils.wis = 0;
    real_abils.con = 0;
    real_abils.cha = 0;
    //GET_SPEED(this) = 0;
    GET_HIT(this) = 25;
    GET_MAX_HIT(this) = 25;
    GET_MANA(this) = 100;
    GET_MAX_MANA(this) = 100;
    GET_MOVE(this) = 50;
    GET_MAX_MOVE(this) = 50;
    GET_STAMINA(this) = 100;
    GET_MAX_STAMINA(this) = 100;
    if (player_specials)
        player_specials->host = NULL;
    AFF_SPEED(this) = 0;
    player.time.last_logon = tme;
    GET_PERC(this) = 100;
    AFK_MSG(this) = NULL;
    BUSY_MSG(this) = NULL;
    GET_PK_CNT(this) = 0;
    GET_PK_RIP(this) = 0;
    GET_PK_POINTS(this) = 0;
    GET_POSTS(this) = 0;
    GET_NAILS(this)  = 0;
    GET_WIRE(this)   = 0;
    GET_PERM_ACCURACY(this)  = 0;
    GET_PERM_EVASION(this)   = 0;
    GET_ORIG_LEV(this) = 0;
    PRETITLE(this) = NULL;
    IMMTITLE(this) = NULL;
    REMORTS(this) = 0;
GET_FIGHT_EVENT(this) = NULL;
GET_MESSAGE_EVENT(this) = NULL;
GET_POINTS_EVENT(this, 0) = NULL;
GET_POINTS_EVENT(this, 1) = NULL;
GET_POINTS_EVENT(this, 2) = NULL;
GET_POINTS_EVENT(this, 3) = NULL;
    GET_REGEN_HIT(this) = 0;
    GET_REGEN_MANA(this) = 0;
    GET_REGEN_MOVE(this) = 0;
    GET_REGEN_STAMINA(this) = 0;
    GET_RP_GROUP(this)  = 0;
    GET_CONVERSIONS(this) = 1;
    SPECIALS(this)->last_note = tme;
    SPECIALS(this)->last_idea = tme;
    SPECIALS(this)->last_penalty = tme;
    SPECIALS(this)->last_news = tme;
    SPECIALS(this)->last_changes = tme;
    GET_LAST_DAM_D(this) = 0;
    GET_LAST_DAM_T(this) = 0;
    EXTRA_BODY(this) = 0;
    GET_RACE(this) = 0;
    //MOB_RACE(this) = 0;
    pnote = NULL;
    SPECIALS(this)->last_reward = 0;
    GET_REWARD(this) = 0;
    GET_AWARD(this) = 0;
    CONCEALMENT(this) = 0;
    PROMPT(this)  = NULL;
    BPROMPT(this) = NULL;
    PAGEWIDTH(this) = 80;
    PAGEHEIGHT(this) = 25;
    LOCKER_EXPIRE(this) = 0;
    LOCKER_LIMIT(this) = 0;
    GET_KILLS(this) = NULL;
    GET_LOGOUTMSG(this) = NULL;
    GET_LOGINMSG(this) = NULL;
    TRADEPOINTS(this) = 0;
    GET_CSNP_LVL(this)=-2;
    mob_specials.join_list = NULL;
    mob_specials.head_join = NULL;
    mob_specials.dam_list = NULL;
    player.description = NULL;
    is_mtransformed=0;

}

string mob_name_by_vnum(mob_vnum v) {
    for (unsigned int i = 0; i <= top_of_mobt; i++)
        if (mob_index[i].vnum == v)
            return string(mob_proto[i].player.short_descr);

    return string();

}

