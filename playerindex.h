//
// C++ Interface: playerindex
//
// Description:
//
//
// Author: Jamie Nelson <mordecai4d@gmail.com>, (C) 2007
//
// Copyright: See COPYING file that comes with this distribution
//
//
/* external ascii pfile vars */
#ifndef PLAYERINDEX_H
#define PLAYERINDEX_H

extern int pfile_backup_minlevel;
extern int backup_wiped_pfiles;
extern struct pclean_criteria_data pclean_criteria[];
extern int selfdelete_fastwipe;
extern int auto_pwipe;
#if !defined(IS_SET)
#define IS_SET(flag,bit)  ((flag) & (bit))
#endif
#if !defined(SET_BIT)
#define SET_BIT(var,bit)  ((var) |= (bit))
#endif
#if !defined(REMOVE_BIT)
#define REMOVE_BIT(var,bit)  ((var) &= ~(bit))
#endif
#if !defined(TOGGLE_BIT)
#define TOGGLE_BIT(var,bit) ((var) ^= (bit))
#endif

struct player_index_element
{
    char *name;
    long id;
    int level;
    int flags;
    time_t last;
    long account;
    short clan;
    short rank;
    bool repair;
    gold_int gc_amount;
    short gt_amount;

    player_index_element()
    {
        name=NULL;
        id=0;
        level=0;
        flags=0;
        last=0;
        account=0;
        clan=0;
        rank=0;
        repair = ( bool ) 0;
        gc_amount = 0;
        gt_amount = 0;
    }
}
;

typedef vector<player_index_element> plrindx;
typedef plrindx::iterator plrindex_it;

class PlayerIndex
{
    public:

        PlayerIndex()
        {
            player_fl = NULL;
            top_of_p_table = 0;
            top_of_p_file = 0;
            TopIdNum = 0;
        }

        void FreeSelf ( void );
        void Build ( void );
        void Save ( void );
        void RemovePlayer ( plrindex_it ptvi );
        void CleanPFiles ( void );
        long TableIndexByName ( const char *name );
        long TableIndexById ( long &id );
        long GetAccByName ( const char *name );
        long GetAccById ( long id );
        int get_account_num ( int num, long acc );
        int create_entry ( const char *name );
        int clean_dir ( char *dirname );
        int LoadChar ( const char *name, Character *ch );
        void change_plrindex_name ( long id, char *change );
        long TopIdNum;      /* highest idnum in use          */


        char *NameById ( long &id )
        {
            for ( plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++ )
                if ( ( *ptvi ).id == id )
                    return ( ( *ptvi ).name );

            return ( NULL );
        }
        long IdByName ( const char *name )
        {
            /** This function needs to use a sorted list of names and binary search - Mord **/
            for ( plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++ )
                if ( !str_cmp ( ( *ptvi ).name, name ) )
                    return ( ( *ptvi ).id );

            return ( -1 );
        }
        int TopOfTable()
        {
            return top_of_p_table;
        }

        plrindx *PlayerTable()
        {
            return &player_table;
        }
        inline bool NameExists ( char *n )
        {
            if ( n && *n )
                for ( plrindex_it ptvi = player_table.begin(); ptvi != player_table.end(); ptvi++ )
                    if ( !str_cmp ( ( *ptvi ).name, n ) )
                        return true;
            return false;
        }

        /** Name **/
        char *NameByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in NameByIndex" ) );
            return player_table[idx].name;
        }
        /** ID **/
        long IdByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in IdByIndex" ) );
            return player_table[idx].id;
        }
        void SetId ( int idx, long i )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetId" ) );
            player_table[idx].id = i;
        }
        /** Last **/
        time_t LastByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in LastByIndex" ) );
            return player_table[idx].last;
        }
        void SetLast ( int idx, time_t l )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetLast" ) );
            player_table[idx].last = l;
        }
        /** Account **/
        long AccByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in AccByIndex" ) );
            return player_table[idx].account;
        }
        void SetAcc ( int idx, long a )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetAcc" ) );
            player_table[idx].account = a;
        }

        /** Level **/
        int LevelByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in LevelByIndex" ) );
            return player_table[idx].level;
        }
        void SetLevel ( int idx, int l )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetLevel" ) );
            player_table[idx].level = l;
        }
        /** Gold **/
        gold_int GoldByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in GoldByIndex" ) );
            return player_table[idx].gc_amount;
        }
        void SetGold ( int idx, gold_int l )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetGold" ) );
            player_table[idx].gc_amount = l;
        }
        /** Tokens **/
        short TokensByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in TokensByIndex" ) );
            return player_table[idx].gt_amount;
        }
        void SetTokens ( int idx, short l )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetTokens" ) );
            player_table[idx].gt_amount = l;
        }
        /** Rank **/
        short RankByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in RankByIndex" ) );
            return player_table[idx].rank;
        }
        void SetRank ( int idx, short r )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetRank" ) );
            player_table[idx].rank = r;
        }
        /** Clan **/
        short ClanByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in ClanByIndex" ) );
            return player_table[idx].clan;
        }
        void SetClan ( int idx, int r )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetClan" ) );
            player_table[idx].clan = r;
        }
        /** Check for Deleted **/
        bool DeletedByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in DeletedByIndex" ) );
            return IS_SET ( player_table[idx].flags, PINDEX_DELETED ) ||
                   IS_SET ( player_table[idx].flags, PINDEX_SELFDELETE ) ||
                   player_table[idx].name[0] == '\0';
        }
        bool DeletedByStruct ( player_index_element *idx )
        {
            return IS_SET ( idx->flags, PINDEX_DELETED ) ||
                   IS_SET ( idx->flags, PINDEX_SELFDELETE ) ||
                   idx->name[0] == '\0';
        }
        bool ValidIndex ( int idx )
        {
            return ( idx >= 0 || idx < (int)player_table.size() );
        }
        /** Flags **/
        long FlagsByIndex ( int idx )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in FlagsByIndex" ) );
            return player_table[idx].flags;
        }
        void SetFlags ( int idx, int f )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in SetFlags" ) );
            SET_BIT ( player_table[idx].flags, f );
        }
        void UnsetFlags ( int idx, int f )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in UnsetFlags" ) );
            REMOVE_BIT ( player_table[idx].flags, f );
        }
        bool IsSet ( int idx, int f )
        {
            if ( !ValidIndex ( idx ) )
                throw ( MudException ( "Index out of range in IsSet" ) );
            return IS_SET ( player_table[idx].flags, f );
        }
        /** Standard Functions **/
        int Size()
        {
            return (int)player_table.size();
        }
        plrindex_it Begin()
        {
            return player_table.begin();
        }
        plrindex_it End()
        {
            return player_table.end();
        }

    private:
        FILE *player_fl;       /* file desc of player file      */
        int top_of_p_table;       /* ref to top of table           */
        int top_of_p_file;        /* ref of size of p file         */
        plrindx player_table; /* index to plr file     */
        plrindx plr_nam_index; /* duplicate of the plr index file, only Sorted by name */

};

struct goldSort
{
    bool operator() ( const player_index_element &a, const player_index_element &b )
    {
        return a.gc_amount > b.gc_amount;
    }
};
struct tokenSort
{
    bool operator() ( const player_index_element &a, const player_index_element &b )
    {
        return a.gt_amount > b.gt_amount;
    }
};

struct nameSort
{
    bool operator() ( const player_index_element &a, const player_index_element &b )
    {
        return str_cmp ( a.name,b.name );
    }
};

extern PlayerIndex pi;
#define player_table  (pi.player_table)

#endif
