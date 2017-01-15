#include "config.h"
#include "sysdep.h"
#include "structs.h"
#include "utils.h"
#include "db.h"
#include "comm.h"
#include "descriptor.h"
#include "dg_scripts.h"
#include "oasis.h"
#include "interpreter.h"
#include "improved-edit.h"
#include "boards.h"

map <int, questcard> questcards;

void qedit_disp_menu ( Descriptor *d )
{
    auto qc = OLC_QC ( d );

    string qflags = "{cc<none>{c0";
    if ( qc->questflags.size() > 0 )
    {
        qflags = "{cy"+ qc->questflags[0];
        for ( int i = 1; i < qc->questflags.size(); ++i )
            qflags += " " + qc->questflags[i];
        qflags += "{c0";
    }

    vector<string> names;
    for ( int i = 0; i < qc->function_triggers.size(); ++i )
        if ( qc->function_triggers[i] == NOTHING )
        {
            if ( i == 5 )
                names.push_back ( "{ccusing generic trigger{c0" );
            else
                names.push_back ( "{cc<none>{c0" );
        }
        else
        {
            int rnum = real_trigger ( qc->function_triggers[i] );
            if ( rnum == NOTHING )
                names.push_back ( "{cc<none>{c0" );
            else
                names.push_back ( "{cy" + string ( GET_TRIG_NAME ( trig_index[ rnum ]->proto ) ) + "{c0" );
        }
    while ( names.size() < 8 )
        names.push_back ( "{cc<none>{c0" );

    string order = "{cc<none>{c0";
    if ( qc->order.size() > 0 )
    {
        order = "{cy" + to_string ( qc->order[0] + 2 );
        for ( int i = 1; i < qc->order.size(); ++i )
            order += " " + to_string ( qc->order[i] + 2 );
        order += "{c0";
    }

    string debug;
    if ( qc->debug.size() == 0 )
        debug = "{cc<none>{c0";
    else
        for ( const auto &dbg : qc->debug )
            debug += "[{cy" + dbg.first + "{c0] ";

    string commands;
    if ( qc->commands.size() == 0 )
        commands = "{cc<none>{c0";
    else
        for ( const auto &c : qc->commands )
            commands += "[{cy" + c.first + "{c0] ";

    get_char_colours ( d->character );
    clear_screen ( d );

    d->Output ( "{cyDon't forget to turn colourcode on when you're going to copy-paste.{c0\r\n\r\n" );
    d->Output (
        "-- Questcard number [%s%d%s]\r\n"
        "%s0%s) Name         : %s%s\r\n"
        "%s1%s) Questflags   : %s\r\n"
        "%s2%s) Description  : [%s%d%s] %s\r\n"
        "%s3%s) Available    : [%s%d%s] %s\r\n"
        "%s4%s) Completed    : [%s%d%s] %s\r\n"
        "%s5%s) Traders      : [%s%d%s] %s\r\n"
        "%s6%s) Achievements : [%s%d%s] %s\r\n"
        "%s7%s) Other        : [%s%d%s] %s\r\n"
        "%s8%s) Unique       : [%s%d%s] %s\r\n"
        "%s9%s) Reset        : [%s%d%s] %s\r\n"
        "%sA%s) Order        : %s\r\n"
        "%sB%s) Debug        : %s\r\n"
        "%sC%s) Commands     : %s\r\n"
        "%sQ%s) Quit\r\n"
        "Enter choice:\r\n",

        cyn, OLC_NUM ( d ), nrm,
        grn, nrm, yel, qc->name.c_str(),
        grn, nrm, qflags.c_str(),
        grn, nrm, cyn, qc->function_triggers[0], nrm, names[0].c_str(),
        grn, nrm, cyn, qc->function_triggers[1], nrm, names[1].c_str(),
        grn, nrm, cyn, qc->function_triggers[2], nrm, names[2].c_str(),
        grn, nrm, cyn, qc->function_triggers[3], nrm, names[3].c_str(),
        grn, nrm, cyn, qc->function_triggers[4], nrm, names[4].c_str(),
        grn, nrm, cyn, qc->function_triggers[5], nrm, names[5].c_str(),
        grn, nrm, cyn, qc->function_triggers[6], nrm, names[6].c_str(),
        grn, nrm, cyn, qc->function_triggers[7], nrm, names[7].c_str(),
        grn, nrm, order.c_str(),
        grn, nrm, debug.c_str(),
        grn, nrm, commands.c_str(),
        grn, nrm
    );

    OLC_MODE ( d ) = QEDIT_MAIN_MENU;
}

ACMD ( do_oasis_qedit )
{
    char buf[MAX_STRING_LENGTH];

    one_argument ( argument, buf );
    if ( !*buf || !is_number ( buf ) )
    {
        ch->Send ( "Usage: qedit <questcard_number>\r\n" );
        return;
    }

    int number = atoi ( buf );

    if ( number <= 0 )
    {
        ch->Send ( "Illegal card number: %d.\r\n", number );
        return;
    }

    for ( Descriptor *d = descriptor_list; d; d = d->next )
    {
        if ( STATE ( d ) == CON_QEDIT )
        {
            if ( d->olc && OLC_NUM ( d ) == number )
            {
                ch->Send ( "That questcard is currently being edited by %s.\r\n", GET_NAME ( d->character ) );
                return;
            }
        }
    }

    Descriptor *d = ch->desc;
    STATE ( d ) = CON_QEDIT;
    if ( d->olc )
    {
        new_mudlog ( BRF, LVL_IMMORT, TRUE, "SYSERR: do_oasis_qedit: %s already had olc structure.", GET_NAME ( ch ) );
        if ( OLC_QC ( d ) )
            free ( OLC_QC ( d ) );
        free ( d->olc );
    }

    CREATE ( d->olc, oasis_olc_data, 1 );
    CREATE ( OLC_QC ( d ), questcard, 1 );
    OLC_NUM ( d ) = number;

    auto it = questcards.find ( number );
    if ( it != questcards.end() )
        *( OLC_QC ( d ) ) = it->second;
    else
    {
        OLC_QC ( d )->name = "Questcard " + to_string ( number );
        OLC_QC ( d )->function_triggers = vector<int> ( 8, NOTHING );
    }

    act ( "$n starts using OLC.", TRUE, ch, 0, 0, TO_ROOM );
    SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_WRITING );
    new_mudlog ( BRF, LVL_IMMORT, TRUE, "OLC: %s starts editing questcard %d", GET_NAME ( ch ), number );
    qedit_disp_menu ( d );
}

void qedit_disp_debug_menu ( Descriptor *d )
{
    string set = "Not set", command = "<none>", name = "<none>";
    trig_vnum vnum = NOTHING;

    if ( OLC_QC ( d )->debug.size() > 0 )
    {
        auto it = OLC_QC ( d )->debug.begin();
        for ( int i = 0; i < OLC_VAL ( d ); ++i )
            it++;

        if ( it != OLC_QC ( d )->debug.end() )
        {
            auto it_tmp = it;
            it_tmp++;
            if ( it_tmp != OLC_QC ( d )->debug.end() )
                set = "Set";
            command = it->first;
            vnum = it->second;
            if ( real_trigger ( vnum ) == NOTHING )
                name = "<none>";
            else
                name = string ( GET_TRIG_NAME ( trig_index[ real_trigger ( vnum ) ]->proto ) );
        }
    }

    d->Output (
        "%s1%s) Debug command: %s%s\r\n"
        "%s2%s) Function trigger: [%s%d%s] %s%s\r\n"
        "%s3%s) Goto next command: %s.\r\n"
        "%sQ%s) Quit\r\n"
        "Enter choice:\r\n",

        grn, nrm, yel, command.c_str(),
        grn, nrm, cyn, vnum, nrm, cyn, name.c_str(),
        grn, nrm, set.c_str(),
        grn, nrm
    );

    OLC_MODE ( d ) = QEDIT_DEBUG_MENU;
}

void qedit_disp_commands_menu ( Descriptor *d )
{
    string set = "Not set", command = "<none>", name = "<none>";
    trig_vnum vnum = NOTHING;

    if ( OLC_QC ( d )->commands.size() > 0 )
    {
        auto it = OLC_QC ( d )->commands.begin();
        for ( int i = 0; i < OLC_VAL ( d ); ++i )
            it++;

        if ( it != OLC_QC ( d )->commands.end() )
        {
            auto it_tmp = it;
            it_tmp++;
            if ( it_tmp != OLC_QC ( d )->commands.end() )
                set = "Set";
            command = it->first;
            vnum = it->second;
            if ( real_trigger ( vnum ) == NOTHING )
                name = "<none>";
            else
                name = string ( GET_TRIG_NAME ( trig_index[ real_trigger ( vnum ) ]->proto ) );
        }
    }

    d->Output (
        "%s1%s) Command: %s%s\r\n"
        "%s2%s) Function trigger: [%s%d%s] %s%s\r\n"
        "%s3%s) Goto next command: %s.\r\n"
        "%sQ%s) Quit\r\n"
        "Enter choice:\r\n",

        grn, nrm, yel, command.c_str(),
        grn, nrm, cyn, vnum, nrm, cyn, name.c_str(),
        grn, nrm, set.c_str(),
        grn, nrm
    );

    OLC_MODE ( d ) = QEDIT_COMMANDS_MENU;
}

void qedit_save_internally ( Descriptor *d )
{
    int num = OLC_NUM ( d );
    questcards[ num ] = *OLC_QC ( d );
}

void save_questcard ( int num )
{
    string filename = string ( LIB_QUESTCARDS ) + to_string ( num );
    ofstream file ( filename );
    if ( file.bad() )
    {
        new_mudlog ( BRF, LVL_BUILDER, TRUE, "SYSERR: save_questcard couldn't write to file %s", filename.c_str() );
        return;
    }

    const auto qc = questcards[ num ];

    string qf;
    if ( qc.questflags.size() > 0 )
    {
        qf = qc.questflags[0];
        for ( int i = 1; i < qc.questflags.size(); ++i )
            qf += " " + qc.questflags[i];
    }

    string order;
    if ( qc.order.size() > 0 )
    {
        order = to_string ( qc.order[0] );
        for ( int i = 1; i < qc.order.size(); ++i )
            order += " " + to_string ( qc.order[i] );
    }

    file << "Name: "         << qc.name << endl;
    file << "Questflags: "   << qf.c_str() << endl;
    file << "Description: "  << qc.function_triggers[0] << endl;
    file << "Available: "    << qc.function_triggers[1] << endl;
    file << "Completed: "    << qc.function_triggers[2] << endl;
    file << "Traders: "      << qc.function_triggers[3] << endl;
    file << "Achievements: " << qc.function_triggers[4] << endl;
    file << "Other: "        << qc.function_triggers[5] << endl;
    file << "Unique: "       << qc.function_triggers[6] << endl;
    file << "Reset: "        << qc.function_triggers[7] << endl;
    file << "Order: "        << order.c_str() << endl;
    if ( qc.debug.size() > 0 )
    {
        file << "Debug:\r\n";
        for ( const auto &dbg : qc.debug )
            file << dbg.first << " " << dbg.second << endl;
        file << "~" << endl;
    }
    if ( qc.commands.size() > 0 )
    {
        file << "Commands:\r\n";
        for ( const auto &cmd : qc.commands )
            file << cmd.first << " " << cmd.second << endl;
    }
    file << "$";
    file.close();
}

void qedit_save_to_disk ( int num )
{
    save_questcard ( num );
}

void qedit_parse ( Descriptor *d, char *arg )
{
    int i = 0;
    switch ( OLC_MODE ( d ) )
    {
        case QEDIT_MAIN_MENU:
            switch ( *arg )
            {
                case '0':
                    OLC_MODE ( d ) = QEDIT_NAME;
                    i = -1;
                    break;
                case '1':
                    OLC_MODE ( d ) = QEDIT_QUESTFLAGS;
                    d->Output ( "Enter the questflags separated by a space:\r\n" );
                    return;
                case '2': // fallthrough
                case '3': // fallthrough
                case '4': // fallthrough
                case '5': // fallthrough
                case '6': // fallthrough
                case '7': // fallthrough
                case '8': // fallthrough
                case '9':
                    OLC_MODE ( d ) = QEDIT_FUNCTION_TRIGGER;
                    OLC_VAL ( d ) = atoi ( arg );
                    i = 1;
                    break;
                case 'a':
                case 'A':
                    OLC_MODE ( d ) = QEDIT_ORDER;
                    d->Output ( "Enter the order in which the function triggers are called (e.g. '5 3 6'):\r\n" );
                    return;
                case 'b':
                case 'B':
                    OLC_VAL ( d ) = 0; // used as a debug index
                    qedit_disp_debug_menu ( d );
                    return;
                case 'c':
                case 'C':
                    OLC_VAL ( d ) = 0; // used as a commands index
                    qedit_disp_commands_menu ( d );
                    return;
                case 'q':
                case 'Q':
                    if ( OLC_VAL ( d ) )  	/* Anything been changed? */
                    {
                        d->Output ( "Do you wish to save the changes to the questcard? (y/n) : " );
                        OLC_MODE ( d ) = QEDIT_CONFIRM_SAVESTRING;
                    }
                    else
                        cleanup_olc ( d, CLEANUP_ALL );
                    return;
                default:
                    qedit_disp_menu ( d );
                    return;
            }
            if ( i == 0 )
                break;
            else if ( i == 1 )
                d->Output ( "\r\nEnter new value: " );
            else if ( i == -1 )
                d->Output ( "\r\nEnter new text:\r\n] " );
            else
                d->Output ( "Oops...\r\n" );
            return;
        case QEDIT_NAME:
            if ( *arg )
                OLC_QC ( d )->name = string ( arg );
            break;
        case QEDIT_QUESTFLAGS:
            if ( *arg )
            {
                vector<string> qf;
                string s = string ( arg );
                stringstream ss ( s );
                while ( ss >> s )
                    qf.push_back ( s );
                OLC_QC ( d )->questflags = qf;
            }
            break;
        case QEDIT_FUNCTION_TRIGGER:       // fallthrough
        case QEDIT_DEBUG_FUNCTION_TRIGGER: // fallthrough
        case QEDIT_COMMAND_FUNCTION_TRIGGER:
            if ( *arg )
            {
                int vnum = atoi ( arg );
                int rnum = real_trigger ( vnum );
                if ( rnum == NOTHING && vnum != NOTHING )
                    d->Output ( "That trigger doesn't exist.\r\n" );
                else if ( vnum != NOTHING && !TRIGGER_CHECK ( trig_index[rnum]->proto, MTRIG_FUNCTION ) )
                    // function trigger types on mob/room/obj all have the same value
                    d->Output ( "Trigger %d is not a funcion trigger.\r\n", vnum );
                else if ( OLC_MODE ( d ) == QEDIT_FUNCTION_TRIGGER )
                    // the first function trigger is option 2 in the menu
                    OLC_QC ( d )->function_triggers[ OLC_VAL ( d ) - 2 ] = vnum;
                else if ( OLC_MODE ( d ) == QEDIT_DEBUG_FUNCTION_TRIGGER )
                {
                    if ( OLC_QC ( d )->debug.size() == 0 )
                        OLC_QC ( d )->debug[ "<none>" ] = vnum;
                    else
                    {
                        auto it = OLC_QC ( d )->debug.begin();
                        for ( int i = 0; i < OLC_VAL ( d ); ++i )
                            it++;
                        if ( it == OLC_QC ( d )->debug.end() )
                            OLC_QC ( d )->debug[ "<none>" ] = vnum;
                        else
                            it->second = vnum;
                    }
                }
                else // QEDIT_COMMAND_FUNCTION_TRIGGER
                {
                    if ( OLC_QC ( d )->commands.size() == 0 )
                        OLC_QC ( d )->commands[ "<none>" ] = vnum;
                    else
                    {
                        auto it = OLC_QC ( d )->commands.begin();
                        for ( int i = 0; i < OLC_VAL ( d ); ++i )
                            it++;
                        if ( it == OLC_QC ( d )->commands.end() )
                            OLC_QC ( d )->commands[ "<none>" ] = vnum;
                        else
                            it->second = vnum;
                    }
                }
            }
            if ( OLC_MODE ( d ) == QEDIT_FUNCTION_TRIGGER )
                break;
            else if ( OLC_MODE ( d ) == QEDIT_DEBUG_FUNCTION_TRIGGER )
            {
                qedit_disp_debug_menu ( d );
                return;
            }
            else // QEDIT_COMMAND_FUNCTION_TRIGGER
            {
                qedit_disp_commands_menu ( d );
                return;
            }
        case QEDIT_ORDER:
            if ( *arg )
            {
                int x;
                vector<int> order;
                string s = string ( arg );
                stringstream ss ( s );
                bool all_good = TRUE;
                while ( ss >> x )
                {
                    if ( x < 2 || x > 7 )
                    {
                        all_good = FALSE;
                        break;
                    }
                    else
                        order.push_back ( x - 2 );
                }
                if ( all_good )
                    OLC_QC ( d )->order = order;
                else
                    d->Output ( "Illegal order value, it must lie between 2 and 7.\r\n" );
            }
            break;
        case QEDIT_DEBUG_MENU:
            switch ( *arg )
            {
                case '1':
                    OLC_MODE ( d ) = QEDIT_DEBUG_COMMAND;
                    d->Output ( "Enter debug command:\r\n" );
                    return;
                case '2':
                {
                    OLC_MODE ( d ) = QEDIT_DEBUG_FUNCTION_TRIGGER;
                    d->Output ( "Enter the function trigger vnum:\r\n" );
                    return;
                }
                case '3':
                    if ( OLC_VAL ( d ) < OLC_QC ( d )->debug.size() )
                        OLC_VAL ( d )++;
                    qedit_disp_debug_menu ( d );
                    return;
                default:
                    OLC_VAL ( d ) = 1;
                    qedit_disp_menu ( d );
                    break;
            }
            break;
        case QEDIT_DEBUG_COMMAND:
            if ( *arg )
            {
                string cmd = string ( arg );
                cmd = cmd.substr ( 0, cmd.find ( ' ' ) );

                if ( OLC_QC ( d )->debug.size() == 0 )
                    OLC_QC ( d )->debug[ cmd ] = NOTHING;
                else
                {
                    auto it = OLC_QC ( d )->debug.begin();
                    int j = 0;
                    for ( const auto &dbg : OLC_QC ( d )->debug )
                    {
                        if ( cmd == dbg.first )
                        {
                            d->Output ( "Debug command %s already exists.\r\n", cmd.c_str() );
                            qedit_disp_debug_menu ( d );
                            return;
                        }
                        if ( j++ < OLC_VAL ( d ) )
                            it++;
                    }

                    if ( it != OLC_QC ( d )->debug.end() )
                    {
                        OLC_QC ( d )->debug[ cmd ] = it->second;
                        OLC_QC ( d )->debug.erase ( it );
                    }
                    else
                        OLC_QC ( d )->debug[ cmd ] = NOTHING;
                }
            }
            qedit_disp_debug_menu ( d );
            return;
        case QEDIT_COMMANDS_MENU:
            switch ( *arg )
            {
                case '1':
                    OLC_MODE ( d ) = QEDIT_COMMAND;
                    d->Output ( "Enter command:\r\n" );
                    return;
                case '2':
                    OLC_MODE ( d ) = QEDIT_COMMAND_FUNCTION_TRIGGER;
                    d->Output ( "Enter the function trigger vnum:\r\n" );
                    return;
                case '3':
                    if ( OLC_VAL ( d ) < OLC_QC ( d )->commands.size() )
                        OLC_VAL ( d )++;
                    qedit_disp_commands_menu ( d );
                    return;
                default:
                    OLC_VAL ( d ) = 1;
                    qedit_disp_menu ( d );
                    return;
            }
            break;
        case QEDIT_COMMAND:
            if ( *arg )
            {
                string cmd = string ( arg );
                cmd = cmd.substr ( 0, cmd.find ( ' ' ) );

                if ( OLC_QC ( d )->commands.size() == 0 )
                    OLC_QC ( d )->commands [ cmd ] = NOTHING;
                else
                {
                    int j = 0;
                    auto it = OLC_QC ( d )->commands.begin();
                    for ( const auto &c : OLC_QC ( d )->commands )
                    {
                        if ( cmd == c.first )
                        {
                            d->Output ( "Command %s already exists.\r\n", cmd.c_str() );
                            qedit_disp_commands_menu ( d );
                            return;
                        }
                        if ( j++ < OLC_VAL ( d ) )
                            it++;
                    }

                    if ( it != OLC_QC ( d )->commands.end() )
                    {
                        OLC_QC ( d )->commands [ cmd ] = it->second;
                        OLC_QC ( d )->commands.erase ( it );
                    }
                    else
                        OLC_QC ( d )->commands [ cmd ] = NOTHING;
                }
            }
            qedit_disp_commands_menu ( d );
            return;
        case QEDIT_CONFIRM_SAVESTRING:
            switch ( *arg )
            {
                case 'y':
                case 'Y':
                    if ( OLC_QC ( d )->order.size() == 0 && OLC_QC ( d )->function_triggers[6] == NOTHING )
                    {
                        d->Output ( "The order hasn't been set!\r\n" );
                        qedit_disp_menu ( d );
                        return;
                    }
                    /*
                     * Save the questcard in memory and to disk.
                     */
                    qedit_save_internally ( d );
                    new_mudlog ( CMP, MAX ( LVL_BUILDER, GET_INVIS_LEV ( d->character ) ), TRUE,
                        "OLC: %s finishes editing questcard %d", GET_NAME ( d->character ), OLC_NUM ( d ) );
                    if ( CONFIG_OLC_SAVE )
                    {
                        qedit_save_to_disk ( OLC_NUM ( d ) );
                        d->Output ( "Questcard saved to disk.\r\n" );
                    }
                    else
                        d->Output ( "Questcard saved to memory.\r\n" );
                    // fallthrough
                case 'n':
                case 'N':
                    cleanup_olc ( d, CLEANUP_ALL );
                    return;
                default:
                    d->Output ( "Invalid choice!\r\n" );
                    d->Output ( "Do you wish to save the questcard? : " );
                    return;
            }
            break;
        default:
            new_mudlog ( BRF, LVL_BUILDER, TRUE, "SYSERR: OLC: qedit_parse(): Reached default case!" );
            d->Output ( "Oops...\r\n" );
            cleanup_olc ( d, CLEANUP_ALL );
            return;
    }

    /*
     * END OF CASE
     * If we get here, we have probably changed something, and now want to
     * return to main menu.  Use OLC_VAL as a 'has changed' flag
     */

    if ( OLC_VAL ( d ) == 0 )
        OLC_VAL ( d ) = 1;
    qedit_disp_menu ( d );
}

