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
    const auto &qc = OLC_QC ( d );

    string qflags;
    if ( qc->questflags.size() == 0 )
        qflags = "{cc<none>{c0";
    else
        for ( const auto &qf : qc->questflags )
        {
            qflags += "\r\n{cy   " + qf.name + " {c0={cy " + qf.value + "\r\n{c0   Resets:{cy ";
            for ( const auto &b : qf.resets )
                if ( b )
                    qflags += "yes ";
                else
                    qflags += "no ";
            qflags += "{c0";
        }

    vector<string> names ( qc->function_triggers.size(), "{cc<none>{c0" );
    for ( int i = 0; i < qc->function_triggers.size(); ++i )
    {
        if ( qc->function_triggers[i] != NOTHING )
        {
            int rnum = real_trigger ( qc->function_triggers[i] );
            if ( rnum != NOTHING )
                names[i] = "{cy" + string ( GET_TRIG_NAME ( trig_index[ rnum ]->proto ) ) + "{c0";
        }
    }

    string order = "{cc<none>{c0";
    if ( qc->order.size() > 0 )
    {
        order = "{cy" + to_string ( qc->order[0] + 3 );
        for ( int i = 1; i < qc->order.size(); ++i )
            order += " " + to_string ( qc->order[i] + 3 );
        order += "{c0";
    }

    string commands;
    if ( qc->commands.size() == 0 )
        commands = "{cc<none>{c0";
    else
    {
        for ( const auto &c : qc->commands )
        {
            commands += "\r\n{cy   " + c.first + " {c0- [{cc" + to_string ( c.second ) + "{c0] ";
            int rnum = real_trigger ( c.second );
            if ( rnum == NOTHING )
                commands += "{cc<none>{c0";
            else
                commands += "{cy" + string ( GET_TRIG_NAME ( trig_index[ rnum ]->proto ) ) + "{c0";
        }
    }

    get_char_colours ( d->character );
    clear_screen ( d );

    d->Output ( "{cyDon't forget to turn colourcode on when you're going to copy-paste.{c0\r\n\r\n" );
    d->Output (
        "-- Questcard number [%s%d%s]\r\n"
        "%s0%s) Name         : %s%s\r\n"
        "%s1%s) Questflags   : %s\r\n"
        "%s2%s) Description  : \r\n%s%s\r\n"
        "%s3%s) Available    : [%s%d%s] %s\r\n"
        "%s4%s) Completed    : [%s%d%s] %s\r\n"
        "%s5%s) Traders      : [%s%d%s] %s\r\n"
        "%s6%s) Achievements : [%s%d%s] %s\r\n"
        "%s7%s) Other        : [%s%d%s] %s\r\n"
        "%s8%s) Unique       : [%s%d%s] %s\r\n"
        "%s9%s) Debug        : [%s%d%s] %s\r\n"
        "%sA%s) Order        : %s\r\n"
        "%sB%s) Commands     : %s\r\n"
        "%sQ%s) Quit\r\n"
        "Enter choice:\r\n",

        cyn, OLC_NUM ( d ), nrm,
        grn, nrm, yel, qc->name.c_str(),
        grn, nrm, qflags.c_str(),
        grn, nrm, yel, qc->description.c_str(),
        grn, nrm, cyn, qc->function_triggers[0], nrm, names[0].c_str(),
        grn, nrm, cyn, qc->function_triggers[1], nrm, names[1].c_str(),
        grn, nrm, cyn, qc->function_triggers[2], nrm, names[2].c_str(),
        grn, nrm, cyn, qc->function_triggers[3], nrm, names[3].c_str(),
        grn, nrm, cyn, qc->function_triggers[4], nrm, names[4].c_str(),
        grn, nrm, cyn, qc->function_triggers[5], nrm, names[5].c_str(),
        grn, nrm, cyn, qc->function_triggers[6], nrm, names[6].c_str(),
        grn, nrm, order.c_str(),
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
        free ( d->olc );
    }

    CREATE ( d->olc, oasis_olc_data, 1 );
    OLC_QC ( d ) = unique_ptr<questcard> (new questcard);
    OLC_NUM ( d ) = number;

    auto it = questcards.find ( number );
    if ( it != questcards.end() )
        *( OLC_QC ( d ) ) = it->second;
    else
    {
        OLC_QC ( d )->name = "Questcard " + to_string ( number );
        OLC_QC ( d )->function_triggers = vector<int> ( 8, NOTHING );
    }
    if ( OLC_QC ( d )->description == "" )
        OLC_QC ( d )->description = "<none>";

    act ( "$n starts using OLC.", TRUE, ch, 0, 0, TO_ROOM );
    SET_BIT_AR ( PLR_FLAGS ( ch ), PLR_WRITING );
    new_mudlog ( BRF, LVL_IMMORT, TRUE, "OLC: %s starts editing questcard %d", GET_NAME ( ch ), number );
    qedit_disp_menu ( d );
}

void qedit_disp_command_menu ( Descriptor *d )
{
    string set = "Not set", command = "{cc<none>{c0", name = "{cc<none>{c0";
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

            if ( it->first != "<none>" )
                command = "{cy" + it->first + "{c0";
            vnum = it->second;
            if ( real_trigger ( vnum ) != NOTHING )
                name = "{cy" + string ( GET_TRIG_NAME ( trig_index[ real_trigger ( vnum ) ]->proto ) ) + "{c0";
        }
    }

    d->Output (
        "%s1%s) Command: %s\r\n"
        "%s2%s) Function trigger: [%s%d%s] %s\r\n"
        "%s3%s) Goto next command: %s.\r\n"
        "%s4%s) Delete\r\n"
        "%sQ%s) Quit\r\n"
        "Enter choice:\r\n",

        grn, nrm, command.c_str(),
        grn, nrm, cyn, vnum, nrm, name.c_str(),
        grn, nrm, set.c_str(),
        grn, nrm,
        grn, nrm
    );

    OLC_MODE ( d ) = QEDIT_COMMAND_MENU;
}

void qedit_disp_questflag_menu ( Descriptor *d )
{
    string name, value, resets, set = "Not set";

    if ( OLC_QC ( d )->questflags.size() > 0 && OLC_VAL ( d ) >= 0 && OLC_VAL ( d ) < OLC_QC ( d )->questflags.size() )
    {
        if ( OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].name == "" )
            name = "{cc<none>{c0";
        else
            name = "{cy" + OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].name + "{c0";

        if ( OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].value == "" )
            value = "{cc<none>{c0";
        else
            value = "{cy" + OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].value + "{c0";

        resets = "";
        for ( const auto &b : OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].resets )
            if ( b )
                resets += "{cyyes ";
            else
                resets += "{cyno ";
        if ( resets == "" )
            resets = "{cc<none>{c0";
        else
            resets += "{c0";

        if ( OLC_VAL ( d ) < OLC_QC ( d )->questflags.size()-1 )
            set = "Set";
    }

    d->Output (
        "%s1%s) Questflag name: %s\r\n"
        "%s2%s) Variables: %s\r\n"
        "%s3%s) Resets: %s\r\n"
        "%s4%s) Goto next questflag: %s.\r\n"
        "%s5%s) Delete\r\n"
        "%sQ%s) Quit\r\n"
        "Enter choice:\r\n",

        grn, nrm, name.c_str(),
        grn, nrm, value.c_str(),
        grn, nrm, resets.c_str(),
        grn, nrm, set.c_str(),
        grn, nrm,
        grn, nrm
    );

    OLC_MODE ( d ) = QEDIT_QUESTFLAG_MENU;
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

    const auto &qc = questcards[ num ];

    string qf;
    if ( qc.questflags.size() > 0 )
    {
        for ( const auto &flag : qc.questflags )
        {
            qf += "\n" + flag.name + "\n" + flag.value + "\n";
            for ( const auto &b : flag.resets )
                if ( b )
                    qf += "yes ";
                else
                    qf += "no ";
        }
        qf += "\n~";
    }

    string order;
    if ( qc.order.size() > 0 )
    {
        order = to_string ( qc.order[0] );
        for ( int i = 1; i < qc.order.size(); ++i )
            order += " " + to_string ( qc.order[i] );
    }

    file << "Name: "         << qc.name << endl;
    if ( qc.questflags.size() > 0 )
        file << "Questflags: "   << qf.c_str() << endl;
    file << "Description: "  << endl << qc.description << endl << "~" << endl;
    if ( qc.function_triggers[0] != NOTHING )
        file << "Available: "    << qc.function_triggers[0] << endl;
    if ( qc.function_triggers[1] != NOTHING )
        file << "Completed: "    << qc.function_triggers[1] << endl;
    if ( qc.function_triggers[2] != NOTHING )
        file << "Traders: "      << qc.function_triggers[2] << endl;
    if ( qc.function_triggers[3] != NOTHING )
        file << "Achievements: " << qc.function_triggers[3] << endl;
    if ( qc.function_triggers[4] != NOTHING )
        file << "Other: "        << qc.function_triggers[4] << endl;
    if ( qc.function_triggers[5] != NOTHING )
        file << "Unique: "       << qc.function_triggers[5] << endl;
    if ( qc.function_triggers[6] != NOTHING )
        file << "Debug: "        << qc.function_triggers[6] << endl;
    if ( qc.order.size() > 0 )
        file << "Order: " << order.c_str() << endl;
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
                    OLC_VAL ( d ) = 0; // used as a questflags index
                    if ( OLC_QC ( d )->questflags.size() == 0 )
                        OLC_QC ( d )->questflags.push_back ( questflag() );
                    qedit_disp_questflag_menu ( d );
                    return;
                case '2':
                {
                    OLC_MODE ( d ) = QEDIT_DESCRIPTION;
                    send_editor_help ( d );
                    d->Output ( "Enter the description:\r\n\r\n" );

                    char *oldtext = nullptr;
                    if ( OLC_QC ( d )->description != "" )
                    {
                        oldtext = strdup ( OLC_QC ( d )->description.c_str() );
                        if ( OLC_STORAGE ( d ) )
                            free ( OLC_STORAGE ( d ) );
                        OLC_STORAGE ( d ) = strdup ( oldtext );
                        d->Output ( "%s", oldtext );
                    }
                    string_write ( d, &OLC_STORAGE ( d ), MAX_MESSAGE_LENGTH, 0, oldtext );
                    return;
                }
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
                    OLC_VAL ( d ) = 0; // used as a commands index
                    qedit_disp_command_menu ( d );
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
        case QEDIT_QUESTFLAG_MENU:
            switch  ( *arg )
            {
                case '1':
                    OLC_MODE ( d ) = QEDIT_QUESTFLAG_NAME;
                    d->Output ( "Enter the questflag name:\r\n" );
                    return;
                case '2':
                    OLC_MODE ( d ) = QEDIT_QUESTFLAG_VALUE;
                    d->Output ( "Enter the variables separated by a space:\r\n" );
                    return;
                case '3':
                    OLC_MODE ( d ) = QEDIT_QUESTFLAG_RESETS;
                    d->Output ( "Enter the resets (e.g. 'yes no yes'):\r\n" );
                    return;
                case '4':
                    OLC_VAL ( d )++;
                    if ( OLC_VAL ( d ) == OLC_QC ( d )->questflags.size() )
                        OLC_QC ( d )->questflags.push_back ( questflag() );
                    qedit_disp_questflag_menu ( d );
                    return;
                case '5':
                    OLC_QC ( d )->questflags.erase ( OLC_QC ( d )->questflags.begin() + OLC_VAL ( d ) );
                    OLC_VAL ( d ) = 1;
                    qedit_disp_menu ( d );
                    return;
                default:
                {
                    const auto &qf = OLC_QC ( d )->questflags[ OLC_VAL ( d ) ];
                    if ( qf.name == "" || qf.value == "" || qf.resets.size() == 0 )
                    {
                        d->Output ( "This questflag is not finished!\r\n" );
                        qedit_disp_questflag_menu ( d );
                        return;
                    }

                    int count_vars = 0;
                    stringstream ss ( qf.value );
                    string s;
                    while ( ss >> s )
                        count_vars++;

                    if ( count_vars != qf.resets.size() )
                    {
                        d->Output ( "The number of variables doesn't equal the number of resets!\r\n" );
                        qedit_disp_questflag_menu ( d );
                        return;
                    }

                    OLC_VAL ( d ) = 1;
                    qedit_disp_menu ( d );
                    return;
                }
            }
        case QEDIT_QUESTFLAG_NAME:
            if ( *arg )
            {
                string s = string ( arg ), name;
                stringstream ss ( s );
                ss >> name;
                bool name_exists = FALSE;
                for ( const auto &qf : OLC_QC ( d )->questflags )
                    if ( qf.name == name )
                    {
                        name_exists = TRUE;
                        break;
                    }
                if ( name_exists )
                    d->Output ( "Questflag '%s' already exists.\r\n", name.c_str() );
                else
                    OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].name = name;
            }
            else
                OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].name = "";
            qedit_disp_questflag_menu ( d );
            return;
        case QEDIT_QUESTFLAG_VALUE:
            if ( *arg )
                OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].value = string ( arg );
            else
                OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].value = "";
            qedit_disp_questflag_menu ( d );
            return;
        case QEDIT_QUESTFLAG_RESETS:
            OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].resets.clear();
            if ( *arg )
            {
                string s = string ( arg );
                stringstream ss ( s );
                while ( ss >> s )
                {
                    switch ( s[0] )
                    {
                        case 'y':
                        case 'Y':
                            OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].resets.push_back ( TRUE );
                            break;
                        case 'n':
                        case 'N':
                            OLC_QC ( d )->questflags[ OLC_VAL ( d ) ].resets.push_back ( FALSE );
                            break;
                        default:
                            d->Output ( "The resets are either 'yes' or 'no'.\r\n" );
                            qedit_disp_questflag_menu ( d );
                            return;
                    }
                }
            }
            qedit_disp_questflag_menu ( d );
            return;
        case QEDIT_FUNCTION_TRIGGER: // fallthrough
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
                    // the first function trigger is option 3 in the menu
                    OLC_QC ( d )->function_triggers[ OLC_VAL ( d ) - 3 ] = vnum;
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
                        {
                            OLC_QC ( d )->commands[ "<none>" ] = vnum;
                            // set OLC_VAL ( d ) to this command
                            auto it = OLC_QC ( d )->commands.begin();
                            for ( OLC_VAL ( d ) = 0; it->first != "<none>"; OLC_VAL ( d )++ )
                                it++;
                        }
                        else
                            it->second = vnum;
                    }
                }
            }
            if ( OLC_MODE ( d ) == QEDIT_FUNCTION_TRIGGER )
                break;
            else // QEDIT_COMMAND_FUNCTION_TRIGGER
            {
                qedit_disp_command_menu ( d );
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
                    if ( x < 3 || x > 7 )
                    {
                        all_good = FALSE;
                        break;
                    }
                    else
                        order.push_back ( x - 3 );
                }
                if ( all_good )
                {
                    OLC_QC ( d )->order = move ( order );
                    OLC_VAL ( d ) = 1;
                }
                else
                    d->Output ( "Illegal order value, it must lie between 3 and 7.\r\n" );
            }
            else
            {
                OLC_QC ( d )->order.clear();
                OLC_VAL ( d ) = 1;
            }
            break;
        case QEDIT_COMMAND_MENU:
            switch ( *arg )
            {
                case '1':
                    OLC_MODE ( d ) = QEDIT_COMMAND;
                    d->Output ( "Enter the command:\r\n" );
                    return;
                case '2':
                    OLC_MODE ( d ) = QEDIT_COMMAND_FUNCTION_TRIGGER;
                    d->Output ( "Enter the function trigger vnum:\r\n" );
                    return;
                case '3':
                {
                    auto it = OLC_QC ( d )->commands.find ( "<none>" );
                    if ( it != OLC_QC ( d )->commands.end() )
                    {
                        d->Output ( "The command hasn't been specified!\r\n" );
                        qedit_disp_command_menu ( d );
                        return;
                    }

                    it = OLC_QC ( d )->commands.begin();
                    for ( int i = 0; i < OLC_VAL ( d ); ++i )
                        it++;
                    if ( it->second == NOTHING )
                    {
                        d->Output ( "The trigger hasn't been specified!\r\n" );
                        qedit_disp_command_menu ( d );
                        return;
                    }

                    if ( OLC_VAL ( d ) < OLC_QC ( d )->commands.size() )
                        OLC_VAL ( d )++;
                    qedit_disp_command_menu ( d );
                    return;
                }
                case '4':
                {
                    auto it = OLC_QC ( d )->commands.begin();
                    for ( int i = 0; i < OLC_VAL ( d ); ++i )
                        it++;
                    if ( it != OLC_QC ( d )->commands.end() )
                        OLC_QC ( d )->commands.erase ( it );
                    OLC_VAL ( d ) = 1;
                    qedit_disp_menu ( d );
                    return;
                }
                default:
                {
                    auto it = OLC_QC ( d )->commands.find ( "<none>" );
                    if ( it != OLC_QC ( d )->commands.end() )
                    {
                        d->Output ( "The command hasn't been specified!\r\n" );
                        qedit_disp_command_menu ( d );
                        return;
                    }

                    it = OLC_QC ( d )->commands.begin();
                    for ( int i = 0; i < OLC_VAL ( d ); ++i )
                        it++;
                    if ( it->second == NOTHING )
                    {
                        d->Output ( "The trigger hasn't been specified!\r\n" );
                        qedit_disp_command_menu ( d );
                        return;
                    }

                    OLC_VAL ( d ) = 1;
                    qedit_disp_menu ( d );
                    return;
                }
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
                            qedit_disp_command_menu ( d );
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

                // set OLC_VAL ( d ) to this command
                auto it = OLC_QC ( d )->commands.begin();
                for ( OLC_VAL ( d ) = 0; it->first != cmd; OLC_VAL ( d )++ )
                    it++;
            }
            qedit_disp_command_menu ( d );
            return;
        case QEDIT_CONFIRM_SAVESTRING:
            switch ( *arg )
            {
                case 'y':
                case 'Y':
                    if ( OLC_QC ( d )->order.size() == 0 && OLC_QC ( d )->function_triggers[5] == NOTHING )
                    {
                        d->Output ( "The order or unique trigger hasn't been set!\r\n" );
                        qedit_disp_menu ( d );
                        return;
                    }
                    if ( OLC_QC ( d )->order.size() > 0 )
                    {
                        for ( const auto &i : OLC_QC ( d )->order )
                            if ( OLC_QC ( d )->function_triggers[i] == NOTHING )
                            {
                                d->Output ( "The order has an empty function trigger!\r\n" );
                                qedit_disp_menu ( d );
                                return;
                            }
                    }
                    for ( const auto &cmd : OLC_QC ( d )->commands )
                        if ( cmd.second == NOTHING )
                        {
                            d->Output ( "Command '%s' doesn't have a trigger set!\r\n", cmd.first.c_str() );
                            qedit_disp_menu ( d );
                            return;
                        }
                    /*
                     * Save the questcard in memory and to disk.
                     */
                    qedit_save_internally ( d );
                    new_mudlog ( CMP, MAX ( LVL_BUILDER, GET_INVIS_LEV ( d->character ) ), TRUE,
                        "OLC: %s saves questcard %d", GET_NAME ( d->character ), OLC_NUM ( d ) );
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

void qedit_string_cleanup ( Descriptor *d, int terminator )
{
    switch ( OLC_MODE ( d ) )
    {
        case QEDIT_DESCRIPTION:
            if ( OLC_STORAGE ( d ) )
            {
                string &desc = OLC_QC ( d )->description;
                desc = string ( OLC_STORAGE ( d ) );
                // remove the trailing \r\n
                if ( desc.size() >= 2 && desc[ desc.size()-2 ] == '\r' )
                    desc = desc.substr ( 0, desc.size() - 2 );
                free ( OLC_STORAGE ( d ) );
                OLC_STORAGE ( d ) = nullptr;
            }
            OLC_VAL ( d ) = 1;
            qedit_disp_menu ( d );
            break;
        default:
            break;
    }
}

