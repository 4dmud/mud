// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_SOCKETS_EXTENSIONs_CPP
#define DLIB_SOCKETS_EXTENSIONs_CPP

#include <string>
#include <sstream>
#include "../sockets.h"
#include "../error.h"
#include "sockets_extensions.h"
#include "../timer.h"
#include "../algs.h"
#include "../timeout.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    connection* connect (
        const std::string& host_or_ip,
        unsigned short port
    )
    {
        std::string ip;
        connection* con;
        if (is_ip_address(host_or_ip))
        {
            ip = host_or_ip;
        }
        else
        {
            if( hostname_to_ip(host_or_ip,ip))
                throw socket_error(ERESOLVE,"unable to resolve '" + host_or_ip + "' in connect()");
        }

        if(create_connection(con,port,ip))
            throw socket_error("unable to connect to '" + host_or_ip + "'"); 

        return con;
    }

// ----------------------------------------------------------------------------------------

    bool is_ip_address (
        std::string ip
    )
    {
        for (std::string::size_type i = 0; i < ip.size(); ++i)
        {
            if (ip[i] == '.')
                ip[i] = ' ';
        }
        std::istringstream sin(ip);
        
        bool bad = false;
        int num;
        for (int i = 0; i < 4; ++i)
        {
            sin >> num;
            if (!sin || num < 0 || num > 255)
            {
                bad = true;
                break;
            }
        }

        if (sin.get() != EOF)
            bad = true;
        
        return !bad;
    }

// ----------------------------------------------------------------------------------------

    void close_gracefully (
        connection* con,
        unsigned long timeout 
    )
    {
        if(con->shutdown_outgoing())
        {
            // there was an error so just close it now and return
            delete con;
            return;
        }

        try
        {
            timeout::kernel_1a t(*con,&connection::shutdown,timeout);

            char junk[100];
            // wait for the other end to close their side
            while (con->read(junk,sizeof(junk)) > 0);
        }
        catch (...)
        {
            delete con;
            throw;
        }

        delete con;
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_SOCKETS_EXTENSIONs_CPP


