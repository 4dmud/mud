// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_SOCKETS_KERNEL_2_CPp_
#define DLIB_SOCKETS_KERNEL_2_CPp_

#include "../platform.h"

#ifdef POSIX


#include "sockets_kernel_2.h"
#include <fcntl.h>
#include "../set.h"



namespace dlib
{
// ----------------------------------------------------------------------------------------

#ifdef HPUX
    typedef int dsocklen_t;
#else
    typedef socklen_t dsocklen_t;
#endif

// ----------------------------------------------------------------------------------------
// stuff to ensure that the signal SIGPIPE is ignored before any connections are made
// so that when a connection object is shutdown the program won't end on a broken pipe

    namespace sockets_kernel_2_mutex
    {
        mutex startup_lock;
    }


    void sockets_startup()
    {
        // mutex crap to make this function thread safe
        sockets_kernel_2_mutex::startup_lock.lock();
        static bool init = false;
        if (init == false)
        {
            init = true;
            signal( SIGPIPE, SIG_IGN);
        }
        sockets_kernel_2_mutex::startup_lock.unlock();
    }


// ----------------------------------------------------------------------------------------

    // lookup functions

    int
    get_local_hostname (
        std::string& hostname
    )
    {
        try 
        {
            char temp[MAXHOSTNAMELEN];

            if (gethostname(temp,MAXHOSTNAMELEN) == -1)
            {
                return OTHER_ERROR;
            }
            // ensure that NUL is at the end of the string
            temp[MAXHOSTNAMELEN-1] = '\0';

            hostname = temp; 
        }
        catch (...)
        {
            return OTHER_ERROR;
        }

        return 0;
    }

// -----------------

    int 
    hostname_to_ip (
        const std::string& hostname,
        std::string& ip,
        int n
    )
    {
        try 
        {
            set<std::string>::kernel_1a sos;

            if (hostname.empty())
                return OTHER_ERROR;

            addrinfo* result = 0;
            if (getaddrinfo(hostname.c_str(),0,0,&result))
            {
                return OTHER_ERROR;
            }
            addrinfo* result_orig = result;

            // loop over all the addrinfo structures and add them to the set.  the reason for doing
            // this dumb crap is because different platforms return all kinds of weird garbage.  many
            // return the same ip multiple times, etc...
            while (result != 0)
            {
                char temp[16];
                inet_ntop (
                    AF_INET,
                    &((reinterpret_cast<sockaddr_in*>(result->ai_addr))->sin_addr),
                    temp,16
                    );

                result = result->ai_next;

                ip.assign(temp);
                if (sos.is_member(ip) == false)
                    sos.add(ip);
            }

            freeaddrinfo(result_orig);

            // now return the nth unique ip address
            int i = 0;
            while (sos.move_next())
            {
                if (i == n)
                {
                    ip = sos.element();
                    return 0;
                }
                ++i;
            }

            return OTHER_ERROR;
        }
        catch (...)
        {
            return OTHER_ERROR;
        }
        return 0;
    }

// -----------------

    int
    ip_to_hostname (
        const std::string& ip,
        std::string& hostname
    )
    {

        try 
        {

            if (ip.empty())
                return OTHER_ERROR;

            sockaddr_in sa;
            sa.sin_family = AF_INET;
            inet_pton(AF_INET,ip.c_str(),&sa.sin_addr);

            char temp[NI_MAXHOST];
            if ( getnameinfo (
                    reinterpret_cast<sockaddr*>(&sa),sizeof(sockaddr_in),
                    temp,
                    NI_MAXHOST,
                    0,
                    0,
                    NI_NAMEREQD
                ) 
            )
            {
                return OTHER_ERROR;
            }
     
            hostname.assign(temp);

        }
        catch (...)
        {
            return OTHER_ERROR;
        }
        return 0;
    }

// ----------------------------------------------------------------------------------------

    connection::
    connection(
        int sock,
        int foreign_port, 
        const std::string& foreign_ip, 
        int local_port,
        const std::string& local_ip
    ) :
        connection_socket(sock),
        connection_foreign_port(foreign_port),
        connection_foreign_ip(foreign_ip),
        connection_local_port(local_port),
        connection_local_ip(local_ip),
        sd(false),
        sdo(false),
        sdr(0)
    {}

// ----------------------------------------------------------------------------------------

    long connection::
    write (
        const char* buf, 
        long num
    )
    {
        const long old_num = num;
        long status;
        while (num > 0)
        {
            if ( (status = ::send(connection_socket,buf,num,0)) <=0)
            {
                // if send was interupted by a signal then restart it
                if (errno == EINTR)
                {
                    continue;
                }
                else
                {
                    // check if shutdown or shutdown_outgoing have been called
                    if (sdo_called())
                        return SHUTDOWN;
                    else
                        return OTHER_ERROR;
                }
            }
            num -= status;
            buf += status;
        } 
        return old_num;
    }

// ----------------------------------------------------------------------------------------

    long connection::
    read (
        char* buf, 
        long num
    )
    {
        long status;
        while (true)
        {
            status = recv(connection_socket,buf,num,0);
            if (status == -1)
            {
                // if recv was interupted then try again
                if (errno == EINTR)
                    continue;
                else
                {
                    if (sd_called())
                        return SHUTDOWN;
                    else
                        return OTHER_ERROR;
                }
            }
            else if (status == 0 && sd_called())
            {
                return SHUTDOWN;
            }

            return status;
        } // while (true)
    }

// ----------------------------------------------------------------------------------------

    connection::
    ~connection (
    )        
    {
        while (true)
        {
            int status = ::close(connection_socket);  
            if (status == -1 && errno == EINTR)
                continue;
            break;
        }
    }


// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // listener object
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    listener::
    listener(
        int sock,
        int port,
        const std::string& ip
    ) :
        listening_socket(sock),
        listening_port(port),
        listening_ip(ip),
        inaddr_any(listening_ip.empty())
    {}

// ----------------------------------------------------------------------------------------

    listener::
    ~listener (
    )        
    {
        while (true)
        {
            int status = ::close(listening_socket);  
            if (status == -1 && errno == EINTR)
                continue;
            break;
        }
    }

// ----------------------------------------------------------------------------------------

    int listener::
    accept (
        connection*& new_connection,
        int timeout
    )
    {
        int incoming;
        sockaddr_in incomingAddr;
        dsocklen_t length = sizeof(sockaddr_in);

        // implement timeout with select if timeout is > 0
        if (timeout > 0)
        {

            fd_set read_set;
            // initialize read_set
            FD_ZERO(&read_set);

            // add the listening socket to read_set
            FD_SET(listening_socket, &read_set);

            timeval time_to_wait;


            // loop on select so if its interupted then we can start it again
            while (true)
            {

                // setup a timeval structure
                time_to_wait.tv_sec = static_cast<long>(timeout/1000);
                time_to_wait.tv_usec = static_cast<long>((timeout%1000)*1000);

                // wait on select
                int status = select(listening_socket+1,&read_set,0,0,&time_to_wait);

                // if select timed out
                if (status == 0)
                    return TIMEOUT;
                
                // if select returned an error
                if (status == -1)
                {
                    // if select was interupted or the connection was aborted
                    // then go back to select
                    if (errno == EINTR || 
                        errno == ECONNABORTED || 
#ifdef EPROTO
                        errno == EPROTO ||
#endif
                        errno == ECONNRESET
                        )
                    {
                        continue;
                    }
                    else
                    {
                        return OTHER_ERROR;
                    }
                }

                // accept the new connection
                incoming=::accept (
                    listening_socket,
                    reinterpret_cast<sockaddr*>(&incomingAddr),
                    &length
                    );

                // if there was an error return OTHER_ERROR
                if ( incoming == -1 )
                {
                    // if accept was interupted then go back to accept
                    if (errno == EINTR || 
                        errno == ECONNABORTED || 
#ifdef EPROTO
                        errno == EPROTO ||
#endif
                        errno == ECONNRESET
                        )
                    {
                        continue;
                    }
                    else
                    {
                        return OTHER_ERROR;
                    }
                }

                // if there were no errors then quit loop
                break;

            }

        }
        // else if there is no time out then just go into accept
        else
        {
            while (true)
            {
                // call accept to get a new connection
                incoming=::accept (
                    listening_socket,
                    reinterpret_cast<sockaddr*>(&incomingAddr),
                    &length
                    );

                // if there was an error return OTHER_ERROR
                if ( incoming == -1 )
                {
                    // if accept was interupted then go back to accept
                    if (errno == EINTR || 
                        errno == ECONNABORTED || 
#ifdef EPROTO
                        errno == EPROTO ||
#endif
                        errno == ECONNRESET
                        )
                    {
                        continue;
                    }
                    else
                    {
                        return OTHER_ERROR;
                    }
                }
                break;
            }

        }

        
        // get the port of the foreign host into foreign_port
        int foreign_port = ntohs(incomingAddr.sin_port);

        // get the IP of the foreign host into foreign_ip
        char foreign_ip[16];
        inet_ntop(AF_INET,&incomingAddr.sin_addr,foreign_ip,16);



        // get the local ip for this connection into local_ip
        char temp_local_ip[16];
        std::string local_ip;
        if (inaddr_any == true)
        {
            sockaddr_in local_info;
            length = sizeof(sockaddr_in);
            // get the local sockaddr_in structure associated with this new connection
            if ( getsockname (
                    incoming,
                    reinterpret_cast<sockaddr*>(&local_info),
                    &length
                ) == -1
            )
            {   // an error occurred
                while (true)
                {
                    int status = ::close(incoming);
                    if (status == -1 && errno == EINTR)
                        continue;
                    break;
                }
                return OTHER_ERROR;
            }
            local_ip = const_cast<char*> (
                inet_ntop(AF_INET,&local_info.sin_addr,temp_local_ip,16)
                );
        }
        else
        {
            local_ip = listening_ip;
        }



        // set the SO_OOBINLINE option
        int flag_value = 1;
        if (setsockopt(incoming,SOL_SOCKET,SO_OOBINLINE,reinterpret_cast<const void*>(&flag_value),sizeof(int)))
        {
            while (true)
            {
                int status = ::close(incoming);
                if (status == -1 && errno == EINTR)
                    continue;
                break;
            }
            return OTHER_ERROR;  
        }



        // make a new connection object for this new connection
        try 
        { 
            new_connection = new connection (
                                    incoming,
                                    foreign_port,
                                    foreign_ip,
                                    listening_port,
                                    local_ip
                                ); 
        }
        catch (...) 
        { 
            while (true)
            {
                int status = ::close(incoming);
                if (status == -1 && errno == EINTR)
                    continue;
                break;
            }
            return OTHER_ERROR; 
        }

        return 0;
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // socket creation functions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------    

    static void
    close_socket (
        int sock
    )
    /*! 
        requires
            - sock == a socket
        ensures
            - sock has been closed
    !*/
    {
        while (true)
        {
            int status = ::close(sock);
            if (status == -1 && errno == EINTR)
                continue;
            break;
        }
    }

// ----------------------------------------------------------------------------------------    

    int 
    create_listener (
        listener*& new_listener,
        unsigned short port,
        const std::string& ip
    )
    {
        sockets_startup();


        sockaddr_in sa;  // local socket structure
        memset(&sa,'\0',sizeof(sockaddr_in)); // initialize sa
        

        int sock = socket (AF_INET, SOCK_STREAM, 0);  // get a new socket

        // if socket() returned an error then return OTHER_ERROR
        if (sock == -1)
        {
            return OTHER_ERROR;
        }

        // set the local socket structure 
        sa.sin_family = AF_INET;
        sa.sin_port = htons(port);
        if (ip.empty())
        {            
            // if the listener should listen on any IP
            sa.sin_addr.s_addr = htons(INADDR_ANY);
        }
        else
        {
            // if there is a specific ip to listen on
            sa.sin_addr.s_addr = inet_addr(ip.c_str());

            // if inet_addr couldn't convert the ip then return an error
            if ( sa.sin_addr.s_addr == ( in_addr_t)(-1))
            {
                close_socket(sock);
                return OTHER_ERROR;
            }
        }

        // set the SO_REUSEADDR option
        int flag_value = 1;
        if (setsockopt(sock,SOL_SOCKET,SO_REUSEADDR,reinterpret_cast<const void*>(&flag_value),sizeof(int)))
        {
            close_socket(sock);
            return OTHER_ERROR;
        }


        // bind the new socket to the requested port and ip
        if (bind(sock,reinterpret_cast<sockaddr*>(&sa),sizeof(sockaddr_in)) == -1)
        {   // if there was an error 
            close_socket(sock); 

            // if the port is already bound then return PORTINUSE
            if (errno == EADDRINUSE)
                return PORTINUSE;
            else
                return OTHER_ERROR;            
        }


        // tell the new socket to listen
        if ( listen(sock,SOMAXCONN) == -1)
        {
            // if there was an error return OTHER_ERROR
            close_socket(sock); 

            // if the port is already bound then return PORTINUSE
            if (errno == EADDRINUSE)
                return PORTINUSE;
            else
                return OTHER_ERROR;   
        }

        // determine the used local port if necessary
        if (port == 0)
        {
            sockaddr_in local_info;
            dsocklen_t length = sizeof(sockaddr_in);
            if ( getsockname(
                sock,
                reinterpret_cast<sockaddr*>(&local_info),
                &length
                ) == -1)
            {
                close_socket(sock);
                return OTHER_ERROR;
            }
            port = ntohs(local_info.sin_port);            
        }

        // initialize a listener object on the heap with the new socket
        try { new_listener = new listener(sock,port,ip); }
        catch(...) { close_socket(sock); return OTHER_ERROR; }

        return 0;
    }

// ----------------------------------------------------------------------------------------

    int 
    create_connection ( 
        connection*& new_connection,
        unsigned short foreign_port, 
        const std::string& foreign_ip, 
        unsigned short local_port,
        const std::string& local_ip
    )
    {
        sockets_startup();
        
        sockaddr_in local_sa;  // local socket structure
        sockaddr_in foreign_sa;  // foreign socket structure
        memset(&local_sa,'\0',sizeof(sockaddr_in)); // initialize local_sa
        memset(&foreign_sa,'\0',sizeof(sockaddr_in)); // initialize foreign_sa

        dsocklen_t length;

        int sock = socket (AF_INET, SOCK_STREAM, 0);  // get a new socket

        // if socket() returned an error then return OTHER_ERROR
        if (sock == -1 )
        {
            return OTHER_ERROR;
        }

        // set the foreign socket structure 
        foreign_sa.sin_family = AF_INET;
        foreign_sa.sin_port = htons(foreign_port);
        foreign_sa.sin_addr.s_addr = inet_addr(foreign_ip.c_str());

        // if inet_addr couldn't convert the ip then return an error
        if ( foreign_sa.sin_addr.s_addr == ( in_addr_t)(-1))
        {
            close_socket(sock);
            return OTHER_ERROR;
        }


        // set up the local socket structure
        local_sa.sin_family = AF_INET;

        // set the local port
        local_sa.sin_port = htons(local_port);

        // set the local ip
        if (local_ip.empty())
        {            
            // if the listener should listen on any IP
            local_sa.sin_addr.s_addr = htons(INADDR_ANY);
        }
        else
        {
            // if there is a specific ip to listen on
            local_sa.sin_addr.s_addr = inet_addr(local_ip.c_str());  

            // if inet_addr couldn't convert the ip then return an error
            if ( local_sa.sin_addr.s_addr == ( in_addr_t)(-1))
            {
                close_socket(sock);
                return OTHER_ERROR;
            }
        }



        

        // bind the new socket to the requested local port and local ip
        if ( bind(sock,reinterpret_cast<sockaddr*>(&local_sa),sizeof(sockaddr_in)) == -1)
        {   // if there was an error 
            close_socket(sock); 

            // if the port is already bound then return PORTINUSE
            if (errno == EADDRINUSE)
                return PORTINUSE;
            else
                return OTHER_ERROR;           
        }

        // connect the socket        
        if ( connect (
                sock,
                reinterpret_cast<sockaddr*>(&foreign_sa),
                sizeof(sockaddr_in)
            ) == -1
        )
        {
            close_socket(sock); 
            // if the port is already bound then return PORTINUSE
            if (errno == EADDRINUSE)
                return PORTINUSE;
            else
                return OTHER_ERROR;    
        }


        // determine the local port and IP and store them in used_local_ip 
        // and used_local_port
        int used_local_port;
        char temp_used_local_ip[16];
        std::string used_local_ip;
        sockaddr_in local_info;

        // determine the port
        if (local_port == 0)
        {
            length = sizeof(sockaddr_in);
            if ( getsockname(
                    sock,
                    reinterpret_cast<sockaddr*>(&local_info),
                    &length
                ) == -1)
            {
                close_socket(sock);
                return OTHER_ERROR;
            }
            used_local_port = ntohs(local_info.sin_port);            
        }
        else
        {
            used_local_port = local_port;
        }

        // determine the ip
        if (local_ip.empty())
        {
            // if local_port is not 0 then we must fill the local_info structure
            if (local_port != 0)
            {
                length = sizeof(sockaddr_in);
                if ( getsockname (
                        sock,
                        reinterpret_cast<sockaddr*>(&local_info),
                        &length
                    ) == -1
                )
                {
                    close_socket(sock);
                    return OTHER_ERROR;
                }
            }
            used_local_ip = inet_ntop(AF_INET,&local_info.sin_addr,temp_used_local_ip,16);
        }
        else
        {
            used_local_ip = local_ip;
        }


        // set the SO_OOBINLINE option
        int flag_value = 1;
        if (setsockopt(sock,SOL_SOCKET,SO_OOBINLINE,reinterpret_cast<const void*>(&flag_value),sizeof(int)))
        {
            close_socket(sock);
            return OTHER_ERROR;  
        }


        // initialize a connection object on the heap with the new socket
        try 
        { 
            new_connection = new connection (
                                    sock,
                                    foreign_port,
                                    foreign_ip,
                                    used_local_port,
                                    used_local_ip
                                ); 
        }
        catch(...) {close_socket(sock);  return OTHER_ERROR; }

        return 0;
    }

// ----------------------------------------------------------------------------------------

}

#endif // POSIX

#endif // DLIB_SOCKETS_KERNEL_2_CPp_

