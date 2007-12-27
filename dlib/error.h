// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_ERROr_ 
#define DLIB_ERROr_ 

#include <string>
#include <new>          // for std::bad_alloc

// breakpoints

extern "C"
{
    void dlib_assert_breakpoint(
    );
    /*!
        ensures
            - this function does nothing 
              It exists just so you can put breakpoints on it in a debugging tool.
              It is called only when an ASSERT or CASSERT fails and is about to
              throw an exception.
    !*/
}

// -------------------------------
// ------ exception classes ------
// -------------------------------

namespace dlib
{

// ----------------------------------------------------------------------------------------

    enum error_type
    {
        EOTHER,        
        EPORT_IN_USE,  
        ETIMEOUT,     
        ECONNECTION, 
        ELISTENER, 
        ERESOLVE,     
        EMONITOR,   
        ECREATE_THREAD,    
        ECREATE_MUTEX,    
        ECREATE_SIGNALER,
        EUNSPECIFIED,   
        EGENERAL_TYPE1,
        EGENERAL_TYPE2,  
        EGENERAL_TYPE3,  
        EINVALID_OPTION,
        ETOO_FEW_ARGS,
        ETOO_MANY_ARGS,
        ESOCKET,
        ETHREAD,
        EGUI,
        EFATAL,
        EBROKEN_ASSERT,
        EIMAGE_LOAD,
        EDIR_CREATE,
        EINCOMPATIBLE_OPTIONS,
        EMISSING_REQUIRED_OPTION,
        EINVALID_OPTION_ARG,
        EMULTIPLE_OCCURANCES,
        ECONFIG_READER,
        EIMAGE_SAVE
    };

// ----------------------------------------------------------------------------------------

    // the base exception class
    class error : public std::exception
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This is the base exception class for the dlib library.  i.e. all 
                exceptions in this library inherit from this class.
        !*/

    public:
        error(
            error_type t,
            const std::string& a
        );
        /*!
            ensures
                - #type == t
                - #info == a
        !*/

        error(
            error_type t
        );
        /*!
            ensures
                - #type == t
                - #info == ""
        !*/

        error(
            const std::string& a
        );
        /*!
            ensures
                - #type == EUNSPECIFIED
                - #info == a
        !*/

        error(
        );
        /*!
            ensures
                - #type == EUNSPECIFIED
                - #info == ""
        !*/

        virtual ~error(
        ) throw();
        /*!
            ensures
                - does nothing
        !*/

        const char* what(
        ) const throw();
        /*!
            ensures
                - if (info.size() != 0) then
                    - returns info.c_str()
                - else
                    - returns type_to_string(type)
        !*/

        const char* type_to_string (
        ) const throw();
        /*!
            ensures
                - returns a string that names the contents of the type member.
        !*/

        const std::string info;  // info about the error
        const error_type type; // the type of the error

    private:
        const error& operator=(const error&);
    };

// ----------------------------------------------------------------------------------------

    class fatal_error : public error
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                As the name says, this object represents some kind of fatal error. 
                It is also the exception thrown by the ASSERT and CASSERT macros.
        !*/

    public:
        fatal_error(
            error_type t,
            const std::string& a
        );
        /*!
            ensures
                - #type == t
                - #info == a
        !*/

        fatal_error(
            error_type t
        );
        /*!
            ensures
                - #type == t
                - #info == ""
        !*/

        fatal_error(
            const std::string& a
        );
        /*!
            ensures
                - #type == EFATAL
                - #info == a
        !*/

        fatal_error(
        );
        /*!
            ensures
                - #type == EFATAL
                - #info == ""
        !*/
    };

// ----------------------------------------------------------------------------------------

    class gui_error : public error
    {
    public:
        gui_error(
            error_type t,
            const std::string& a
        );
        /*!
            ensures
                - #type == t
                - #info == a
        !*/

        gui_error(
            error_type t
        );
        /*!
            ensures
                - #type == t
                - #info == ""
        !*/

        gui_error(
            const std::string& a
        );
        /*!
            ensures
                - #type == EGUI 
                - #info == a
        !*/

        gui_error(
        );
        /*!
            ensures
                - #type == EGUI
                - #info == ""
        !*/
    };

// ----------------------------------------------------------------------------------------

    class socket_error : public error
    {
    public:
        socket_error(
            error_type t,
            const std::string& a
        );
        /*!
            ensures
                - #type == t
                - #info == a
        !*/

        socket_error(
            error_type t
        );
        /*!
            ensures
                - #type == t
                - #info == ""
        !*/

        socket_error(
            const std::string& a
        );
        /*!
            ensures
                - #type == ESOCKET
                - #info == a
        !*/

        socket_error(
        );
        /*!
            ensures
                - #type == ESOCKET
                - #info == ""
        !*/
    };

// ----------------------------------------------------------------------------------------

    class thread_error : public error
    {
    public:
        thread_error(
            error_type t,
            const std::string& a
        );
        /*!
            ensures
                - #type == t
                - #info == a
        !*/

        thread_error(
            error_type t
        );
        /*!
            ensures
                - #type == t
                - #info == ""
        !*/

        thread_error(
            const std::string& a
        );
        /*!
            ensures
                - #type == ETHREAD
                - #info == a
        !*/

        thread_error(
        );
        /*!
            ensures
                - #type == ETHREAD
                - #info == ""
        !*/
    };

// ----------------------------------------------------------------------------------------

}

#ifdef NO_MAKEFILE
#include "error.cpp"
#endif

#endif // DLIB_ERROr_

