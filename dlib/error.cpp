// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_ERROR_CPp_
#define DLIB_ERROR_CPp_

#include "error.h"

// ----------------------------------------------------------------------------------------

extern "C"
{
    void dlib_assert_breakpoint (
    )
    {
    }
}

// ----------------------------------------------------------------------------------------

namespace dlib
{

// ----------------------------------------------------------------------------------------

    const char* error::
    what(
    ) const throw()
    {
        if (info.size() > 0)
            return info.c_str(); 
        else
            return type_to_string();
    }

// ----------------------------------------------------------------------------------------

    const char* error::
    type_to_string (
    ) const throw()
    {
        switch (type)
        {
            case EOTHER: return "EOTHER";
            case EPORT_IN_USE: return "EPORT_IN_USE";
            case ETIMEOUT: return "ETIMEOUT";
            case ECONNECTION: return "ECONNECTION"; 
            case ELISTENER: return "ELISTENER"; 
            case ERESOLVE: return "ERESOLVE";     
            case EMONITOR: return "EMONITOR";   
            case ECREATE_THREAD: return "ECREATE_THREAD";    
            case ECREATE_MUTEX: return "ECREATE_MUTEX";    
            case ECREATE_SIGNALER: return "ECREATE_SIGNALER";
            case EUNSPECIFIED: return "EUNSPECIFIED";   
            case EGENERAL_TYPE1: return "EGENERAL_TYPE1";
            case EGENERAL_TYPE2: return "EGENERAL_TYPE2";  
            case EGENERAL_TYPE3: return "EGENERAL_TYPE3";  
            case EINVALID_OPTION: return "EINVALID_OPTION";
            case ETOO_FEW_ARGS: return "ETOO_FEW_ARGS";
            case ETOO_MANY_ARGS: return "ETOO_MANY_ARGS";
            case ESOCKET: return "ESOCKET";
            case ETHREAD: return "ETHREAD";
            case EGUI: return "EGUI";
            case EFATAL: return "EFATAL";
            case EBROKEN_ASSERT: return "EBROKEN_ASSERT";
            case EIMAGE_LOAD: return "EIMAGE_LOAD";
            case EDIR_CREATE: return "EDIR_CREATE";
            case EINCOMPATIBLE_OPTIONS: return "EINCOMPATIBLE_OPTIONS";
            case EMISSING_REQUIRED_OPTION: return "EMISSING_REQUIRED_OPTION";
            case EINVALID_OPTION_ARG: return "EINVALID_OPTION_ARG";
            case EMULTIPLE_OCCURANCES: return "EMULTIPLE_OCCURANCES";
            case ECONFIG_READER: return "ECONFIG_READER";
            case EIMAGE_SAVE: return "EIMAGE_SAVE";
            default: return "undefined error type";
        }
    }

// ----------------------------------------------------------------------------------------

    error::
    error(
        error_type t,
        const std::string& a
    ):
        info(a),
        type(t) 
    {}

// ----------------------------------------------------------------------------------------

    error::
    error(
        error_type t
    ):
        type(t) 
    {}

// ----------------------------------------------------------------------------------------

    error::
    error(
        const std::string& a
    ):
        info(a),
        type(EUNSPECIFIED) 
    {}

// ----------------------------------------------------------------------------------------

    error::
    error(
    ):
        type(EUNSPECIFIED) 
    {}

// ----------------------------------------------------------------------------------------

    error::
    ~error(
    ) throw() 
    {} 

// ----------------------------------------------------------------------------------------

    fatal_error::
    fatal_error(
        error_type t,
        const std::string& a
    ):
        error(t,a)
    {}

// ----------------------------------------------------------------------------------------

    fatal_error::
    fatal_error(
        error_type t
    ):
        error(t)
    {}

// ----------------------------------------------------------------------------------------

    fatal_error::
    fatal_error(
        const std::string& a
    ):
        error(EFATAL,a)
    {}

// ----------------------------------------------------------------------------------------

    fatal_error::
    fatal_error(
    ):
        error(EFATAL)
    {}

// ----------------------------------------------------------------------------------------

    gui_error::
    gui_error(
        error_type t,
        const std::string& a
    ):
        error(t,a)
    {}

// ----------------------------------------------------------------------------------------

    gui_error::
    gui_error(
        error_type t
    ):
        error(t)
    {}

// ----------------------------------------------------------------------------------------

    gui_error::
    gui_error(
        const std::string& a
    ):
        error(EGUI,a)
    {}

// ----------------------------------------------------------------------------------------

    gui_error::
    gui_error(
    ):
        error(EGUI)
    {}

// ----------------------------------------------------------------------------------------

    socket_error::
    socket_error(
        error_type t,
        const std::string& a
    ):
        error(t,a)
    {}

// ----------------------------------------------------------------------------------------

    socket_error::
    socket_error(
        error_type t
    ):
        error(t)
    {}

// ----------------------------------------------------------------------------------------

    socket_error::
    socket_error(
        const std::string& a
    ):
        error(ESOCKET,a)
    {}

// ----------------------------------------------------------------------------------------

    socket_error::
    socket_error(
    ):
        error(ESOCKET)
    {}

// ----------------------------------------------------------------------------------------

    thread_error::
    thread_error(
        error_type t,
        const std::string& a
    ):
        error(t,a)
    {}

// ----------------------------------------------------------------------------------------

    thread_error::
    thread_error(
        error_type t):
        error(t)
    {}

// ----------------------------------------------------------------------------------------

    thread_error::
    thread_error(
        const std::string& a
    ):
        error(ETHREAD,a)
    {}

// ----------------------------------------------------------------------------------------

    thread_error::
    thread_error(
    ):
        error(ETHREAD)
    {}

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_ERROR_CPp_

