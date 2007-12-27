// Copyright (C) 2004  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_MISC_API_KERNEl_ABSTRACT_
#ifdef DLIB_MISC_API_KERNEl_ABSTRACT_

#include <string>
#include "../uintn.h"
#include "../algs.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    /*!
        GENERAL COMMENTS
            This file just contains miscellaneous api functions

        GENERAL WARNING
            Don't call any of these functions before winmain() or main() has been 
            entered.  
    !*/

// ----------------------------------------------------------------------------------------

    void sleep (
        unsigned long milliseconds
    );
    /*!
        ensures
            - causes the calling thread to sleep for the given number of 
              milliseconds.
    !*/

// ----------------------------------------------------------------------------------------

    std::string get_current_dir (
    );
    /*!
        ensures
            - if (no errors occurr) then
                - returns the path to the current working directory
            - else
                - returns ""
        throws
            - std::bad_alloc
    !*/

// ----------------------------------------------------------------------------------------

    class dir_create_error : public error { 
    public:
        const std::string name
    }; 

    void create_directory (
        const std::string& dir
    );
    /*!
        ensures
            - if (dir does not already exist) then
                - creates the given directory.
            - else
                - the call to create_directory() has no effect.
        throws
            - dir_create_error
                This exception is thrown if we were unable to create the requested
                directory.  The type member of the exception will bet set to
                EDIR_CREATE and the name member will be set to dir.
    !*/

// ----------------------------------------------------------------------------------------

    class timestamper 
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a timer that is capable of returning 
                timestamps.

                Note that the time is measured in microseconds but you are not 
                guaranteed to have that level of resolution.  The actual resolution
                is implementation dependent.
        !*/

    public:
        uint64 get_timestamp (
        ) const;
        /*!
            ensures
                - returns a timestamp that measures the time in microseconds since an 
                  arbitrary point in the past.  Note that this arbitray point remains
                  the same between all calls to get_timestamp().
        !*/
    };

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_MISC_API_KERNEl_ABSTRACT_

