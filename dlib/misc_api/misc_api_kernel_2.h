// Copyright (C) 2004  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_MISC_API_KERNEl_2_
#define DLIB_MISC_API_KERNEl_2_

#include "misc_api_kernel_abstract.h"
#include "../algs.h"
#include <string>
#include "../uintn.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    void sleep (
        unsigned long milliseconds
    );

// ----------------------------------------------------------------------------------------

    std::string get_current_dir (
    );

// ----------------------------------------------------------------------------------------

    class timestamper 
    {
    public:
        uint64 get_timestamp (
        ) const;
    };

// ----------------------------------------------------------------------------------------

    class dir_create_error : public error 
    {
    public:
        dir_create_error(
            const std::string& dir_name
        ) : 
            error(EDIR_CREATE,"Error creating directory '" + dir_name + "'."),
            name(dir_name)
        {}
        const std::string& name;
    }; 


    void create_directory (
        const std::string& dir
    );

// ----------------------------------------------------------------------------------------

}

#ifdef NO_MAKEFILE
#include "misc_api_kernel_2.cpp"
#endif

#endif // DLIB_MISC_API_KERNEl_2_

