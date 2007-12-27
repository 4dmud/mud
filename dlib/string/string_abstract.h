// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_STRINg_ABSTRACT_
#ifdef DLIB_STRINg_ABSTRACT_

#include <string>
#include <iostream>
#include "../error.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    class string_cast_error : public error
    {
    public:
        string_cast_error(const std::string& str):error(str) {}
    };

    template <
        typename T
        >
    const T string_cast (
        const std::string& str
    );
    /*!
        requires
            - T is not a pointer type
        ensures
            - returns str converted to T
        throws
            - string_cast_error
                This exception is thrown if string_cast() is unable to convert
                str into a T.  Also, string_cast_error::info == str
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename charT
        >
    const std::string narrow (
        const std::basic_string<charT>& str
    );
    /*!
        ensures
            - returns str as a std::string by converting every character in it to a char.
              Note that any characters that do not have a mapping to type char will be 
              converted to a space.
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename charT
        >
    const std::basic_string<charT> wrap_string (
        const std::basic_string<charT>& str,
        const unsigned long first_pad,
        const unsigned long rest_pad,
        const unsigned long max_per_line = 79
    );
    /*!
        requires
            - first_pad < max_per_line
            - rest_pad < max_per_line
            - rest_pad >= first_pad
        ensures
            - returns a copy of str S such that:
                - S is broken up into lines separated by the \n character.
                - The first line starts with first_pad space characters.
                - The second and all subsequent lines start with rest_pad space characters.
                - The first line is no longer than max_per_line - (rest_pad-first_pad) characters.
                - The second and all subsequent lines are no longer than max_per_line characters. 
    !*/

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_STRINg_ABSTRACT_

