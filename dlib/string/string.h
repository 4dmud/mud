// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_STRINg_
#define DLIB_STRINg_ 

#include <string>
#include <iostream>
#include "../algs.h"
#include "../error.h"
#include "../assert.h"
#include "string_abstract.h"

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
    inline const T string_cast (
        const std::string& str
    )
    {
        COMPILE_TIME_ASSERT(is_pointer_type<T>::value == false);

        using namespace std;
        istringstream sin(str);
        T temp;
        sin >> temp;
        if (!sin) throw string_cast_error(str);
        if (sin.get() != EOF) throw string_cast_error(str);   
        return temp;
    }

    template <>
    inline const std::string string_cast<std::string> (
        const std::string& str
    ) { return str; }

    // this is a workaround for Borland's compiler.  
    template <typename T>
    inline const T string_cast (const char* str){ return string_cast<T>(std::string(str)); }

// ----------------------------------------------------------------------------------------

    template <
        typename charT
        >
    const std::string narrow (
        const std::basic_string<charT>& str
    )
    {
        std::string temp;
        temp.reserve(str.size());
        std::string::size_type i;
        std::basic_ostringstream<charT> sout;
        for (i = 0; i < str.size(); ++i)
        {
            temp += sout.narrow(str[i],' ');
        }
        return temp;
    }

    template <>
    inline const std::string narrow<char> (
        const std::string& str
    )
    { 
        return str;
    }

// ----------------------------------------------------------------------------------------

    template <
        typename charT
        >
    const std::basic_string<charT> wrap_string (
        const std::basic_string<charT>& str,
        const unsigned long first_pad,
        const unsigned long rest_pad,
        const unsigned long max_per_line = 79
    )
    {
        ASSERT ( first_pad < max_per_line && rest_pad < max_per_line && 
                 rest_pad >= first_pad,
                 "\tconst std::basic_string<charT> wrap_string()"
                 << "\n\tfirst_pad:    " << first_pad
                 << "\n\trest_pad:     " << rest_pad
                 << "\n\tmax_per_line: " << max_per_line  );

        using namespace std;

        basic_ostringstream<charT> sout;
        basic_istringstream<charT> sin(str);

        for (unsigned long i = 0; i < rest_pad; ++i)
            sout << _dT(charT," ");
        const basic_string<charT> pad(sout.str());
        sout.str(_dT(charT,""));

        for (unsigned long i = 0; i < first_pad; ++i)
            sout << _dT(charT," ");


        typename basic_string<charT>::size_type remaining = max_per_line - rest_pad;

        basic_string<charT> temp;

        sin >> temp;
        while (sin)
        {
            if (temp.size() > remaining)
            {
                if (temp.size() + rest_pad >= max_per_line)
                {
                    string::size_type i = 0;
                    for (; i < temp.size(); ++i)
                    {
                        sout << temp[i];
                        --remaining;
                        if (remaining == 0)
                        {
                            sout << _dT(charT,"\n") << pad;
                            remaining = max_per_line - rest_pad;
                        }
                    }
                }
                else
                {
                    sout << _dT(charT,"\n") << pad << temp;
                    remaining = max_per_line - rest_pad - temp.size();
                }
            }
            else if (temp.size() == remaining)
            {
                sout << temp;
                remaining = 0;
            }
            else
            {
                sout << temp;
                remaining -= temp.size();
            }

            sin >> temp;
            if (remaining == 0 && sin)
            {
                sout << _dT(charT,"\n") << pad;
                remaining = max_per_line - rest_pad;
            }
            else
            {
                sout << _dT(charT," ");
                --remaining;
            }
        }

        return sout.str();
    }

    template <
        typename charT
        >
    const std::basic_string<charT> wrap_string (
        const charT* str,
        const unsigned long first_pad,
        const unsigned long rest_pad,
        const unsigned long max_per_line = 79
    ) { return wrap_string(std::basic_string<charT>(str),first_pad,rest_pad,max_per_line); }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_STRINg_

