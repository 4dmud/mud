// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_ARRAY_EXPANd_1_
#define DLIB_ARRAY_EXPANd_1_

#include "array_kernel_abstract.h"

namespace dlib
{

    template <
        typename array_base
        >
    class array_expand_1 : public array_base
    {

    public:

        void expand (
            unsigned long new_size
        );

    };

    template <
        typename array_base
        >
    inline void swap (
        array_expand_1<array_base>& a, 
        array_expand_1<array_base>& b 
    ) { a.swap(b); }
    /*!
        provides a global swap function
    !*/

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// member function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename array_base
        >
    void array_expand_1<array_base>::
    expand (
        unsigned long new_size
    )
    {
        if (this->max_size() < new_size)
        {
            array_base temp;
            temp.set_max_size(new_size);
            temp.set_size(new_size);
            for (unsigned long i = 0; i < this->size(); ++i)
            {
                exchange((*this)[i],temp[i]);
            }
            temp.swap(*this);
        }
        else
        {
            this->set_size(new_size);
        }
    }
}

// ----------------------------------------------------------------------------------------

#endif // DLIB_ARRAY_EXPANd_1_

