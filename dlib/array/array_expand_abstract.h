// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_ARRAY_EXPANd_ABSTRACT_
#ifdef DLIB_ARRAY_EXPANd_ABSTRACT_

#include "array_kernel_abstract.h"

namespace dlib
{

    template <
        typename array_base
        >
    class array_expand : public array_base
    {

        /*!
            REQUIREMENTS ON ARRAY_BASE
                must be an implementation of array/array_kernel_abstract.h 

            POINTERS AND REFERENCES
                expand() may invalidate pointers and references to internal data.

            WHAT THIS EXTENSION DOES FOR ARRAY
                This extension gives an array the ability to expand its size() beyond
                its max_size() without clearing out all its elements.
        !*/

        public:

            void expand (
                unsigned long new_size
            );
            /*!
                ensures
                    - #size() == new_size
                    - #max_size() == max(new_size,max_size())
                    - for all i < size():
                        - #(*this)[i] == (*this)[i]
                          (i.e. all the original elements of *this are still present
                          and at their same positions.)
                    - for all valid i >= size():
                        - #(*this)[i] has an undefined value
                          (i.e. any new elements of the array have an undefined value)
                throws
                    - std::bad_alloc or any exception thrown by T's constructor.
                       If an exception is thrown then it has no effect on *this.
            !*/

    };

    template <
        typename array_base
        >
    inline void swap (
        array_expand<array_base>& a, 
        array_expand<array_base>& b 
    ) { a.swap(b); }
    /*!
        provides a global swap function
    !*/

}

#endif // DLIB_ARRAY_EXPANd_ABSTRACT_

