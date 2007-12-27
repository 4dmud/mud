// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_QUEUE_SORt_ABSTRACT_
#ifdef DLIB_QUEUE_SORt_ABSTRACT_


#include "queue_kernel_abstract.h"

namespace dlib
{

    template <
        typename queue_base
        >
    class queue_sort : public queue_base
    {

        /*!
            REQUIREMENTS ON QUEUE_BASE
                queue_base is instiatned with type T and
                is an implementation of queue/queue_kernel_abstract.h

            POINTERS AND REFERENCES TO INTERNAL DATA
                sort() may invalidate pointers and references to internal data.

            WHAT THIS EXTENSION DOES FOR QUEUE
                This gives a queue the ability to sort its contents by calling sort().
        !*/


        public:

            void sort (
            );
            /*!
                ensures
                    - for all elements in #*this the ith element is <= the i+1 element
                    - #at_start() == true
                throws
                    - std::bad_alloc or any exception thrown by T's constructor
                        data may be lost if sort() throws
            !*/
    };

    template <
        template queue_base
        >
    inline void swap (
        queue_sort<queue_base>& a, 
        queue_sort<queue_base>& b 
    ) { a.swap(b); }  
    /*!
        provides a global swap function
    !*/

}

#endif // DLIB_QUEUE_SORt_ABSTRACT_

