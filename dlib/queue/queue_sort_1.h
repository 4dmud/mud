// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_QUEUE_SORt_1_
#define DLIB_QUEUE_SORt_1_

#include "queue_sort_abstract.h"
#include "../algs.h"

namespace dlib
{

    template <
        typename queue_base 
        >
    class queue_sort_1 : public queue_base
    {
        typedef typename queue_base::type T;

        public:

            /*!
                this is a median of three version of the QuickSort algorithm  and
                it sorts queues of less than 30 elements with a selection sort
            !*/

            void sort (
            );

        private:

            void sort_this_queue (
                queue_base& queue
            );
            /*!
                ensures
                    each element in the queue is < the element behind it
            !*/

            void selection_sort (
                queue_base& queue
            );
            /*!
                ensures
                    sorts queue with a selection sort
            !*/

    };

    template <
        typename queue_base
        >
    inline void swap (
        queue_sort_1<queue_base>& a, 
        queue_sort_1<queue_base>& b 
    ) { a.swap(b); }   

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // member function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename queue_base
        >
    void queue_sort_1<queue_base>::
    sort (
    )
    {
        if (this->size() > 1)
        {
            sort_this_queue(*this);
        }
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // private member function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename queue_base
        >
    void queue_sort_1<queue_base>::
    sort_this_queue (
        queue_base& queue
    )
    {
        if (queue.size() < 30)
        {
            selection_sort(queue);
        }
        else
        {
            queue_base left, right;
            T partition_element;
            
            T temp;

            queue.dequeue(partition_element);
            queue.dequeue(temp);
            
            dlib::median(partition_element,temp,queue.current());
            
            queue.enqueue(temp);

            // partition queue into left and right
            while (queue.size() > 0)
            {
                queue.dequeue(temp);
                if (temp < partition_element)
                {
                    left.enqueue(temp);
                }
                else
                {
                    right.enqueue(temp);
                }
            }
        
            sort_this_queue(left);
            sort_this_queue(right);
            
            // combine the two queues
            left.swap(queue);
            queue.enqueue(partition_element);
            queue.cat(right);
        }
    }

// ----------------------------------------------------------------------------------------

    template <
        typename queue_base
        >
    void queue_sort_1<queue_base>::
    selection_sort (
        queue_base& queue
    )
    {
        if (queue.size() > 2)
        {
            T temp[29];
            unsigned long qsize = queue.size();

            for (unsigned long i = 0; i < qsize; ++i)
                queue.dequeue(temp[i]);

            unsigned long smallest;
            for (unsigned long i = 0; i < qsize - 1; ++i)
            {    
                // find smallest element and swap into i
                smallest = i;
                for (unsigned long j = i+1; j < qsize; ++j)
                {
                    if (temp[j] < temp[smallest])
                        smallest = j;
                }
                exchange(temp[smallest],temp[i]);
            }

            for (unsigned long i = 0; i < qsize; ++i)
                queue.enqueue(temp[i]);
        }
        else if (queue.size() == 2)
        {
            T temp1;
            queue.dequeue(temp1);
            if (temp1 < queue.current()) 
            {
                T temp2;
                queue.enqueue(temp1);
                queue.dequeue(temp2);
                queue.enqueue(temp2);
            }
            else
            {
                queue.enqueue(temp1);
            }
        }
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_QUEUE_SORt_1_

