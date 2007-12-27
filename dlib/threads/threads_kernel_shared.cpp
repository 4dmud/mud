// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_THREADS_KERNEL_SHARED_CPp_
#define DLIB_THREADS_KERNEL_SHARED_CPp_

#include "threads_kernel_shared.h"
#include "../assert.h"
#include <iostream>


#ifndef DLIB_THREAD_POOL_TIMEOUT
// default to 30000 milliseconds
#define DLIB_THREAD_POOL_TIMEOUT 30000
#endif

namespace dlib
{

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// threader functions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    namespace threads_kernel_shared 
    {
        void threader::
        call_end_handlers (
        )
        {
            reg.m.lock();
            const thread_id_type id = get_thread_id();
            thread_id_type id_copy;
            unsigned long count = reg.reg.count(id);
            member_function_pointer<>::kernel_1a mfp;

            // Remove all the member function pointers for this thread from the tree 
            // and call them.
            for (unsigned long i = 0; i < count; ++i)
            {
                reg.reg.remove(id,id_copy,mfp);
                reg.m.unlock();
                mfp();
                reg.m.lock();
            }
            reg.m.unlock();
        }

    // ------------------------------------------------------------------------------------

        bool threader::
        create_new_thread (
            void (*funct)(void*),
            void* param
        )
        {

            // get a lock on the data mutex
            auto_mutex M(data_mutex);

            // loop to ensure that the new function poitner is in the data
            while (true)
            {
                // if the data is empty then add new data and quit loop
                if (function_pointer == 0)
                {
                    parameter = param;
                    function_pointer = funct;
                    break;
                }
                else
                {
                    // wait for data to become empty
                    data_empty.wait();
                }
            }


            // get a thread for this new data
            // if a new thread must be crated
            if (pool_count == 0)
            {
                // make thread and add it to the pool
                if ( threads_kernel_shared_helpers::spawn_thread(thread_starter, this) == false )
                {
                    function_pointer = 0;
                    parameter = 0;
                    data_empty.signal();
                    return false;
                }
                ++total_count;
            }
            // wake up a thread from the pool
            else
            {
                data_ready.signal();
            }

            return true;
        }

    // ------------------------------------------------------------------------------------

        void thread_starter (
            void* object
        )
        {
            // get a reference to the calling threader object
            threader& self = *reinterpret_cast<threader*>(object);


            self.data_mutex.lock();

            // indicate that this thread is now in the thread pool
            ++self.pool_count;

            while (true)
            {
                // if data is ready then process it and launch the thread
                // if its not ready then go back into the pool
                while (self.function_pointer != 0)
                {                
                    // indicate that this thread is now out of the thread pool
                    --self.pool_count;

                    // get the data for the function call
                    void (*funct)(void*) = self.function_pointer;
                    void* param = self.parameter;
                    self.function_pointer = 0;

                    // signal that the data is now empty
                    self.data_empty.signal();

                    self.data_mutex.unlock();
                    // call funct with its intended parameter
                    try
                    {
                        funct(param);
                        self.call_end_handlers();
                    }
                    catch (std::exception& e)
                    {
                        std::cerr << "An exception was thrown in a thread and was not caught.  Its what() string is:\n"
                             << e.what() << std::endl;

                        self.data_mutex.lock();
                        --self.total_count;
                        self.destructed.signal();
                        self.data_mutex.unlock();

                        abort();
                    }
                    catch (...)
                    {
                        std::cerr << "An exception was thrown in a thread and was not caught." << std::endl;

                        self.data_mutex.lock();
                        --self.total_count;
                        self.destructed.signal();
                        self.data_mutex.unlock();

                        abort();
                    }

                    self.data_mutex.lock();

                    // indicate that this thread is now back in the thread pool
                    ++self.pool_count;
                }

                if (self.destruct == true)
                    break;

                // if we timed out and there isn't any work to do then
                // this thread will quit this loop and end.
                if (self.data_ready.wait_or_timeout(DLIB_THREAD_POOL_TIMEOUT) == false && 
                    self.function_pointer == 0)
                    break;

            }


            // indicate that this thread is now out of the thread pool
            --self.pool_count;
            --self.total_count;

            self.destructed.signal();
            self.data_mutex.unlock();
        }

    // ------------------------------------------------------------------------------------

        class main_thread_id_helper
        {
        public:
            main_thread_id_helper ()
            {
                get_main_thread_id();
            }
        };

        // make sure get_main_thread_id() gets called from the main thread first
        static main_thread_id_helper junk;

    // ------------------------------------------------------------------------------------

    }

// ----------------------------------------------------------------------------------------


}

#endif // DLIB_THREADS_KERNEL_SHARED_CPp_

