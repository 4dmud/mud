// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <dlib/misc_api.h>
#include <dlib/pipe.h>

#include "tester.h"

namespace  
{
    using namespace test;
    using namespace dlib;
    using namespace std;

    logger dlog("test.pipe");

    namespace pipe_kernel_test_helpers
    {
        const unsigned long proc1_count = 10000;
        mutex m;
        signaler s(m);
        unsigned long threads_running = 0;
        bool found_error;

        inline void add_running_thread (
        )
        {
            auto_mutex M(m);
            ++threads_running;
        }

        inline void remove_running_thread (
        )
        {
            auto_mutex M(m);
            --threads_running;
            s.broadcast();
        }

        inline void wait_for_threads (
        )
        {
            auto_mutex M(m);
            while (threads_running > 0)
                s.wait();
        }

        template <
            typename pipe
            >
        void threadproc1 (
            void* param
        )
        {
            add_running_thread();
            pipe& p = *reinterpret_cast<pipe*>(param);
            try
            {

                int last = -1;
                for (unsigned long i = 0; i < proc1_count; ++i)
                {
                    int cur;
                    CASSERT(p.dequeue(cur) == true,"");
                    CASSERT(last + 1 == cur,"");
                    last = cur;
                }
                CASSERT(p.size() == 0,"");
            }
            catch(exception& e)
            {
                auto_mutex M(m);
                found_error = true;
                cout << "\n\nERRORS FOUND" << endl;
                cout << e.what() << endl;
                dlog << LWARN << "ERRORS FOUND";
                dlog << LWARN << e.what();
                p.disable();
            }        

            remove_running_thread();
        }


        template <
            typename pipe
            >
        void threadproc2 (
            void* param
        )
        {
            add_running_thread();
            pipe& p = *reinterpret_cast<pipe*>(param);
            try
            {

                int last = -1;
                int cur;
                while (p.dequeue(cur))
                {
                    CASSERT(last < cur,"");
                    last = cur;
                }
                auto_mutex M(m);
            }
            catch(exception& e)
            {
                auto_mutex M(m);
                found_error = true;
                cout << "\n\nERRORS FOUND" << endl;
                cout << e.what() << endl;
                dlog << LWARN << "ERRORS FOUND";
                dlog << LWARN << e.what();
                p.disable();
            }        
            remove_running_thread();
        }



        template <
            typename pipe
            >
        void threadproc3 (
            void* param
        )
        {
            add_running_thread();
            pipe& p = *reinterpret_cast<pipe*>(param);
            try
            {

                int last = -1;
                int cur;
                while (p.dequeue_or_timeout(cur,100000))
                {
                    CASSERT(last < cur,"");
                    last = cur;
                }
                auto_mutex M(m);
            }
            catch(exception& e)
            {
                auto_mutex M(m);
                found_error = true;
                cout << "\n\nERRORS FOUND" << endl;
                cout << e.what() << endl;
                dlog << LWARN << "ERRORS FOUND";
                dlog << LWARN << e.what();
                p.disable();
            }        
            remove_running_thread();
        }


    }



    template <
        typename pipe
        >
    int pipe_kernel_test (
        const string& type
    )
    /*!
        requires
            - pipe is an implementation of pipe/pipe_kernel_abstract.h and
              is instantiated with int
        ensures
            - runs tests on pipe for compliance with the specs 
            - returns 0 if no errors were detected, 1 otherwise.
    !*/
    {        
        using namespace pipe_kernel_test_helpers;
        found_error = false;

        try 
        {

            print_spinner();
            pipe test(10), test2(100);
            pipe test_0(0), test2_0(0);
            pipe test_1(1), test2_1(1);

            CASSERT(test.size() == 0,"");
            CASSERT(test2.size() == 0,"");
            CASSERT(test_0.size() == 0,"");
            CASSERT(test2_0.size() == 0,"");
            CASSERT(test_1.size() == 0,"");
            CASSERT(test2_1.size() == 0,"");

            test.empty();
            test2.empty();
            CASSERT(test.size() == 0,"");
            CASSERT(test2.size() == 0,"");

            test_0.empty();
            test2_0.empty();
            CASSERT(test_0.size() == 0,"");
            CASSERT(test2_0.size() == 0,"");

            test_1.empty();
            test2_1.empty();
            CASSERT(test_1.size() == 0,"");
            CASSERT(test2_1.size() == 0,"");



            int a;
            a = 3;
            test.enqueue(a);
            CASSERT(test.size() == 1,"");
            a = 5;
            test.enqueue(a);
            CASSERT(test.size() == 2,"");

            a = 0;
            test.dequeue(a);
            CASSERT(a == 3,"");
            CASSERT(test.size() == 1,"");

            a = 0;
            test.dequeue(a);
            CASSERT(a == 5,"");
            CASSERT(test.size() == 0,"");


            print_spinner();
            {
                dlog << LINFO << "starting normal length pipe tests";
                create_new_thread(&threadproc1<pipe>,&test);
                create_new_thread(&threadproc2<pipe>,&test2);
                create_new_thread(&threadproc2<pipe>,&test2);
                create_new_thread(&threadproc2<pipe>,&test2);

                for (unsigned long i = 0; i < proc1_count; ++i)
                {
                    a = i;
                    test.enqueue(a);
                }
                CASSERT(test.is_enqueue_enabled() == true,"");
                test.disable_enqueue();
                CASSERT(test.is_enqueue_enabled() == false,"");
                for (unsigned long i = 0; i < proc1_count; ++i)
                {
                    a = i;
                    test.enqueue(a);
                }

                for (unsigned long i = 0; i < 100000; ++i)
                {
                    a = i;
                    if (i%2 == 0)
                        test2.enqueue(a);
                    else
                        test2.enqueue_or_timeout(a,100000);
                }

                test2.wait_for_num_blocked_dequeues(3);
                CASSERT(test2.size() == 0,"");
                test2.disable();

                wait_for_threads();
                CASSERT(test2.size() == 0,"");

                test2.enable();

                print_spinner();

                create_new_thread(&threadproc3<pipe>,&test2);
                create_new_thread(&threadproc3<pipe>,&test2);


                for (unsigned long i = 0; i < 100000; ++i)
                {
                    a = i;
                    if (i%2 == 0)
                        test2.enqueue(a);
                    else
                        test2.enqueue_or_timeout(a,100000);
                }

                test2.wait_for_num_blocked_dequeues(2);
                CASSERT(test2.size() == 0,"");
                test2.disable();

                wait_for_threads();
                CASSERT(test2.size() == 0,"");

            }


            print_spinner();
            {
                dlog << LINFO << "starting 0 length pipe tests";
                create_new_thread(&threadproc1<pipe>,&test_0);
                create_new_thread(&threadproc2<pipe>,&test2_0);
                create_new_thread(&threadproc2<pipe>,&test2_0);
                create_new_thread(&threadproc2<pipe>,&test2_0);
                dlog << LTRACE << "0: 1";

                for (unsigned long i = 0; i < proc1_count; ++i)
                {
                    a = i;
                    test_0.enqueue(a);
                }

                dlog << LTRACE << "0: 2";
                CASSERT(test_0.is_enqueue_enabled() == true,"");
                test_0.disable_enqueue();
                CASSERT(test_0.is_enqueue_enabled() == false,"");
                for (unsigned long i = 0; i < proc1_count; ++i)
                {
                    a = i;
                    test_0.enqueue(a);
                }

                dlog << LTRACE << "0: 3";
                for (unsigned long i = 0; i < 100000; ++i)
                {
                    a = i;
                    if (i%2 == 0)
                        test2_0.enqueue(a);
                    else
                        test2_0.enqueue_or_timeout(a,100000);
                }

                print_spinner();
                dlog << LTRACE << "0: 4";
                test2_0.wait_for_num_blocked_dequeues(3);
                CASSERT(test2_0.size() == 0,"");
                test2_0.disable();

                wait_for_threads();
                CASSERT(test2_0.size() == 0,"");

                dlog << LTRACE << "0: 5";
                test2_0.enable();


                create_new_thread(&threadproc3<pipe>,&test2_0);
                create_new_thread(&threadproc3<pipe>,&test2_0);


                for (unsigned long i = 0; i < 20000; ++i)
                {
                    if ((i%100) == 0)
                        print_spinner();

                    a = i;
                    if (i%2 == 0)
                        test2_0.enqueue(a);
                    else
                        test2_0.enqueue_or_timeout(a,100000);
                }

                dlog << LTRACE << "0: 6";
                test2_0.wait_for_num_blocked_dequeues(2);
                CASSERT(test2_0.size() == 0,"");
                test2_0.disable();

                wait_for_threads();
                CASSERT(test2_0.size() == 0,"");

                dlog << LTRACE << "0: 7";
            }

            print_spinner();
            {
                dlog << LINFO << "starting 1 length pipe tests";
                create_new_thread(&threadproc1<pipe>,&test_1);
                create_new_thread(&threadproc2<pipe>,&test2_1);
                create_new_thread(&threadproc2<pipe>,&test2_1);
                create_new_thread(&threadproc2<pipe>,&test2_1);

                for (unsigned long i = 0; i < proc1_count; ++i)
                {
                    a = i;
                    test_1.enqueue(a);
                }
                CASSERT(test_1.is_enqueue_enabled() == true,"");
                test_1.disable_enqueue();
                CASSERT(test_1.is_enqueue_enabled() == false,"");
                for (unsigned long i = 0; i < proc1_count; ++i)
                {
                    a = i;
                    test_1.enqueue(a);
                }
                print_spinner();

                for (unsigned long i = 0; i < 100000; ++i)
                {
                    a = i;
                    if (i%2 == 0)
                        test2_1.enqueue(a);
                    else
                        test2_1.enqueue_or_timeout(a,100000);
                }

                test2_1.wait_for_num_blocked_dequeues(3);
                CASSERT(test2_1.size() == 0,"");
                test2_1.disable();

                wait_for_threads();
                CASSERT(test2_1.size() == 0,"");

                test2_1.enable();


                create_new_thread(&threadproc3<pipe>,&test2_1);
                create_new_thread(&threadproc3<pipe>,&test2_1);


                for (unsigned long i = 0; i < 100000; ++i)
                {
                    a = i;
                    if (i%2 == 0)
                        test2_1.enqueue(a);
                    else
                        test2_1.enqueue_or_timeout(a,100000);
                }

                test2_1.wait_for_num_blocked_dequeues(2);
                CASSERT(test2_1.size() == 0,"");
                test2_1.disable();

                wait_for_threads();
                CASSERT(test2_1.size() == 0,"");

            }

            test.enable_enqueue();
            test_0.enable_enqueue();
            test_1.enable_enqueue();

            CASSERT(test.is_enabled(),"");
            CASSERT(test.is_enqueue_enabled(),"");
            CASSERT(test_0.is_enabled(),"");
            CASSERT(test_0.is_enqueue_enabled(),"");
            CASSERT(test_1.is_enabled(),"");
            CASSERT(test_1.is_enqueue_enabled(),"");

            CASSERT(test.size() == 0,"");
            CASSERT(test_0.size() == 0,"");
            CASSERT(test_1.size() == 0,"");
            CASSERT(test.max_size() == 10,"");
            CASSERT(test_0.max_size() == 0,"");
            CASSERT(test_1.max_size() == 1,"");


            for (int i = 0; i < 100; ++i)
            {
                a = 1;
                test.enqueue_or_timeout(a,0);
                a = 1;
                test_0.enqueue_or_timeout(a,0);
                a = 1;
                test_1.enqueue_or_timeout(a,0);
            }

            CASSERT(test.size() == 10,"size: " << test.size() );
            CASSERT(test_0.size() == 0,"size: " << test.size() );
            CASSERT(test_1.size() == 1,"size: " << test.size() );

            for (int i = 0; i < 10; ++i)
            {
                a = 0;
                CASSERT(test.enqueue_or_timeout(a,10) == false,"");
                a = 0;
                CASSERT(test_0.enqueue_or_timeout(a,10) == false,"");
                a = 0;
                CASSERT(test_1.enqueue_or_timeout(a,10) == false,"");
            }

            CASSERT(test.size() == 10,"size: " << test.size() );
            CASSERT(test_0.size() == 0,"size: " << test.size() );
            CASSERT(test_1.size() == 1,"size: " << test.size() );

            for (int i = 0; i < 10; ++i)
            {
                a = 0;
                CASSERT(test.dequeue_or_timeout(a,0) == true,"");
                CASSERT(a == 1,"");
            }

            CASSERT(test.max_size() == 10,"");
            CASSERT(test_0.max_size() == 0,"");
            CASSERT(test_1.max_size() == 1,"");

            a = 0;
            CASSERT(test_1.dequeue_or_timeout(a,0) == true,"");

            CASSERT(test.max_size() == 10,"");
            CASSERT(test_0.max_size() == 0,"");
            CASSERT(test_1.max_size() == 1,"");


            CASSERT(a == 1,"a: " << a);

            CASSERT(test.size() == 0,"");
            CASSERT(test_0.size() == 0,"");
            CASSERT(test_1.size() == 0,"");

            CASSERT(test.dequeue_or_timeout(a,0) == false,"");
            CASSERT(test_0.dequeue_or_timeout(a,0) == false,"");
            CASSERT(test_1.dequeue_or_timeout(a,0) == false,"");
            CASSERT(test.dequeue_or_timeout(a,10) == false,"");
            CASSERT(test_0.dequeue_or_timeout(a,10) == false,"");
            CASSERT(test_1.dequeue_or_timeout(a,10) == false,"");

            CASSERT(test.size() == 0,"");
            CASSERT(test_0.size() == 0,"");
            CASSERT(test_1.size() == 0,"");

            if (found_error)
                return 1;
            else
                return 0;
        }
        catch(exception& e)
        {
            cout << "\n\nERRORS FOUND in " << type << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERRORS FOUND in " << type;
            dlog << LWARN << e.what();
            return 1;
        }        
    }




    class pipe_tester : public tester
    {
    public:
        pipe_tester (
        ) :
            tester ("test_pipe",
                    "Runs tests on the pipe component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += pipe_kernel_test<dlib::pipe<int>::kernel_1a>    ("kernel_1a");
            print_spinner();
            return (n == 0);
        }
    } a;

}


