// Copyright (C) 2007  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <string>
#include <sstream>

#include <dlib/threads.h>
#include "tester.h"

namespace  
{
    using namespace test;
    using namespace std;
    using namespace dlib;
  
    logger dlog("test.multithreaded_object");

    dlib::mutex cm;
    int count;

    class test1 :  multithreaded_object
    {
    public:
        test1 ()
        {
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            clear();
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
        }

        ~test1 ()
        {
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            stop();
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            wait();
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
        }

    private:
    };

    class test2 : private multithreaded_object
    {
    public:
        test2()
        {
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            register_thread(*this,&test2::thread);
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            clear();
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            register_thread(*this,&test2::thread);
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
        }

        ~test2()
        {
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            stop();
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            wait();
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
        }

    private:

        void thread()
        {
            auto_mutex M(cm);
            ++count;
        }

    };

    class test3_c1 : private multithreaded_object
    {
    public:
        test3_c1()
        {
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            register_thread(*this,&test3_c1::thread);
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            start();
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(is_running() == true,"");
        }

        ~test3_c1()
        {
            CASSERT(number_of_threads_registered() == 1,"");
            stop();
            CASSERT(is_running() == false,"");
            CASSERT(number_of_threads_registered() == 1,"");
            wait();
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
        }

    private:

        void thread()
        {
            cm.lock();
            ++count;
            cm.unlock();
            // wait until we are supposed to stop
            while (!should_stop())
                dlib::sleep(1);
        }

    };

    class test4_c2 : private multithreaded_object
    {
    public:
        test4_c2()
        {
            CASSERT(number_of_threads_registered() == 0,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            register_thread(*this,&test4_c2::thread);
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
            start();
            CASSERT(number_of_threads_registered() == 1,"");
            CASSERT(number_of_threads_alive() == 1,"");
            CASSERT(is_running() == true,"");
            register_thread(*this,&test4_c2::thread);
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(number_of_threads_alive() == 2,"");
            CASSERT(is_running() == true,"");
            start();
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(number_of_threads_alive() == 2,"");
            CASSERT(is_running() == true,"");
            start();
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(number_of_threads_alive() == 2,"");
            CASSERT(is_running() == true,"");
            start();
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(number_of_threads_alive() == 2,"");
            CASSERT(is_running() == true,"");
            start();
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(number_of_threads_alive() == 2,"");
            CASSERT(is_running() == true,"");
            pause();
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(number_of_threads_alive() == 2,"");
            CASSERT(is_running() == false,"");
        }

        ~test4_c2()
        {
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(number_of_threads_alive() == 2,"");
            CASSERT(is_running() == false,"is_running(): " << is_running());
            stop();
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(is_running() == false,"");
            wait();
            CASSERT(number_of_threads_registered() == 2,"");
            CASSERT(number_of_threads_alive() == 0,"");
            CASSERT(is_running() == false,"");
        }

    private:

        void thread()
        {
            auto_mutex M(cm);
            ++count;
            while (!should_stop())
                dlib::sleep(10);
        }

    };


    class test5 : private multithreaded_object
    {
    public:
        test5()
        {
            register_thread(*this,&test5::thread1);
            register_thread(*this,&test5::thread2);
            register_thread(*this,&test5::thread3);
            register_thread(*this,&test5::thread3);
            start();
        }

        ~test5()
        {
            stop();
            wait();
        }

    private:

        void thread1()
        {
            while (!should_stop())
                dlib::sleep(10);
        }

        void thread2()
        {
            while (!should_stop())
                dlib::sleep(10);
        }

        void thread3()
        {
            while (!should_stop())
                dlib::sleep(10);
        }

    };


    int multithreaded_object_test (
    )
    /*!
        ensures
            - runs tests on dlib::multithreaded_object for compliance with the specs 
            - returns 0 if no errors are found, 1 otherwise
    !*/
    {        

        try 
        {
            count = 0;
 
            for (int i = 0; i < 5; ++i)
            {
                {
                    test1 a1;
                    test2 a2;
                    test3_c1 a3;
                    test4_c2 a4;
                    test5 a5;
                }
                CASSERT(count == (i+1)*3,"");
            }
            count = 0;

            for (int i = 0; i < 5; ++i)
            {
                {
                    test1 a1;
                    test2 a2;
                    test3_c1 a3;
                    test4_c2 a4;
                    test5 a5;
                    dlib::sleep(50);
                }
                CASSERT(count == (i+1)*3,"");
            }
            return 0;
        }
        catch(error e)
        {
            cout << "\n\nERRORS FOUND in multithreaded_object" << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND in multithreaded_object";
            dlog << LWARN << e.info;
            return 1;
        }        
    }


    class multithreaded_object_tester : public tester
    {
    public:
        multithreaded_object_tester (
        ) :
            tester ("test_multithreaded_object",
                    "Runs tests on the multithreaded_object component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            print_spinner();
            n += multithreaded_object_test();
            return (n == 0);
        }
    } a;

}



