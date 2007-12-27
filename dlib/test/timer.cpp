// Copyright (C) 2007  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>

#include <dlib/timer.h>
#include <dlib/timeout.h>
#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.timer");

    class timer_test_helper
    {
    public:
        mutex m;
        int count;

        timer_test_helper():count(0){}
        void add() 
        { 
            m.lock(); 
            print_spinner();
            ++count; 
            m.unlock(); 
        }

        void delayed_add()
        {
            dlib::sleep(1000);
            add();
        }
    };

    template <
        typename timer_t
        >
    int timer_test (
        const string& type 
    )
    /*!
        requires
            - timer_t is an implementation of timer/timer_kernel_aseqract.h is instantiated 
              timer_test_helper
        ensures
            - runs tests on timer_t for compliance with the specs 
            - returns 0 if no errors were detected, 1 otherwise.
    !*/
    {        

        try 
        {
            for (int j = 0; j < 3; ++j)
            {
                timer_test_helper h;

                timer_t t1(h,&timer_test_helper::add);
                timer_t t2(h,&timer_test_helper::add);
                timer_t t3(h,&timer_test_helper::add);

                CASSERT(t1.delay_time() == 1000,"");
                CASSERT(t2.delay_time() == 1000,"");
                CASSERT(t3.delay_time() == 1000,"");
                CASSERT(t1.is_running() == false,"");
                CASSERT(t2.is_running() == false,"");
                CASSERT(t3.is_running() == false,"");
                CASSERT(t1.action_function() == &timer_test_helper::add,"");
                CASSERT(t2.action_function() == &timer_test_helper::add,"");
                CASSERT(t3.action_function() == &timer_test_helper::add,"");
                CASSERT(&t1.action_object() == &h,"");
                CASSERT(&t2.action_object() == &h,"");
                CASSERT(&t3.action_object() == &h,"");

                t1.set_delay_time(1000);
                t2.set_delay_time(500);
                t3.set_delay_time(200);

                CASSERT(t1.delay_time() == 1000,"");
                CASSERT(t2.delay_time() == 500,"");
                CASSERT(t3.delay_time() == 200,"");
                CASSERT(t1.is_running() == false,"");
                CASSERT(t2.is_running() == false,"");
                CASSERT(t3.is_running() == false,"");
                CASSERT(t1.action_function() == &timer_test_helper::add,"");
                CASSERT(t2.action_function() == &timer_test_helper::add,"");
                CASSERT(t3.action_function() == &timer_test_helper::add,"");
                CASSERT(&t1.action_object() == &h,"");
                CASSERT(&t2.action_object() == &h,"");
                CASSERT(&t3.action_object() == &h,"");
                dlib::sleep(1100);
                print_spinner();
                CASSERT(h.count == 0,"");

                t1.stop_and_wait();
                t2.stop_and_wait();
                t3.stop_and_wait();

                dlib::sleep(1100);
                print_spinner();
                CASSERT(h.count == 0,"");
                CASSERT(t1.delay_time() == 1000,"");
                CASSERT(t2.delay_time() == 500,"");
                CASSERT(t3.delay_time() == 200,"");
                CASSERT(t1.is_running() == false,"");
                CASSERT(t2.is_running() == false,"");
                CASSERT(t3.is_running() == false,"");
                CASSERT(t1.action_function() == &timer_test_helper::add,"");
                CASSERT(t2.action_function() == &timer_test_helper::add,"");
                CASSERT(t3.action_function() == &timer_test_helper::add,"");
                CASSERT(&t1.action_object() == &h,"");
                CASSERT(&t2.action_object() == &h,"");
                CASSERT(&t3.action_object() == &h,"");

                t1.start();
                t2.start();
                t3.start();

                CASSERT(t1.delay_time() == 1000,"");
                CASSERT(t2.delay_time() == 500,"");
                CASSERT(t3.delay_time() == 200,"");
                CASSERT(t1.is_running() == true,"");
                CASSERT(t2.is_running() == true,"");
                CASSERT(t3.is_running() == true,"");
                CASSERT(t1.action_function() == &timer_test_helper::add,"");
                CASSERT(t2.action_function() == &timer_test_helper::add,"");
                CASSERT(t3.action_function() == &timer_test_helper::add,"");
                CASSERT(&t1.action_object() == &h,"");
                CASSERT(&t2.action_object() == &h,"");
                CASSERT(&t3.action_object() == &h,"");

                t1.stop();
                t2.stop();
                t3.stop();

                CASSERT(t1.delay_time() == 1000,"");
                CASSERT(t2.delay_time() == 500,"");
                CASSERT(t3.delay_time() == 200,"");
                CASSERT(t1.is_running() == false,"");
                CASSERT(t2.is_running() == false,"");
                CASSERT(t3.is_running() == false,"");
                CASSERT(t1.action_function() == &timer_test_helper::add,"");
                CASSERT(t2.action_function() == &timer_test_helper::add,"");
                CASSERT(t3.action_function() == &timer_test_helper::add,"");
                CASSERT(&t1.action_object() == &h,"");
                CASSERT(&t2.action_object() == &h,"");
                CASSERT(&t3.action_object() == &h,"");

                CASSERT(h.count == 0,"");
                dlib::sleep(1100);
                print_spinner();
                CASSERT(h.count == 0,"");

                for (int i = 1; i <= 3; ++i)
                {
                    t1.start();
                    t2.start();
                    t3.start();

                    CASSERT(t1.is_running() == true,"");
                    CASSERT(t2.is_running() == true,"");
                    CASSERT(t3.is_running() == true,"");

                    dlib::sleep(1100);
                    // this should allow the timers to trigger 8 times
                    t1.stop();
                    t2.stop();
                    t3.stop();

                    CASSERT(h.count == 8*i,"h.count: " << h.count << " i: " << i);
                    dlib::sleep(1100);
                    CASSERT(h.count == 8*i,"h.count: " << h.count << " i: " << i);
                }


                h.count = 0;
                t1.start();
                dlib::sleep(300);
                CASSERT(h.count == 0,h.count);
                t1.set_delay_time(400);
                dlib::sleep(200);
                CASSERT(h.count == 1,h.count);
                dlib::sleep(250);
                CASSERT(h.count == 1,h.count);
                dlib::sleep(100);
                CASSERT(h.count == 2,h.count);
                t1.set_delay_time(2000);
                CASSERT(h.count == 2,h.count);
                dlib::sleep(1000);
                CASSERT(h.count == 2,h.count);
                t1.clear();

                h.count = 0;
                t3.start();
                CASSERT(t3.is_running() == true,"");
                CASSERT(t3.delay_time() == 200,"");
                CASSERT(h.count == 0,h.count);
                t3.clear();
                CASSERT(t3.is_running() == false,"");
                CASSERT(t3.delay_time() == 1000,"");
                CASSERT(h.count == 0,h.count);
                dlib::sleep(200);
                CASSERT(t3.is_running() == false,"");
                CASSERT(t3.delay_time() == 1000,"");
                CASSERT(h.count == 0,h.count);


                {
                    h.count = 0;
                    timer_t t4(h,&timer_test_helper::delayed_add);
                    t4.set_delay_time(100);
                    t4.start();
                    CASSERT(h.count == 0,h.count);
                    dlib::sleep(400);
                    CASSERT(h.count == 0,h.count);
                    t4.stop_and_wait();
                    CASSERT(h.count == 1,h.count);
                    CASSERT(t4.is_running() == false,"");
                }

                {
                    h.count = 0;
                    timer_t t4(h,&timer_test_helper::delayed_add);
                    t4.set_delay_time(100);
                    t4.start();
                    CASSERT(h.count == 0,h.count);
                    dlib::sleep(400);
                    CASSERT(h.count == 0,h.count);
                    t4.clear();
                    CASSERT(t4.is_running() == false,"");
                    CASSERT(h.count == 0,h.count);
                    t4.stop_and_wait();
                    CASSERT(h.count == 1,h.count);
                    CASSERT(t4.is_running() == false,"");
                }

                {
                    h.count = 0;
                    timer_t t5(h,&timer_test_helper::delayed_add);
                    t5.set_delay_time(100);
                    t5.start();
                    CASSERT(h.count == 0,h.count);
                    dlib::sleep(400);
                    CASSERT(h.count == 0,h.count);
                }
                CASSERT(h.count == 1,h.count);

            }


            return 0;
        }
        catch(error e)
        {
            cout << "\n\nERRORS FOUND in " << type << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND in " << type;
            dlog << LWARN << e.info;
            return 1;
        }        
    }




    class timer_tester : public tester
    {
    public:
        timer_tester (
        ) :
            tester ("test_timer",
                    "Runs tests on the timer component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            n += timer_test<timer<timer_test_helper>::kernel_1a>    ("kernel_1a");
            print_spinner();
            n += timer_test<timer<timer_test_helper>::kernel_2a>    ("kernel_2a");
            print_spinner();
            return (n == 0);
        }
    } a;

}


