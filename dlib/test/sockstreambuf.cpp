// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.


#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <dlib/sockets.h>
#include <dlib/misc_api.h>
#include <dlib/sockstreambuf.h>

#include "tester.h"

namespace  
{

    using namespace test;
    using namespace dlib;
    using namespace std;

    dlib::mutex m;
    dlib::signaler s(m);
    bool thread_running;

    logger dlog("test.sockstreambuf");

// ----------------------------------------------------------------------------------------

    template <typename ssb>
    void thread_proc (
        void* param
    )
    {
        
        listener& list = *reinterpret_cast<listener*>(param);
        connection* con;
        list.accept(con);

        ssb buf(con);
        ostream out(&buf);


        char ch;
        char* bigbuf = new char[1000000];


        for (int i = 'a'; i < 'z'; ++i)
        {
            ch = i;
            out << ch << " ";
        }

        out.put('A');

        for (int i = 0; i < 256; ++i)
        {
            ch = i;
            out.write(&ch,1);
        }

        for (int i = -100; i < 25600; ++i)
        {
            out << i << " ";
        }

        out.put('A');

        for (int i = -100; i < 25600; ++i)
        {
            out.write((char*)&i,sizeof(i));
        }

        for (int i = 0; i < 1000000; ++i)
        {
            bigbuf[i] = (i&0xFF);
        }
        out.write(bigbuf,1000000);

        out.put('d');
        out.put('a');
        out.put('v');
        out.put('i');
        out.put('s');


        string tstring = "this is a test";
        int tint = -853;
        unsigned int tuint = 89;
        serialize(tstring,out);
        serialize(tint,out);
        serialize(tuint,out);


        out.flush();


        auto_mutex M(m);
        thread_running = false;
        s.signal();

        dlib::sleep(300);
        delete con;
        delete &list;

        delete [] bigbuf;
    }

    template <typename ssb>
    int sockstreambuf_test (
    )
    /*!
        requires
            - ssb is an implementation of sockstreambuf/sockstreambuf_kernel_abstract.h 
        ensures
            - runs tests on ssb for compliance with the specs
            - returns 0 if there aren't any errors, 1 otherwise
    !*/
    {        
        char ch;
        char* bigbuf = new char[1000000];
        connection* con;

        try 
        {
            thread_running = true;
            listener* list;
            if (create_listener(list,0))
            {
                cout << "\n\nUnable to create a listener\n";
                dlog << LWARN << "Unable to create a listener";
                return 1;
            }

            create_new_thread(thread_proc<ssb>,list);

            if (create_connection(con,list->get_listening_port(),"127.0.0.1"))
            {
                cout << "\n\nUnable to create a connection\n";
                dlog << LWARN << "Unable to create a connection";
                return 1;
            }

            ssb buf(con);
            istream in(&buf);
            


            for (int i = 'a'; i < 'z'; ++i)
            {
                in >> ch;
                char c = i;
                CASSERT(ch == c,"ch: " << (int)ch << "  c: " << (int)c);
            }

            in.get();
            CASSERT(in.peek() == 'A', "*" << in.peek() << "*");
            in.get();

            for (int i = 0; i < 256; ++i)
            {
                in.read(&ch,1);
                char c = i;
                CASSERT(ch == c,"ch: " << (int)ch << "  c: " << (int)c );
            }

            for (int i = -100; i < 25600; ++i)
            {
                int n = 0;
                in >> n;
                CASSERT(n == i,"n: " << n << "   i:" << i);
            }

            in.get();
            CASSERT(in.peek() == 'A', "*" << in.peek() << "*");
            in.get();

            for (int i = -100; i < 25600; ++i)
            {
                int n;
                in.read((char*)&n,sizeof(n));
                CASSERT(n == i,"n: " << n << "   i:" << i);
            }

            in.read(bigbuf,1000000);
            for (int i = 0; i < 1000000; ++i)
            {
                CASSERT(bigbuf[i] == (char)(i&0xFF),"");
            }

            CASSERT(in.get() == 'd',"");
            CASSERT(in.get() == 'a',"");
            CASSERT(in.get() == 'v',"");
            CASSERT(in.get() == 'i',"");

            CASSERT(in.peek() == 's',"");

            CASSERT(in.get() == 's',"");

            in.putback('s');
            CASSERT(in.peek() == 's',"");

            CASSERT(in.get() == 's',"");


            string tstring;
            int tint;
            unsigned int tuint;
            deserialize(tstring,in);
            deserialize(tint,in);
            deserialize(tuint,in);

            CASSERT(tstring == "this is a test","");
            CASSERT(tint == -853,"");
            CASSERT(tuint == 89,"");



            auto_mutex M(m);
            while (thread_running)
                s.wait();

            delete [] bigbuf;
        }
        catch(error& e)
        {
            delete con;
            delete [] bigbuf;
            cout << "\n\nERRORS FOUND in " << typeid(ssb).name() << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND in " << typeid(ssb).name();
            dlog << LWARN << e.info;
            return 1;
        }        
        delete con;
        return 0;
    }

// ----------------------------------------------------------------------------------------


    class sockstreambuf_tester : public tester
    {
    public:
        sockstreambuf_tester (
        ) :
            tester ("test_sockstreambuf",
                    "Runs tests on the sockstreambuf component.")
        {}

        bool perform_test (
        )
        {
            int n = 0;
            print_spinner();
            n += sockstreambuf_test<sockstreambuf::kernel_1a>();
            print_spinner();
            n += sockstreambuf_test<sockstreambuf::kernel_2a>();
            return (n == 0);
        }
    } a;

}


