// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_CMD_LINE_PARSER_KERNEl_TEST_H_
#define DLIB_CMD_LINE_PARSER_KERNEl_TEST_H_


#include <string>
#include <dlib/string.h>

#include <dlib/cmd_line_parser.h>

#include "tester.h"

namespace  
{
    using namespace test;
    using namespace dlib;
    using namespace std;

    logger dlog("test.cmd_line_parser");

    template <
        typename clp
        >
    int cmd_line_parser_kernel_test (
        const string& type
    )
    /*!
        requires
            - clp is an implementation of cmd_line_parser_kernel_abstract.h
        ensures
            - runs tests on clp for compliance with the specs 
            - returns 0 if no errors are found, returns 1 otherwise
    !*/
    {        
        typedef typename clp::char_type ct;
        typedef typename clp::string_type string_type;

        try 
        {

            

            int argc;
            ct* argv[100];            
            bool ok;
           
            for (int j = 0; j < 3; ++j)
            {
                clp test, test2;




                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start(),"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.current_element_valid() == false,"");
            


                CASSERT(test.parsed_line() == false,"");
                CASSERT(test.option_is_defined(_dT(ct,"a")) == false,"");
                CASSERT(test.option_is_defined(_dT(ct,"a")) == false,"");
                CASSERT(test.option_is_defined(_dT(ct,"a")) == false,"");

                CASSERT(test.parsed_line() == false,"");
                CASSERT(test.option_is_defined(_dT(ct,"a")) == false,"");
                CASSERT(test.option_is_defined(_dT(ct,"b")) == false,"");
                CASSERT(test.option_is_defined(_dT(ct,"\0")) == false,"");

                CASSERT(test.current_element_valid() == false,"");
                CASSERT(test.at_start() == false,"");
                CASSERT(test.move_next() == false,"");
                CASSERT(test.current_element_valid() == false,"");



                // program arg1 --davis arg2 -cZzarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis");
                argv[3] = _dT(ct,"arg2");
                argv[4] = _dT(ct,"-cZzarg");
                argv[5] = _dT(ct,"asdf");
                argc = 6;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"));
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                for (int k = 0; k < 5; ++k)
                {

                    try { test.parse(argc,argv); }
                    catch (error& e)
                    {
                        CASSERT(false,e.info);
                    }

                    CASSERT(test.option(_dT(ct,"davis")).name() == _dT(ct,"davis"),"");
                    CASSERT(test.option(_dT(ct,"c")).name() == _dT(ct,"c"),"");
                    CASSERT(test.option(_dT(ct,"Z")).name() == _dT(ct,"Z"),"");
                    CASSERT(test.option(_dT(ct,"davis")).number_of_arguments() == 0,"");
                    CASSERT(test.option(_dT(ct,"c")).number_of_arguments() == 0,"");
                    CASSERT(test.option(_dT(ct,"Z")).number_of_arguments() == 2,"");
                    CASSERT(test.number_of_arguments() == 2,"");
                    CASSERT(test[0] == _dT(ct,"arg1"),"");
                    CASSERT(test[1] == _dT(ct,"arg2"),"");
                    CASSERT(test.option(_dT(ct,"d")).count()==0,"");
                    CASSERT(test.option(_dT(ct,"davis")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"c")).count()==1,test.option(_dT(ct,"c")).count());
                    CASSERT(test.option(_dT(ct,"Z")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(0,0) == _dT(ct,"zarg"),"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(1,0) == _dT(ct,"asdf"),"");

                }



                swap(test,test2);

                



                // program arg1 --davis arg2 -cZ zarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis");
                argv[3] = _dT(ct,"arg2");
                argv[4] = _dT(ct,"-cZ");
                argv[5] = _dT(ct,"zarg");
                argv[6] = _dT(ct,"asdf");
                argc = 7;




                for (int k = 0; k < 5; ++k)
                {

                    try { test2.parse(argc,argv); }
                    catch (error& e)
                    {
                        CASSERT(false,e.info);
                    }

                    CASSERT(test2.option(_dT(ct,"davis")).name() == _dT(ct,"davis"),"");
                    CASSERT(test2.option(_dT(ct,"c")).name() == _dT(ct,"c"),"");
                    CASSERT(test2.option(_dT(ct,"Z")).name() == _dT(ct,"Z"),"");
                    CASSERT(test2.option(_dT(ct,"davis")).number_of_arguments() == 0,"");
                    CASSERT(test2.option(_dT(ct,"c")).number_of_arguments() == 0,"");
                    CASSERT(test2.option(_dT(ct,"Z")).number_of_arguments() == 2,"");
                    CASSERT(test2.number_of_arguments() == 2,"");
                    CASSERT(test2[0] == _dT(ct,"arg1"),"");
                    CASSERT(test2[1] == _dT(ct,"arg2"),"");
                    CASSERT(test2.option(_dT(ct,"d")).count()==0,"");
                    CASSERT(test2.option(_dT(ct,"davis")).count()==1,"");
                    CASSERT(test2.option(_dT(ct,"c")).count()==1,"");
                    CASSERT(test2.option(_dT(ct,"Z")).count()==1,"");
                    CASSERT(test2.option(_dT(ct,"Z")).argument(1,0) == _dT(ct,"asdf"),"");
                    CASSERT(test2.option(_dT(ct,"Z")).argument(0,0) == _dT(ct,"zarg"),
                           narrow(_dT(ct,"*") + test2.option(_dT(ct,"Z")).argument(0,0) + _dT(ct,"*")));
                    

                }





                // program arg1 --davis= darg darg2 arg2 -cZzarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis=");
                argv[3] = _dT(ct,"darg");
                argv[4] = _dT(ct,"darg2");
                argv[5] = _dT(ct,"arg2");
                argv[6] = _dT(ct,"-cZzarg");
                argv[7] = _dT(ct,"asdf");
                argc = 8;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 2);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                for (int k = 0; k < 5; ++k)
                {

                    try { test.parse(argc,argv); }
                    catch (error& e)
                    {
                        CASSERT(false,e.info);
                    }

                    CASSERT(test.parsed_line(),"");

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        if (test.element().name() == _dT(ct,"d"))
                        {
                            CASSERT(test.element().count() == 0,"");
                        }
                        else
                        {                            
                            CASSERT(test.element().count() == 1,"");
                        }

                    }
                    CASSERT(count == 4,count);

                    CASSERT(test.option(_dT(ct,"davis")).name() == _dT(ct,"davis"),"");
                    CASSERT(test.option(_dT(ct,"c")).name() == _dT(ct,"c"),"");
                    CASSERT(test.option(_dT(ct,"Z")).name() == _dT(ct,"Z"),"");
                    CASSERT(test.option(_dT(ct,"davis")).number_of_arguments() == 2,"");
                    CASSERT(test.option(_dT(ct,"c")).number_of_arguments() == 0,"");
                    CASSERT(test.option(_dT(ct,"Z")).number_of_arguments() == 2,"");
                    CASSERT(test.number_of_arguments() == 2,"");
                    CASSERT(test[0] == _dT(ct,"arg1"),"");
                    CASSERT(test[1] == _dT(ct,"arg2"),"");
                    CASSERT(test.option(_dT(ct,"d")).count()==0,"");
                    CASSERT(test.option(_dT(ct,"davis")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"c")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(0,0) == _dT(ct,"zarg"),"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(1,0) == _dT(ct,"asdf"),"");
                    CASSERT(test.option(_dT(ct,"davis")).argument(0,0) == _dT(ct,"darg"),"");
                    CASSERT(test.option(_dT(ct,"davis")).argument(1,0) == _dT(ct,"darg2"),
                           narrow(test.option(_dT(ct,"davis")).argument(1,0)));
                }










                test.clear();







                // program arg1 --dav-is=darg darg2 arg2 -cZzarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--dav-is=darg");
                argv[3] = _dT(ct,"darg2");
                argv[4] = _dT(ct,"arg2");
                argv[5] = _dT(ct,"-cZzarg");
                argv[6] = _dT(ct,"asdf");
                argc = 7;


                test.add_option(_dT(ct,"dav-is"),_dT(ct,"davis option"), 2);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                for (int k = 0; k < 5; ++k)
                {

                    try { test.parse(argc,argv); }
                    catch (error& e)
                    {
                        CASSERT(false,e.info);
                    }

                    CASSERT(test.parsed_line(),"");

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        if (test.element().name() == _dT(ct,"d"))
                        {
                            CASSERT(test.element().count() == 0,"");
                        }
                        else
                        {                            
                            CASSERT(test.element().count() == 1,"");
                        }

                    }
                    CASSERT(count == 4,count);

                    CASSERT(test.option(_dT(ct,"dav-is")).name() == _dT(ct,"dav-is"),"");
                    CASSERT(test.option(_dT(ct,"c")).name() == _dT(ct,"c"),"");
                    CASSERT(test.option(_dT(ct,"Z")).name() == _dT(ct,"Z"),"");
                    CASSERT(test.option(_dT(ct,"dav-is")).number_of_arguments() == 2,"");
                    CASSERT(test.option(_dT(ct,"c")).number_of_arguments() == 0,"");
                    CASSERT(test.option(_dT(ct,"Z")).number_of_arguments() == 2,"");
                    CASSERT(test.number_of_arguments() == 2,"");
                    CASSERT(test[0] == _dT(ct,"arg1"),"");
                    CASSERT(test[1] == _dT(ct,"arg2"),"");
                    CASSERT(test.option(_dT(ct,"d")).count()==0,"");
                    CASSERT(test.option(_dT(ct,"dav-is")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"c")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(0,0) == _dT(ct,"zarg"),"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(1,0) == _dT(ct,"asdf"),"");
                    CASSERT(test.option(_dT(ct,"dav-is")).argument(0,0) == _dT(ct,"darg"),"");
                    CASSERT(test.option(_dT(ct,"dav-is")).argument(1,0) == _dT(ct,"darg2"),
                           narrow(test.option(_dT(ct,"dav-is")).argument(1,0)));
                }









                test.clear();







                // program arg1 --davis=darg darg2 arg2 -cZzarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis=darg");
                argv[3] = _dT(ct,"darg2");
                argv[4] = _dT(ct,"arg2");
                argv[5] = _dT(ct,"-cZzarg");
                argv[6] = _dT(ct,"asdf");
                argc = 7;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 2);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                for (int k = 0; k < 5; ++k)
                {

                    try { test.parse(argc,argv); }
                    catch (error& e)
                    {
                        CASSERT(false,e.info);
                    }

                    CASSERT(test.parsed_line(),"");

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        if (test.element().name() == _dT(ct,"d"))
                        {
                            CASSERT(test.element().count() == 0,"");
                        }
                        else
                        {                            
                            CASSERT(test.element().count() == 1,"");
                        }

                    }
                    CASSERT(count == 4,count);

                    CASSERT(test.option(_dT(ct,"davis")).name() == _dT(ct,"davis"),"");
                    CASSERT(test.option(_dT(ct,"c")).name() == _dT(ct,"c"),"");
                    CASSERT(test.option(_dT(ct,"Z")).name() == _dT(ct,"Z"),"");
                    CASSERT(test.option(_dT(ct,"davis")).number_of_arguments() == 2,"");
                    CASSERT(test.option(_dT(ct,"c")).number_of_arguments() == 0,"");
                    CASSERT(test.option(_dT(ct,"Z")).number_of_arguments() == 2,"");
                    CASSERT(test.number_of_arguments() == 2,"");
                    CASSERT(test[0] == _dT(ct,"arg1"),"");
                    CASSERT(test[1] == _dT(ct,"arg2"),"");
                    CASSERT(test.option(_dT(ct,"d")).count()==0,"");
                    CASSERT(test.option(_dT(ct,"davis")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"c")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(0,0) == _dT(ct,"zarg"),"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(1,0) == _dT(ct,"asdf"),"");
                    CASSERT(test.option(_dT(ct,"davis")).argument(0,0) == _dT(ct,"darg"),"");
                    CASSERT(test.option(_dT(ct,"davis")).argument(1,0) == _dT(ct,"darg2"),
                           narrow(test.option(_dT(ct,"davis")).argument(1,0)));
                }









                test.clear();







                // program arg1 --davis=darg arg2 -cZzarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis=darg");
                argv[3] = _dT(ct,"arg2");
                argv[4] = _dT(ct,"-cZzarg");
                argv[5] = _dT(ct,"asdf");
                argc = 6;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 1);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                for (int k = 0; k < 5; ++k)
                {

                    try { test.parse(argc,argv); }
                    catch (error& e)
                    {
                        CASSERT(false,e.info);
                    }

                    CASSERT(test.parsed_line(),"");

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        if (test.element().name() == _dT(ct,"d"))
                        {
                            CASSERT(test.element().count() == 0,"");
                        }
                        else
                        {                            
                            CASSERT(test.element().count() == 1,"");
                        }

                    }
                    CASSERT(count == 4,count);

                    CASSERT(test.option(_dT(ct,"davis")).name() == _dT(ct,"davis"),"");
                    CASSERT(test.option(_dT(ct,"c")).name() == _dT(ct,"c"),"");
                    CASSERT(test.option(_dT(ct,"Z")).name() == _dT(ct,"Z"),"");
                    CASSERT(test.option(_dT(ct,"davis")).number_of_arguments() == 1,"");
                    CASSERT(test.option(_dT(ct,"c")).number_of_arguments() == 0,"");
                    CASSERT(test.option(_dT(ct,"Z")).number_of_arguments() == 2,"");
                    CASSERT(test.number_of_arguments() == 2,"");
                    CASSERT(test[0] == _dT(ct,"arg1"),"");
                    CASSERT(test[1] == _dT(ct,"arg2"),"");
                    CASSERT(test.option(_dT(ct,"d")).count()==0,"");
                    CASSERT(test.option(_dT(ct,"davis")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"c")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(0,0) == _dT(ct,"zarg"),"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(1,0) == _dT(ct,"asdf"),"");
                    CASSERT(test.option(_dT(ct,"davis")).argument(0,0) == _dT(ct,"darg"),"");
                }









                test.clear();






                // program arg1 --davis darg arg2 -cZzarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis");
                argv[3] = _dT(ct,"darg");
                argv[4] = _dT(ct,"arg2");
                argv[5] = _dT(ct,"-cZzarg");
                argv[6] = _dT(ct,"asdf");
                argc = 7;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 1);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                for (int k = 0; k < 5; ++k)
                {

                    try { test.parse(argc,argv); }
                    catch (error& e)
                    {
                        CASSERT(false,e.info);
                    }

                    CASSERT(test.parsed_line(),"");

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        if (test.element().name() == _dT(ct,"d"))
                        {
                            CASSERT(test.element().count() == 0,"");
                        }
                        else
                        {                            
                            CASSERT(test.element().count() == 1,"");
                        }

                    }
                    CASSERT(count == 4,count);

                    CASSERT(test.option(_dT(ct,"davis")).name() == _dT(ct,"davis"),"");
                    CASSERT(test.option(_dT(ct,"c")).name() == _dT(ct,"c"),"");
                    CASSERT(test.option(_dT(ct,"Z")).name() == _dT(ct,"Z"),"");
                    CASSERT(test.option(_dT(ct,"davis")).number_of_arguments() == 1,"");
                    CASSERT(test.option(_dT(ct,"c")).number_of_arguments() == 0,"");
                    CASSERT(test.option(_dT(ct,"Z")).number_of_arguments() == 2,"");
                    CASSERT(test.number_of_arguments() == 2,"");
                    CASSERT(test[0] == _dT(ct,"arg1"),"");
                    CASSERT(test[1] == _dT(ct,"arg2"),"");
                    CASSERT(test.option(_dT(ct,"d")).count()==0,"");
                    CASSERT(test.option(_dT(ct,"davis")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"c")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(0,0) == _dT(ct,"zarg"),"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(1) == _dT(ct,"asdf"),"");
                    CASSERT(test.option(_dT(ct,"davis")).argument(0,0) == _dT(ct,"darg"),"");
                }









                test.clear();

                // this string is incorrect because there is no avis option
                // program arg1 --avis darg arg2 -cZzarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--avis");
                argv[3] = _dT(ct,"darg");
                argv[4] = _dT(ct,"arg2");
                argv[5] = _dT(ct,"-cZzarg");
                argv[6] = _dT(ct,"asdf");
                argc = 7;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 1);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                for (int k = 0; k < 5; ++k)
                {

                    ok = false;
                    try { test.parse(argc,argv); }
                    catch (typename clp::cmd_line_parse_error& e)
                    {
                        CASSERT(e.type == EINVALID_OPTION,"");
                        CASSERT(e.item == _dT(ct,"avis"),"");
                        ok = true;
                    }
                    CASSERT(ok,"");


                }











                test.clear();

                // the c argument appears twice.  make sure its count is correct
                // program arg1 --davis darg arg2 -ccZzarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis");
                argv[3] = _dT(ct,"darg");
                argv[4] = _dT(ct,"arg2");
                argv[5] = _dT(ct,"-ccZ");
                argv[6] = _dT(ct,"zarg");
                argv[7] = _dT(ct,"asdf");
                argc = 8;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 1);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                for (int k = 0; k < 5; ++k)
                {

                    ok = false;
                    test.parse(argc,argv); 

                    CASSERT(test.option(_dT(ct,"c")).count()==2,"");

                }















                test.clear();

                // this is a bad line because the davis argument requires 2 arguments but
                // only gets one. 
                // program arg1 --davis darg darg2 --davis zarg 
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis");
                argv[3] = _dT(ct,"darg");
                argv[4] = _dT(ct,"darg2");
                argv[5] = _dT(ct,"--davis");
                argv[6] = _dT(ct,"zarg");
                argc = 7;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 2);
                test.add_option(_dT(ct,"b"),_dT(ct,"b option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                CASSERT(test.option(_dT(ct,"davis")).description() == _dT(ct,"davis option"),"");
                CASSERT(test.option(_dT(ct,"b")).description() == _dT(ct,"b option"),"");
                CASSERT(test.option(_dT(ct,"d")).description() == _dT(ct,"d option"),"");
                CASSERT(test.option(_dT(ct,"Z")).description() == _dT(ct,"Z option"),"");

                for (int k = 0; k < 5; ++k)
                {

                    ok = false;
                    try { test.parse(argc,argv); }
                    catch (typename clp::cmd_line_parse_error& e)
                    {
                        CASSERT(e.type == ETOO_FEW_ARGS,"");
                        CASSERT(e.num == 2,"");
                        CASSERT(e.item == _dT(ct,"davis"),"");
                        ok = true;
                    }
                    CASSERT(ok,"");

                    

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        CASSERT(test.element().count() == 0,"");
                        CASSERT(test.option_is_defined(test.element().name()),"");
                    }
                    CASSERT(count == 4,count);


                }


















                test.clear();

                // this is a bad line because the davis argument is not defined
                // program arg1 --davis darg arg2 -davis zarg asdf
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis");
                argv[3] = _dT(ct,"darg");
                argv[4] = _dT(ct,"arg2");
                argv[5] = _dT(ct,"--davis");
                argv[6] = _dT(ct,"zarg");
                argv[7] = _dT(ct,"asdf");
                argc = 8;


                test.add_option(_dT(ct,"mavis"),_dT(ct,"mavis option"), 1);
                test.add_option(_dT(ct,"b"),_dT(ct,"b option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                CASSERT(test.option(_dT(ct,"mavis")).description() == _dT(ct,"mavis option"),"");
                CASSERT(test.option(_dT(ct,"b")).description() == _dT(ct,"b option"),"");
                CASSERT(test.option(_dT(ct,"d")).description() == _dT(ct,"d option"),"");
                CASSERT(test.option(_dT(ct,"Z")).description() == _dT(ct,"Z option"),"");

                for (int k = 0; k < 5; ++k)
                {

                    ok = false;
                    try { test.parse(argc,argv); }
                    catch (typename clp::cmd_line_parse_error& e)
                    {
                        CASSERT(e.type == EINVALID_OPTION,"");
                        CASSERT(e.item == _dT(ct,"davis"),"");
                        ok = true;
                    }
                    CASSERT(ok,"");

                    

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        CASSERT(test.element().count() == 0,"");
                        CASSERT(test.option_is_defined(test.element().name()),"");
                    }
                    CASSERT(count == 4,count);


                }















                test.clear();


                argv[0] = _dT(ct,"program");
                argc = 1;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 1);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),2);


                CASSERT(test.option(_dT(ct,"davis")).description() == _dT(ct,"davis option"),"");
                CASSERT(test.option(_dT(ct,"c")).description() == _dT(ct,"c option"),"");
                CASSERT(test.option(_dT(ct,"d")).description() == _dT(ct,"d option"),"");
                CASSERT(test.option(_dT(ct,"Z")).description() == _dT(ct,"Z option"),"");

                for (int k = 0; k < 5; ++k)
                {
                   
                    test.parse(argc,argv); 
                    

                    CASSERT(test.number_of_arguments() == 0,"");

                    int count = 0;
                    while (test.move_next())
                    {
                        ++count;
                        CASSERT(test.element().count() == 0,"");
                        CASSERT(test.option_is_defined(test.element().name()),"");
                    }
                    CASSERT(count == 4,count);


                }












                test.clear();

                // this is to make sure the -- command works right
                // program arg1 --davis -darg -- arg2 -c asdf -Zeat -Zat -Zjoe's
                argv[0] = _dT(ct,"program");
                argv[1] = _dT(ct,"arg1");
                argv[2] = _dT(ct,"--davis");
                argv[3] = _dT(ct,"-darg");
                argv[4] = _dT(ct,"-Zeat");
                argv[5] = _dT(ct,"-Zat");
                argv[6] = _dT(ct,"-Zjoe's");
                argv[7] = _dT(ct,"--");
                argv[8] = _dT(ct,"arg2");
                argv[9] = _dT(ct,"-c");
                argv[10] = _dT(ct,"asdf");

                argc = 11;


                test.add_option(_dT(ct,"davis"),_dT(ct,"davis option"), 1);
                test.add_option(_dT(ct,"c"),_dT(ct,"c option"));
                test.add_option(_dT(ct,"d"),_dT(ct,"d option"));
                test.add_option(_dT(ct,"Z"),_dT(ct,"Z option"),1);


                CASSERT(test.option(_dT(ct,"davis")).description() == _dT(ct,"davis option"),"");
                CASSERT(test.option(_dT(ct,"c")).description() == _dT(ct,"c option"),"");
                CASSERT(test.option(_dT(ct,"d")).description() == _dT(ct,"d option"),"");
                CASSERT(test.option(_dT(ct,"Z")).description() == _dT(ct,"Z option"),"");

                for (int k = 0; k < 5; ++k)
                {

                    test.parse(argc,argv);

                    CASSERT(test.number_of_arguments() == 4,test.number_of_arguments());
                    CASSERT(test[0] == _dT(ct,"arg1"),"");
                    CASSERT(test[1] == _dT(ct,"arg2"),"");
                    CASSERT(test[2] == _dT(ct,"-c"),"");
                    CASSERT(test[3] == _dT(ct,"asdf"),"");

                    CASSERT(test.option(_dT(ct,"davis")).count()==1,"");
                    CASSERT(test.option(_dT(ct,"davis")).argument() == _dT(ct,"-darg"),"");
                    CASSERT(test.option(_dT(ct,"c")).count()==0,"");
                    CASSERT(test.option(_dT(ct,"d")).count()==0,"");
                    CASSERT(test.option(_dT(ct,"Z")).count()==3,"");

                    CASSERT(test.option(_dT(ct,"Z")).argument(0,0) == _dT(ct,"eat"),"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(0,1) == _dT(ct,"at"),"");
                    CASSERT(test.option(_dT(ct,"Z")).argument(0,2) == _dT(ct,"joe's"),"");


                }













            }

                




            return 0;
        }
        catch(error& e)
        {
            cout << "\n\nERRORS FOUND in " << type << endl;
            cout << e.info << endl;
            dlog << LWARN << "ERRORS FOUND in " << type;
            dlog << LWARN << e.info;
            return 1;
        }        
        catch (bad_cast& e)
        {
            cout << "\n\nERRORS FOUND (bad_cast) in " << type << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERRORS FOUND (bad_cast) in " << type;
            dlog << LWARN << e.what();
            return 1;
        }
    }

}


#endif // DLIB_CMD_LINE_PARSER_KERNEl_TEST_H_

