// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_TEST_CONDITIONING_CLASs_H_
#define DLIB_TEST_CONDITIONING_CLASs_H_


#include <sstream>
#include <string>
#include <ctime>
#include <cstdlib>

#include <dlib/conditioning_class.h>

#include "tester.h"

namespace  
{

    using namespace test;
    using namespace std;
    using namespace dlib;

    logger dlog("test.conditioning_class");

    template <
        typename cc,
        typename cc2
        >
    int conditioning_class_kernel_test (
        const string& type
    )
    /*!
        requires
            - cc is an implementation of conditioning_class/conditioning_class_kernel_abstract.h            
              the alphabet_size for cc is 256
            - cc2 is an implementation of conditioning_class/conditioning_class_kernel_abstract.h            
              the alphabet_size for cc2 is 2
        ensures
            - runs tests on cc for compliance with the specs 
            - returns 0 if no errors were found, 1 otherwise.
    !*/
    {        

        try 
        {
            srand(static_cast<unsigned int>(time(0)));







            typename cc::global_state_type gs;
            typename cc2::global_state_type gs2;


           

            for (int g = 0; g < 5; ++g)
            {
                print_spinner();
                unsigned long amount=g+1;
                cc2 test(gs2);
                cc2 test2(gs2);


                CASSERT(test.get_memory_usage() != 0,"");

                const unsigned long alphabet_size = 2;                
                

                CASSERT(test.get_total() == 1,"");

                CASSERT(test.get_count(alphabet_size-1)==1,"");
                for (unsigned long i = 0; i < alphabet_size-1; ++i)
                {
                    unsigned long low_count, high_count, total_count;
                    CASSERT(test.get_range(i,low_count,high_count,total_count) == 0,i);
                    CASSERT(test.get_count(i) == 0,"");
                    CASSERT(test.get_total() == 1,"");
                }



                for (unsigned long i = 0; i < alphabet_size; ++i)
                {
                    test.increment_count(i,static_cast<unsigned short>(amount));
                    unsigned long low_count = 0, high_count = 0, total_count = 0;

                    if (i ==alphabet_size-1)
                    {
                        CASSERT(test.get_range(i,low_count,high_count,total_count) == 1+amount,"");

                        CASSERT(high_count == low_count+1+amount,"");
                        CASSERT(total_count == test.get_total(),"");


                        CASSERT(test.get_count(i) == 1+amount,"");
                    }
                    else
                    {
                        CASSERT(test.get_range(i,low_count,high_count,total_count) == amount,"");

                        CASSERT(high_count == low_count+amount,"");
                        CASSERT(total_count == test.get_total(),"");


                        CASSERT(test.get_count(i) == amount,"");
                    }
                    CASSERT(test.get_total() == (i+1)*amount + 1,"");
                } 
                

                for (unsigned long i = 0; i < alphabet_size; ++i)
                {                
                    unsigned long temp = static_cast<unsigned long>(::rand()%40);
                    for (unsigned long j = 0; j < temp; ++j)
                    {
                        test.increment_count(i,static_cast<unsigned short>(amount));
                        if (i == alphabet_size-1)
                        {
                            CASSERT(test.get_count(i) == (j+1)*amount + 1 + amount,"");                    
                        }
                        else
                        {
                            CASSERT(test.get_count(i) == (j+1)*amount + amount,"");                    
                        }
                    }

                    unsigned long target = test.get_total()/2;
                    unsigned long symbol = i, low_count = 0, high_count = 0, total_count = 0;
     
                    if (i == alphabet_size-1)
                    {
                        CASSERT(test.get_range(symbol,low_count,high_count,total_count)==temp*amount+1+amount,"");
                        CASSERT(high_count-low_count == temp*amount+1+amount,"");
                    }
                    else
                    {
                        CASSERT(test.get_range(symbol,low_count,high_count,total_count)==temp*amount + amount,"");
                        CASSERT(high_count-low_count == temp*amount + amount,"");
                    }
                    CASSERT(total_count == test.get_total(),"");

                    test.get_symbol(target,symbol,low_count,high_count);
                    CASSERT(test.get_count(symbol) == high_count-low_count,"");
                    CASSERT(low_count <= target,"");
                    CASSERT(target < high_count,"");
                    CASSERT(high_count <= test.get_total(),"");

                }

                test.clear();


                for (unsigned long i = 0; i < alphabet_size-1; ++i)
                {
                    test.increment_count(i);
                    unsigned long low_count, high_count, total_count;
                    CASSERT(test.get_range(i,low_count,high_count,total_count) == 1,"");

                    CASSERT(high_count == low_count+1,"");
                    CASSERT(total_count == test.get_total(),"");

                    CASSERT(test.get_count(i) == 1,"");
                    CASSERT(test.get_total() == i+2,"");
                } 

           


                unsigned long counts[alphabet_size];


                print_spinner();
                for (int k = 0; k < 40; ++k)
                {
                    unsigned long range = ::rand()%50000 + 2;

                    test.clear();
                   
                    for (unsigned long i = 0; i < alphabet_size-1; ++i)
                        counts[i] = 0;
                    unsigned long total = 1;
                    counts[alphabet_size-1] = 1;


                    for (unsigned long i = 0; i < alphabet_size; ++i)
                    {                
                        unsigned long temp = static_cast<unsigned long>(::rand()%range);
                        for (unsigned long j = 0; j < temp; ++j)
                        {
                            test.increment_count(i);  

                            
                            if (total >= 65535)
                            {
                                total = 0;
                                for (unsigned long i = 0; i < alphabet_size; ++i)
                                {
                                    counts[i] >>= 1;
                                    total += counts[i];
                                }
                                if (counts[alphabet_size-1]==0)
                                {
                                    counts[alphabet_size-1] = 1;
                                    ++total;
                                }
                            }
                            counts[i] = counts[i] + 1;
                            ++total;
            

                        }


                        unsigned long temp_total = 0;
                        for (unsigned long a = 0; a < alphabet_size; ++a)
                        {
                            temp_total += test.get_count(a);
                        }
                        CASSERT(temp_total == test.get_total(),
                            "temp_total == " << temp_total << endl <<
                            "test.get_total() == " << test.get_total()
                            );

                        CASSERT(test.get_count(alphabet_size-1) == counts[alphabet_size-1],"");
                        CASSERT(test.get_total() == total,
                            "test.get_total() == " << test.get_total() << endl <<
                            "total == " << total
                            );

                        unsigned long target = test.get_total()/2;
                        unsigned long symbol = i, low_count = 0, high_count = 0, total_count = 0;


                        CASSERT(test.get_range(symbol,low_count,high_count,total_count)==counts[symbol],"");

                        if (counts[symbol] != 0)
                        {
                            CASSERT(total_count == total,"");

                            CASSERT(high_count <= total,"");
                            CASSERT(low_count < high_count,"");
                            CASSERT(high_count <= test.get_total(),"");
                            CASSERT(test.get_count(symbol) == high_count-low_count,"");
                        }


                        if (target < total)
                        {
                            test.get_symbol(target,symbol,low_count,high_count);


                            CASSERT(high_count <= total,"");
                            CASSERT(low_count < high_count,"");
                            CASSERT(high_count <= test.get_total(),"");
                            CASSERT(test.get_count(symbol) == high_count-low_count,"");
                            CASSERT(test.get_count(symbol) == counts[symbol],"");
                        }




                    }

                }

                print_spinner();

                for (unsigned long h = 0; h < 20; ++h)
                {
                    test.clear();
                    CASSERT(test.get_total() == 1,"");

                    // fill out test with some numbers
                    unsigned long temp = ::rand()%30000 + 50000;
                    for (unsigned long j = 0; j < temp; ++j)
                    {
                        unsigned long symbol = (unsigned long)::rand()%alphabet_size;
                        test.increment_count(symbol);                    
                    }

                    // make sure all symbols have a count of at least one
                    for (unsigned long j = 0; j < alphabet_size; ++j)
                    {   
                        if (test.get_count(j) == 0)
                            test.increment_count(j);
                    }

                    unsigned long temp_total = 0;
                    for (unsigned long j = 0; j < alphabet_size; ++j)
                    {
                        temp_total += test.get_count(j);
                    }
                    CASSERT(temp_total == test.get_total(),"");


                    unsigned long low_counts[alphabet_size];
                    unsigned long high_counts[alphabet_size];
                    // iterate over all the symbols
                    for (unsigned long j = 0; j < alphabet_size; ++j)
                    {
                        unsigned long total;
                        unsigned long count = test.get_range(j,low_counts[j],high_counts[j],total);
                        CASSERT(count == test.get_count(j),"");
                        CASSERT(count == high_counts[j] - low_counts[j],"");

                    }


                    // make sure get_symbol() matches what get_range() told us
                    for (unsigned long j = 0; j < alphabet_size; ++j)
                    {                    
                        for (unsigned long k = low_counts[j]; k < high_counts[j]; ++k)
                        {
                            unsigned long symbol, low_count, high_count;
                            test.get_symbol(k,symbol,low_count,high_count);
                            CASSERT(high_count - low_count == test.get_count(symbol),"");
                            CASSERT(j == symbol,
                                "j == " << j << endl <<
                                "k == " << k << endl <<
                                "symbol == " << symbol << endl <<
                                "low_counts[j] == " << low_counts[j] << endl <<
                                "high_counts[j] == " << high_counts[j] << endl <<
                                "low_counts[symbol] == " << low_counts[symbol] << endl <<
                                "high_counts[symbol] == " << high_counts[symbol] << endl << 
                                "low_count == " << low_count << endl << 
                                "high_count == " << high_count << endl << 
                                "temp.count(j) == " << test.get_count(j)
                                );
                            CASSERT(low_count == low_counts[j],
                                "symbol:        " << j << "\n" <<
                                "target:        " << k << "\n" <<
                                "low_count:     " << low_count << "\n" <<
                                "low_counts[j]: " << low_counts[j]);
                            CASSERT(high_count == high_counts[j],"");
                        }

                    }

                }



                print_spinner();

                for (int h = 0; h < 10; ++h)
                {


                    test.clear();
                   
                    for (unsigned long k = 0; k < alphabet_size-1; ++k)
                    {
                        counts[k] = 0;
                    }
                    counts[alphabet_size-1] = 1;
                    unsigned long total = 1;
                    unsigned long i = ::rand()%alphabet_size;

                    unsigned long temp = 65536;
                    for (unsigned long j = 0; j < temp; ++j)
                    {
                        test.increment_count(i);  

                        
                        if (total >= 65535)
                        {
                            total = 0;
                            for (unsigned long i = 0; i < alphabet_size; ++i)
                            {
                                counts[i] >>= 1;
                                total += counts[i];
                            }
                            if (counts[alphabet_size-1] == 0)
                            {
                                ++total;
                                counts[alphabet_size-1] = 1;
                            }
                        }
                        counts[i] = counts[i] + 1;
                        ++total;
        
                    }


                    CASSERT(test.get_total() == total,"");

                    unsigned long target = test.get_total()/2;
                    unsigned long symbol = i, low_count = 0, high_count = 0, total_count = 0;


                    CASSERT(test.get_range(symbol,low_count,high_count,total_count)==counts[symbol],"");

                    if (counts[symbol] != 0)
                    {
                        CASSERT(total_count == total,"");

                        CASSERT(high_count <= total,"");
                        CASSERT(low_count < high_count,"");
                        CASSERT(high_count <= test.get_total(),"");
                        CASSERT(test.get_count(symbol) == high_count-low_count,"");
                    }



                    test.get_symbol(target,symbol,low_count,high_count);


                    CASSERT(high_count <= total,"");
                    CASSERT(low_count < high_count,"");
                    CASSERT(high_count <= test.get_total(),"");
                    CASSERT(test.get_count(symbol) == high_count-low_count,"");
                    CASSERT(test.get_count(symbol) == counts[symbol],"");





                    

                }

            } // for (int g = 0; g < 5; ++g)












          
            for (int g = 0; g < 5; ++g)
            {
                print_spinner();
                unsigned long amount=g+1;
                cc test(gs);
                cc test2(gs);

                CASSERT(test.get_memory_usage() != 0,"");

                const unsigned long alphabet_size = 256;                
                

                CASSERT(test.get_total() == 1,"");

                CASSERT(test.get_count(alphabet_size-1)==1,"");
                for (unsigned long i = 0; i < alphabet_size-1; ++i)
                {
                    unsigned long low_count, high_count, total_count;
                    CASSERT(test.get_range(i,low_count,high_count,total_count) == 0,"");
                    CASSERT(test.get_count(i) == 0,"");
                    CASSERT(test.get_total() == 1,"");
                }


                bool oom = false;
                for (unsigned long i = 0; i < alphabet_size; ++i)
                {
                    bool status = test.increment_count(i,static_cast<unsigned short>(amount));
                    unsigned long low_count = 0, high_count = 0, total_count = 0;
                    if (!status)
                        oom = true;

                    if (status)
                    {
                        if (i ==alphabet_size-1)
                        {
                            CASSERT(test.get_range(i,low_count,high_count,total_count) == 1+amount,"");

                            CASSERT(high_count == low_count+1+amount,"");
                            CASSERT(total_count == test.get_total(),"");


                            CASSERT(test.get_count(i) == 1+amount,"");
                        }
                        else
                        {
                            CASSERT(test.get_range(i,low_count,high_count,total_count) == amount,"");

                            CASSERT(high_count == low_count+amount,"");
                            CASSERT(total_count == test.get_total(),"");


                            CASSERT(test.get_count(i) == amount,"");
                        }
                        if (!oom)
                            CASSERT(test.get_total() == (i+1)*amount + 1,"");
                    }
                } 
                

                oom = false;
                for (unsigned long i = 0; i < alphabet_size; ++i)
                {        
                    unsigned long temp = static_cast<unsigned long>(::rand()%40);
                    for (unsigned long j = 0; j < temp; ++j)
                    {
                        bool status = test.increment_count(i,static_cast<unsigned short>(amount));
                        if (!status)
                            oom = true;
                        if (status)
                        {
                            if (i == alphabet_size-1)
                            {
                                CASSERT(test.get_count(i) == (j+1)*amount + 1 + amount,"");                    
                            }
                            else
                            {
                                CASSERT(test.get_count(i) == (j+1)*amount + amount,"");                    
                            }
                        }
                    }

                    unsigned long target = test.get_total()/2;
                    unsigned long symbol = i, low_count = 0, high_count = 0, total_count = 0;
     
                    if (!oom)
                    {
                        if (i == alphabet_size-1)
                        {
                            CASSERT(test.get_range(symbol,low_count,high_count,total_count)==temp*amount+1+amount,"");
                            CASSERT(high_count-low_count == temp*amount+1+amount,"");
                        }
                        else
                        {
                            CASSERT(test.get_range(symbol,low_count,high_count,total_count)==temp*amount + amount,"");
                            CASSERT(high_count-low_count == temp*amount + amount,"");
                        }
                        CASSERT(total_count == test.get_total(),"");
                    

                        test.get_symbol(target,symbol,low_count,high_count);
                        CASSERT(test.get_count(symbol) == high_count-low_count,"");
                        CASSERT(low_count <= target,"");
                        CASSERT(target < high_count,"");
                        CASSERT(high_count <= test.get_total(),"");
                    }

                }

                test.clear();


                oom = false;
                for (unsigned long i = 0; i < alphabet_size-1; ++i)
                {
                    if(!test.increment_count(i))
                        oom = true;
                    unsigned long low_count, high_count, total_count;

                    if (!oom)
                    {
                        CASSERT(test.get_range(i,low_count,high_count,total_count) == 1,"");

                        CASSERT(high_count == low_count+1,"");
                        CASSERT(total_count == test.get_total(),"");

                        CASSERT(test.get_count(i) == 1,"");
                        CASSERT(test.get_total() == i+2,"");
                    }
                } 

           

                unsigned long counts[alphabet_size];


                for (int k = 0; k < 40; ++k)
                {
                    unsigned long range = ::rand()%50000 + 2;

                    test.clear();
                   
                    for (unsigned long i = 0; i < alphabet_size-1; ++i)
                        counts[i] = 0;
                    unsigned long total = 1;
                    counts[alphabet_size-1] = 1;


                    oom = false;
                    for (unsigned long i = 0; i < alphabet_size; ++i)
                    {                
                        unsigned long temp = static_cast<unsigned long>(::rand()%range);
                        for (unsigned long j = 0; j < temp; ++j)
                        {
                            if (!test.increment_count(i))
                                oom = true;

                            
                            if (total >= 65535)
                            {

                                total = 0;
                                for (unsigned long i = 0; i < alphabet_size; ++i)
                                {
                                    counts[i] >>= 1;
                                    total += counts[i];
                                }
                                if (counts[alphabet_size-1]==0)
                                {
                                    counts[alphabet_size-1] = 1;
                                    ++total;
                                }
                            }
                            counts[i] = counts[i] + 1;
                            ++total;
            

                        }


                        unsigned long temp_total = 0;
                        for (unsigned long a = 0; a < alphabet_size; ++a)
                        {
                            temp_total += test.get_count(a);
                        }

                        if (!oom)
                        {
                            CASSERT(temp_total == test.get_total(),
                                "temp_total == " << temp_total << endl <<
                                "test.get_total() == " << test.get_total()
                                );

                            CASSERT(test.get_count(alphabet_size-1) == counts[alphabet_size-1],"");
                            CASSERT(test.get_total() == total,
                                "test.get_total() == " << test.get_total() << endl <<
                                "total == " << total
                                );
                        }

                        unsigned long target = test.get_total()/2;
                        unsigned long symbol = i, low_count = 0, high_count = 0, total_count = 0;

                        if (!oom)
                        {

                            CASSERT(test.get_range(symbol,low_count,high_count,total_count)==counts[symbol],"");

                            if (counts[symbol] != 0)
                            {
                                CASSERT(total_count == total,"");

                                CASSERT(high_count <= total,"");
                                CASSERT(low_count < high_count,"");
                                CASSERT(high_count <= test.get_total(),"");
                                CASSERT(test.get_count(symbol) == high_count-low_count,"");
                            }


                            if (target < total)
                            {
                                test.get_symbol(target,symbol,low_count,high_count);


                                CASSERT(high_count <= total,"");
                                CASSERT(low_count < high_count,"");
                                CASSERT(high_count <= test.get_total(),"");
                                CASSERT(test.get_count(symbol) == high_count-low_count,"");
                                CASSERT(test.get_count(symbol) == counts[symbol],"");
                            }
                        }



                    }

                }

                oom = false;
                for (unsigned long h = 0; h < 20; ++h)
                {
                    test.clear();
                    CASSERT(test.get_total() == 1,"");

                    // fill out test with some numbers
                    unsigned long temp = ::rand()%30000 + 50000;
                    for (unsigned long j = 0; j < temp; ++j)
                    {
                        unsigned long symbol = (unsigned long)::rand()%alphabet_size;
                        if (!test.increment_count(symbol))
                            oom = true;
                    }

                    // make sure all symbols have a count of at least one
                    for (unsigned long j = 0; j < alphabet_size; ++j)
                    {   
                        if (test.get_count(j) == 0)
                            test.increment_count(j);
                    }

                    unsigned long temp_total = 0;
                    for (unsigned long j = 0; j < alphabet_size; ++j)
                    {
                        temp_total += test.get_count(j);
                    }
                    if (!oom)
                        CASSERT(temp_total == test.get_total(),"");


                    unsigned long low_counts[alphabet_size];
                    unsigned long high_counts[alphabet_size];

                    if (!oom)
                    {

                        // iterate over all the symbols
                        for (unsigned long j = 0; j < alphabet_size; ++j)
                        {
                            unsigned long total;
                            unsigned long count = test.get_range(j,low_counts[j],high_counts[j],total);
                            CASSERT(count == test.get_count(j),"");
                            CASSERT(count == high_counts[j] - low_counts[j],"");

                        }

                    


                        // make sure get_symbol() matches what get_range() told us
                        for (unsigned long j = 0; j < alphabet_size; ++j)
                        {                    
                            for (unsigned long k = low_counts[j]; k < high_counts[j]; ++k)
                            {
                                unsigned long symbol, low_count, high_count;
                                test.get_symbol(k,symbol,low_count,high_count);
                                CASSERT(high_count - low_count == test.get_count(symbol),"");
                                CASSERT(j == symbol,
                                    "j == " << j << endl <<
                                    "k == " << k << endl <<
                                    "symbol == " << symbol << endl <<
                                    "low_counts[j] == " << low_counts[j] << endl <<
                                    "high_counts[j] == " << high_counts[j] << endl <<
                                    "low_counts[symbol] == " << low_counts[symbol] << endl <<
                                    "high_counts[symbol] == " << high_counts[symbol] << endl << 
                                    "low_count == " << low_count << endl << 
                                    "high_count == " << high_count << endl << 
                                    "temp.count(j) == " << test.get_count(j)
                                    );
                                CASSERT(low_count == low_counts[j],
                                    "symbol:        " << j << "\n" <<
                                    "target:        " << k << "\n" <<
                                    "low_count:     " << low_count << "\n" <<
                                    "low_counts[j]: " << low_counts[j]);
                                CASSERT(high_count == high_counts[j],"");
                            }

                        }
                    }

                }




                for (int h = 0; h < 10; ++h)
                {


                    test.clear();
                   
                    for (unsigned long k = 0; k < alphabet_size-1; ++k)
                    {
                        counts[k] = 0;
                    }
                    counts[alphabet_size-1] = 1;
                    unsigned long total = 1;
                    unsigned long i = ::rand()%alphabet_size;

                    unsigned long temp = 65536;
                    for (unsigned long j = 0; j < temp; ++j)
                    {
                        test.increment_count(i);  

                        
                        if (total >= 65535)
                        {
                            total = 0;
                            for (unsigned long i = 0; i < alphabet_size; ++i)
                            {
                                counts[i] >>= 1;
                                total += counts[i];
                            }
                            if (counts[alphabet_size-1] == 0)
                            {
                                ++total;
                                counts[alphabet_size-1] = 1;
                            }
                        }
                        counts[i] = counts[i] + 1;
                        ++total;
        
                    }


                    CASSERT(test.get_total() == total,"");

                    unsigned long target = test.get_total()/2;
                    unsigned long symbol = i, low_count = 0, high_count = 0, total_count = 0;


                    CASSERT(test.get_range(symbol,low_count,high_count,total_count)==counts[symbol],"");

                    if (counts[symbol] != 0)
                    {
                        CASSERT(total_count == total,"");

                        CASSERT(high_count <= total,"");
                        CASSERT(low_count < high_count,"");
                        CASSERT(high_count <= test.get_total(),"");
                        CASSERT(test.get_count(symbol) == high_count-low_count,"");
                    }



                    test.get_symbol(target,symbol,low_count,high_count);


                    CASSERT(high_count <= total,"");
                    CASSERT(low_count < high_count,"");
                    CASSERT(high_count <= test.get_total(),"");
                    CASSERT(test.get_count(symbol) == high_count-low_count,"");
                    CASSERT(test.get_count(symbol) == counts[symbol],"");





                    

                }

            } // for (int g = 0; g < 5; ++g)


            return 0;
        }
        catch(error& e)
        {
            cout << "\n\nERRORS FOUND in " << type << endl;
            cout << e.what() << endl; 
            dlog << LWARN << "ERRORS FOUND in " << type;
            dlog << LWARN << e.info;
            return 1;
        }    
        catch (exception& e)
        {
            cout << "ERROR OCCURRED (probably ran out of memory)" << endl;
            cout << e.what() << endl;
            dlog << LWARN << "ERROR OCCURRED (probably ran out of memory)";
            dlog << LWARN << e.what();
            return 1;
        }
    }

}

#endif // DLIB_TEST_CONDITIONING_CLASs_H_

