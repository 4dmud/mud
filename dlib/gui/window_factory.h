// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_WINDOW_FACTORy_
#define DLIB_WINDOW_FACTORy_

#include "../gui.h"
#include "window_factory_abstract.h"
#include "../threads.h"

namespace dlib
{

    template <
        typename T
        >
    class window_factory : private window
    {
    public:

        // make button a friend so that it can cast window_factory objects 
        // to window objects.
        friend class button<window_factory<T > >;

        window_factory() : b(*this), b2(*this) 
        {
            b.set_click_handler(&window_factory::on_button_click);
            b2.set_click_handler(&window_factory::on_button2_click);
        }
        
        T* make_window (
        ) const 
        {
            T* temp;
            wfh_mutex.lock();
            b.click();
            temp = new_window;
            wfh_mutex.unlock();
            return temp;
        }

        void delete_window (
            T* w
        ) const
        {
            wfh_mutex.lock();
            new_window = w;
            b2.click();            
            wfh_mutex.unlock();
        }

    private:
        mutable T* new_window;
        dlib::mutex wfh_mutex;

        void on_button_click()
        {
            new_window = new T;
        }

        void on_button2_click()
        {
            delete new_window;
        }

        mutable button<window_factory<T> > b;
        mutable button<window_factory<T> > b2;


        // restricted functions
        window_factory(window_factory<T>&);        // copy constructor
        window_factory<T>& operator=(window_factory<T>&);    // assignment operator
    };


}

#endif // DLIB_WINDOW_FACTORy_

