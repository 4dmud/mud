// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_WINDOW_FACTORy_ABSTRACT_
#ifdef DLIB_WINDOW_FACTORy_ABSTRACT_

#include "gui_kernel_abstract.h"

namespace dlib
{

    template <
        typename T
        >
    class window_factory  
    {
        /*!
           WHAT THIS OBJECT REPRESENTS
                This object represents a factory for creating windows.  
                The reason this object is useful is that it allows you to 
                construct/destruct windows outside of the main program thread.

            THREAD SAFTY
                It is always safe to call make_window() and delete_window()
        !*/

    public:
        
        window_factory (
        );
        /*!
            requires
                - is called from the main program thread
                - winmain() function has been entered
            ensures
                - #*this has been properly initialized 
        !*/

        T* make_window (
        ) const;
        /*!
            ensures
                - makes a new window of type T and returns a pointer to it.
                  Note that the window is created via a call to new so someone
                  must remember to call delete (or delete_window()) on it at some point.
        !*/

        void delete_window (
            T* w
        ) const;
        /*!
            ensures
                - deletes the window pointed to by w and does so from the main program
                  thread.
        !*/

    private:

        // restricted functions
        window_factory(window_factory<T>&);        // copy constructor
        window_factory<T>& operator=(window_factory<T>&);    // assignment operator
    };

}

#endif // DLIB_WINDOW_FACTORy_ABSTRACT_

