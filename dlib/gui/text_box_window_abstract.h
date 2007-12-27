// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_TEXT_BOX_WINDOw_ABSTRACT_
#ifdef DLIB_TEXT_BOX_WINDOw_ABSTRACT_

#include <string>
#include "gui_kernel_abstract.h"

namespace dlib
{

    class text_box_window : public window
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a window with two text boxes and 
                two buttons.  

                / **************************\
                |                           |
                |       top text box        |
                |                           |
                |***************************|
                |                           |
                |     bottom text box       |
                |                           |
                |***************************|
                | ***********   *********** |
                | * button1 *   * button2 * |
                | ***********   *********** |
                \************************** /

            NOTE
                if you redefine on_window_resized() then in your new definition
                you should also call text_box_window's on_window_resized() function to
                ensure that the window still resizes everything correctly when the 
                window is resized
        !*/

    public:

        text_box_window (
            bool resizable = true
        );
        /*!
            requires
                - is called from main program thread
                - winmain() function has been entered
            ensures
                - #*this is properly initialized
                - if (resizable == true) then 
                    - the window will be resizable by the user
                - else 
                    - the window will NOT be resizable by the user
        !*/

        virtual ~text_box_window (
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/


        void set_button1_name (
            const std::string& name
        );
        /*!
            ensures
                - the name of button1 will be name
        !*/

        void set_button2_name (
            const std::string& name
        );
        /*!
            ensures
                - the name of button2 will be name
        !*/


        const std::string get_top_text (
        ) const;
        /*!
            ensures
                - returns the text that appears in the top text box
        !*/


        const std::string get_bottom_text (
        ) const;
        /*!
            ensures
                - returns the text that appears in the bottom text box
        !*/

        void set_top_text (
            const std::string& text,
            int scroll = 0
        );
        /*!
            requires
                - -1 <= scroll <= 1
            ensures
                - the top text box contains the string from text 
                - if (scroll == -1) then
                    - the top text box will scroll to the top
                - if (scroll == 1) then
                    - the top text box will scroll to the bottom
        !*/


        void set_bottom_text (
            const std::string& text,
            int scroll = 0
        );
        /*!
            requires
                - -1 <= scroll <= 1
            ensures
                - the bottom text box contains the string from text 
                - if (scroll == -1) then
                    - the bottom text box will scroll to the top
                - if (scroll == 1) then
                    - the bottom text box will scroll to the bottom
        !*/

    protected:

        // do nothing by default
        virtual void on_button1_click (
        ){}
        /*!
            ensures
                - is called when button1 is clicked by the user
        !*/

        // do nothing by default
        virtual void on_button2_click (
        ){}
        /*!
            ensures
                - is called when button2 is clicked by the user
        !*/

    private:

        // restricted functions
        text_box_window(text_box_window&);        // copy constructor
        text_box_window& operator=(text_box_window&);    // assignment operator
    };

}

#endif // DLIB_TEXT_BOX_WINDOw_ABSTRACT_

