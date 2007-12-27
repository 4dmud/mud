// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_TEXT_BOX_WINDOw_
#define DLIB_TEXT_BOX_WINDOw_

#include "../gui.h"
#include "text_box_window_abstract.h"

namespace dlib
{

    class text_box_window : public window
    {

        int middle;        

    public:
        
        text_box_window (
            bool resizable = true
        ) : 
            window(resizable ),
            button1(*this),
            button2(*this),
            sb(*this),
            top(*this),
            bottom(*this)
        {

            button1.set_click_handler(&text_box_window::on_button1_click);
            button2.set_click_handler(&text_box_window::on_button2_click);
            sb.set_move_handler(&text_box_window::on_sb_move);

            set_size(510,380);

            middle = 190;

            int width, height;
            get_client_area(width,height);

            top.set_position(0,3);
            top.set_size(width,middle-3);
            bottom.set_position(0,middle+3);
            bottom.set_size(width,height - middle- 43);

            sb.set_size(width,3);
            sb.set_position(0,middle);
            sb.set_range(0,height-40);

            button2.set_position(width-80,height-35);
            button2.set_size(75,31);   

            button1.set_position(5,height-35);
            button1.set_size(75,31);
        }


        virtual ~text_box_window (
        ){}

        void set_button1_name (
            const std::string& name
        )
        {
            button1.set_name(name);
        }

        void set_button2_name (
            const std::string& name
        )
        {
            button2.set_name(name);
        }


        std::string get_top_text (
        ) const
        {
            return top.get_text();
        }


        std::string get_bottom_text (
        ) const
        {
            return bottom.get_text();
        }

        void set_top_text (
            const std::string& text,
            int scroll = 0
        )
        {
            top.set_text(text);
            if (scroll < 0)
                top.scroll_to_top();
            else if (scroll > 0)
                top.scroll_to_bottom();
        }


        void set_bottom_text (
            const std::string& text,
            int scroll = 0
        )
        {
            bottom.set_text(text);
            if (scroll < 0)
                bottom.scroll_to_top();
            else if (scroll > 0)
                bottom.scroll_to_bottom();
        }

    protected:

        // do nothing by default
        virtual void on_button1_click (
        ){}

        // do nothing by default
        virtual void on_button2_click (
        ){}

        void on_window_resized(
        )
        {
            int width, height;
            get_client_area(width,height);

            middle = (height-43)/2;

            top.set_size(width,middle-3);
            bottom.set_position(0,middle+3);
            bottom.set_size(width,height - middle- 43);

            sb.set_size(width,3);
            sb.set_position(0,middle);
            sb.set_range(0,height-40);

            button2.set_position(width-80,height-35);
            button2.set_size(75,31);   

            button1.set_position(5,height-35);
            button1.set_size(75,31);
        }

    private:

        void on_sb_move ()
        {
            int width, height;
            int temp, temp_middle;
            get_client_area(width,height);
            sb.get_position(temp,temp_middle); 

            middle = temp_middle;                
            bottom.set_position(0,middle+3);
            top.set_size(width,middle-3);
            bottom.set_size(width,height - middle- 43);
        }


        button<text_box_window> button1;
        button<text_box_window> button2;
        splitter<text_box_window> sb;

        text_box<text_box_window> top;
        text_box<text_box_window> bottom;

    private:

        // restricted functions
        text_box_window(text_box_window&);        // copy constructor
        text_box_window& operator=(text_box_window&);    // assignment operator
    };

}

#endif // DLIB_TEXT_BOX_WINDOw_

