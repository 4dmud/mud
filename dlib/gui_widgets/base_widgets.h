// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#ifndef DLIB_BASE_WIDGETs_
#define DLIB_BASE_WIDGETs_

#include "base_widgets_abstract.h"
#include "drawable.h"
#include "../gui_core.h"
#include "../algs.h"
#include "../member_function_pointer.h"
#include "../timer.h"
#include "../map.h"
#include "../array2d.h"
#include "../pixel.h"
#include "../image_transforms.h"



namespace dlib
{

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class dragable
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class dragable : public drawable
    {
        /*!
            INITIAL VALUE
                - drag == false

            CONVENTION
                - if (the user is holding the left button down over this object) then
                    - drag == true
                    - x == the x position of the mouse relative to the upper left corner
                      of this object.
                    - y == the y position of the mouse relative to the upper left corner
                      of this object.
                - else
                    - drag == false
        !*/

    public:

        dragable(  
            drawable_window& w,
            unsigned long events = 0
        ) : 
            drawable(w,events | MOUSE_MOVE | MOUSE_CLICK),
            drag(false)
        {}

        virtual ~dragable(
        ) = 0;

        rectangle dragable_area (
        ) const { auto_mutex M(m); return area; }

        void set_dragable_area (
            const rectangle& area_ 
        ) { auto_mutex M(m); area = area_; } 

    protected:

        virtual void on_drag (
        ){}

        void on_mouse_move (
            unsigned long state,
            long x,
            long y
        );

        void on_mouse_down (
            unsigned long btn,
            unsigned long ,
            long x,
            long y,
            bool 
        );

    private:

        rectangle area;
        bool drag;
        long x, y;

        // restricted functions
        dragable(dragable&);        // copy constructor
        dragable& operator=(dragable&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class button_action 
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class button_action : public drawable
    {
        /*!
            INITIAL VALUE
                - is_depressed_ == false
                - seen_click == false

            CONVENTION
                - is_depressed_ == is_depressed()
                - if (the user has clicked the button but hasn't yet released the
                      left mouse button) then
                    - seen_click == true
                - else 
                    - seen_click == false
        !*/

    public:

        button_action(  
            drawable_window& w,
            unsigned long events = 0
        ) :
            drawable(w,events | MOUSE_MOVE | MOUSE_CLICK),
            is_depressed_(false),
            seen_click(false)
        {}


        virtual ~button_action(
        ) = 0;

    protected:

        bool is_depressed (
        ) const;

        virtual void on_button_down (
        ){}

        virtual void on_button_up (
            bool mouse_over
        ){}

        void on_mouse_down (
            unsigned long btn,
            unsigned long ,
            long x,
            long y,
            bool
        );

        void on_mouse_leave (
        );

        void on_mouse_move (
            unsigned long state,
            long x,
            long y
        );

        void on_mouse_up (
            unsigned long btn,
            unsigned long,
            long x,
            long y
        );


    private:
        mutable bool is_depressed_;
        bool seen_click;

        void on_user_event (
            int 
        );

        // restricted functions
        button_action(button_action&);        // copy constructor
        button_action& operator=(button_action&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class arrow_button 
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class arrow_button : public button_action 
    {
        /*!
            INITIAL VALUE
                dir == whatever is given to the constructor

            CONVENTION
                - dir == direction()
        !*/

    public:
        enum arrow_direction 
        {
            UP,
            DOWN,
            LEFT,
            RIGHT
        };

        arrow_button(  
            drawable_window& w,
            arrow_direction dir_ 
        ) : 
            button_action(w),
            dir(dir_)
        {
            enable_events();
        }

        virtual ~arrow_button(
        ){ disable_events();  parent.invalidate_rectangle(rect); }

        arrow_direction direction (
        ) const 
        { 
            auto_mutex M(m); 
            return dir; 
        }

        void set_direction (
            arrow_direction new_direction
        )
        {
            auto_mutex M(m);
            dir = new_direction;
            parent.invalidate_rectangle(rect);
        }

        void set_size (
            unsigned long width,
            unsigned long height
        );

        bool is_depressed (
        ) const
        {
            auto_mutex M(m);
            return button_action::is_depressed();
        }

        template <
            typename T
            >
        void set_button_down_handler (
            T& object,
            void (T::*event_handler)()
        )
        {
            auto_mutex M(m);
            button_down_handler.set(object,event_handler);
        }

        template <
            typename T
            >
        void set_button_up_handler (
            T& object,
            void (T::*event_handler)(bool mouse_over)
        )
        {
            auto_mutex M(m);
            button_up_handler.set(object,event_handler);
        }

    protected:

        void draw (
            const canvas& c
        ) const;

        void on_button_down (
        )
        { 
            if (button_down_handler.is_set())
                button_down_handler(); 
        }

        void on_button_up (
            bool mouse_over
        )
        { 
            if (button_up_handler.is_set())
                button_up_handler(mouse_over); 
        }

    private:

        arrow_direction dir;
        member_function_pointer<>::kernel_1a button_down_handler;
        member_function_pointer<bool>::kernel_1a button_up_handler;

        // restricted functions
        arrow_button(arrow_button&);        // copy constructor
        arrow_button& operator=(arrow_button&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class scroll_bar 
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class scroll_bar : public drawable 
    {
        /*!
            INITIAL VALUE
                - ori == a value given by the constructor
                - width_ == 16
                - pos == 0
                - max_pos == 0
                - js == 10

            CONVENTION
                - ori == orientation()
                - b1 == the button that is near the 0 end of the scroll bar
                - b2 == the button that is near the max_pos() end of the scroll bar

                - max_pos == max_slider_pos()
                - pos == slider_pos()
                - js == jump_size()
        !*/

    public:
        enum bar_orientation 
        {
            HORIZONTAL,
            VERTICAL
        };

        scroll_bar(  
            drawable_window& w,
            bar_orientation orientation_
        );

        virtual ~scroll_bar(
        );

        bar_orientation orientation (
        ) const;

        void set_orientation (
            bar_orientation new_orientation   
        );

        void set_length (
            unsigned long length
        );

        unsigned long max_slider_pos (
        ) const;

        void set_max_slider_pos (
            unsigned long mpos
        );

        void set_slider_pos (
            unsigned long pos
        );

        unsigned long slider_pos (
        ) const;

        template <
            typename T
            >
        void set_scroll_handler (
            T& object,
            void (T::*eh)()
        ) { scroll_handler.set(object,eh); }

        void set_pos (
            long x,
            long y
        );

        void enable (
        )
        {
            if (!hidden)
                show_slider();
            if (max_pos != 0)
            {
                b1.enable();
                b2.enable();
            }
            drawable::enable();
        }

        void disable (
        )
        {
            hide_slider();
            b1.disable();
            b2.disable();
            drawable::disable();
        }
            
        void hide (
        )
        {
            hide_slider();
            top_filler.hide();
            bottom_filler.hide();
            b1.hide();
            b2.hide();
            drawable::hide();
        }
            
        void show (
        )
        {
            b1.show();
            b2.show();
            drawable::show();
            top_filler.show();
            if (enabled)
                show_slider();
        }

        void set_z_order (
            long order
        )
        {
            slider.set_z_order(order);
            top_filler.set_z_order(order);
            bottom_filler.set_z_order(order);
            b1.set_z_order(order);
            b2.set_z_order(order);
            drawable::set_z_order(order);
        }

        void set_jump_size (
            unsigned long js
        );

        unsigned long jump_size (
        ) const;


    private:

        void hide_slider (
        );
        /*!
            ensures
                - hides the slider and makes any other changes needed so that the
                  scroll_bar still looks right.
        !*/

        void show_slider (
        );
        /*!
            ensures
                - shows the slider and makes any other changes needed so that the
                  scroll_bar still looks right.
        !*/


        void on_slider_drag (
        ); 
        /*!
            requires
                - is called whenever the user drags the slider
        !*/

        void draw (
            const canvas& c
        ) const;

        void b1_down (
        );

        void b1_up (
            bool mouse_over
        );

        void b2_down (
        );

        void b2_up (
            bool mouse_over
        );

        void top_filler_down (
        );

        void top_filler_up (
            bool mouse_over
        );

        void bottom_filler_down (
        );

        void bottom_filler_up (
            bool mouse_over
        );

        void on_user_event (
            int i
        )
        {
            switch (i)
            {
                case 0:
                    b1_down();
                    break;
                case 1:
                    b2_down();
                    break;
                case 2:
                    top_filler_down();
                    break;
                case 3:
                    bottom_filler_down();
                    break;
                case 4:
                    // if the position we are supposed to switch the slider too isn't 
                    // already set
                    if (delayed_pos != pos)
                    {
                        set_slider_pos(delayed_pos);
                        if (scroll_handler.is_set())
                            scroll_handler();
                    }
                    break;
                default:
                    break;
            }
        }

        void delayed_set_slider_pos (
            unsigned long dpos
        ) 
        {
            delayed_pos = dpos;
            parent.trigger_user_event(this,4); 
        }

        void b1_down_t (
        ) { parent.trigger_user_event(this,0); }

        void b2_down_t (
        ) { parent.trigger_user_event(this,1); }

        void top_filler_down_t (
        ) { parent.trigger_user_event(this,2); }

        void bottom_filler_down_t (
        ) { parent.trigger_user_event(this,3); }


        class filler : public button_action
        {
            friend class scroll_bar;
        public:
            filler (
                drawable_window& w,
                scroll_bar& object,
                void (scroll_bar::*down)(),
                void (scroll_bar::*up)(bool)
            ):
                button_action(w)
            {
                bup.set(object,up);
                bdown.set(object,down);

                enable_events();
            }

            ~filler (
            )
            {
               disable_events();
            }

            void set_size (
                unsigned long width,
                unsigned long height
            )
            {
                rectangle old(rect);
                const unsigned long x = rect.left();
                const unsigned long y = rect.top();
                rect.set_right(x+width-1);
                rect.set_bottom(y+height-1);

                parent.invalidate_rectangle(rect+old);
            }

        private:

            void draw (
                const canvas& c
            ) const
            {
                if (is_depressed())
                    draw_checkered(rect,c,0,0,0,43,47,55);
                else
                    draw_checkered(rect,c,255,255,255,212,208,200);
            }

            void on_button_down (
            ) { bdown(); } 

            void on_button_up (
                bool mouse_over
            ) { bup(mouse_over); } 

            member_function_pointer<>::kernel_1a bdown;
            member_function_pointer<bool>::kernel_1a bup;
        };

        class slider_class : public dragable
        {
            friend class scroll_bar;
        public:
            slider_class ( 
                drawable_window& w,
                scroll_bar& object,
                void (scroll_bar::*handler)()
            ) :
                dragable(w)
            {
                mfp.set(object,handler);
                enable_events();
            }

            ~slider_class (
            )
            {
               disable_events();
            }

            void set_size (
                unsigned long width,
                unsigned long height
            )
            {
                rectangle old(rect);
                const unsigned long x = rect.left();
                const unsigned long y = rect.top();
                rect.set_right(x+width-1);
                rect.set_bottom(y+height-1);

                parent.invalidate_rectangle(rect+old);
            }

        private:
            void on_drag (
            )
            {
                mfp();
            }

            void draw (
                const canvas& c
            ) const
            {
                fill_rect(rect,c,212,208,200);
                draw_button_up(rect,c);
            }

            member_function_pointer<>::kernel_1a mfp;
        };


        void adjust_fillers (
        );
        /*!
            ensures
                - top_filler and bottom_filler appear in their correct positions
                  relative to the current positions of the slider and the b1 and
                  b2 buttons
        !*/

        unsigned long get_slider_size (
        ) const;
        /*!
            ensures
                - returns the length in pixels the slider should have based on the current
                  state of this scroll bar
        !*/


        const unsigned long width_;
        arrow_button b1, b2;
        slider_class slider;
        bar_orientation ori; 
        filler top_filler, bottom_filler;
        member_function_pointer<>::kernel_1a scroll_handler;

        unsigned long pos;
        unsigned long max_pos; 
        unsigned long js;

        timer<scroll_bar>::kernel_2a b1_timer;
        timer<scroll_bar>::kernel_2a b2_timer;
        timer<scroll_bar>::kernel_2a top_filler_timer;
        timer<scroll_bar>::kernel_2a bottom_filler_timer;
        unsigned long delayed_pos;

        // restricted functions
        scroll_bar(scroll_bar&);        // copy constructor
        scroll_bar& operator=(scroll_bar&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class widget_group 
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class widget_group : public drawable
    {
        /*!
            INITIAL VALUE
                widgets.size() == 0

            CONVENTION
                Widgets contains all the drawable objects and their relative positions
                that are in *this.
        !*/

        struct relpos
        {
            unsigned long x;
            unsigned long y;
        };

    public:
        widget_group(  
            drawable_window& w,
            unsigned long events = 0
        ) : drawable(w,events) { rect = rectangle(0,0,-1,-1); enable_events();}

        virtual ~widget_group(
        ){ disable_events(); }

        void empty (
        );

        void add (
            drawable& widget,
            unsigned long x,
            unsigned long y
        );

        bool is_member (
            const drawable& widget
        ) const;

        void remove (
            const drawable& widget
        );

        unsigned long size (
        ) const; 

        void set_pos (
            long x,
            long y
        );

        void set_z_order (
            long order
        );

        void show (
        );

        void hide (
        );

        void enable (
        );

        void disable (
        );

        void fit_to_contents (
        );

    protected:

        // this object doesn't draw anything but also isn't abstract
        void draw (
            const canvas& c
        ) const {}

    private:

        map<drawable*,relpos>::kernel_1a_c widgets;

        // restricted functions
        widget_group(widget_group&);        // copy constructor
        widget_group& operator=(widget_group&);    // assignment operator
    };


// ----------------------------------------------------------------------------------------

    class image_widget : public dragable
    {
        /*!
            INITIAL VALUE
                img.size() == 0

            CONVENTION
                - img == the image this object displays
        !*/

    public:

        image_widget(  
            drawable_window& w
        ): dragable(w){ enable_events(); }

        ~image_widget(
        )
        {
            disable_events();
            parent.invalidate_rectangle(rect); 
        }

        template <
            typename image_type
            >
        void set_image (
            const image_type& new_img
        )
        {
            auto_mutex M(m);
            assign_image(img,new_img);
            rect.set_right(rect.left()+img.width()-1); 
            rect.set_bottom(rect.top()+img.height()-1);
            parent.invalidate_rectangle(rect);
        }

    private:

        void draw (
            const canvas& c
        ) const
        {
            rectangle area = rect.intersect(c);
            if (area.is_empty())
                return;

            draw_image(img,rect.left(),rect.top(),c);
        }

        array2d<rgb_pixel>::kernel_1a img;

        // restricted functions
        image_widget(image_widget&);        // copy constructor
        image_widget& operator=(image_widget&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------


}

#ifdef NO_MAKEFILE
#include "base_widgets.cpp"
#endif

#endif // DLIB_BASE_WIDGETs_

