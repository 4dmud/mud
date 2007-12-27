// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#ifndef DLIB_DRAWABLe_
#define DLIB_DRAWABLe_

#include "drawable_abstract.h"
#include "../gui_core.h"
#include "../set.h"
#include "../binary_search_tree.h"
#include "../algs.h"
#include "../pixel.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class drawable_window  
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class drawable;
    class drawable_window : public base_window
    {
        /*!
            INITIAL VALUE
                - lastx == -1
                - lasty == -1
                - event_id == 1

            CONVENTION
                - red == the red part of the background color of this window
                - green == the green part of the background color of this window
                - blue == the blue part of the background color of this window

                - widgets == this binary search tree contains every drawable that is in
                  this window.  It is a mapping of each drawable's z-order to a pointer
                  to said drawable.
                - widget_set == a set that contains all the widgets in this window and
                  want to receive events.

                - mouse_move == this is a set of drawables that are in this window and 
                  want to receive the mouse movement events.
                - mouse_wheel == this is a set of drawables that are in this window and 
                  want to receive the mouse wheel events.
                - mouse_click == this is a set of drawables that are in this window and 
                  want to receive the mouse click events.
                - window_resized == this is a set of drawables that are in this window and 
                  want to receive the window_resized event.
                - keyboard == this is a set of drawables that are in this window and 
                  want to receive keyboard events.
                - focus == this is a set of drawables that are in this window and 
                  want to receive focus events.
                - window_moved == this is a set of drawables that are in this window and 
                  want to receive window move events.

                - lastx == the x coordinate that we last saw the mouse at or -1 if the 
                  mouse is outside this window.
                - lasty == the y coordinate that we last saw the mouse at or -1 if the 
                  mouse is outside this window.

                - event_id == a number we use to tag events so we don't end up sending
                  an event to a drawable more than once.  This could happen if one of the
                  event handlers does something to reset the enumerator while we are
                  dispatching events (e.g. creating a new widget).
        !*/
    public:

        drawable_window(
            bool resizable = true,
            bool undecorated = false
        ) : 
            base_window(resizable,undecorated),
            red(212),
            green(208),
            blue(200),
            lastx(-1),
            lasty(-1),
            event_id(1)
        {}

        void set_background_color (
            unsigned long red,
            unsigned long green,
            unsigned long blue
        );

        virtual inline ~drawable_window()=0;

    private:

        void paint (
            const canvas& c
        );

    protected:

        void on_window_resized(
        );

        void on_window_moved(
        );
               
        void on_mouse_down (
            unsigned long btn,
            unsigned long state,
            long x,
            long y,
            bool is_double_click
        );

        void on_mouse_up (
            unsigned long btn,
            unsigned long state,
            long x,
            long y
        );

        void on_mouse_move (
            unsigned long state,
            long x,
            long y
        );

        void on_mouse_leave (
        );

        void on_mouse_enter (
        );

        void on_wheel_up (
        );

        void on_wheel_down (
        );
        
        void on_focus_gained (
        );

        void on_focus_lost (
        );

        void on_keydown (
            unsigned long key,
            bool is_printable,
            bool shift,
            bool ctrl
        );

        void on_user_event (
            void* p,
            int i
        );

    private:
        
        friend class drawable;


        unsigned long red;
        unsigned long green;
        unsigned long blue;

        typedef set<drawable*>::kernel_1a_c set_of_drawables;

        binary_search_tree<long,set_of_drawables>::kernel_1a_c widgets;

        set_of_drawables widget_set;
        set_of_drawables mouse_move;
        set_of_drawables mouse_wheel;
        set_of_drawables mouse_click;
        set_of_drawables window_resized;
        set_of_drawables keyboard;
        set_of_drawables focus;
        set_of_drawables window_moved;

        long lastx, lasty;
        unsigned long event_id;


        // restricted functions
        drawable_window(drawable_window&);        // copy constructor
        drawable_window& operator=(drawable_window&);    // assignment operator


    };

    drawable_window::~drawable_window(){}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class drawable  
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    enum 
    {
        MOUSE_MOVE = 1,
        MOUSE_CLICK = 2,
        MOUSE_WHEEL = 4,
        WINDOW_RESIZED = 8,
        KEYBOARD_EVENTS = 16,
        FOCUS_EVENTS = 32,
        WINDOW_MOVED = 64
    };

    class drawable 
    {

        /*!
            INITIAL VALUE 
                - enabled_events == false
                - event_id == 0

            CONVENTION
                - events == a bitset specifying what events this drawable is to receive.

                - z_order_value == z_order()

                - if (this drawable has been added to the partent window's sets and
                  binary search tree) then
                    - enabled_events == true
                - else
                    - enabled_events == false

                - event_id == the id of the last event we got from our parent window
        !*/

    public:

        friend class drawable_window;

        drawable (
            drawable_window& w,
            unsigned long events_ = 0
        ) :
            m(w.wm),
            parent(w),
            hidden(false),
            enabled(true),
            lastx(w.lastx),
            lasty(w.lasty),
            z_order_value(0),
            events(events_),
            enabled_events(false),
            event_id(0)
        {}

        virtual ~drawable (
        );

        long z_order (
        ) const
        {
            m.lock();
            long temp = z_order_value;
            m.unlock();
            return temp;
        }

        virtual void set_z_order (
            long order
        );

        const rectangle get_rect (
        ) const 
        {
            auto_mutex M(m);
            return rect;
        }

        long bottom (
        ) const 
        { 
            auto_mutex M(m); 
            return rect.bottom(); 
        }

        long top (
        ) const 
        { 
            auto_mutex M(m); 
            return rect.top(); 
        }

        long left (
        ) const 
        { 
            auto_mutex M(m); 
            return rect.left(); 
        }

        long right (
        ) const 
        { 
            auto_mutex M(m); 
            return rect.right(); 
        }

        long width (
        ) const 
        { 
            auto_mutex M(m); 
            return rect.width(); 
        }

        long height (
        ) const 
        { 
            auto_mutex M(m); 
            return rect.height(); 
        }

        bool is_enabled (
        ) const
        {
            auto_mutex M(m);
            return enabled;
        }

        virtual void enable (
        ) 
        {
            auto_mutex M(m);
            enabled = true;
            parent.invalidate_rectangle(rect);
        }

        virtual void disable (
        ) 
        {
            auto_mutex M(m);
            enabled = false;
            parent.invalidate_rectangle(rect);
        }

        bool is_hidden (
        ) const
        {
            auto_mutex M(m);
            return hidden;
        }

        virtual void set_pos (
            long x,
            long y
        )
        {
            m.lock();       
            rectangle old(rect);            

            const unsigned long width = rect.width();
            const unsigned long height = rect.height();
            rect.set_top(y);
            rect.set_left(x);
            rect.set_right(static_cast<long>(x+width)-1);
            rect.set_bottom(static_cast<long>(y+height)-1);
            
            parent.invalidate_rectangle(rect+old);
            m.unlock();
        }

        virtual void show (
        )
        {
            m.lock();
            hidden = false;
            parent.invalidate_rectangle(rect);
            m.unlock();
        }

        virtual void hide (
        )
        {
            m.lock();
            hidden = true;
            parent.invalidate_rectangle(rect);
            m.unlock();
        }

        base_window& parent_window (
        ) { return parent; }

        const base_window& parent_window (
        ) const { return parent; }

    protected:   
        rectangle rect;
        const rmutex& m;
        drawable_window& parent;
        bool hidden;
        bool enabled;
        const long& lastx;
        const long& lasty;

        
        void enable_events (
        );

        bool events_are_enabled (
        ) const { auto_mutex M(m); return enabled_events; }

        void disable_events (
        );

        static void draw_sunken_rectangle (
            const rectangle& border,
            const canvas& c
        );
        
        static inline void draw_pixel (
            long x,
            long y,
            const canvas& c,
            unsigned long red,
            unsigned long green,
            unsigned long blue
        )
        {
            if (c.contains(x,y))
            {
                canvas::pixel& p = c[y-c.top()][x-c.left()];
                p.red = red;
                p.green = green;
                p.blue = blue;
            }
        }

        static void draw_checkered (
            const rectangle& area,
            const canvas& c,
            unsigned char red1,
            unsigned char green1,
            unsigned char blue1,
            unsigned char red2,
            unsigned char green2,
            unsigned char blue2
        );

        static void draw_button_down (
            const rectangle& btn,
            const canvas& c
        );

        static void draw_button_up (
            const rectangle& btn,
            const canvas& c
        );

        static void draw_circle (
            long x,
            long y,
            unsigned long radius,
            const canvas& c,
            unsigned char red = 0,
            unsigned char green = 0,
            unsigned char blue = 0
        );

        static void draw_solid_circle (
            long x,
            long y,
            unsigned long radius,
            const canvas& c,
            unsigned char red = 0,
            unsigned char green = 0,
            unsigned char blue = 0
        );

        template <
            typename image_type 
            >
        static void draw_image (
            const image_type& img,
            long x,
            long y,
            const canvas& c
        )
        {
            rectangle rect(x,y,img.width()+x-1,img.height()+y-1);
            rectangle area = c.intersect(rect);
            if (area.is_empty())
                return;

            for (long row = area.top(); row <= area.bottom(); ++row)
            {
                for (long col = area.left(); col <= area.right(); ++col)
                {
                    canvas::pixel& p = c[row-c.top()][col-c.left()];
                    const typename image_type::type& ip = img[row-rect.top()][col-rect.left()];

                    assign_pixel(p,ip);
                }
            }
        }

        static void draw_line (
            long x1,
            long y1,
            long x2,
            long y2,
            const canvas& c,
            unsigned char red = 0,
            unsigned char green = 0,
            unsigned char blue = 0
        );

        static void draw_rectangle (
            rectangle rect,
            const canvas& c,
            unsigned char red = 0,
            unsigned char green = 0,
            unsigned char blue = 0
        );

        static void fill_rect (
            const rectangle& rect,
            const canvas& c,
            unsigned char red,
            unsigned char green,
            unsigned char blue
        );

        static void fill_rect_with_vertical_gradient (
            const rectangle& rect,
            const canvas& c,
            unsigned char red_top,
            unsigned char green_top,
            unsigned char blue_top,
            unsigned char red_bottom,
            unsigned char green_bottom,
            unsigned char blue_bottom
        );

    private:

        long z_order_value;
        const unsigned long events;
        bool enabled_events;
        unsigned long event_id;


        // restricted functions
        drawable(drawable&);        // copy constructor
        drawable& operator=(drawable&);    // assignment operator


    protected:

        virtual void draw (
            const canvas& c
        ) const=0;

        virtual void on_user_event (
            int i
        ){}

        virtual void on_window_resized(
        ){}

        virtual void on_window_moved(
        ){}
               
        virtual void on_mouse_down (
            unsigned long btn,
            unsigned long state,
            long x,
            long y,
            bool is_double_click
        ){}

        virtual void on_mouse_up (
            unsigned long btn,
            unsigned long state,
            long x,
            long y
        ){}

        virtual void on_mouse_move (
            unsigned long state,
            long x,
            long y
        ){}

        virtual void on_mouse_leave (
        ){}

        virtual void on_mouse_enter (
        ){}

        virtual void on_wheel_up (
        ){}

        virtual void on_wheel_down (
        ){}

        virtual void on_focus_gained (
        ){}

        virtual void on_focus_lost (
        ){}

        virtual void on_keydown (
            unsigned long key,
            bool is_printable,
            bool shift,
            bool ctrl
        ){}
    };

// ----------------------------------------------------------------------------------------

}

#ifdef NO_MAKEFILE
#include "drawable.cpp"
#endif

#endif // DLIB_DRAWABLe_

