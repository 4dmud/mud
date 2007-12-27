// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_GUI_CORE_KERNEl_2_
#define DLIB_GUI_CORE_KERNEl_2_


#include <string>

#include "gui_core_kernel_abstract.h"
#include "../algs.h"
#include "../threads.h"
#include "rectangle.h"
#include "../binary_search_tree.h"
#include <string.h>
#include "../pixel.h"


namespace dlib
{

// ----------------------------------------------------------------------------------------

    namespace gui_core_kernel_2_globals
    {
        void event_handler ();
        void trigger_user_event_threadproc (void*);

        // This is a forward declaration for a struct that contains any 
        // X11 variables.  This allows me to avoid having any dlib header files
        // include the X11 headers.  Which in turn speeds build times and simplifies
        // build setups.
        struct x11_base_windowstuff;
    }

// ----------------------------------------------------------------------------------------

    void put_on_clipboard (
        const std::string& str
    );

// ----------------------------------------------------------------------------------------

    void get_from_clipboard (
        std::string& str
    );

// ----------------------------------------------------------------------------------------

    class canvas : public rectangle
    {
    public:
        struct pixel
        {
            unsigned char blue;
            unsigned char green;
            unsigned char red;
        private:
            unsigned char _padding;
        };



        ~canvas() { delete [] bits; }

        inline pixel* operator[] (
            unsigned long row
        ) const
        {
            ASSERT(row < height(),
                "\tpixel* canvas::operator[]"
                << "\n\tyou have to give a row that is less than the height()"
                << "\n\tthis:     " << this
                << "\n\trow:      " << row 
                << "\n\theight(): " << height() 
                );
            unsigned char* temp = bits + row_width*row;
            return reinterpret_cast<pixel*>(temp);
        }

        void fill (
            unsigned char red_,
            unsigned char green_,
            unsigned char blue_
        ) const;

    private:

        friend void gui_core_kernel_2_globals::event_handler ();

        canvas (
            unsigned char* bits__,
            unsigned long left__,
            unsigned long top__,            
            unsigned long right__,            
            unsigned long bottom__   
        ) : 
            rectangle(left__,top__,right__,bottom__),
            bits(bits__),
            width_(width()),
            height_(height()),
            row_width(width_*4)
        {}

        // restricted functions
        canvas();        // normal constructor
        canvas(canvas&);        // copy constructor
        canvas& operator=(canvas&);    // assignment operator    

        unsigned char* const bits;
        const unsigned long width_;
        const unsigned long height_;
        const unsigned long row_width;
    };

    template <>
    struct pixel_traits<canvas::pixel>
    {
        const static bool rgb = true;
        const static bool grayscale = false;
        const static bool hsi = false;
    };

// -----------------

    void end_program (
    );

// -----------------

    class base_window
    {
        friend void gui_core_kernel_2_globals::event_handler ();
        friend void gui_core_kernel_2_globals::trigger_user_event_threadproc (void*);

    public:

        enum  mouse_state_masks
        {
            NONE = 0,
            LEFT = 1,
            RIGHT = 2,
            MIDDLE = 4,
            SHIFT = 8,
            CONTROL = 16
        };

        enum on_close_return_code
        {
            DO_NOT_CLOSE_WINDOW,
            CLOSE_WINDOW
        };

        enum non_printable_keyboard_keys
        {
            KEY_BACKSPACE,
            KEY_SHIFT,
            KEY_CTRL,
            KEY_ALT,
            KEY_PAUSE,
            KEY_CAPS_LOCK,
            KEY_ESC,
            KEY_PAGE_UP,
            KEY_PAGE_DOWN,
            KEY_END,
            KEY_HOME,
            KEY_LEFT,           // This is the left arrow key
            KEY_RIGHT,          // This is the right arrow key
            KEY_UP,             // This is the up arrow key
            KEY_DOWN,           // This is the down arrow key
            KEY_INSERT,
            KEY_DELETE,
            KEY_SCROLL_LOCK,
  
            // Function Keys
            KEY_F1,
            KEY_F2,
            KEY_F3,
            KEY_F4,
            KEY_F5,
            KEY_F6,
            KEY_F7,
            KEY_F8,
            KEY_F9,
            KEY_F10,
            KEY_F11,
            KEY_F12
        };

    private:

        gui_core_kernel_2_globals::x11_base_windowstuff& x11_stuff;

        int x, y, width, height;
        bool is_mapped;

        const bool resizable;
        bool has_been_destroyed;
        bool has_been_resized;  // true if someone called set_size() and the on_window_resized() event 
                                // hasn't yet occurred.
        bool has_been_moved;    // true if someone called set_pos() and the on_window_moved() event
                                // hasn't yet occurred.


        // The following 3 variables (and x11_stuff.last_click_time) are only accessed from the 
        // event handling loop (except for being initialized below). They record the last 
        // mouse click event details.
        long last_click_x, last_click_y;
        unsigned long last_click_button;


    protected:
        const rmutex& wm; 

    public:

        base_window (
            bool resizable_ = true,
            bool undecorated = false
        );

        virtual ~base_window (
        );

        void close_window (
        );

        bool is_closed (
        ) const;

        void set_title (
            const std::string& title_
        );

        virtual void show (
        );    

        void hide(
        );    

        void set_size (
            int width_,
            int height_
        );

        void set_pos (
            long x_,
            long y_
        );

        void get_pos (
            long& x_,
            long& y_
        );

        void get_size (
            unsigned long& width_,
            unsigned long& height_
        ) const;

        void invalidate_rectangle (
            const rectangle& rect
        );

        void trigger_user_event (
            void* p,
            int i
        );

    protected:

        virtual on_close_return_code on_window_close(
        ){return CLOSE_WINDOW;}

        virtual void on_window_resized(
        ){}

        virtual void on_window_moved(
        ){}
        virtual void on_user_event (
            void* p,
            int i
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

    private:

        virtual void paint (
            const canvas& c
        ) =0;



        base_window(base_window&);        // copy constructor
        base_window& operator=(base_window&);    // assignment operator

    };

// ----------------------------------------------------------------------------------------


}


#ifdef NO_MAKEFILE
#include "gui_core_kernel_2.cpp"
#endif

#endif // DLIB_GUI_CORE_KERNEl_2_

