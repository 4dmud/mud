// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_GUI_CORE_KERNEl_1_
#define DLIB_GUI_CORE_KERNEl_1_


#include <string>
#include "../windows_magic.h"


#include <windows.h>
#include <winuser.h>
#include <windowsx.h>
#include <commctrl.h>

#include "gui_core_kernel_abstract.h"

#ifdef _MSC_VER
// Disable the following warnings for Visual Studio
//
// These two warnings have to do with converting points to and from the LONG
// type.  But both these types are 32 bits in windows so it is fine.
#pragma warning(disable: 4244; disable: 4312)
#endif 

#include "../algs.h"
#include "../sync_extension.h"
#include "../binary_search_tree.h"
#include "../threads.h"
#include "rectangle.h"
#include "../assert.h"
#include "../queue.h"
#include "../pixel.h"


namespace dlib
{

// ----------------------------------------------------------------------------------------

    class base_window;
    namespace gui_core_kernel_1_globals
    {
        HWND make_window (
            DWORD dwStyle
        );
        /*!
            ensures
                - creates a window by calling CreateWindow and passes on the 
                  dwStyle argument.  
                - returns the HWND that is returned by CreateWindow
                - ensures that CreateWindow is called from the event handler thread
                - if (it was unable to create a window) then
                    - returns NULL or helper_window
        !*/

        void give_window_focus (
            HWND hwnd
        );
        /*!
            ensures
                - calls SetActiveWindow(hwnd) from the event handling thread.
        !*/

        void destroy_window (
            HWND hwnd
        );
        /*!
            ensures
                - calls DestroyWindow(hwnd) from the event handling thread.  
        !*/


        LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM);

        typedef sync_extension<binary_search_tree<HWND,base_window*>::kernel_1a>::kernel_1a 
            window_table_type;

        // this variable holds a mapping from window handles to the base_window
        // objects which represent them.  Note that this objects mutex is always locked
        // when inside the event loop.
        extern window_table_type window_table;

        // this is the id of the event processing thread
        extern const thread_id_type event_thread_id;
    }

// ----------------------------------------------------------------------------------------

    class canvas : public rectangle
    {
    public:
        struct pixel
        {
            unsigned char blue;
            unsigned char green;
            unsigned char red;
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

        friend LRESULT CALLBACK gui_core_kernel_1_globals::WndProc (HWND, UINT, WPARAM, LPARAM);

        canvas (
            unsigned char* bits__,
            unsigned long padding__,
            unsigned long left__,
            unsigned long top__,            
            unsigned long right__,            
            unsigned long bottom__           
        ) : 
            rectangle(left__,top__,right__,bottom__),
            bits(bits__),
            width_(width()),
            height_(height()),
            row_width(width_*3+padding__)
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

// ----------------------------------------------------------------------------------------

    void end_program (
    );

// ----------------------------------------------------------------------------------------

    void put_on_clipboard (
        const std::string& str
    );

// ----------------------------------------------------------------------------------------

    void get_from_clipboard (
        std::string& str
    );

// ----------------------------------------------------------------------------------------

    class base_window
    {
        friend LRESULT CALLBACK gui_core_kernel_1_globals::WndProc (HWND, UINT, WPARAM, LPARAM);

        HWND hwnd;
        DWORD style;
        bool has_been_destroyed;

        // This is true if the mouse is in this window.  false otherwise.
        // also note that this variable is only accessed from the event handling thread
        // (except for being initialized below in the constructor, but that is inside
        // the window_table mutex so it doesn't matter).
        bool mouse_in;

    protected:
        const rmutex& wm;

    public:

        base_window (
            bool resizable = true,
            bool undecorated = false
        );

        virtual ~base_window (
        );

        void close_window (
        );

        bool is_closed (
        ) const;

        void set_title (
            const std::string& title
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
            unsigned long& width,
            unsigned long& height
        ) const;

        void invalidate_rectangle (
            const rectangle& rect
        );

        void trigger_user_event (
            void* p,
            int i
        );

        enum on_close_return_code
        {
            DO_NOT_CLOSE_WINDOW,
            CLOSE_WINDOW
        };

        enum  mouse_state_masks
        {
            NONE = 0,
            LEFT = 1,
            RIGHT = 2,
            MIDDLE = 4,
            SHIFT = 8,
            CONTROL = 16
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

    protected:

        virtual on_close_return_code on_window_close(
        ){return CLOSE_WINDOW;}

        virtual void on_user_event (
            void* p,
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
#include "gui_core_kernel_1.cpp"
#endif

#endif // DLIB_GUI_CORE_KERNEl_1_

