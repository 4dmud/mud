// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_GUI_CORE_KERNEl_ABSTRACT_
#ifdef DLIB_GUI_CORE_KERNEl_ABSTRACT_

#include <string>
#include "../algs.h"

namespace dlib
{

    /*!
        OVERVIEW:
            This is a set of objects and functions which provide a very basic
            framework for manipulating windows.  It is intended to provide a 
            portable interface which can be used to build a more complex windowing 
            toolkit.

        PROGRAM ENTRY POINT:
            When using the gui_core component the entry point to the program is
            void winmain(int argc, char** argv), not the familiar 
            int main(int argc, char** argv).  

            For winmain, argc and argv contain the same values as the normal 
            C++ main() arguments.  

            Note that letting winmain() return does NOT cause the program to 
            terminate.  winmain() ending has no effect on any global resources
            either (i.e. they don't get destructed or anything).  To end the 
            program you should call end_program().

        EXCEPTIONS
            Do not let an exception leave any of the base_window event handlers or 
            winmain. The results of doing so are undefined.

        THREAD SAFETY
            Event Handlers
                All event handlers are executed in a special event handling thread. 
                This means that you must not do anything that will take a long time or
                block while in an event handler.  Doing so will freeze all event 
                processing.  
                
                Also, don't rely on get_thread_id() always returning the same ID from
                inside event handlers.

            end_program()
                It is always safe to call this function from any thread.

            rectangle
                If you are going to access a rectangle object from more than one
                thread then you must synchronize access to it.

            canvas
                Never access a canvas object outside of the paint() callback
                that supplied it.  Only access a canvas object from the event 
                handling thread.  After the paint() event handler has returned do 
                not access that canvas object again.

            base_window
                All methods for this class are thread safe.  You may call them 
                from any thread and do not need to serialize access.

        GENERAL WARNING
            You can't make base_window objects before winmain() has been entered or
            call end_program(), get_from_clipboard() or put_on_clipboard() before 
            winmain() has been entered.
    !*/

// ----------------------------------------------------------------------------------------

    void end_program (
    );
    /*!
        requires
            - winmain() function has been entered
        ensures
            - Begins to terminates this process.  Note that this does not trigger 
              on_window_close() events (or any other events for that matter). 
            - The program will wait for all threads to terminate before ending (this 
              includes winmain()) but once this function is called C++ will start to 
              destruct global and static objects so any threads still running need to be 
              able to survive that.
        throws
            - dlib::gui_error
                This exception is thrown if there is an error while processing this request.
    !*/

// ----------------------------------------------------------------------------------------

    void put_on_clipboard (
        const std::string& str
    );
    /*!
        ensures
            - posts the contents of str to the system clipboard
        throws
            - std::bad_alloc
    !*/

// ----------------------------------------------------------------------------------------

    void get_from_clipboard (
        std::string& str
    );
    /*!
        ensures
            - if (there is string data on the system clipboard) then
                - #str == the data from the clipboard
            - else
                - #str == ""
        throws
            - std::bad_alloc
    !*/

// ----------------------------------------------------------------------------------------

    class rectangle
    {
        /*!
            INITIAL VALUE
                The initial value of this object is defined by its constructor.                

            WHAT THIS OBJECT REPRESENTS
                This object represents a rectangular region inside a window. 
                The region is the rectangle with its upper left corner at 
                position (left(),top()) and its lower right corner 
                at (right(),bottom()).

                Note that the origin of the coordinate plane, i.e. (0,0), is located
                at the upper left corner of the window.  Additionally points such as
                (1,1) or (3,5) represent locations on the screen that are below and 
                to the right of the origin.

                Also note that rectangles where top() > bottom() or left() > right() 
                represent empty rectangles.
        !*/

    public:

        rectangle (
            const rectangle& rect
        );
        /*!
            ensures
                - #*this represents the same rectangle as rect
        !*/

        rectangle (
        );
        /*!
            ensures
                - #left() == 1
                - #top() == 1
                - #right() == 0
                - #bottom() == 0
        !*/

        rectangle (
            long left_,
            long top_,
            long right_,
            long bottom_
        );
        /*!
            ensures
                - #left() == left_
                - #top() == top_
                - #right() == right_
                - #bottom() == bottom_
        !*/

        long left (
        ) const;
        /*!
            ensures
                - returns the x coordinate for the left side of this rectangle
        !*/

        void set_left (
            long left_
        );
        /*!
            ensures
                - #left() == left_
        !*/

        long top (
        ) const;
        /*!
            ensures
                - returns the y coordinate for the top of this rectangle
        !*/

        void set_top (
            long top_
        );
        /*!
            ensures
                - #top() == top_
        !*/

        long right (
        ) const;
        /*!
            ensures
                - returns the x coordinate for the right side of this rectangle
        !*/

        void set_right (
            long right_
        );
        /*!
            ensures
                - #right() == right_
        !*/

        long bottom (
        ) const;
        /*!
            ensures
                - returns the y coordinate for the bottom of this rectangle
        !*/
       
        void set_bottom (
            long bottom_
        );
        /*!
            ensures
                - #bottom() == bottom_
        !*/

        bool is_empty (
        ) const;
        /*!
            ensures
                - if (top() > bottom() || left() > right()) then
                    - returns true
                - else
                    - returns false
        !*/

        unsigned long width (
        ) const;
        /*!
            ensures
                - if (is_empty()) then
                    - returns 0
                - else
                    - returns the width of this rectangle.
                      (i.e. right() - left() + 1)
        !*/

        unsigned long height (
        ) const;
        /*!
            ensures
                - if (is_empty()) then
                    - returns 0
                - else
                    - returns the height of this rectangle.
                      (i.e. bottom() - top() + 1)
        !*/

        rectangle operator + (
            const rectangle& rhs
        ) const;
        /*!
            ensures
                - if (rhs.is_empty() == false && this->is_empty() == false) then
                    - returns the smallest rectangle that contains both *this and 
                      rhs.
                - if (rhs.is_empty() == true && this->is_empty() == false) then
                    - returns *this
                - if (rhs.is_empty() == false && this->is_empty() == true) then
                    - returns rhs
                - if (rhs.is_empty() == true && this->is_empty() == true) then
                    - returns a rectangle that has is_empty() == true
        !*/

        rectangle intersect (
            const rectangle& rhs
        ) const;
        /*!
            ensures
                - if (there is a region of intersection between *this and rhs) then
                    - returns a rectangle that represents the intersection of *this 
                      and rhs.
                - else
                    - returns a rectangle where is_empty() == true
        !*/

        bool contains (
            long x,
            long y
        ) const;
        /*!
            ensures
                - if (the point (x,y) is contained in this rectangle) then
                    - returns true
                - else
                    - returns false
        !*/

        rectangle& operator= (
            const rectangle& rect
        );
        /*!
            ensures
                - #*this represents the same rectangle as rect
                - returns #*this
        !*/

        bool operator== (
            const rectangle& rect
        ) const;
        /*!
            ensures
                - if (top() == rect.top() && left() == rect.left() &&
                      right() == rect.right() && bottom() == rect.bottom()) then
                    - returns true
                - else
                    - returns false
        !*/

        bool operator!= (
            const rectangle& rect
        ) const;
        /*!
            ensures
                - returns !(*this == rect)
        !*/
    };

// ----------------------------------------------------------------------------------------

    class canvas : public rectangle
    {
        /*!
            POINTERS AND REFERENCES TO INTERNAL DATA
                All functions of this object may invalidate pointers and references
                to internal data.  

            INITIAL VALUE
                The initial value of each pixel is undefined.  
                is_empty() == false

            WHAT THIS OBJECT REPRESENTS
                This object represents a rectangular area of a window that you 
                can draw on. 

                Each pixel can be accessed with the following syntax:
                    canvas_instance[y][x].red   == the red value for this pixel
                    canvas_instance[y][x].blue  == the blue value for this pixel
                    canvas_instance[y][x].green == the green value for this pixel

                The origin, i.e. (0,0), of the x,y coordinate plane of the canvas is in 
                the upper left corner of the canvas.  Note that the upper left corner 
                of the canvas appears at the point (left(),top()) in its window.
        !*/

    public:

        struct pixel
        {
            /*!
                WHAT THIS OBJECT REPRESENTS
                    This object represents a single pixel.  Each pixel's value
                    ranges from 0 to 255 with 0 indicating that the color is not
                    present in the pixel at all and 255 indicating that the color
                    is present in the pixel with maximum intensity.

                    Note that the structure, order, and size of of this struct are 
                    implementation dependent.  It will always contain fields called 
                    red, green, and blue but they may not be in that order and there 
                    may be padding.  

                    Also note that pixel_traits<> is defined for this pixel type,
                    thus you can use it in assign_pixel() calls.
            !*/
            unsigned char red;
            unsigned char green;
            unsigned char blue;
        };


        pixel* operator[] (
            unsigned long row
        ) const;
        /*!
            requires
                - row < height()
            ensures
                - returns an array of width() pixel structs that represents the given
                  row of pixels in the canvas.  
        !*/

        void fill (
            unsigned char red,
            unsigned char green,
            unsigned char blue
        ) const;
        /*!
            ensures
                - for all valid values of x and y:
                    - (#*this)[y][x].red = red
                    - (#*this)[y][x].green = green
                    - (#*this)[y][x].blue = blue
        !*/
            
    private:

        // restricted functions
        canvas();        // normal constructor
        canvas(canvas&);        // copy constructor
        canvas& operator=(canvas&);    // assignment operator    
    };

// ----------------------------------------------------------------------------------------

    class base_window
    {

        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a window on the desktop.  A window has a "client 
                area" that is a region of the screen that you can draw whatever you like 
                on.  You implemnet the paint() callback and use the canvas object to do 
                this drawing.

            INITIAL STATE
                - The initial state of the window is to be hidden.  This means you need
                  to call show() to make it appear.
                - is_closed() == false

            paint() callback:
                This is where you will do all your drawing.  It is triggered when
                part of the window needs to be drawn/redrawn.

            mouse events:
                It is important to note a few things about the mouse events.  First,
                the on_mouse_move() event is not triggered for each pixel the mouse crosses
                but rather its frequency and precision is implementation dependent.  
                
                Second, it is possible that a mouse button may be depressed but the 
                corresponding button release event does not go to the window.  For instance, 
                if the mouse is outside the window and some other application jumps to the 
                top it is possible that the new application will receive any mouse button 
                release events rather than the original window.  But the point is that 
                you should not rely on always getting a button up event for every button
                down event.

            keydown event:
                Note that the existence of a typematic action (holding down a key
                and having it start to repeat itself after a moment) for each key is
                totally implementation dependent.  So don't rely on it for any key
                and conversely don't assume it isn't present either.  

            The base_window::wm mutex
                This is a reference to a global rmutex.  All instances of base_window make
                reference to the same global rmutex.  It is used to synchronize access to 
                the base_window to make it thread safe.  It is also always locked before 
                an event handler is called.
        !*/

    public:

        enum on_close_return_code
        {
            DO_NOT_CLOSE_WINDOW,
            CLOSE_WINDOW
        };

        enum mouse_state_masks
        {
            /*!
                These constants represent the various buttons referenced by
                mouse events.
            !*/
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

        base_window (
            bool resizable = true,
            bool undecorated = false
        );
        /*!
            requires
                - winmain() function has been entered
                - if (undecorated == true) then
                    - resizable == false
            ensures
                - #*this has been properly initialized 
                - if (resizable == true) then 
                    - this window will be resizable by the user
                - else 
                    - this window will not be resizable by the user
                - if (undecorated == true) then
                    - this window will not have any graphical elements outside
                      of its drawable area or appear in the system task bar. It
                      also won't take the input focus from other windows.
                      (it is suitable for making things such as popup menus)
            throws
                - std::bad_alloc
                - dlib::thread_error
                - dlib::gui_error
                    This exception is thrown if there is an error while 
                    creating this window.
        !*/

        virtual ~base_window (
        );
        /*!
            requires
                - is_closed() == true
            ensures
                - does NOT trigger the on_window_close() event
                - all resources associated with *this have been released                
        !*/

        void close_window (
        );
        /*!
            ensures
                - #is_closed() == true
                  (i.e. permanently closes this window.  The window is removed from the 
                  screen and no more events will be dispatched to this window. )
                - does NOT trigger the on_window_close() event
        !*/

        bool is_closed (
        ) const;
        /*!
            ensures
                - returns true if this window has been closed, false otherwise.
                  (Note that closed windows do not receive any callbacks at all.
                   They are also not visible on the screen.)
        !*/

        void set_title (
            const std::string& title
        );
        /*!
            requires
                - is_closed() == false
            ensures
                - sets the title of the window
        !*/

        virtual void show (
        );
        /*!
            requires
                - is_closed() == false
            ensures
                - this window will appear on the screen
        !*/

        void hide(
        );
        /*!
            requires
                - is_closed() == false
            ensures
                - the window does not appear on the screen
        !*/

        void set_size (
            int width,
            int height
        );
        /*!
            requires
                - is_closed() == false
            ensures
                - The width of the client area of this window is at least width
                  pixels.
                - The height of the client area of this window is at least height
                  pixels.
                - if (the window wasn't already this size) then
                    - triggers the on_window_resized() callback
        !*/

        void set_pos (
            long x,
            long y
        );
        /*!
            requires
                - is_closed() == false
            ensures 
                - sets the upper left corner of this window to the position (x,y) 
                  on the desktop.  Note that the origin (0,0) is at the upper left
                  corner of the desktop.
        !*/

        void get_pos (
            long& x,
            long& y
        ) const;
        /*!
            requires
                - is_closed() == false
            ensures
                - #x == the x coordinate of the upper left corner of the client area of
                  this window.
                - #y == the y coordinate of the upper left corner of the client area of
                  this window.
                - i.e. the point (#x,#y) on the desktop is coincident with the point
                  (0,0) in the client area of this window.
        !*/

        void get_size (
            unsigned long& width,
            unsigned long& height
        ) const;
        /*!
            requires
                - is_closed() == false
            ensures
                - #width == the width of the client area of this window in pixels
                - #height == the height of the client area of this window in pixels
        !*/

        void invalidate_rectangle (
            const rectangle& rect
        );
        /*!
            ensures
                - if (is_closed() == false) then
                    - causes the area of this window defiend by rect to become invalid.
                      This means that a paint() message will be dispatched to repaint
                      this area of the window.  Note that it is possible that the 
                      resulting paint() message may include a bigger rectangle than
                      the one defined by rect.
        !*/

        void trigger_user_event (
            void* p,
            int i
        );
        /*!
            ensures
                - will never block (even if some other thread has a lock on the
                  global mutex referenced by wm.)
                - if (is_closed() == false) then
                    - causes the on_user_event() event to be called with 
                      the given arguments.
        !*/

    protected:
        const rmutex& wm;

        // let the window close by default
        virtual on_close_return_code on_window_close(
        ){return CLOSE_WINDOW;}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when the user attempts to close this window
                - if (this function returns CLOSE_WINDOW) then
                    - #is_closed() == true  (i.e. this window will be closed)
                    - it is safe to call "delete this;" inside on_window_close() 
                      if *this was allocated on the heap and no one will try to 
                      access *this anymore.
                - else
                    - this window will not be closed and the attempt to close it
                      by the user will have no effect. 
                    - #is_closed() == false
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_user_event (
            void* p,
            int i
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called whenever someone calls trigger_user_event()
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_window_resized(
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when this window is resized
            ensures
                - does not change the state of mutex wm
        !*/
             
        // do nothing by default
        virtual void on_window_moved(
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when this window's position changes 
            ensures
                - does not change the state of mutex wm
        !*/
             
        // do nothing by default  
        virtual void on_mouse_down (
            unsigned long btn,
            unsigned long state,
            long x,
            long y,
            bool is_double_click
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when the user depresses one of the mouse buttons
                - btn == the button that was depressed. (either LEFT, MIDDLE, or RIGHT)
                - state == the bitwise OR of the buttons that are currently depressed 
                  excluding the button given by btn. (from the mouse_state_masks enum) 
                - (x,y) == the position of the mouse (relative to the upper left corner
                  of the window) when this event occurred.  Note that the mouse may be
                  outside the window.
                - if (this is the second button press of a double click) then
                    - is_double_click == true
                - else
                    - is_double_click == false
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_mouse_up (
            unsigned long btn,
            unsigned long state,
            long x,
            long y
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when the user releases one of the mouse buttons
                - btn == the button that was released. (either LEFT, MIDDLE, or RIGHT)
                - state == the bitwise OR of the buttons that are currently depressed
                  (from the mouse_state_masks enum)
                - (x,y) == the position of the mouse (relative to the upper left corner
                  of the window) when this event occurred.  Note that the mouse may be
                  outside the window.
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_mouse_move (
            unsigned long state,
            long x,
            long y
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when the user moves the mouse
                - state == the bitwise OR of the buttons that are currently depressed
                  (from the mouse_state_masks enum)
                - (x,y) == the position of the mouse (relative to the upper left corner
                  of the window) when this event occurred. 
                - if (the user is holding down one or more of the mouse buttons) then
                    - the mouse move events will continue to track the mouse even if
                      it goes out of the window.  This will continue until the user
                      releases all the mouse buttons.
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_mouse_leave (
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when the mouse leaves this window
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_mouse_enter (
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when the mouse enters this window
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_focus_gained (
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when this window gains input focus 
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_focus_lost (
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when this window loses input focus 
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_wheel_up (
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called every time the mouse wheel is scrolled up one notch
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_wheel_down (
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called every time the mouse wheel is scrolled down one notch
            ensures
                - does not change the state of mutex wm
        !*/

        // do nothing by default
        virtual void on_keydown (
            unsigned long key,
            bool is_printable,
            bool shift,
            bool ctrl
        ){}
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when a keyboard key is pressed or if a key is held
                  down then this is called repeatedly at a certain rate once the
                  typematic action begins (note that some keys might not have any 
                  typematic action on some platforms).
                - if (is_printable) then
                    - key == the character that was pressed. (e.g. 'a', 'b', '1' etc...)
                    - this is a printable character.  Note that ' ', '\t', and 
                      '\n' (this is the return/enter key) are all considered printable.
                - else
                    - key == one of the non_printable_keyboard_keys enums.  
                - if (the ctrl key was down when this key was pressed) then
                    - ctrl == true
                - else
                    - ctrl == false
                - if (key is not in the range 'a' to 'z' or 'A' to 'Z') then
                    - if (the shift key was down when this key was pressed) then                    
                        - shift == true
                    - else
                        - shift == false
            ensures
                - does not change the state of mutex wm
        !*/

    private:

        virtual void paint (
            const canvas& c
        ) =0;
        /*!
            requires
                - is_closed() == false
                - mutex wm is locked
                - is called when part of the window needs to be repainted for 
                  any reason.
                - c == a canvas object that represents the invalid area of this
                  window which needs to be painted.
            ensures
                - does not change the state of mutex wm
        !*/

        base_window(base_window&);        // copy constructor
        base_window& operator=(base_window&);    // assignment operator

    };

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_GUI_CORE_KERNEl_ABSTRACT_

