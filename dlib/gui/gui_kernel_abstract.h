// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_GUI_KERNEl_ABSTRACT_
#ifdef DLIB_GUI_KERNEl_ABSTRACT_

#include <string>

namespace dlib
{

// ----------------------------------------------------------------------------------------

    /*!
        ****** EXCEPTIONS ******:
            Nothing in here will throw any exceptions.

        ****** EXCEPTION PROPAGATION ******:
            If an exception is thrown inside an event handler and leaves the event 
            handler then it will propagate out to the start_program() function.
            So placing a try block around the start_program() function will catch
            all exceptions thrown in any event handler.



        ****** THREAD SAFETY ******:
            All events happen in the main thread of the program.
            That means that when events like a button being clicked happen
            the click handler function will be run in the programs main thread.
            This also means that performing some long running computation will
            hang the GUI so do it in another thread.

            You MUST also only construct the GUI objects in the main program
            thread.  Additionally, GUI objects can only be constructed once
            the winmain() function has been entered.  This means GUI objects
            can not be created at the global scope.   

            The following functions are the only thread-safe functions:
                message_box()
                question_box()
                select_directory_box()
                open_file_box()
                open_multiple_files_box()
                save_file_box()
                button::click()
                flat_button::click()

                note about the click() functions.  these functions can be used to
                notify the main thread that some event has happened.  for example, when a
                worker thread finishes some work it can notify the main thread
                by calling click().  When click() is called the click handler function 
                for the button executes and click() does not return until the handler 
                funciton finishes.  The handler function will execute in the main thread 
                just like all other events.


            All other functions need to be mutex locked if they are going to be
            called from multiple threads.




    --> --> --> --> --> --> IMPORTANT, LOOK HERE <-- <-- <-- <-- <-- <--

        ****** ENTRY POINT ******:  
            When using the gui objects the entry point to the program is
            void winmain(int argc, char** argv), not the familiar 
            int main(int argc, char** argv).  

            For winmain, argc and argv contain exactly the same values as
            the normal C++ main() arguments.


            You also need to call start_program() to make the gui fire up.  
            Note that message_box() will always work, even if the start_program()
            function has not been called or end_program() has been called.

    --> --> --> --> --> --> IMPORTANT, LOOK HERE  <-- <-- <-- <-- <-- <--




        ****** OVERVIEW ******

        The window object is the top level object and it may contain any of the 
        following objects:

            button,
            flat_button,
            text_field,
            text_box,
            label,
            list_box,
            etched_rectangle,
            sunken_rectangle,
            sunken_3d_rectangle,
            named_rectangle,
            progress_bar,
            check_box,
            radio_button,
            password_field,
            splitter
    !*/

// ----------------------------------------------------------------------------------------

    void message_box (
        const std::string& title,
        const std::string& text,
        bool require_answer = false
    );
    /*!
        ensures
            - displays a popup window with the title title and the message
              contained in text 
            - blocks until user clicks the ok button
            - it is safe to call this function in the main program thread.  
              It will not cause the program to hang.
            - if (require_answer) then 
                - the ok button must be clicked before the user can proceed
    !*/

// -----------------

    bool question_box (
        const std::string& title,
        const std::string& text,
        bool require_answer = false
    );
    /*!
        ensures
            - displays a popup window with the title title and the message
              contained in text.  This window contains a yes and no button
              and can be used to prompt the user with a yes or no question    
            - it is safe to call this function in the main program thread.  
              It will not cause the program to hang.
            - if (the user clicks ok) then 
                - returns true
            - else
                - returns false
            - if (require_answer) then 
                - the user must click yes or no before proceeding
    !*/

// -----------------

    bool select_directory_box (
        std::string& dir_name
    );
    /*!
        ensures
            - promps the user with a select directory dialog box 
            - if (the user selects a directory) then 
                - the directory will be in #dir_name 
                - select_directory_box() returns true 
            - if (the user does NOT select a directory) then 
                - select_directory_box() returns false
            - it is safe to call this function in the main program thread.  
              It will not cause the program to hang.
    !*/

// -----------------

    bool open_file_box (
        std::string& file_name
    );
    /*!
        ensures
            - promps the user with an open file dialog box 
            - if (the user selects a file) then 
                - the file name will be in #file_name 
                - this process's current working directry will be changed to the one
                  which contains the selected file
                - open_file_box() returns true 
            - if (the user does NOT select a file) then 
                - open_file_box() returns false
            - it is safe to call this function in the main program thread.  
              It will not cause the program to hang.
    !*/

// -----------------

    template <
        typename queue  // must be an implementation of queue/queue_kernel_abstract.h
                        // and must be instantiated with std::string
        >
    bool open_multiple_files_box (
        queue& file_names
    );
    /*!
        ensures
            - promps the user with an open file dialog box which allows more than one 
              file to be selected 
            - if (the user selects some files) then 
                - the file names will be in #file_names 
                - this process's current working directry will be changed to the one
                  which contains the selected files
                - open_multiple_files_box() returns true
            - if (the user does NOT select any files) then 
                - open_multiple_files_box() returns false
            - it is safe to call this function in the main program thread.  
              It will not cause the program to hang.
    !*/

// -----------------

    bool save_file_box (
        std::string& file_name
    );
    /*!
        ensures
            - promps the user with a save file dialog box 
            - if (the user selects a file) then 
                - the file name will be in #file_name 
                - this process's current working directry will be changed to the one
                  which contains the selected file
                - save_file_box() returns true 
            if (the user does NOT select a file) then 
                - save_file_box() returns false
            - it is safe to call this function in the main program thread.  
              It will not cause the program to hang.
    !*/

// -----------------

    void start_program (
    );
    /*!
        requires
            - is called only once during the lifetime of the program
            - is called from main program thread
        ensures
            - the gui will start responding 
            - blocks until end_program() is called
    !*/

// -----------------

    void end_program (
    );
    /*!
        requires
            - is called only once during the lifetime of the program
            - start_program() has already been called
            - is called from the main program thread
        ensures
            - causes the start_program() function to end
    !*/

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class window
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class window
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a window on the desktop.

            INITIAL STATE
                the initial state of the window is to be hidden.  this means you need
                to call show() to make it appear.

            on_window_close() callback:
                There are some interesting things to note about this call back.  
                First is that it is safe to call delete this from this callback as
                long as *this was created on the heap and no one will try to use the
                object.  Also note that this call back is not called if the window
                is closed as a result of the window object being destructed.
        !*/


    public:

        window (
        );
        /*!
            requires
                - is called from main program thread
                - winmain() function has been entered
            ensures
                - #*this has been properly initialized 
                - the window is not resizable by the user
        !*/

        window (
            bool resizable
        );
        /*!
            requires
                - is called from main program thread
                - winmain() function has been entered
            ensures
                - #*this has been properly initialized 
                - if (resizable == true) then 
                    - this window will be resizable by the user
                - else 
                    - it will not be resizable by the user
        !*/

        virtual ~window (
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - if (this window has not already been closed) then
                    - closes the window
                - does NOT trigger the on_window_close() event
                - all resources associated with *this have been released                
        !*/

        void set_title (
            const std::string& title
        );
        /*!
            ensures
                - sets the title of the window
        !*/

        void show (
        );
        /*!
            ensures
                - this window will appear on the screen
        !*/

        void hide (
        );
        /*!
            ensures
                - the window is hidden from the screen
        !*/

        void minimize (
        );
        /*!
            ensures
                - this window is minimized
        !*/

        void maximize (
        );
        /*!
            ensures
                - this window is maximized
        !*/

        void set_size (
            int width,
            int height
        );
        /*!
            ensures
                - sets the width of this window 
                - sets the height of this window
                - if (the window wasn't already this size) then
                    - triggers the on_window_resized() callback
        !*/

        void set_position (
            int x,
            int y
        );
        /*!
            ensures
                - sets the upper left corner of this window to the position (x,y) 
                  on the desktop
        !*/

        void get_client_area (
            int& width,
            int& height
        ) const;
        /*!
            ensures
                - #width == the width of the client area of this window
                - #height == the height of the client area of this window
        !*/

        void set_focus (
        );
        /*!
            ensures
                - sets the focus to this window and displays it on the screen
        !*/


    protected:

        // do nothing by default
        virtual void on_window_close(
        ){}
        /*!
            requires
                - is called when this window is closed by the user
                - is called from the main program thread
                - if (*this was allocated on the heap and no one will ever refer to
                  this object again and no one else will or has tried to delete this) then
                    - it is safe to call delete this inside on_window_close()
        !*/

        // do nothing by default
        virtual void on_window_resized(
        ){}
        /*!
            requires
                - is called when this window is resized
                - is called from the main program thread
        !*/

    private:

        // restricted functions
        window(window&);        // copy constructor
        window& operator=(window&);    // assignment operator

    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class object
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a generic object that can appear in a window
        !*/

    public:

        object (
            window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures
                - #*this is properly initialized 
                - #*this has been added to window w
        !*/

        virtual ~object (
        ) =0;
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        virtual void set_size (
            int width,
            int height
        );
        /*!
            ensures
                - sets the width of this object to width 
                - sets the height of this object to height
        !*/

        virtual void set_position (
            int x,
            int y
        );
        /*!
            ensures
                - sets the position of the upper left corner of this object in the client 
                  area to the point (x,y)
        !*/

        virtual void show (
        );
        /*!
            ensures
                - the object appears in the client area
        !*/

        virtual void hide (
        );
        /*!
            ensures
                - the object does NOT appear in the client area
        !*/

        virtual void enable (
        );
        /*!
            ensures
                - the object is enabled
        !*/

        virtual void disable (
        );        
        /*!
            ensures
                - the object is disable and appears grayed
        !*/

        virtual void set_focus (
        );
        /*!
            ensures
                - sets the focus to this object
        !*/


    private:

        // restricted functions
        object(object&);        // copy constructor
        object& operator=(object&);    // assignment operator

    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class button
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class button : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple 3D push button

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        button (
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~button(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - sets the name of the button to name
        !*/

        void click (
        );
        /*!
            ensures
                - click() is thread-safe
                - if (an event handler function has been set for *this) then
                    - the button is clicked and the event handler function executes 
                    - click() does NOT return until the event handler function has 
                      finished 
                    - click() will always cause the event handler function to execute, 
                      even if the button is disabled
                - else
                    - click() does nothing
        !*/


        void set_click_handler (
            void (parent_window::*event_handler)()
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in parent_window
            ensures
                - the event_handler function is called when the button is clicked by 
                  the user
        !*/

    private:

        // restricted functions
        button(button&);        // copy constructor
        button& operator=(button&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class flat_button
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class flat_button : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple 2D flat push button

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        flat_button (
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~flat_button(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - sets the name of the button to name
        !*/

        void click (
        );
        /*!
            ensures
                - click() is thread-safe
                - if (an event handler function has been set for *this) then
                    - the button is clicked and the event handler function executes 
                    - click() does NOT return until the event handler function has 
                      finished 
                    - click() will always cause the event handler function to execute, 
                      even if the button is disabled
                - else
                    - click() does nothing
        !*/

        void set_click_handler (
            void (parent_window::*event_handler)()
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in parent_window
            ensures
                - the event_handler function is called when the button is clicked by
                  the user
        !*/

    private:

        // restricted functions
        flat_button(flat_button&);        // copy constructor
        flat_button& operator=(flat_button&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class text_field
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class text_field : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple one line text field

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:
        text_field(
            parent_window& w,
            bool scrollable = false
        );
        /*!
            requires
                - is called from main program thread
            ensures
                - #*this is properly initialized 
                - #*this has been added to parent_window w
                - if (scrollable) then 
                    - the text_field will have the ability to horizontally scroll its 
                      contents 
                - else
                    - the text_field will only be capable of holding as many letters 
                      as can fit in its box at the same time                 
        !*/

        virtual ~text_field(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_text (
            const std::string& text
        );
        /*!
            ensures
                - #get_text() == text
        !*/

        const std::string get_text (
        ) const;
        /*!
            ensures
                - returns the text that currently appears in the text field
        !*/ 


    private:

        // restricted functions
        text_field(text_field&);        // copy constructor
        text_field& operator=(text_field&);    // assignment operator
        
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class text_box
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class text_box : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple multi line text box with a scroll bar

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        text_box (
            parent_window& w
            bool readonly = false
        );
        /*!
            requires
                - is called from main program thread
            ensures
                - #*this is properly initialized 
                - #*this has been added to parent_window w
                - if (readonly == true) then 
                    - this text_box will be readonly
                - else 
                    - it will be editable by the user
        !*/

        virtual ~text_box(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_text (
            const std::string& text
        );
        /*!
            ensures
                - #get_text() == text
        !*/

        const std::string get_text (
        ) const;
        /*!
            ensures
                - returns the text that currently appears in the text box
        !*/ 

        void scroll_to_top (
        );
        /*!
            ensures
                - the text box has been scrolled to the top of its text
        !*/

        void scroll_to_bottom (
        );
        /*!
            ensures
                - the text box has been scrolled to the bottom of its text
        !*/


    private:

        // restricted functions
        text_box(text_box&);        // copy constructor
        text_box& operator=(text_box&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class label
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class label : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple text label

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear. and
                get_text() == ""

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        label(  
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~label(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_text (
            const std::string& text
        );
        /*!
            ensures
                - #get_text() == text
        !*/

        const std::string get_text (
        ) const;
        /*!
            ensures
                - returns the text of this label
        !*/

    private:

        // restricted functions
        label(label&);        // copy constructor
        label& operator=(label&);    // assignment operator
    };


// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class list_box
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class list_box : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple list box

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        list_box(
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~list_box(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        int size(
        ) const;
        /*!
            ensures
                - returns the number of items in *this
        !*/ 

        const std::string get_element (
            int index
        ) const;
        /*!
            requires
                - 0 <= index < size()
            ensures
                - returns the string from the list box at position index
        !*/

        void insert_element (
            int index,
            const std::string& element
        );
        /*!
            requires
                - 0 <= index <= size()
            ensures
                - inserts element into the list box at position index
        !*/

        void remove_element (
            int index
        );
        /*!
            requires
                - 0 <= index < size()
            ensures
                - removes the element with the specified index from the list box
        !*/

        void clear(
        );
        /*!
            ensures
                - #size() == 0
        !*/

        int get_selected_pos (
        ) const;
        /*!
            ensures
                - returns the index of the selected item 
                - returns -1 if no item is selected 
        !*/

        void set_element_double_clicked_handler (
            void (parent_window::*event_handler)(int index)
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in parent_window
            ensures
                - the event_handler function is called when an element in the list box 
                  is double clicked 
                - index is the position of the double clicked element
        !*/          
       
        void set_element_selected_handler (
            void (parent_window::*event_handler)(int index)
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in parent_window
            ensures
                - the event_handler function is called when the user clicks on an element
                  in the list box or changes the selection using the arrow keys
                - index is the position of the selected element
        !*/
  
    private:

        // restricted functions
        list_box(list_box&);        // copy constructor
        list_box& operator=(list_box&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class etched_rectangle
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class etched_rectangle : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple etched rectangle

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        etched_rectangle(
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~etched_rectangle(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

    private:

        // restricted functions
        etched_rectangle(etched_rectangle&);        // copy constructor
        etched_rectangle& operator=(etched_rectangle&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class sunken_rectangle
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class sunken_rectangle : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple sunken rectangle

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        sunken_rectangle(
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~sunken_rectangle(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

     private:

        // restricted functions
        sunken_rectangle(sunken_rectangle&);        // copy constructor
        sunken_rectangle& operator=(sunken_rectangle&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class sunken_3d_rectangle
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class sunken_3d_rectangle : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a 3D sunken rectangle

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        sunken_3d_rectangle(
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~sunken_3d_rectangle(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

    private:

        // restricted functions
        sunken_3d_rectangle(sunken_3d_rectangle&);        // copy constructor
        sunken_3d_rectangle& operator=(sunken_3d_rectangle&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class named_rectangle
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class named_rectangle : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple rectangle which 
                has a name that appears in its upper left corner

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        named_rectangle(
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~named_rectangle(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - sets the name of the named rectangle to name
        !*/

    private:

        // restricted functions
        named_rectangle(named_rectangle&);        // copy constructor
        named_rectangle& operator=(named_rectangle&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class progress_bar
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class progress_bar : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple progress bar

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

                in the initial state of a progress bar its range
                is from 0 to 100 and its initial position is 0

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        progress_bar(
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~progress_bar(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_range (
            int low,
            int high
        );
        /*!
            requires
                - 0 <= low <= 65535 
                - 0 <= high <= 65535 
                - low <= high
            ensures
                - the range of the progress bar will be from low to high
        !*/

        void set_pos (
            int pos
        );
        /*!
            requires
                - low range value <= pos <= high range value
            ensures
                - the progress bar will now be at position pos
        !*/

    private:

        // restricted functions
        progress_bar(progress_bar&);        // copy constructor
        progress_bar& operator=(progress_bar&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class check_box
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class check_box : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple two state check_box. 
                it is either checked or unchecked and when a user clicks it it
                toggles its state.

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.
                the initial state of the check_box is to be unchecked.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:


        check_box (
            parent_window& w,
            bool name_on_left = false
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
                - if (name_on_left) then 
                    - the name of the check box will appear to the left of the box
                - else 
                    - the name will appear to its right                
        !*/

        virtual ~check_box(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - sets the name of the check box to name
        !*/

        bool is_checked (
        ) const;
        /*!
            ensures
                - returns true if the check box is currently checked 
                - returns false if its not checked
        !*/

        void set_checked (
        );
        /*!
            ensures
                - the check box appears in its checked state
        !*/

        void set_unchecked (
        );
        /*! 
            ensures
                - the check box appears in its unchecked state
        !*/ 

        void set_click_handler (
            void (parent_window::*event_handler)()
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in parent_window
            ensures
                - the event_handler function is called when the check box is toggled by 
                  the user
                - this event is NOT triggered by calling set_checked() or set_unchecked()
        !*/  

    private:

        // restricted functions
        check_box(check_box&);        // copy constructor
        check_box& operator=(check_box&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class radio_button
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class radio_button : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple two state radio_button. 
                it is either checked or unchecked and when a user clicks it, it does
                NOT toggel its state.  It will just fire the on_click() event

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.
                the initial state of the radio_button is to be unchecked.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:

        radio_button (
            parent_window& w,
            bool name_on_left = false
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
                - if (name_on_left) then 
                    - the name of the radio button will appear to the left of the box
                - else 
                    - the name will appear to its right                
        !*/

        virtual ~radio_button(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - sets the name of the radio button to name
        !*/

        bool is_checked (
        ) const;
        /*!
            ensures
                - returns true if the radio button is currently checked 
                - returns false if its not checked
        !*/

        void set_checked (
        );
        /*!
            ensures
                - the radio button appears in its checked state
        !*/

        void set_unchecked (
        );
        /*! 
            ensures
                - the radio button appears in its unchecked state
        !*/ 


        void set_click_handler (
            void (parent_window::*event_handler)()
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in parent_window
            ensures
                - the event_handler function is called when the radio button is toggled 
                  by the user
                - this event is NOT triggered by calling set_checked() or set_unchecked()
        !*/ 

    private:

        // restricted functions
        radio_button(radio_button&);        // copy constructor
        radio_button& operator=(radio_button&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class password_field
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class password_field : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a simple one line text field that will 
                replace characters typed with *'s

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:
        password_field(
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~password_field(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        const std::string get_text (
        ) const;
        /*!
            ensures
                - returns the text that the user has entered into the password field
        !*/ 

    private:

        // restricted functions
        password_field(password_field&);        // copy constructor
        password_field& operator=(password_field&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class splitter
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    class splitter : public object
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a splitter bar, the kind that you can drag
                and use to resize parts of a window.

                The window will move up and down or left and right when you drag 
                it depending on what its size is.  For example, if it is wider
                than it is tall then it will be able to move up and down, if its
                taller than it is wide then it will be able to move left and right.

                the range is also determined by the width vs. height of the splitter.
                if its wide then the range is for the y axis, if its tall then its for 
                the x axis

            INITIAL STATE
                the initial state of this object is to be shown.  this means you do NOT 
                need to call show() to make it appear.
                and the initial range is 0 to 0

            REQUIREMENTS ON parent_window
                is of type window or a subclass of window
        !*/

    public:
        splitter(
            parent_window& w
        );
        /*!
            requires
                - is called from main program thread
            ensures 
                - #*this is properly initialized 
                - #*this has been added to parent_window w
        !*/

        virtual ~splitter(
        );
        /*!
            requires
                - is called from the main program thread
            ensures
                - all resources associated with *this have been released
        !*/

        void get_position (
            int& x,
            int& y
        ) const;
        /*!
            ensures
                - returns the current position of the upper left corner of the splitter
        !*/

        void set_range (
            int start,
            int end
        );
        /*!
            requires
                - end >= start
            ensures
                - the slider will not allow it self to slide outside the range 
                  start to end
        !*/

        void set_move_handler (
            void (parent_window::*event_handler)()
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in parent_window
            ensures
                - the event_handler function is called when the user drags the splitter. 
                  note that it may be called many times over the course of being 
                  dragged across the screen.
        !*/    

    private:

        // restricted functions
        splitter(splitter&);        // copy constructor
        splitter& operator=(splitter&);    // assignment operator        
    };

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_GUI_KERNEl_ABSTRACT_

