// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#undef DLIB_WIDGETs_ABSTRACT_
#ifdef DLIB_WIDGETs_ABSTRACT_

#include "fonts_abstract.h"
#include "drawable_abstract.h"
#include "base_widgets_abstract.h"

#include "../gui_core.h"
#include <string>
#include "../interfaces/enumerable.h"

namespace dlib
{

    /*!
        GENERAL REMARKS
            This component is a collection of various windowing widgets such as buttons,
            labels, text boxes, and so on.  This component also includes the drawable
            interface, drawable_window, and font handling objects.  The file you are
            currently viewing defines all the high level graphical widgets which are 
            provided by this component that can appear in a drawable_window.  To view 
            the specifications for the other members of this component look at 
            fonts_abstract.h, base_widgets_abstract.h, and drawable_abstract.h

        THREAD SAFETY
            All objects and functions defined in this file are thread safe.  You may
            call them from any thread without serializing access to them.

        EVENT HANDLERS
            Note that all event handlers, including the user registered callback
            functions, are executed in the event handling thread. 
    !*/

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // function open_file_box() 
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename T
        >
    void open_file_box (
        T& object,
        void (T::*event_handler)(const std::string&) 
    );
    /*!
        requires
            - event_handler == a valid pointer to a member function of object T.
        ensures
            - Displays a window that will allow the user to select a file. 
            - The event_handler function is called on object if the user selects
              a file.  If the user closes the window without selecting a file
              then nothing occurs.
    !*/

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // function message_box() 
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    void message_box (
        const std::string& title,
        const std::string& message
    );
    /*!
        ensures
            - displays a message box with the given title and message.  It will have a 
              single button and when the user clicks it the message box will go away.
    !*/

    template <
        typename T
        >
    void message_box (
        const std::string& title,
        const std::string& message,
        T& object,
        void (T::*event_handler)() 
    );
    /*!
        requires
            - event_handler == a valid pointer to a member function of object T.
        ensures
            - Displays a message box with the given title and message.  It will have a 
              single button and when the user clicks it the message box will go away.
            - The event_handler function is called on object when the user clicks
              ok or otherwise closes the message box window. 
    !*/

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class label
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class label : public drawable
    {
        /*!
            INITIAL VALUE
                text() == ""
                the text color will be black

            WHAT THIS OBJECT REPRESENTS
                This object represents a simple text label.  The size of the label
                is automatically set to be just big enough to contain its text.
        !*/

    public:

        label(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~label(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        void set_text (
            const std::string& text
        );
        /*!
            ensures
                - #text() == text
            throws
                - std::bad_alloc
        !*/

        const std::string text (
        ) const;
        /*!
            ensures
                - returns the text of this label
            throws
                - std::bad_alloc
        !*/

        void set_color (
            unsigned char red,
            unsigned char green,
            unsigned char blue
        );
        /*!
            ensures
                - text will be drawn with the given color
        !*/

        void get_color (
            unsigned char& red,
            unsigned char& green,
            unsigned char& blue
        ) const;
        /*! 
            ensures
                - #red == the red component of the current text color
                - #green == the green component of the current text color
                - #blue == the blue component of the current text color
        !*/

    private:

        // restricted functions
        label(label&);        // copy constructor
        label& operator=(label&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class button
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class button : public button_action 
    {
        /*!
            INITIAL VALUE
                name() == ""

            WHAT THIS OBJECT REPRESENTS
                This object represents a simple button.  

                When this object is disabled it means it will not respond to user clicks.
        !*/

    public:

        button(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~button(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        void set_size (
            unsigned long width_,
            unsigned long height_
        );
        /*! 
            ensures
                - if (width and height are big enough to contain the name of this button) then
                    - #width() == width_
                    - #height() == height_
                    - #top() == top()
                    - #left() == left()
                    - i.e. The location of the upper left corner of this button stays the
                      same but its width and height are modified
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - #name() == name
                - this button has been resized such that it is big enough to contain
                  the new name.
            throws
                - std::bad_alloc
        !*/

        const std::string name (
        ) const;
        /*!
            ensures
                - returns the name of this button
            throws
                - std::bad_alloc
        !*/

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*event_handler)()
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T 
            ensures
                - the event_handler function is called on object when the button is 
                  clicked by the user.
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
        !*/

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*event_handler)(button& self)
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T 
            ensures
                - &self == this
                - the event_handler function is called on object when the button is 
                  clicked by the user.
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
        !*/

    private:

        // restricted functions
        button(button&);        // copy constructor
        button& operator=(button&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class text_field
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class text_field : public drawable
    {
        /*!
            INITIAL VALUE
                text() == ""
                width() == 10
                height() == a height appropriate for the font used.
                The text color will be black.

            WHAT THIS OBJECT REPRESENTS
                This object represents a simple one line text input field.  
        !*/

    public:

        text_field(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~text_field(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        void set_text (
            const std::string& text
        );
        /*!
            requires
                - text.find_first_of('\n') == std::string::npos 
                  (i.e. there aren't any new lines in text)
            ensures
                - #text() == text
            throws
                - std::bad_alloc
        !*/

        const std::string text (
        ) const;
        /*!
            ensures
                - returns the text of this text_field
            throws
                - std::bad_alloc
        !*/

        void set_width (
            unsigned long width_
        );
        /*! 
            ensures
                - if (width >= 10) then
                    - #width()  == width_
                    - #height() == height()
                    - #top()    == top()
                    - #left()   == left()
                    - i.e. The width of this drawable is set to the given width but 
                      nothing else changes.
        !*/

        void set_color (
            unsigned char red,
            unsigned char green,
            unsigned char blue
        );
        /*!
            ensures
                - text will be drawn with the given color
        !*/

        void get_color (
            unsigned char& red,
            unsigned char& green,
            unsigned char& blue
        ) const;
        /*! 
            ensures
                - #red == the red component of the current text color
                - #green == the green component of the current text color
                - #blue == the blue component of the current text color
        !*/

    private:

        // restricted functions
        text_field(text_field&);        // copy constructor
        text_field& operator=(text_field&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class check_box
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class check_box : public button_action 
    {
        /*!
            INITIAL VALUE
                name() == ""
                is_checked() == false

            WHAT THIS OBJECT REPRESENTS
                This object represents a simple two state check box.  Is is either
                checked or unchecked and when a user clicks on it it toggles its
                state.

                When this object is disabled it means it will not respond to user clicks.
        !*/

    public:

        check_box(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~check_box(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - #name() == name
                - this check_box has been resized such that it is big enough to contain
                  the new name.
            throws
                - std::bad_alloc
        !*/

        bool is_checked (
        ) const;
        /*!
            ensures
                - if (this box is currently checked) then
                    - returns true
                - else
                    - returns false
        !*/

        const std::string name (
        ) const;
        /*!
            ensures
                - returns the name of this check_box.  The name is a string
                  that appears to the right of the actual check box.
            throws
                - std::bad_alloc
        !*/

        void set_checked (
        );
        /*!
            ensures
                - #is_checked() == true 
        !*/

        void set_unchecked (
        );
        /*! 
            ensures
                - #is_checked() == false 
        !*/ 

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*event_handler)(check_box& self)
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T.
            ensures
                - the event_handler function is called on object when the check box is 
                  toggled by the user. self will be a reference to the check_box object
                  that the user clicked.
                - this event is NOT triggered by calling set_checked() or set_unchecked().
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
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

    class radio_button : public button_action 
    {
        /*!
            INITIAL VALUE
                name() == ""
                is_checked() == false

            WHAT THIS OBJECT REPRESENTS
                This object represents a simple two state radio button.  Is is either
                checked or unchecked and when a user clicks on it it toggles its
                state.

                When this object is disabled it means it will not respond to user clicks.
        !*/

    public:

        radio_button(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~radio_button(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - #name() == name
                - this radio_button has been resized such that it is big enough to contain
                  the new name.
            throws
                - std::bad_alloc
        !*/

        bool is_checked (
        ) const;
        /*!
            ensures
                - if (this box is currently checked) then
                    - returns true
                - else
                    - returns false
        !*/

        const std::string name (
        ) const;
        /*!
            ensures
                - returns the name of this radio_button.  The name is a string
                  that appears to the right of the actual check box.
            throws
                - std::bad_alloc
        !*/

        void set_checked (
        );
        /*!
            ensures
                - #is_checked() == true 
        !*/

        void set_unchecked (
        );
        /*! 
            ensures
                - #is_checked() == false 
        !*/ 

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*event_handler)(radio_button& self)
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T.
            ensures
                - the event_handler function is called on object when the this radio button 
                  is toggled by the user.  self will be a reference to the radio_button 
                  object the user clicked.
                - this event is NOT triggered by calling set_checked() or set_unchecked().
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
        !*/

    private:

        // restricted functions
        radio_button(radio_button&);        // copy constructor
        radio_button& operator=(radio_button&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class tabbed_display
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class tabbed_display : public drawable
    {
        /*!
            INITIAL VALUE
                number_of_tabs() == 1
                selected_tab() == 0

            WHAT THIS OBJECT REPRESENTS
                This object represents a row of tabs that are user selectable.  

                When this object is disabled it means it will not respond to user clicks.
        !*/

    public:

        tabbed_display(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~tabbed_display(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        void set_size (
            unsigned long width_,
            unsigned long height_
        );
        /*! 
            ensures
                - if (width and height are big enough to contain the tabs) then
                    - #width() == width_
                    - #height() == height_
                    - #top() == top()
                    - #left() == left()
                    - i.e. The location of the upper left corner of this widget stays the
                      same but its width and height are modified
        !*/

        void set_number_of_tabs (
            unsigned long num
        );
        /*!
            requires
                - num > 0
            ensures
                - #number_of_tabs() == num
                - no tabs have any widget_groups associated with them.
                - for all valid idx:
                    - #tab_name(idx) == ""
            throws
                - std::bad_alloc
        !*/

        unsigned long selected_tab (
        ) const;
        /*!
            ensures
                - returns the index of the currently selected tab
        !*/

        unsigned long number_of_tabs (
        ) const;
        /*!
            ensures
                - returns the number of tabs in this tabbed_display
        !*/

        const std::string& tab_name (
            unsigned long idx
        ) const;
        /*!
            requires
                - idx < number_of_tabs()
            ensures
                - returns a const reference to the name of the tab given by idx
        !*/

        void set_tab_name (
            unsigned long idx,
            const std::string& new_name
        );
        /*!
            requires
                - idx < number_of_tabs()
            ensures
                - #tab_name(idx) == new_name
            throws
                - std::bad_alloc
        !*/

        void set_tab_group (
            unsigned long idx,
            widget_group& group
        );
        /*!
            requires
                - idx < number_of_tabs()
            ensures
                - if (is_hidden()) then
                    - group.is_hidden() == true
                - else
                    - whenever the tab with index idx is selected group.is_hidden() == false 
                    - whenever the tab with index idx is deselected group.is_hidden() == true 
                - whenever the position of *this changes the position of group will be
                  updated so that it is still inside the tabbed_display.  The position of group
                  will also be updated after this call to set_tab_group().
                - any previous calls to set_tab_group() with this index are overridden by this
                  new call.  (i.e. you can only have one widget_group associated with a single
                  tab at a time)
        !*/

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*event_handler)(unsigned long new_idx, unsigned long old_idx)
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T 
            ensures
                - The event_handler function is called on object when the user clicks
                  on a tab that isn't already selected.  new_idx will give the index of 
                  the newly selected tab and old_idx will give the index of the tab 
                  that was previously selected. 
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
        !*/

    private:

        // restricted functions
        tabbed_display(tabbed_display&);        // copy constructor
        tabbed_display& operator=(tabbed_display&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class named_rectangle
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class named_rectangle : public drawable 
    {
        /*!
            INITIAL VALUE
                name() == ""

            WHAT THIS OBJECT REPRESENTS
                This object represents a simple named rectangle.  
        !*/

    public:

        named_rectangle(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~named_rectangle(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        void set_size (
            unsigned long width_,
            unsigned long height_
        );
        /*! 
            ensures
                - #width() == width_
                - #height() == height_
                - #top() == top()
                - #left() == left()
                - i.e. The location of the upper left corner of this widget stays the
                  same but its width and height are modified
        !*/

        void wrap_around (
            const rectangle& rect
        );
        /*!
            ensures
                - This object will be repositioned and sized so that it fits
                  around the given rectangle.
        !*/

        void set_name (
            const std::string& name
        );
        /*!
            ensures
                - #name() == name
            throws
                - std::bad_alloc
        !*/

        const std::string name (
        ) const;
        /*!
            ensures
                - returns the name of this named_rectangle
            throws
                - std::bad_alloc
        !*/

    private:

        // restricted functions
        named_rectangle(named_rectangle&);        // copy constructor
        named_rectangle& operator=(named_rectangle&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class mouse_tracker
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class mouse_tracker : public dragable 
    {
        /*!
            INITIAL VALUE
                dragable_area() == rectangle(0,0,500,500)

            WHAT THIS OBJECT REPRESENTS
                This object represents a simple dragable box that displays the 
                current location of the mouse.  

                Also, if you hold shift and left click on the parent window then the 
                mouse_tracker will place a single red pixel where you clicked and will
                display the mouse position relative to that point.
        !*/

    public:

        mouse_tracker(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~mouse_tracker(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

    private:

        // restricted functions
        mouse_tracker(mouse_tracker&);        // copy constructor
        mouse_tracker& operator=(mouse_tracker&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class list_box
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class list_box : public drawable, 
                     public enumerable<const std::string>
    {
        /*!
            INITIAL VALUE
                multiple_select_enabled() == false 
                size() == 0

            ENUMERATION ORDER
                The enumerator will iterate over the elements in the list_box from
                the 0th element to the (size()-1)th element.  i.e. (*this)[0] to
                (*this)[size()-1].

            WHAT THIS OBJECT REPRESENTS
                This object represents a simple textual list box.  It contains a 
                vertical list of strings which the user may select from.
        !*/

    public:

        list_box(  
            drawable_window& w
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~list_box(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        void set_size (
            unsigned long width_,
            unsigned long height_
        );
        /*! 
            ensures
                - #width() == width_
                - #height() == height_
                - #top() == top()
                - #left() == left()
                - i.e. The location of the upper left corner of this widget stays the
                  same but its width and height are modified
        !*/

        bool is_selected (
            unsigned long index
        ) const;
        /*!
            requires
                - index < size()
            ensures
                - if (the item given by index is currently selected) then
                    - returns true
                - else
                    - returns false
        !*/

        void select (
            unsigned long index 
        );
        /*!
            requires
                - index < size()
            ensures
                - #is_selected(index) == true
        !*/

        void unselect (
            unsigned long index 
        );
        /*!
            requires
                - index < size()
            ensures
                - #is_selected(index) == false
        !*/

        template <typename T>
        void get_selected (
            T& list
        ) const;
        /*!
            requires
                - T == an implementation of dlib/queue/queue_kernel_abstract.h 
                - T::type == unsigned long
            ensures
                - #list == a list of all the currently selected indices for this list_box.
        !*/

        unsigned long get_selected (
        ) const;
        /*!
            requires
                - multiple_select_enabled() == false
            ensures
                - if (there is currently something selected) then
                    - returns the index of the selected item
                - else
                    - returns size()
        !*/

        template <typename T>
        void load (
            const T& list
        );
        /*!
            requires
                - T == compatible with dlib::enumerable<std::string>
            ensures
                - #size() == list.size()
                - Copies all the strings from list into *this in the order in which they are enumerated.
                  (i.e. The first one goes into (*this)[0], the second into (*this)[1], and so on...)
        !*/

        const std::string& operator[] (
            unsigned long index
        ) const;
        /*!
            requires
                - index < size()
            ensures
                - returns the name of the indexth item/row in this list box.
        !*/

        bool multiple_select_enabled (
        ) const;
        /*!
            ensures
                - if (this object will allow the user to select more than one item at a time) then
                    - returns true
                - else
                    - returns false
        !*/

        void enable_multiple_select (
        ); 
        /*!
            ensures
                - #multiple_select_enabled() == true
        !*/

        void disable_multiple_select (
        );
        /*!
            ensures
                - #multiple_select_enabled() == false
        !*/

        template <
            typename T
            >
        void set_double_click_handler (
            T& object,
            void (T::*event_handler)(unsigned long index)
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T.
            ensures
                - The event_handler function is called on object when the user double 
                  clicks on one of the rows in this list box.  index gives the row 
                  number for the item the user clicked.
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
        !*/

    private:

        // restricted functions
        list_box(list_box&);        // copy constructor
        list_box& operator=(list_box&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_WIDGETs_ABSTRACT_

