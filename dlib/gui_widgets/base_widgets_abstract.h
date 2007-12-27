// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_BASE_WIDGETs_ABSTRACT_
#ifdef DLIB_BASE_WIDGETs_ABSTRACT_

#include "fonts_abstract.h"
#include "drawable_abstract.h"

#include "../gui_core.h"
#include <string>

namespace dlib
{

    /*!
        GENERAL REMARKS
            This file contains objects that are useful for creating complex drawable 
            widgets.

        THREAD SAFETY
            All objects and functions defined in this file are thread safe.  You may
            call them from any thread without serializing access to them.

        EVENT HANDLERS
            If you derive from any of the drawable objects and redefine any of the on_*() 
            event handlers then you should ensure that your version calls the same event 
            handler in the base object so that the base class part of your object will also 
            be able to process the event. 

            Also note that all event handlers, including the user registered callback
            functions, are executed in the event handling thread. 
    !*/

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class dragable
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class dragable : public drawable
    {
        /*!
            INITIAL VALUE
                dragable_area() == an initial value for its type 

            WHAT THIS OBJECT REPRESENTS
                This object represents a drawable object that is dragable by the mouse.  
                You use it by inheriting from it and defining the draw() method and any
                of the on_*() event handlers you need.  

                This object is dragable by the user when is_enabled() == true and 
                not dragable otherwise.
        !*/

    public:

        dragable(  
            drawable_window& w,
            unsigned long events = 0
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
                - This object will not receive any events or draw() requests until 
                  enable_events() is called
                - This object will be dragable once enable_events() has been called.
                - this object will receive mouse move and mouse click events once
                  enable_events() has been called.
                - if (events & MOUSE_WHEEL) then
                    - once enable_events() has been called this drawable will receive 
                      the following events related to mouse wheel scrolling: 
                      on_wheel_up and on_wheel_down.
                - if (events & WINDOW_RESIZED) then
                    - once enable_events() has been called this drawable will receive 
                      the following event related to its parent window resizing:  
                      on_window_resized.    
                - if (events & KEYBOARD_EVENTS) then
                    - once enable_events() has been called this drawable will receive 
                      the following keyboard event: on_keydown.                
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~dragable(
        ) = 0;
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        rectangle dragable_area (
        ) const;
        /*!
            ensures
                - returns the area that this dragable can be dragged around in. 
        !*/

        void set_dragable_area (
            const rectangle& area 
        ); 
        /*!
            ensures
                - #dragable_area() == area
        !*/

    protected:

        // does nothing by default
        virtual void on_drag (
        ){}
        /*!
            requires
                - enable_events() has been called
                - is_enabled() == true
                - is_hidden() == false
                - mutex drawable::m is locked
                - is called when the user drags this object
                - get_rect() == the rectangle that defines the new position
                  of this object.
            ensures
                - does not change the state of mutex drawable::m. 
        !*/

    private:

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
                is_depressed() == false

            WHAT THIS OBJECT REPRESENTS
                This object represents the clicking action of a push button.  It provides
                simple callbacks that can be used to make various kinds of button 
                widgets.

                You use it by inheriting from it and defining the draw() method and any
                of the on_*() event handlers you need.  
        !*/

    public:

        button_action(  
            drawable_window& w,
            unsigned long events = 0
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
                - #*this will not receive any events or draw() requests until 
                  enable_events() is called
                - this object will receive mouse move and mouse click events once
                  enable_events() has been called.
                - if (events & MOUSE_WHEEL) then
                    - once enable_events() has been called this drawable will receive 
                      the following events related to mouse wheel scrolling: 
                      on_wheel_up and on_wheel_down.
                - if (events & WINDOW_RESIZED) then
                    - once enable_events() has been called this drawable will receive 
                      the following event related to its parent window resizing:  
                      on_window_resized.    
                - if (events & KEYBOARD_EVENTS) then
                    - once enable_events() has been called this drawable will receive 
                      the following keyboard event: on_keydown.                
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~button_action(
        ) = 0;
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

    protected:

        bool is_depressed (
        ) const;
        /*!
            requires
                - mutex drawable::m is locked
            ensures
                - if (this button is currently in a depressed state) then
                    - the user has left clicked on this drawable and is still
                      holding the left mouse button down over it.
                    - returns true
                - else
                    - returns false
        !*/

        // does nothing by default
        virtual void on_button_down (
        ){}
        /*!
            requires
                - enable_events() has been called
                - mutex drawable::m is locked
                - is_enabled() == true
                - is_hidden() == false
                - the area in parent_window() defined by get_rect() has been invalidated. 
                  (This means you don't have to call invalidate_rectangle())
                - is called whenever this object transitions from the state where
                  is_depressed() == false to is_depressed() == true
            ensures
                - does not change the state of mutex drawable::m. 
        !*/

        // does nothing by default
        virtual void on_button_up (
            bool mouse_over
        ){}
        /*!
            requires
                - enable_events() has been called
                - mutex drawable::m is locked
                - is_enabled() == true
                - is_hidden() == false
                - the area in parent_window() defined by get_rect() has been invalidated. 
                  (This means you don't have to call invalidate_rectangle())
                - is called whenever this object transitions from the state where
                  is_depressed() == true to is_depressed() == false 
                - if (the mouse was over this button when this event occurred) then
                    - mouse_over == true
                - else
                    - mouse_over == false
            ensures
                - does not change the state of mutex drawable::m. 
        !*/

    private:

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
                direction() == a value given to the constructor.

            WHAT THIS OBJECT REPRESENTS
                This object represents a push button with an arrow in the middle. 

                When this object is disabled it means it will not respond to user clicks.
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
            arrow_direction dir 
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #direction() == dir
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~arrow_button(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        arrow_direction direction (
        ) const;
        /*!
            ensures
                - returns the direction that this arrow_button's arrow points
        !*/

        void set_direction (
            arrow_direction new_direction
        );
        /*!
            ensures
                - #direction() == new_direction
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
                - i.e. The location of the upper left corner of this button stays the
                  same but its width and height are modified
        !*/

        bool is_depressed (
        ) const;
        /*!
            ensures
                - if (this button is currently in a depressed state) then
                    - the user has left clicked on this drawable and is still
                      holding the left mouse button down over it.
                    - returns true
                - else
                    - returns false
        !*/

        template <
            typename T
            >
        void set_button_down_handler (
            T& object,
            void (T::*event_handler)()
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T
            ensures
                - The event_handler function is called whenever this object transitions
                  from the state where is_depressed() == false to is_depressed() == true
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
        !*/

        template <
            typename T
            >
        void set_button_up_handler (
            T& object,
            void (T::*event_handler)(bool mouse_over)
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T
            ensures
                - The event_handler function is called whenever this object transitions
                  from the state where is_depressed() == true to is_depressed() == false.
                  furthermore:
                    - if (the mouse was over this button when this event occurred) then
                        - mouse_over == true
                    - else
                        - mouse_over == false
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
        !*/

    private:

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
                orientation() == a value given to the constructor.
                max_slider_pos() == 0
                slider_pos() == 0
                jump_size() == 10

            WHAT THIS OBJECT REPRESENTS
                This object represents a scroll bar.  The slider_pos() of the scroll bar
                ranges from 0 to max_slider_pos().  The 0 position of the scroll_bar is
                in the top or left side of the scroll_bar depending on its orientation.

                When this object is disabled it means it will not respond to user clicks.
        !*/

    public:
        enum bar_orientation 
        {
            HORIZONTAL,
            VERTICAL
        };

        scroll_bar(  
            drawable_window& w,
            bar_orientation orientation 
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #orientation() == orientation 
                - #parent_window() == w
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~scroll_bar(
        );
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        bar_orientation orientation (
        ) const;
        /*!
            ensures
                - returns the orientation of this scroll_bar 
        !*/

        void set_orientation (
            bar_orientation new_orientation   
        );
        /*!
            ensures
                - #orientation() == new_orientation
        !*/

        void set_length (
            unsigned long length,
        );
        /*! 
            ensures
                - if (orientation() == HORIZONTAL) then
                    - #width() == length
                - else
                    - #height() == length
        !*/

        unsigned long max_slider_pos (
        ) const;
        /*!
            ensures
                - returns the maximum value that slider_pos() can take. 
        !*/

        void set_max_slider_pos (
            unsigned long mpos
        );
        /*!
            ensures
                - #max_slider_pos() == mpos
                - if (slider_pos() > mpos) then
                    - #slider_pos() == mpos
        !*/

        void set_slider_pos (
            unsigned long pos
        );
        /*!
            requires
                - pos <= max_slider_pos()
            ensures
                - #slider_pos() == pos
        !*/

        unsigned long slider_pos (
        ) const;
        /*!
            ensures
                - returns the current position of the slider box within the scroll bar.
        !*/

        unsigned long jump_size (
        ) const;
        /*!
            ensures
                - returns the number of positions that the slider bar will jump when the
                  user clicks on the empty gaps above or below the slider bar.
                  (note that the slider will jump less than the jump size if it hits the 
                  end of the scroll bar)
        !*/

        void set_jump_size (
            unsigned long js 
        );
        /*!
            ensures
                - #jump_size() == js 
        !*/


        template <
            typename T
            >
        void set_scroll_handler (
            T& object,
            void (T::*event_handler)()
        );
        /*!
            requires
                - event_handler is a valid pointer to a member function in T
            ensures
                - The event_handler function is called whenever the user causes the slider box
                  to move.  
                - This event is NOT triggered by calling set_slider_pos()
                - any previous calls to this function are overridden by this new call.  
                  (i.e. you can only have one event handler associated with this 
                  event at a time)
            throws
                - std::bad_alloc
        !*/

    private:

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
                size() == 0
                is_empty() == true
                left() == 0
                top() == 0

            WHAT THIS OBJECT REPRESENTS
                This object represents a grouping of drawable widgets.  It doesn't draw 
                anything itself, rather it lets you manipulate the position, enabled
                status, and visibility of a set of widgets as a group.
        !*/

    public:
        widget_group(  
            drawable_window& w,
            unsigned long events = 0
        );
        /*!
            ensures 
                - #*this is properly initialized 
                - #*this has been added to window w
                - #parent_window() == w
                - events will be passed unaltered to the second argument of the drawable 
                  constructor.
            throws
                - std::bad_alloc
                - dlib::thread_error
        !*/

        virtual ~widget_group(
        );
        /*!
            ensures
                - all resources associated with *this have been released.
        !*/

        void empty (
        );
        /*!
            ensures
                - #size() == 0
        !*/

        void fit_to_contents (
        );
        /*!
            ensures
                - does not change the position of this object. 
                  (i.e. the upper left corner of get_rect() remains at the same position)
                - if (size() == 0) then
                    - #is_empty() == true
                - else
                    - #get_rect() will be the smallest rectangle that contains all the 
                      widgets in this group and the upper left corner of get_rect(). 
        !*/

        unsigned long size (
        ) const;
        /*!
            ensures
                - returns the number of widgets currently in *this.
        !*/

        void add (
            drawable& widget,
            unsigned long x,
            unsigned long y
        );
        /*!
            ensures
                - #is_member(widget) == true
                - if (is_member(widget) == false) then
                    - #size() == size() + 1
                - else
                    - #size() == size()
                - The following conditions apply to this function as well as to all of the 
                  following functions so long as is_member(widget) == true: 
                  enable(), disable(), hide(), show(), set_z_order(), and set_pos().
                    - #widget.left() == left()+x
                    - #widget.width() == widget.width()
                    - #widget.top() == top()+y
                    - #widget.height() == widget.height()
                    - #widget.is_hidden() == is_hidden()
                    - #widget.is_enabled() == is_enabled()
                    - #widget.z_order() == z_order()
            throws
                - std::bad_alloc
        !*/

        bool is_member (
            const drawable& widget
        ) const;
        /*!
            ensures
                - returns true if widget is currently in this object, returns false otherwise.
        !*/

        void remove (
            const drawable& widget
        );
        /*!
            ensures
                - #is_member(widget) == false 
                - if (is_member(widget) == true) then
                    - #size() == size() - 1
                - else
                    - #size() == size()
        !*/

    protected:

        // this object doesn't draw anything but also isn't abstract
        void draw (
            const canvas& c
        ) const {}

    private:

        // restricted functions
        widget_group(widget_group&);        // copy constructor
        widget_group& operator=(widget_group&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------

    class image_widget : public dragable
    {
        /*!
            INITIAL VALUE
                dragable_area() == an initial value for its type.
                This object isn't displaying anything. 

            WHAT THIS OBJECT REPRESENTS
                This object represents a dragable image.  You give it an image to display
                by calling set_image().

                Also note that initially the dragable area is empty so it won't be 
                dragable unless you call set_dragable_area() to some non-empty region.
        !*/

    public:

        image_window(  
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

        virtual ~image_window(
        ) = 0;
        /*!
            ensures
                - all resources associated with *this have been released
        !*/

        template <
            typename image_type // is an implementation of array2d/array2d_kernel_abstract.h.
                                // Also, pixel_traits<typename image_type::type> must be defined 
            >
        void set_image (
            const image_type& img
        );
        /*!
            ensures
                - #width() == img.width()
                - #height() == img.height()
                - #*this widget is now displaying the given image img.
        !*/

    private:

        // restricted functions
        image_window(image_window&);        // copy constructor
        image_window& operator=(image_window&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_BASE_WIDGETs_ABSTRACT_


