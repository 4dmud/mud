// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.

#ifndef DLIB_WIDGETs_
#define DLIB_WIDGETs_

#include "widgets_abstract.h"
#include "drawable.h"
#include "../gui_core.h"
#include "fonts.h"
#include <string>
#include <sstream>
#include "../algs.h"
#include "../timer.h"
#include "base_widgets.h"
#include "../member_function_pointer.h"
#include "../array.h"
#include "../sequence.h"
#include "../dir_nav.h"
#include "../queue.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class label  
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class label : public drawable
    {
    public:
        label(
            drawable_window& w
        ) : 
            drawable(w),
            f(*default_font::get_font()),
            red(0),
            green(0),
            blue(0)
        {
            enable_events();
        }

        ~label()
        { disable_events(); parent.invalidate_rectangle(rect); }

        void set_text (
            const std::string& text
        );

        const std::string text (
        ) const;

        void set_color (
            unsigned char red_,
            unsigned char green_,
            unsigned char blue_
        );

        void get_color (
            unsigned char& red_,
            unsigned char& green_,
            unsigned char& blue_
        ) const;

    private:
        std::string text_;
        const font& f;
        unsigned char red, green, blue;


        // restricted functions
        label(label&);        // copy constructor
        label& operator=(label&);    // assignment operator

    protected:

        void draw (
            const canvas& c
        ) const;

    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class button  
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class button : public button_action 
    {
    public:
        button(
            drawable_window& w
        ) : 
            button_action(w),
            f(*default_font::get_font()),
            name_width(0),
            padding(4)
        {
            enable_events();
        }
        
        ~button() { disable_events(); parent.invalidate_rectangle(rect); }

        void set_size (
            unsigned long width,
            unsigned long height
        );

        void set_name (
            const std::string& name_
        );

        const std::string name (
        ) const;

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*event_handler_)()
        )
        {
            auto_mutex M(m);
            event_handler.set(object,event_handler_);
            event_handler_self.clear();
        }

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*event_handler_)(button&)
        )
        {
            auto_mutex M(m);
            event_handler_self.set(object,event_handler_);
            event_handler.clear();
        }

    private:

        // restricted functions
        button(button&);        // copy constructor
        button& operator=(button&);    // assignment operator


        std::string name_;
        const font& f;

        // this is the width of the name string
        unsigned long name_width;
        // this is the minimum amount of padding that can separate the name from the
        // edge of the button
        const unsigned long padding;

        member_function_pointer<>::kernel_1a event_handler;
        member_function_pointer<button&>::kernel_1a event_handler_self;

    protected:

        void draw (
            const canvas& c
        ) const;

        void on_button_up (
            bool mouse_over
        );

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
                red == 0
                green == 0
                blue == 0
                cursor_pos == 0
                text_width == 0
                text_ == ""
                has_focus == false
                cursor_visible == false
                recent_movement == false
                highlight_start == 0
                highlight_end == -1
                shift_pos == -1
                text_pos == 0
    
            CONVENTION
                - cursor_pos == the position of the cursor in the string text_.  The 
                  cursor appears before the letter text_[cursor_pos]
                - cursor_x == the x coordinate of the cursor relative to the left side 
                  of rect.  i.e. the number of pixels that separate the cursor from the
                  left side of the text_field.
                - has_focus == true if this text field has keyboard input focus
                - cursor_visible == true if the cursor should be painted
                - text_ == text()
                - text_pos == the index of the first letter in text_ that appears in 
                  this text field.
                - text_width == the width of text_[text_pos] though text_[text.size()-1]

                - if (has_focus && the user has recently moved the cursor) then
                    - recent_movement == true
                - else
                    - recent_movement == false

                - if (highlight_start <= highlight_end) then
                    - text[highlight_start] though text[highlight_end] should be
                      highlighted

                - if (shift_pos != -1) then
                    - has_focus == true
                    - the shift key is being held down or the left mouse button is
                      being held down.
                    - shift_pos == the position of the cursor when the shift or mouse key
                      was first pressed.

        !*/

    public:
        text_field(
            drawable_window& w
        ) : 
            drawable(w,MOUSE_CLICK | KEYBOARD_EVENTS | MOUSE_MOVE),
            f(*default_font::get_font()),
            red(0),
            green(0),
            blue(0),
            text_width(0),
            text_pos(0),
            recent_movement(false),
            has_focus(false),
            cursor_visible(false),
            cursor_pos(0),
            highlight_start(0),
            highlight_end(-1),
            shift_pos(-1),
            t(*this,&text_field::timer_action)
        {
            rect.set_bottom(f.height()+ (f.height()-f.ascender())*2);
            rect.set_right(9);
            cursor_x = (f.height()-f.ascender());
            enable_events();

            t.set_delay_time(500);
        }

        ~text_field (
        )
        {
            disable_events();
            parent.invalidate_rectangle(rect); 
            t.stop_and_wait();
        }

        void set_text (
            const std::string& text_
        );

        const std::string text (
        ) const;

        void set_color (
            unsigned char red_,
            unsigned char green_,
            unsigned char blue_
        );

        void get_color (
            unsigned char& red_,
            unsigned char& green_,
            unsigned char& blue_
        ) const;

        void set_width (
            unsigned long width
        );

    private:

        void on_user_event (
            int
        )
        {
            if (recent_movement == false)
            {
                cursor_visible = !cursor_visible; 
                parent.invalidate_rectangle(rect); 
            }
            else
            {
                if (cursor_visible == false)
                {
                    cursor_visible = true;
                    parent.invalidate_rectangle(rect); 
                }
                recent_movement = false;
            }
        }

        void timer_action (
        ) { parent.trigger_user_event(this,0); }
        /*!
            ensures
                - flips the state of cursor_visible
        !*/

        void move_cursor (
            unsigned long pos
        );
        /*!
            requires
                - pos <= text_.size() 
            ensures
                - moves the cursor to the position given by pos and moves the text 
                  in the text box if necessary
                - if the position changes then the parent window will be updated
        !*/

        rectangle get_text_rect (
        ) const;
        /*!
            ensures
                - returns the rectangle that should contain the text in this widget
        !*/

        std::string text_;
        const font& f;
        unsigned char red, green, blue;

        unsigned long text_width;
        unsigned long text_pos;


        bool recent_movement;
        bool has_focus;
        bool cursor_visible;
        long cursor_pos;
        unsigned long cursor_x;

        // this tells you what part of the text is highlighted
        long highlight_start;
        long highlight_end;
        long shift_pos;


        timer<text_field>::kernel_2a t;

        // restricted functions
        text_field(text_field&);        // copy constructor
        text_field& operator=(text_field&);    // assignment operator


    protected:

        void draw (
            const canvas& c
        ) const;


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

        void on_keydown (
            unsigned long key,
            bool is_printable,
            bool shift,
            bool ctrl
        );
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
                - checked == false
                - name_ == ""

            CONVENTION
                - is_checked() == checked
                - name() == name_

                - if (event_handler.is_set()) then
                    - event_handler() is what is called to process click events
                      on this object.
        !*/

    public:

        check_box(  
            drawable_window& w
        );

        virtual ~check_box(
        );

        void set_name (
            const std::string& name
        );

        bool is_checked (
        ) const;

        const std::string name (
        ) const;

        void set_checked (
        );

        void set_unchecked (
        );

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*eh)(check_box&)
        )
        {
            auto_mutex M(m);
            event_handler.set(object,eh);
        }

    private:

        bool checked;
        std::string name_;

        const font& f;

        member_function_pointer<check_box&>::kernel_1a event_handler;


        // restricted functions
        check_box(check_box&);        // copy constructor
        check_box& operator=(check_box&);    // assignment operator

    protected:

        void draw (
            const canvas& c
        ) const;

        void on_button_up (
            bool mouse_over
        );

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
                - checked == false
                - name_ == ""

            CONVENTION
                - is_checked() == checked
                - name() == name_

                - if (event_handler.is_set()) then
                    - event_handler() is what is called to process click events
                      on this object.
        !*/

    public:

        radio_button(  
            drawable_window& w
        );

        virtual ~radio_button(
        );

        void set_name (
            const std::string& name
        );

        bool is_checked (
        ) const;

        const std::string name (
        ) const;

        void set_checked (
        );

        void set_unchecked (
        );

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*eh)(radio_button&)
        )
        {
            auto_mutex M(m);
            event_handler.set(object,eh);
        }

    private:

        bool checked;
        std::string name_;

        const font& f;

        member_function_pointer<radio_button&>::kernel_1a event_handler;


        // restricted functions
        radio_button(radio_button&);        // copy constructor
        radio_button& operator=(radio_button&);    // assignment operator

    protected:

        void draw (
            const canvas& c
        ) const;

        void on_button_up (
            bool mouse_over
        );

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
                - tabs.size() == 0
                - f == *default_font::get_font()
                - selected_tab_ == 0

            CONVENTION
                - number_of_tabs() == tabs.size()
                - tab_name(idx) == tabs[idx]
                - if (tabs.size() > 0) then
                    - selected_tab_ == the index of the tab that is currently selected

                - for all valid i:
                    - tabs[i].width == f.compute_size(tabs[i].name)
                    - tabs[i].rect == the rectangle that defines where this tab is
                    - if (tabs[i].group != 0) then
                        - tabs[i].group == a pointer to the widget_group for this tab.

                - left_pad == the amount of padding in a tab to the left of the name string.
                - right_pad == the amount of padding in a tab to the right of the name string.
                - top_pad == the amount of padding in a tab to the top of the name string.
                - bottom_pad == the amount of padding in a tab to the bottom of the name string.

                - if (event_handler.is_set()) then
                    - event_handler() is what is called to process click events
                      on this object.
        !*/

    public:

        tabbed_display(  
            drawable_window& w
        );

        virtual ~tabbed_display(
        );

        void set_size (
            unsigned long width,
            unsigned long height
        );

        void set_number_of_tabs (
            unsigned long num
        );

        unsigned long number_of_tabs (
        ) const;

        const std::string& tab_name (
            unsigned long idx
        ) const;

        void set_tab_name (
            unsigned long idx,
            const std::string& new_name
        );

        void set_pos (
            long x,
            long y
        );

        template <
            typename T
            >
        void set_click_handler (
            T& object,
            void (T::*eh)(unsigned long new_idx,unsigned long old_idx)
        )
        {
            auto_mutex M(m);
            event_handler.set(object,eh);
        }

        void set_tab_group (
            unsigned long idx,
            widget_group& group
        );

        void show (
        );

        void hide (
        );

    protected:
        void on_mouse_down (
            unsigned long btn,
            unsigned long state,
            long x,
            long y,
            bool is_double_click
        );

        void draw (
            const canvas& c
        ) const;

    private:
        void recompute_tabs (
        );
        /*!
            ensures
                - recomputes the rectangles for all the tabs and makes this object
                  wider if needed
        !*/

        void draw_tab (
            const rectangle& tab,
            const canvas& c
        ) const;
        /*!
            ensures
                - draws the outline of a tab as given by the rectangle onto c
        !*/

        struct tab_data
        {
            tab_data() : width(0), group(0) {}

            std::string name;
            unsigned long width;
            rectangle rect;
            widget_group* group;
        };

        unsigned long selected_tab_;

        array<tab_data>::kernel_2a_c tabs;
        const font& f;

        const long left_pad;
        const long right_pad;
        const long top_pad;
        const long bottom_pad;

        member_function_pointer<unsigned long,unsigned long>::kernel_1a event_handler;

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
                name == ""

            CONVENTION
                name_ == name()
        !*/

    public:

        named_rectangle(  
            drawable_window& w
        );

        virtual ~named_rectangle(
        );

        void set_size (
            unsigned long width,
            unsigned long height
        );

        void set_name (
            const std::string& name
        );

        const std::string name (
        ) const;

        void wrap_around (
            const rectangle& rect
        );

    protected:
        void draw (
            const canvas& c
        ) const;

    private:

        const font& f;
        std::string name_;

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

    public:

        mouse_tracker(  
            drawable_window& w
        ); 

        ~mouse_tracker(
        );

        void show (
        );

        void hide (
        );

        void enable (
        );

        void disable (
        );

        void set_pos (
            long x,
            long y
        );

    protected:

        void on_mouse_move (
            unsigned long state,
            long x,
            long y
        );

        void on_drag (
        );

        void draw (
            const canvas& c
        ) const;

        void on_mouse_down (
            unsigned long btn,
            unsigned long state,
            long x,
            long y,
            bool is_double_click
        );


    private:

        const long offset;
        named_rectangle nr;
        label x_label;
        label y_label; 
        std::ostringstream sout;

        long click_x, click_y;

        // restricted functions
        mouse_tracker(mouse_tracker&);        // copy constructor
        mouse_tracker& operator=(mouse_tracker&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // function message_box()  
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    namespace message_box_helper
    {
        class box_win : public drawable_window
        {
        public:
            box_win (
                const std::string& title_,
                const std::string& message_
            ) : 
                drawable_window(false),
                title(title_),
                message(message_),
                msg(*this),
                btn_ok(*this)
            {
                msg.set_pos(20,20);
                msg.set_text(message);
                rectangle msg_rect = msg.get_rect();

                btn_ok.set_name("OK");
                btn_ok.set_size(60,btn_ok.height());
                if (msg_rect.width() >= 60)
                    btn_ok.set_pos(msg_rect.width()/2+msg_rect.left()-btn_ok.width()/2,msg_rect.bottom()+15);
                else
                    btn_ok.set_pos(20,msg_rect.bottom()+15);
                btn_ok.set_click_handler(*this,&box_win::on_click);

                rectangle size = btn_ok.get_rect() + msg_rect;
                set_size(size.right()+20,size.bottom()+20);


                show();
                set_title(title_);
            }

            ~box_win (
            )
            {
               close_window();
            }

            template <
                typename T
                >
            void set_click_handler (
                T& object,
                void (T::*event_handler_)()
            )
            {
                auto_mutex M(wm);
                event_handler.set(object,event_handler_);
            }

        private:

            static void deleter_thread (
                void* param
            )
            {
                box_win& w = *reinterpret_cast<box_win*>(param);
                w.close_window();
                delete &w;
            }

            void on_click (
            )
            {
                if (event_handler.is_set())
                    event_handler();
                hide();
                create_new_thread(&deleter_thread,this);
            }

            on_close_return_code on_window_close (
            )
            {
                if (event_handler.is_set())
                    event_handler();
                delete this;
                return CLOSE_WINDOW;
            }

            const std::string title;
            const std::string message;
            label msg;
            button btn_ok;

            member_function_pointer<>::kernel_1a event_handler;
        };
    }

    template <
        typename T
        >
    void message_box (
        const std::string& title,
        const std::string& message,
        T& object,
        void (T::*event_handler)() 
    )
    {
        using namespace message_box_helper;
        box_win* win = new box_win(title,message);
        win->set_click_handler(object,event_handler);
    }

    inline void message_box (
        const std::string& title,
        const std::string& message
    )
    {
        using namespace message_box_helper;
        new box_win(title,message);
    }

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
                - ms_enabled == false
                - items.size() == 0
                - pos == 0
                - text_start = 0
                - last_selected = 0

            CONVENTION
                - size() == items.size()
                - (*this)[i] == items[i].name
                - is_selected(i) == items[i].is_selected

                - items[i].width == the width of items[i].name as given by font::compute_size() 
                - items[i].height == the height of items[i].name as given by font::compute_size() 

                - items[pos] == the item currently being displayed at the top of the list box
                - sbv == our vertical scroll bar
                - sbh == our horizontal scroll bar
                - text_area == the area that is free to be used for text display (e.g. not occluded 
                  by scroll bars or anything)
                - text_start == the amount of pixels the text should be shifted to the left (but the
                  part outside this widget should be clipped).  This is used by the horizontal 
                  scroll bar.
                - pos == the first line that is shown in the list box
                - last_selected == the last item the user selected
        !*/

    public:

        list_box(  
            drawable_window& w
        );

        ~list_box(
        );

        void set_size (
            unsigned long width_,
            unsigned long height_
        );

        void set_pos (
            long x,
            long y
        );

        bool is_selected (
            unsigned long index
        ) const;

        void select (
            unsigned long index 
        );

        void unselect (
            unsigned long index 
        );

        template <typename T>
        void get_selected (
            T& list
        ) const
        {
            auto_mutex M(m);
            list.clear();
            for (unsigned long i = 0; i < items.size(); ++i)
            {
                if (items[i].is_selected)
                {
                    unsigned long idx = i;
                    list.enqueue(idx);
                }
            }
        }

        template <typename T>
        void load (
            const T& list
        )
        {
            auto_mutex M(m);
            items.clear();
            unsigned long i = 0;
            items.set_max_size(list.size());
            items.set_size(list.size());
            list.reset();
            while (list.move_next())
            {
                items[i].is_selected = false;
                items[i].name = list.element();
                f.compute_size(items[i].name,items[i].width, items[i].height);
                ++i;
            }
            pos = 0;
            adjust_sliders();
            parent.invalidate_rectangle(rect);
            last_selected = 0;
        }

        const std::string& operator[] (
            unsigned long index
        ) const;

        bool multiple_select_enabled (
        ) const;

        void enable_multiple_select (
        ); 

        void disable_multiple_select (
        );

        template <
            typename T
            >
        void set_double_click_handler (
            T& object,
            void (T::*eh)(unsigned long index)
        ) { auto_mutex M(m); event_handler.set(object,eh); }

        bool at_start (
        ) const;

        void reset (
        ) const;

        bool current_element_valid (
        ) const;

        const std::string& element (
        ) const;

        const std::string& element (
        );

        bool move_next (
        ) const;

        unsigned long size (
        ) const;

        void show(
        );

        void hide (
        );

        void enable (
        );

        void disable (
        );

        void set_z_order (
            long order
        );

        unsigned long get_selected (
        ) const;

    private:

        void sbv_handler (
        );

        void sbh_handler (
        );

        void adjust_sliders (
        );
        /*!
            requires
                - m is locked
            ensures
                - adjusts the scroll bars so that they are properly displayed
        !*/

        void on_wheel_up (
        );

        void on_wheel_down (
        );

        void on_mouse_down (
            unsigned long btn,
            unsigned long state,
            long x,
            long y,
            bool is_double_click
        );

        void draw (
            const canvas& c
        ) const;

        struct data
        {
            std::string name;
            bool is_selected;
            unsigned long width;
            unsigned long height;
        };

        const static long pad = 2;

        bool ms_enabled;
        const font& f;
        array<data>::kernel_2a_c items;
        member_function_pointer<unsigned long>::kernel_1a event_handler;
        unsigned long pos;
        unsigned long text_start;
        unsigned long last_selected;
        scroll_bar sbv;
        scroll_bar sbh;
        rectangle text_area;


        // restricted functions
        list_box(list_box&);        // copy constructor
        list_box& operator=(list_box&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // function open_file_box() 
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    namespace open_file_box_helper
    {
        class box_win : public drawable_window
        {
        public:
            box_win (
            ) : 
                lbl_dirs(*this),
                lbl_files(*this),
                lb_dirs(*this),
                lb_files(*this),
                btn_open(*this),
                btn_cancel(*this),
                btn_root(*this)
            {

                cur_dir = -1;
                set_size(500,300);

                lbl_dirs.set_text("Directories:");
                lbl_files.set_text("Files:");
                btn_open.set_name("Open");
                btn_cancel.set_name("Cancel");
                btn_root.set_name("/");

                btn_root.set_click_handler(*this,&box_win::on_root_click);
                btn_cancel.set_click_handler(*this,&box_win::on_cancel_click);
                btn_open.set_click_handler(*this,&box_win::on_open_click);
                lb_dirs.set_double_click_handler(*this,&box_win::on_dirs_click);


                btn_root.set_pos(5,5);

                set_sizes();
                set_title("Open File");

                on_root_click();

                show();
            }

            ~box_win (
            )
            {
               close_window();
            }

            template <
                typename T
                >
            void set_click_handler (
                T& object,
                void (T::*event_handler_)(const std::string&)
            )
            {
                auto_mutex M(wm);
                event_handler.set(object,event_handler_);
            }

        private:

            void set_sizes(
            )
            {
                unsigned long width, height;
                get_size(width,height);

                lbl_dirs.set_pos(0,btn_root.bottom()+5);
                lb_dirs.set_pos(0,lbl_dirs.bottom());
                lb_dirs.set_size(width/2,height-lb_dirs.top()-btn_cancel.height()-10);

                lbl_files.set_pos(lb_dirs.right(),btn_root.bottom()+5);
                lb_files.set_pos(lb_dirs.right(),lbl_files.bottom());
                lb_files.set_size(width-lb_files.left(),height-lb_files.top()-btn_cancel.height()-10);

                btn_open.set_pos(width - btn_open.width()-25,lb_files.bottom()+5);
                btn_cancel.set_pos(btn_open.left() - btn_cancel.width()-5,lb_files.bottom()+5);

            }

            void on_window_resized (
            )
            {
                set_sizes();
            }

            void deleter_thread (
            ) 
            {  
                close_window();
                delete this; 
            }

            void on_dirs_click (
                unsigned long idx
            )
            {
                const std::string old_path = path;
                const long old_cur_dir = cur_dir;

                button* new_btn = new button(*this);
                new_btn->set_name(lb_dirs[idx]);
                new_btn->set_click_handler(*this,&box_win::on_path_button_click);

                // remove any path buttons that won't be part of the path anymore
                if (sob.size())
                {
                    while (sob.size() > (unsigned long)(cur_dir+1))
                    {
                        delete sob[cur_dir+1];
                        button* junk;
                        sob.remove(cur_dir+1,junk);
                    }
                }

                if (sob.size())
                    new_btn->set_pos(sob[sob.size()-1]->right()+5,sob[sob.size()-1]->top());
                else
                    new_btn->set_pos(btn_root.right()+5,btn_root.top());

                cur_dir = sob.size();
                sob.add(sob.size(),new_btn);

                path += lb_dirs[idx] + directory::get_separator() ;
                if (set_dir(prefix + path) == false)
                {
                    sob.remove(sob.size()-1,new_btn);
                    delete new_btn;
                    path = old_path;
                    cur_dir = old_cur_dir;
                }
            }

            void on_cancel_click (
            )
            {
                hide();
                create_new_thread<box_win,&box_win::deleter_thread>(*this);
            }

            void on_open_click (
            )
            {
                if (lb_files.get_selected() != lb_files.size())
                {
                    if (event_handler.is_set())
                        event_handler(prefix + path + lb_files[lb_files.get_selected()]);
                    hide();
                    create_new_thread<box_win,&box_win::deleter_thread>(*this);
                }
            }

            void on_path_button_click (
                button& btn
            )
            {
                std::string new_path;
                for (unsigned long i = 0; i < sob.size(); ++i)
                {
                    new_path += sob[i]->name() + directory::get_separator();
                    if (sob[i] == &btn)
                    {
                        cur_dir = i;
                        break;
                    }
                }
                if (path != new_path)
                {
                    path = new_path;
                    set_dir(prefix+path);
                }
            }

            bool set_dir (
                const std::string& dir
            )
            {
                try
                {
                    directory d(dir);
                    queue<directory>::kernel_1a_c qod;
                    queue<file>::kernel_1a_c qof;
                    queue<std::string>::sort_1a_c qos;
                    d.get_dirs(qod);
                    d.get_files(qof);

                    qod.reset();
                    while (qod.move_next())
                    {
                        std::string temp = qod.element().name();
                        qos.enqueue(temp);
                    }
                    qos.sort();
                    lb_dirs.load(qos);
                    qos.clear();

                    qof.reset();
                    while (qof.move_next())
                    {
                        std::string temp = qof.element().name();
                        qos.enqueue(temp);
                    }
                    qos.sort();
                    lb_files.load(qos);
                    return true;
                }
                catch (directory::listing_error& )
                {
                    return false;
                }
                catch (directory::dir_not_found&)
                {
                    return false;
                }
            }

            void on_root_click (
            )
            {
                queue<directory>::kernel_1a_c qod, qod2;
                queue<file>::kernel_1a_c qof;
                queue<std::string>::sort_1a_c qos;
                get_filesystem_roots(qod);
                path.clear();
                cur_dir = -1;
                if (qod.size() == 1)
                {
                    qod.current().get_files(qof);
                    qod.current().get_dirs(qod2);
                    prefix = qod.current().full_name();

                    qod2.reset();
                    while (qod2.move_next())
                    {
                        std::string temp = qod2.element().name();
                        qos.enqueue(temp);
                    }
                    qos.sort();
                    lb_dirs.load(qos);
                    qos.clear();

                    qof.reset();
                    while (qof.move_next())
                    {
                        std::string temp = qof.element().name();
                        qos.enqueue(temp);
                    }
                    qos.sort();
                    lb_files.load(qos);
                }
                else
                {
                    prefix.clear();
                    qod.reset();
                    while (qod.move_next())
                    {
                        std::string temp = qod.element().full_name();
                        temp = temp.substr(0,temp.size()-1);
                        qos.enqueue(temp);
                    }
                    qos.sort();
                    lb_dirs.load(qos);
                    qos.clear();
                    lb_files.load(qos);
                }
            }

            on_close_return_code on_window_close (
            )
            {
                delete this;
                return CLOSE_WINDOW;
            }

            label lbl_dirs;
            label lbl_files;
            list_box lb_dirs;
            list_box lb_files;
            button btn_open;
            button btn_cancel;
            button btn_root;
            std::string path;
            std::string prefix;
            int cur_dir;

            member_function_pointer<const std::string&>::kernel_1a event_handler;
            sequence<button*>::kernel_2a_c sob;
        };
    }

    template <
        typename T
        >
    void open_file_box (
        T& object,
        void (T::*event_handler)(const std::string&) 
    )
    {
        using namespace open_file_box_helper;
        box_win* win = new box_win();
        win->set_click_handler(object,event_handler);
    }

// ----------------------------------------------------------------------------------------

}

#ifdef NO_MAKEFILE
#include "widgets.cpp"
#endif

#endif // DLIB_WIDGETs_

