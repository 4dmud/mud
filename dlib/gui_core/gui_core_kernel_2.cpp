// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_GUI_CORE_KERNEL_2_CPp_
#define DLIB_GUI_CORE_KERNEL_2_CPp_
#include "../platform.h"

#ifdef POSIX

#include "gui_core_kernel_2.h"


void winmain(int argc, char** argv);

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <iostream>
#include "../assert.h"
#include "../queue.h"
#include <cstring>
#include <cmath>
#include <X11/Xatom.h>
#include "../sync_extension.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    namespace gui_core_kernel_2_globals
    {
        void event_handler ();
        void trigger_user_event_threadproc (void*);

        struct x11_base_windowstuff
        {
            Window hwnd;
            Time last_click_time;
        };

        typedef sync_extension<binary_search_tree<Window,base_window*>::kernel_1a>::kernel_1a 
            window_table_type;

        int depth;
        Display* disp;
        static Screen* screen;

        Atom delete_window; 
        static Window exit_window;
        static std::string clipboard;


        window_table_type window_table;

        // note that this is the thread that will perform all the event
        // processing.
        const thread_id_type event_thread_id = get_thread_id();

        struct user_event_type
        {
            Window w;
            void* p;
            int i;
        };

        typedef sync_extension<queue<user_event_type,memory_manager<char>::kernel_1b>::kernel_2a_c>::kernel_1a queue_of_user_events;
        queue_of_user_events user_events;
        queue_of_user_events user_events_temp;

    // ----------------------------------------------------------------------------------------

        Bool XCheckIfEventPredicate (
            Display* disp,
            XEvent* event,
            XPointer arg
        )
        /*!
            ensures
                - if (event is an Expose event for the window pointed to by arg) then
                    - returns true
                - else
                    - returns false
        !*/
        {
            if (event->type == Expose)
            {
                XExposeEvent* e = reinterpret_cast<XExposeEvent*>(event);
                Window* win= reinterpret_cast<Window*>(arg);
                if (e->window == *win)
                {
                    return 1;
                }
            }
            return 0;
        }

    // ----------------------------------------------------------------------------------------
    
        static bool map_keys (
            KeySym keycode,
            bool shift,
            bool caps,
            unsigned long& result,
            bool& is_printable
        )
        /*!
            requires
                - if (shift was down for this key) then
                    - shift == true
                - if (caps lock was on for this key) then
                    - caps == true
                - keycode == the keycode from windows that we are to process
                - keycode < keyboard_keys_size
            ensures
                - if (this key should be ignored) then
                    - returns false
                - else
                    - returns true
                    - #is_printable == true if result is a printable ascii character
                    - #result == the keycode converted into the proper number to tbe 
                      returned by the event handler.
        !*/
        {
            is_printable = true;
            if (keycode <= 'z' && keycode >= 'a' || 
                keycode <= 'Z' && keycode >= 'A' || 
                keycode <= '9' && keycode >= '0')
            {
                result = keycode;
            }
            else
            {
                is_printable = false;
                switch (keycode)
                {
                case XK_Home:   result = base_window::KEY_HOME; break;
                case XK_Left:   result = base_window::KEY_LEFT; break;
                case XK_Right:  result = base_window::KEY_RIGHT; break;
                case XK_Down:   result = base_window::KEY_DOWN; break;
                case XK_Up:     result = base_window::KEY_UP; break;
                case XK_Prior:  result = base_window::KEY_PAGE_UP; break;
                case XK_Next:   result = base_window::KEY_PAGE_DOWN;     break;
                case XK_End:    result = base_window::KEY_END; break;
                case XK_Escape:    result = base_window::KEY_ESC; break;
                
                case XK_KP_Delete:    result = base_window::KEY_DELETE; break;
                case XK_KP_Prior:    result = base_window::KEY_PAGE_UP; break;
                case XK_KP_Next:    result = base_window::KEY_PAGE_DOWN; break;


                case XK_F1:    result = base_window::KEY_F1; break;
                case XK_F2:    result = base_window::KEY_F2; break;
                case XK_F3:    result = base_window::KEY_F3; break;
                case XK_F4:    result = base_window::KEY_F4; break;
                case XK_F5:    result = base_window::KEY_F5; break;
                case XK_F6:    result = base_window::KEY_F6; break;
                case XK_F7:    result = base_window::KEY_F7; break;
                case XK_F8:    result = base_window::KEY_F8; break;
                case XK_F9:    result = base_window::KEY_F9; break;
                case XK_F10:    result = base_window::KEY_F10; break;
                case XK_F11:    result = base_window::KEY_F11; break;
                case XK_F12:    result = base_window::KEY_F12; break;
                    
                    
                case XK_Shift_L:    result = base_window::KEY_SHIFT; break;
                case XK_Shift_R:    result = base_window::KEY_SHIFT; break;
                case XK_Control_L:    result = base_window::KEY_CTRL; break;
                case XK_Control_R:    result = base_window::KEY_CTRL; break;
                case XK_Caps_Lock:    result = base_window::KEY_CAPS_LOCK; break;
                case XK_Alt_L:    result = base_window::KEY_ALT; break;
                case XK_Alt_R:    result = base_window::KEY_ALT; break;

                    
                case XK_BackSpace:    result = base_window::KEY_BACKSPACE; break;
                case XK_Delete:    result = base_window::KEY_DELETE; break;
                case XK_Scroll_Lock:    result = base_window::KEY_SCROLL_LOCK; break;
                case XK_Pause:    result = base_window::KEY_PAUSE; break;
                case XK_Insert:    result = base_window::KEY_INSERT; break;
                case XK_KP_Insert:    result = base_window::KEY_INSERT; break;




                case XK_exclam:    
                    is_printable = true;
                    result = '!'; break;
                case XK_quotedbl:    
                    is_printable = true;
                    result = '"'; break;
                case XK_numbersign:    
                    is_printable = true;
                    result = '#'; break;
                case XK_dollar:    
                    is_printable = true;
                    result = '$'; break;
                case XK_percent:    
                    is_printable = true;
                    result = '%'; break;
                case XK_ampersand:    
                    is_printable = true;
                    result = '&'; break;
                case XK_apostrophe:    
                    is_printable = true;
                    result = '\''; break;
                case XK_parenleft:    
                    is_printable = true;
                    result = '('; break;
                case XK_parenright:    
                    is_printable = true;
                    result = ')'; break;
                case XK_asterisk:    
                    is_printable = true;
                    result = '*'; break;
                case XK_plus:    
                    is_printable = true;
                    result = '+'; break;
                case XK_comma:    
                    is_printable = true;
                    result = ','; break;
                case XK_minus:    
                    is_printable = true;
                    result = '-'; break;
                case XK_period:    
                    is_printable = true;
                    result = '.'; break;
                case XK_slash:    
                    is_printable = true;
                    result = '/'; break;
                case XK_colon:    
                    is_printable = true;
                    result = ':'; break;
                case XK_semicolon:    
                    is_printable = true;
                    result = ';'; break;
                case XK_less:    
                    is_printable = true;
                    result = '<'; break;
                case XK_equal:    
                    is_printable = true;
                    result = '='; break;
                case XK_greater:    
                    is_printable = true;
                    result = '>'; break;
                case XK_question:    
                    is_printable = true;
                    result = '?'; break;
                case XK_at:    
                    is_printable = true;
                    result = '@'; break;
                case XK_grave:    
                    is_printable = true;
                    result = '`'; break;
                case XK_underscore:    
                    is_printable = true;
                    result = '_'; break;
                case XK_asciicircum:    
                    is_printable = true;
                    result = '^'; break;
                case XK_bracketleft:    
                    is_printable = true;
                    result = '['; break;
                case XK_backslash:    
                    is_printable = true;
                    result = '\\'; break;
                case XK_bracketright:    
                    is_printable = true;
                    result = ']'; break;
                case XK_asciitilde:    
                    is_printable = true;
                    result = '~'; break;
                case XK_braceleft:    
                    is_printable = true;
                    result = '{'; break;
                case XK_bar:    
                    is_printable = true;
                    result = '|'; break;
                case XK_braceright:    
                    is_printable = true;
                    result = '}'; break;
            



                case XK_space:    
                    is_printable = true;
                    result = ' '; break;
                case XK_Return:    
                    is_printable = true;
                    result = '\n'; break;
                case XK_Tab:    
                    is_printable = true;
                    result = '\t'; break;
                case XK_KP_Divide: 
                    is_printable = true;
                    result = '/'; break;
                case XK_KP_Decimal: 
                    is_printable = true;
                    result = '.'; break;
                case XK_KP_Subtract: 
                    is_printable = true;
                    result = '-'; break;
                case XK_KP_Add: 
                    is_printable = true;
                    result = '+'; break;
                case XK_KP_Multiply: 
                    is_printable = true;
                    result = '*'; break;
                case XK_KP_Equal: 
                    is_printable = true;
                    result = '='; break;

                case XK_KP_0: 
                    is_printable = true;
                    result = '0'; break;
                case XK_KP_1: 
                    is_printable = true;
                    result = '1'; break;
                case XK_KP_2: 
                    is_printable = true;
                    result = '2'; break;
                case XK_KP_3: 
                    is_printable = true;
                    result = '3'; break;
                case XK_KP_4: 
                    is_printable = true;
                    result = '4'; break;
                case XK_KP_5: 
                    is_printable = true;
                    result = '5'; break;
                case XK_KP_6: 
                    is_printable = true;
                    result = '6'; break;
                case XK_KP_7: 
                    is_printable = true;
                    result = '7'; break;
                case XK_KP_8: 
                    is_printable = true;
                    result = '8'; break;
                case XK_KP_9: 
                    is_printable = true;
                    result = '9'; break;

                default:
                    return false;
                }
            }

            return true;
        }

    // ----------------------------------------------------------------------------------------

        void event_handler (
        )
        /*!
            ensures
                - will handle all events and event dispatching            
        !*/
        {       
            try
            {
                bool quit_event_loop = false;
                while (quit_event_loop == false)
                {
                    XEvent ev;                
                    XNextEvent(disp,&ev);


                    // get a lock on the window_table's mutex
                    auto_mutex window_table_locker(window_table.get_mutex());

                    // if this event is for one of the windows in the window_table
                    // then get that window out of the table and put it into win.
                    XAnyEvent* _ae = reinterpret_cast<XAnyEvent*>(&ev);
                    base_window** win_ = window_table[_ae->window];
                    base_window* win = 0;
                    if (win_)
                        win = *win_;


                    // ignore messages for unmapped windows
                    if (ev.type != MapNotify && win != 0) 
                    {
                        if (win->is_mapped == false)
                           continue;
                    }


                    switch (ev.type)
                    {

                    case SelectionRequest:
                        {
                            XSelectionRequestEvent* req = reinterpret_cast<XSelectionRequestEvent*>(&ev.xselectionrequest);
                            XEvent respond;

                            if (req->target == XA_STRING)
                            {
                                XChangeProperty (disp,
                                                 req->requestor,
                                                 req->property,
                                                 XA_STRING,
                                                 8,
                                                 PropModeReplace,
                                                 reinterpret_cast<const unsigned char*>(clipboard.c_str()),
                                                 clipboard.size()+1);
                                respond.xselection.property=req->property;
                            }
                            else 
                            {
                                respond.xselection.property= None;
                            }
                            respond.xselection.type= SelectionNotify;
                            respond.xselection.display= req->display;
                            respond.xselection.requestor= req->requestor;
                            respond.xselection.selection=req->selection;
                            respond.xselection.target= req->target;
                            respond.xselection.time = req->time;
                            XSendEvent (disp, req->requestor,0,0,&respond);
                            XFlush (disp);

                        } break;

                    case MapNotify:
                        {
                            if (win == 0)
                                break;

                            win->is_mapped = true;

                            if (win->resizable == false)
                            {
                                XSizeHints* hints = XAllocSizeHints();
                                hints->flags = PMinSize|PMaxSize;
                                hints->min_width = win->width;
                                hints->max_width = win->width;
                                hints->max_height = win->height; 
                                hints->min_height = win->height; 
                                XSetNormalHints(gui_core_kernel_2_globals::disp,win->x11_stuff.hwnd,hints);
                                XFree(hints);
                            }

                            XResizeWindow(gui_core_kernel_2_globals::disp,win->x11_stuff.hwnd,win->width,win->height);
            
                            XMoveWindow(gui_core_kernel_2_globals::disp,win->x11_stuff.hwnd,win->x,win->y);
                            XFlush(gui_core_kernel_2_globals::disp);

                            if (win->has_been_resized)
                            {
                                win->has_been_resized = false;
                                win->on_window_resized();
                            }

                            if (win->has_been_moved)
                            {
                                win->has_been_moved = false;
                                win->on_window_moved();
                            }


                        } break;


                    case KeyPress:
                        {
                            XKeyPressedEvent* e = reinterpret_cast<XKeyPressedEvent*>(&ev);

                            if (win == 0)
                                break;

                            bool shift = ((e->state & ShiftMask)!=0);
                            bool ctrl = ((e->state & ControlMask)!=0);
                            bool caps = ((e->state & LockMask)!=0);

                            char buffer[2];
                            KeySym key;
                            XLookupString(e,buffer,2,&key,NULL);

                            bool is_printable;
                            unsigned long result;

                            if (map_keys(key,shift,caps,result,is_printable))
                            {
                                // signal the keyboard event
                                win->on_keydown(result,is_printable,shift,ctrl);
                            }
                            
                        } break;

                    case FocusIn:
                        {
                            if (win == 0)
                                break;

                            // signal the focus event 
                            win->on_focus_gained();
                        } break;

                    case FocusOut:
                        {
                            if (win == 0)
                                break;

                            // signal the focus event 
                            win->on_focus_lost();
                        } break;

                    case ButtonPress:
                    case ButtonRelease:
                        {
                            XButtonEvent* e = reinterpret_cast<XButtonEvent*>(&ev);

                            if (win == 0)
                                break;

                            unsigned long btn = base_window::NONE;
                            if (e->button == Button1)
                                btn = base_window::LEFT;
                            else if (e->button == Button3)
                                btn = base_window::RIGHT;
                            else if (e->button == Button2)
                                btn = base_window::MIDDLE;


                            // only send the event if this is a button we support
                            if (btn != (unsigned long)base_window::NONE)
                            {

                                unsigned long state = 0;
                                if (e->state & ControlMask)
                                    state |= base_window::CONTROL;
                                if (e->state & Button1Mask)
                                    state |= base_window::LEFT;
                                if (e->state & Button2Mask)
                                    state |= base_window::MIDDLE;
                                if (e->state & Button3Mask)
                                    state |= base_window::RIGHT;
                                if (e->state & ShiftMask)
                                    state |= base_window::SHIFT;

                                if (ev.type == ButtonPress)
                                {
                                    bool is_double_click = false;
                                    if (win->last_click_button == btn &&
                                        std::abs((long)win->last_click_x - (long)e->x) < 5 &&
                                        std::abs((long)win->last_click_y - (long)e->y) < 5 &&
                                        e->time - win->x11_stuff.last_click_time <= 400)
                                    {
                                        // this is a double click
                                        is_double_click = true;
                                        // set this to make sure the next click can't be
                                        // interpreted as a double click
                                        win->last_click_button = base_window::NONE;
                                    }
                                    else
                                    {
                                        win->last_click_button = btn;
                                        win->last_click_x = e->x;
                                        win->last_click_y = e->y;
                                        win->x11_stuff.last_click_time = e->time;
                                    }

                                    // remove the clicked button from the state
                                    state &= (~btn);
                                    win->on_mouse_down(btn,state,e->x,e->y,is_double_click);

                                }
                                else
                                {
                                    // remove the clicked button from the state
                                    state &= (~btn);
                                    win->on_mouse_up(btn,state,e->x,e->y);
                                }
                            }
                            else if (e->button == Button4 && ev.type == ButtonPress)
                            {
                                win->on_wheel_up();
                            }
                            else if (e->button == Button5 && ev.type == ButtonPress)
                            {
                                win->on_wheel_down();
                            }
                            
                        } break;
 
                    case LeaveNotify:
                        {
                            if (win == 0)
                                break;

                            win->on_mouse_leave();
                            
                        } break;

                    case EnterNotify:
                        {
                            if (win == 0)
                                break;

                            win->on_mouse_enter();
                        } break;

                    case MotionNotify:
                        {
                            XMotionEvent* e = reinterpret_cast<XMotionEvent*>(&ev);

                            if (win == 0)
                                break;

                            unsigned long state = 0;
                            if (e->state & ControlMask)
                                state |= base_window::CONTROL;
                            if (e->state & Button1Mask)
                                state |= base_window::LEFT;
                            if (e->state & Button2Mask)
                                state |= base_window::MIDDLE;
                            if (e->state & Button3Mask)
                                state |= base_window::RIGHT;
                            if (e->state & ShiftMask)
                                state |= base_window::SHIFT;

                            win->on_mouse_move(state,e->x,e->y);
                            
                        } break;

                    case ConfigureNotify:
                        {
                            XConfigureEvent* e = reinterpret_cast<XConfigureEvent*>(&ev);
                            if (e->window == exit_window)
                            {
                                // this is the signal to quit the event handler
                                quit_event_loop = true;
                                break;
                            }

                            if (win == 0)
                                break;

                            if (win->width != e->width ||
                                win->height != e->height ||
                                win->has_been_resized)
                            {
                                win->has_been_resized = false;
                                // this is a resize
                                win->width = e->width;
                                win->height = e->height;
                                win->on_window_resized();
                            }
                            if (win->x != e->x ||
                                win->y != e->y ||
                                win->has_been_moved)
                            {
                                win->has_been_moved = false;
                                // this is a move
                                win->x = e->x;
                                win->y = e->y;
                                win->on_window_moved();
                            }
                            
                        } break;

                    case ClientMessage:
                        {
                            XClientMessageEvent* e = reinterpret_cast<XClientMessageEvent*>(&ev);
                            if ((Atom)e->data.l[0] == delete_window)
                            {
                                if (win == 0)
                                    break;


                                if (win->on_window_close() == base_window::DO_NOT_CLOSE_WINDOW)
                                {
                                    ASSERT(win->has_been_destroyed == false,
                                        "\tYou called close_window() inside the on_window_close() event but" 
                                        << "\n\tthen returned DO_NOT_CLOSE_WINDOW.  You can do one or the other but not both."
                                        << "\n\tthis:     " << win 
                                        );
                                    // the client has decided not to close the window
                                    // after all
                                }
                                else
                                {                                
                                    if (window_table[e->window])
                                    {
                                        window_table.destroy(e->window);
                                        XDestroyWindow(disp,e->window);
                                        win->has_been_destroyed = true;
                                    }
                                    else
                                    {
                                        // in this case the window must have self destructed by
                                        // calling delete this;  so we don't have to do anything.
                                    }
                                }
                            }
                        } break;

                    case Expose:
                        {
                            XExposeEvent* e = reinterpret_cast<XExposeEvent*>(&ev);

                            if (win == 0)
                                break;

                            // take all the expose events for this window out
                            XEvent etemp;
                            int x = e->x;
                            int y = e->y;
                            int width = e->width;
                            int height = e->height;  



                            // What we are doing here with this loop is we are combining
                            // all of the Expose events for this window that are 
                            // currently in the queue.  This weird if is here to make
                            // sure that we only do this for when we are resizing the
                            // window.  For resizes the Expose area usually has x and
                            // y equal to zero.  We want to avoid repainting the entire
                            // window over and over during a resize.
                            if (x == 0 && y == 0 && width*height > 1500)
                            while (XCheckIfEvent(disp,&etemp,XCheckIfEventPredicate,reinterpret_cast<XPointer>(&(e->window))))
                            {
                                XExposeEvent* e2 = reinterpret_cast<XExposeEvent*>(&etemp);
                                if (e2->x < x)
                                {
                                    width += x - e2->x;
                                    x = e2->x;                                
                                }
                                if (e2->y < y)
                                {
                                    height += y - e2->y;
                                    y = e2->y;
                                }
                                if (e2->width + e2->x > width + x)
                                {
                                    width = e2->width + e2->x - x;
                                }
                                if (e2->height + e2->y > height + y)
                                {
                                    height = e2->height + e2->y - y;
                                }                                
                            }

                            // I'm not sure if this sort of thing can happen but
                            // if it does then just ignore this entire event.
                            if (width == 0 || height == 0)
                            {
                                break;
                            }

                            unsigned char* const bitmap = new unsigned char[width*height*4];
                            unsigned char* const end = bitmap + width*height*4;

                            unsigned char* temp;
                            canvas c(bitmap,x,y,x+width-1,y+height-1);


                            win->paint(c);

                            // the user might have called win->close_window() and if they did
                            // then just stop right here.  We don't want to paint the window.
                            if (win->has_been_destroyed)
                                break;

                            // if the color depth we are working with isn't 24bits then we need
                            // to transform our image into whatever it is supposed to be.
                            if (depth != 24)
                            {
                                // convert this image into an 8 bit image
                                unsigned int red_bits;
                                unsigned int green_bits;
                                unsigned int blue_bits;
                                if (depth != 16)
                                {
                                    unsigned int bits = depth/3;
                                    unsigned int extra = depth%3;
                                    red_bits = bits;
                                    green_bits = bits;
                                    blue_bits = bits;
                                    if (extra)
                                    {
                                        ++red_bits;
                                        --extra;
                                    }
                                    if (extra)
                                    {
                                        ++green_bits;
                                    }
                                }
                                else if (depth == 16)
                                {
                                    red_bits = 5;
                                    green_bits = 6;
                                    blue_bits = 5;
                                }

                                if (depth == 16) 
                                { 
                                    temp = bitmap;
                                    unsigned char *red, *green, *blue;
                                    while (temp != end)
                                    {
                                        blue = temp;
                                        ++temp;
                                        green = temp;
                                        ++temp;
                                        red = temp;
                                        ++temp;
                                        ++temp;

                                        const unsigned long r = static_cast<unsigned long>(*red)>>(8-red_bits);
                                        const unsigned long g = static_cast<unsigned long>(*green)>>(8-green_bits);
                                        const unsigned long b = static_cast<unsigned long>(*blue)>>(8-blue_bits);

                                        unsigned long color = (r<<(depth-red_bits))| (g<<(depth-red_bits-green_bits))| b;

                                        *blue  = (color>>0)&0xFF;
                                        *green = (color>>8)&0xFF;
                                    }
                                }
                                else if (depth < 24)
                                {
                                    temp = bitmap;
                                    unsigned char *red, *green, *blue;
                                    while (temp != end)
                                    {
                                        blue = temp;
                                        ++temp;
                                        green = temp;
                                        ++temp;
                                        red = temp;
                                        ++temp;
                                        ++temp;

                                        const unsigned long r = static_cast<unsigned long>(*red)>>(8-red_bits);
                                        const unsigned long g = static_cast<unsigned long>(*green)>>(8-green_bits);
                                        const unsigned long b = static_cast<unsigned long>(*blue)>>(8-blue_bits);

                                        unsigned long color = (b<<(depth-blue_bits))| (g<<(depth-blue_bits-green_bits))| r;

                                        *blue  = (color>>0)&0xFF;
                                        *green = (color>>8)&0xFF;
                                        *red   = (color>>16)&0xFF;
                                    }
                                }
                                else if (depth > 24)
                                {
                                    temp = bitmap;
                                    unsigned char *red, *green, *blue, *four;
                                    while (temp != end)
                                    {
                                        blue = temp;
                                        ++temp;
                                        green = temp;
                                        ++temp;
                                        red = temp;
                                        ++temp;
                                        four = temp;
                                        ++temp;

                                        const unsigned long r = static_cast<unsigned long>(*red)<<(red_bits-8);
                                        const unsigned long g = static_cast<unsigned long>(*green)<<(green_bits-8);
                                        const unsigned long b = static_cast<unsigned long>(*blue)<<(blue_bits-8);

                                        unsigned long color = (b<<(depth-blue_bits))| (g<<(depth-blue_bits-green_bits))| r;

                                        *blue  = (color>>0)&0xFF;
                                        *green = (color>>8)&0xFF;
                                        *red   = (color>>16)&0xFF;
                                        *four  = (color>>24)&0xFF;
                                    }
                                }
                            } // if (depth != 24)



                            XImage img;
                            memset(&img,0,sizeof(img));
                            img.width = width;
                            img.height = height;
                            img.depth = depth;
                            img.data = reinterpret_cast<char*>(bitmap);
                            img.bitmap_bit_order = LSBFirst;
                            img.byte_order = LSBFirst;
                            img.format = ZPixmap;
                            img.bitmap_pad = 32;
                            img.bitmap_unit = 32;
                            img.bits_per_pixel = 32;


                            XInitImage(&img);

                            GC gc = XCreateGC(disp, e->window, 0, NULL);

                            XPutImage(disp,e->window,gc,&img,0,0,x,y,width,height);

                            XFreeGC(disp,gc);
                        } break;
                    } // switch (ev.type)
                }
            }
            catch (std::exception& e)
            {
                std::cout << "Exception thrown: " << e.what() << std::endl;
                abort();
            }
            catch (...)
            {
                std::cout << "Unknown exception thrown in event handler." << std::endl;
                abort();
            }
        }
 
    // ----------------------------------------------------------------------------------------
        
        struct winmain_helper_args
        {
            int argc;
            char** argv;
        };

        static void winmain_helper (
            void* args
        )
        {
            winmain_helper_args& a = *reinterpret_cast<winmain_helper_args*>(args);

            try
            {
                winmain(a.argc,a.argv);
            }
            catch (std::exception& e)
            {
                delete &a;
                std::cout << "Exception thrown in winmain(): " << e.what() << std::endl;
                end_program();
                abort();
            }
            catch (...)
            {
                delete &a;
                std::cout << "Unknown Exception thrown in winmain()." << std::endl;
                end_program();
                abort();
            }
            delete &a;
        }


    }

// ----------------------------------------------------------------------------------------

    void canvas::
    fill (
        unsigned char red_,
        unsigned char green_,
        unsigned char blue_
    ) const
    {
        const unsigned long red = red_;
        const unsigned long green = green_;
        const unsigned long blue = blue_;

        unsigned long* start = reinterpret_cast<unsigned long*>(bits);
        unsigned long* end = start + width_*height_;

        unsigned long pixel_value = 1;

        if (*reinterpret_cast<unsigned char*>(&pixel_value) == 1)
        {
            // we are on a little endian computer
            pixel_value = blue | (green<<8) | (red<<16);
        }
        else
        {
            // we are on a big endian computer
            pixel_value = (blue<<24) | (green<<16) | (red<<8);
        }

        while (start != end)
        {
            *start = pixel_value;
            ++start;
        }
    }

// ----------------------------------------------------------------------------------------

    void put_on_clipboard (
        const std::string& str
    )
    {
        using namespace gui_core_kernel_2_globals;
        auto_mutex M(window_table.get_mutex());
        clipboard = str;
        clipboard[0] = clipboard[0];

        XSetSelectionOwner(disp,XA_PRIMARY,exit_window,CurrentTime);
    }

// ----------------------------------------------------------------------------------------

    Bool clip_peek_helper (
        Display *display,
        XEvent *event,
        XPointer arg
    )
    {
        if ( event->type == SelectionNotify)
        {
            return True;
        }
        else
        {
            return False;
        }
    }

    void get_from_clipboard (
        std::string& str
    )
    {
        using namespace gui_core_kernel_2_globals;
        auto_mutex M(window_table.get_mutex());
        str.clear();
        unsigned char *data = 0;
        Window sown;
        Atom  type;
        int format, result;
        unsigned long len, bytes_left, dummy;
        XEvent e;

        try
        {
            sown = XGetSelectionOwner (disp, XA_PRIMARY);
            if (sown == exit_window)
            {
                // if we are copying from ourselfs then don't fool with the Xwindows junk.
                str = clipboard;
                str[0] = str[0];
            }
            else if (sown != None)
            {
                // request that the selection be copied into the XA_PRIMARY property
                // of the exit_window.  It doesn't matter what window we put it in 
                // so long as it is one under the control of this process and exit_window
                // is easy to use here so that is what I'm using.
                XConvertSelection (disp, XA_PRIMARY, XA_STRING, XA_PRIMARY,
                                   exit_window, CurrentTime);

                // This will wait until we get a SelectionNotify event which should happen
                // really soon.
                XPeekIfEvent(disp,&e,clip_peek_helper,0);

                // See how much data we got
                XGetWindowProperty (disp, exit_window, 
                                    XA_PRIMARY,    // Tricky..
                                    0, 0,         // offset - len
                                    0,        // Delete 0==FALSE
                                    AnyPropertyType,  //flag
                                    &type,        // return type
                                    &format,      // return format
                                    &len, &bytes_left,  //that 
                                    &data);             
                if (data)
                {
                    XFree(data);
                    data = 0;
                }
                if (bytes_left > 0 && type == XA_STRING)
                {
                    result = XGetWindowProperty (disp, exit_window, 
                                                 XA_PRIMARY, 0,bytes_left,0,
                                                 AnyPropertyType, &type,&format,
                                                 &len, &dummy, &data);
                    if (result == Success && type == XA_STRING)
                    {
                        str = (char*)data;
                    }
                    if (data)
                    {
                        XFree(data);
                        data = 0;
                    }
                }
            }
        }
        catch (...)
        {
            if (data) 
                XFree(data);
        }
    }

// ----------------------------------------------------------------------------------------

    void end_program (
    )
    {   
        using namespace gui_core_kernel_2_globals;
        XConfigureEvent event;
        event.type = ConfigureNotify;
        event.send_event = True;
        event.display = disp;
        event.window = exit_window;
        event.x = 1;
        XFlush(disp);
        XPutBackEvent(disp,reinterpret_cast<XEvent*>(&event));
        XFlush(disp);

        // Do this if we aren't in the event thread.  This should cause 
        // XNextEvent() to unblock so that it will see this ConfigureNotify 
        // event we are putting onto the event queue.
        if (get_main_thread_id() != get_thread_id())
        {
            XSendEvent(disp,exit_window,False,0,reinterpret_cast<XEvent*>(&event));
            XFlush(disp);
        }
    }

// ----------------------------------------------------------------------------------------

    namespace gui_core_kernel_2_globals
    {
        void trigger_user_event_threadproc (
            void*
        )
        {
            auto_mutex M(window_table.get_mutex());

            user_events.lock();
            user_events.swap(user_events_temp);
            user_events.unlock();


            user_events_temp.reset();
            // now dispatch all these user events
            while (user_events_temp.move_next())
            {
                base_window** win_ = window_table[user_events_temp.element().w];
                base_window* win;
                // if this window exists in the window table then dispatch
                // its event.
                if (win_)
                {
                    win = *win_;
                    win->on_user_event(
                        user_events_temp.element().p,
                        user_events_temp.element().i
                    );
                }
            }
            user_events_temp.clear();
        }
    }

    void base_window::
    trigger_user_event (
        void* p,
        int i
    )
    {
        using namespace gui_core_kernel_2_globals;
        user_event_type e;
        e.w = x11_stuff.hwnd;
        e.p = p;
        e.i = i;
        {
            auto_mutex M(user_events.get_mutex());
            user_events.enqueue(e);

            // we only need to start a thread to deal with this if there isn't already
            // one out working on the queue
            if (user_events.size() == 1)
                create_new_thread (trigger_user_event_threadproc,0);
        }
    }

// ----------------------------------------------------------------------------------------

    base_window::
    base_window (
        bool resizable_,
        bool undecorated
    ) :
        x11_stuff(*(new gui_core_kernel_2_globals::x11_base_windowstuff)),
        is_mapped(false),
        resizable(resizable_),
        has_been_destroyed(false),
        has_been_resized(false),
        has_been_moved(false),
        wm(gui_core_kernel_2_globals::window_table.get_mutex())
    {
        ASSERT(!(undecorated == true && resizable_ == true),
            "\tbase_window::base_window()"
            << "\n\tThere is no such thing as an undecorated window that is resizable by the user."
            << "\n\tthis:     " << this
            );

        auto_mutex M(wm);
        
        x11_stuff.last_click_time = 0;
        last_click_x = 0;
        last_click_y = 0;
        last_click_button = NONE;

        XSetWindowAttributes attr;
        memset(&attr,'\0',sizeof(attr));

        unsigned long valuemask = 0;
        if (undecorated)
        {
            attr.override_redirect = True;
            valuemask = CWOverrideRedirect;
        }

        x11_stuff.hwnd = XCreateWindow(
                        gui_core_kernel_2_globals::disp,
                        DefaultRootWindow(gui_core_kernel_2_globals::disp),
                        0,
                        0,
                        10,  // this is the default width of a window
                        10,  // this is the default width of a window
                        0,
                        gui_core_kernel_2_globals::depth,
                        InputOutput,
                        CopyFromParent,
                        valuemask,
                        &attr
                        );

        Window temp = x11_stuff.hwnd;
        base_window* ttemp = this;
        gui_core_kernel_2_globals::window_table.add(temp,ttemp);
        
        XSelectInput(
            gui_core_kernel_2_globals::disp,
            x11_stuff.hwnd,
            StructureNotifyMask|ExposureMask|ButtonPressMask|ButtonReleaseMask|
            PointerMotionMask|LeaveWindowMask|EnterWindowMask|KeyPressMask|
            KeyReleaseMask| FocusChangeMask
            );

        XSetWMProtocols(
            gui_core_kernel_2_globals::disp,
            x11_stuff.hwnd,
            &gui_core_kernel_2_globals::delete_window,
            1
            );


        // these are just default values
        x = 0;
        y = 0;
        width = 10;
        height = 10;

        if (resizable == false)
        {
            XSizeHints* hints = XAllocSizeHints();
            hints->flags = PMinSize|PMaxSize;
            hints->min_width = width;
            hints->max_width = width;
            hints->max_height = height; 
            hints->min_height = height; 
            XSetNormalHints(gui_core_kernel_2_globals::disp,x11_stuff.hwnd,hints);
            XFree(hints);
        }
    }

// ----------------------------------------------------------------------------------------

    base_window::
    ~base_window (
    )
    {
        ASSERT(is_closed() == true,
            "\tbase_window::~base_window"
            << "\n\tYou must call close_window() in your window object's destructor."
            << "\n\tthis:     " << this
            );
        // do this just for good measure (we might be in release mode so the above 
        // ASSERT wouldn't fire)
        close_window();
        delete &x11_stuff;
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    close_window (
    )
    {
        auto_mutex M(wm);
        if (has_been_destroyed == false)
        {
            has_been_destroyed = true;

            gui_core_kernel_2_globals::window_table.destroy(x11_stuff.hwnd);           

            XDestroyWindow(gui_core_kernel_2_globals::disp,x11_stuff.hwnd);
            x11_stuff.hwnd = 0;
        }   
    }

// ----------------------------------------------------------------------------------------

    bool base_window::
    is_closed (
    ) const
    {
        auto_mutex M(wm);
        return has_been_destroyed;
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    set_title (
        const std::string& title_
    )
    {
        ASSERT(is_closed() == false,
            "\tvoid base_window::set_title"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );
        // I'm pretty sure the pointer won't be modified even though
        // it isn't const anymore.
        char *title = const_cast<char*>(title_.c_str());
        XTextProperty property;
        XStringListToTextProperty(&title,1,&property);
        XSetWMName(gui_core_kernel_2_globals::disp,x11_stuff.hwnd,&property);
        XFree(property.value);
        XFlush(gui_core_kernel_2_globals::disp);
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    show (
    )    
    {
        ASSERT(is_closed() == false,
            "\tvoid base_window::show"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );
        XMapWindow(gui_core_kernel_2_globals::disp,x11_stuff.hwnd);
        XFlush(gui_core_kernel_2_globals::disp);
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    hide (
    )    
    {
        ASSERT(is_closed() == false,
            "\tvoid base_window::hide"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );
        XUnmapWindow(gui_core_kernel_2_globals::disp,x11_stuff.hwnd);
        XFlush(gui_core_kernel_2_globals::disp);
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    set_size (
        int width_,
        int height_
    )
    {
        auto_mutex a(wm);
        ASSERT(is_closed() == false,
            "\tvoid base_window::set_size"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            << "\n\twidth:    " << width_ 
            << "\n\theight:   " << height_ 
            );

        // do some sanity checking on these values
        if (width_ < 1)
            width_ = 1;
        if (height_ < 1)
            height_ = 1;

        width = width_;
        height = height_;
        has_been_resized = true;

        if (resizable == false)
        {
            XSizeHints* hints = XAllocSizeHints();
            hints->flags = PMinSize|PMaxSize;
            hints->min_width = width;
            hints->max_width = width;
            hints->max_height = height; 
            hints->min_height = height; 
            XSetNormalHints(gui_core_kernel_2_globals::disp,x11_stuff.hwnd,hints);
            XFree(hints);
        }

        XResizeWindow(gui_core_kernel_2_globals::disp,x11_stuff.hwnd,width,height);
        
        XFlush(gui_core_kernel_2_globals::disp);
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    set_pos (
        long x_,
        long y_
    )
    {
        auto_mutex a(wm);
        ASSERT(is_closed() == false,
            "\tvoid base_window::set_pos"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            << "\n\tx:        " << x_ 
            << "\n\ty:        " << y_ 
            );

        x = x_;
        y = y_;

        has_been_moved = true;

        XMoveWindow(gui_core_kernel_2_globals::disp,x11_stuff.hwnd,x,y);
        XFlush(gui_core_kernel_2_globals::disp);
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    get_pos (
        long& x_,
        long& y_
    )
    {
        auto_mutex a(wm);
        ASSERT(is_closed() == false,
            "\tvoid base_window::get_pos"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );

        // we can't really trust the values we have for x and y because some window managers
        // will have reported bogus values back in the ConfigureNotify event.  So just to be
        // on the safe side we will use XTranslateCoordinates() 
        int rx, ry;
        Window desktop_window = DefaultRootWindow(gui_core_kernel_2_globals::disp);
        Window junk;
        XTranslateCoordinates(gui_core_kernel_2_globals::disp,x11_stuff.hwnd,desktop_window,0,0,&rx, &ry, &junk);
        x_ = rx;
        y_ = ry;
        x = rx;
        y = ry;
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    get_size (
        unsigned long& width_,
        unsigned long& height_
    ) const
    {
        auto_mutex M(wm);
        ASSERT(is_closed() == false,
            "\tvoid base_window::get_size"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );

        width_ = width;
        height_ = height;
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    invalidate_rectangle (
        const rectangle& rect
    )
    {
        auto_mutex a(wm);
        if (is_mapped == false)
            return;

        if (rect.is_empty() == false && !has_been_destroyed)
        {
            const long x = rect.left();
            const long y = rect.top();
            const unsigned long width = rect.width();
            const unsigned long height = rect.height();
            
            XClearArea(gui_core_kernel_2_globals::disp,x11_stuff.hwnd,x,y,width,height,1);
            XFlush(gui_core_kernel_2_globals::disp);
        }
    }

}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // the real WinMain function
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

int main (int argc, char** argv)
{
    using namespace std;
    using namespace dlib;
    using namespace gui_core_kernel_2_globals;
    try
    {
        if (XInitThreads() == 0)
        {
            cout << "Unable to initialize threading support.\n";
            return 1;
        }


        disp = XOpenDisplay(NULL);
        if (disp == 0)
        {
            cout << "Unable to connect to the X display.\n";
            return 1;
        }

        screen = DefaultScreenOfDisplay(disp);
        depth = DefaultDepthOfScreen(screen);
        delete_window = XInternAtom(disp,"WM_DELETE_WINDOW",1); 


        // make this window just so we can send messages to it and trigger
        // events in the event thread
        XSetWindowAttributes attr;
        exit_window = XCreateWindow(
                        disp,
                        DefaultRootWindow(disp),
                        0,
                        0,
                        10,  // this is the default width of a window
                        10,  // this is the default width of a window
                        0,
                        depth,
                        InputOutput,
                        CopyFromParent,
                        0,
                        &attr
                        );




        // spawn a thread that will call the program entry point.
        winmain_helper_args* a = new winmain_helper_args;
        a->argc = argc;
        a->argv = argv;
        if (dlib::create_new_thread(winmain_helper,a) == false)
        {
            cout << "Unable to start needed threads.\n";
            return 1;
        }

        // start the event handler
        event_handler();
        
        XCloseDisplay(disp);

        return 0;
    }
    catch (std::exception& e)
    {
        cout << "\nEXCEPTION THROWN: \n" << e.what() << endl;
        abort();
    }
    catch (...)
    {
        cout << "UNKNOWN EXCEPTION THROWN.\n" << endl;
        abort();
    }
}


#endif // POSIX

#endif // DLIB_GUI_CORE_KERNEL_2_CPp_

