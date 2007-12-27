// Copyright (C) 2005  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_GUI_CORE_KERNEL_1_CPp_
#define DLIB_GUI_CORE_KERNEL_1_CPp_
#include "../platform.h"

#ifdef WIN32

#include "gui_core_kernel_1.h"


void winmain(int argc, char** argv);

#include <sstream>
#include "../threads.h"
#include "../assert.h"
#include "../queue.h"
#include "../sync_extension.h"
#include "../queue.h"

namespace dlib
{

// ----------------------------------------------------------------------------------------

    namespace gui_core_kernel_1_globals
    {

        static TCHAR window_class_name[] = TEXT ("w3049u6qc2d94thw9m34f4we0gvwa3-tgkser0-b9gm 05");
        static HINSTANCE hInstance;
        static HWND helper_window = NULL;    

        static bool quit_windows_loop = false;


        static bool set_window_title_done = true;
        static std::string window_title;
        static bool move_window_done = true;
        static HWND move_window_hwnd = NULL;
        static int move_window_width = 0;
        static int move_window_height = 0;
        static int move_window_x = 0;
        static int move_window_y = 0;
        static bool request_new_window = false;
        static DWORD dwStyle;
        static HWND new_window = NULL;
        // the window_table.get_mutex() mutex locks the above 11 variables


        window_table_type window_table;

        static rsignaler et_signaler(window_table.get_mutex());

        // note that this is the thread that will perform all the event
        // processing.
        const thread_id_type event_thread_id = get_thread_id();

        struct user_event_type
        {
            HWND w;
            void* p;
            int i;
        };

        typedef sync_extension<queue<user_event_type,memory_manager<char>::kernel_1b>::kernel_2a_c>::kernel_1a queue_of_user_events;
        queue_of_user_events user_events;

        enum USER_OFFSETS
        {
            CREATE_WINDOW,
            DESTROY_WINDOW,
            SET_ACTIVE_WINDOW,
            QUIT_PROGRAM,
            USER_EVENTS_READY,
            CALL_MOVE_WINDOW,
            SHOW_WINDOW_SHOW,
            SHOW_WINDOW_HIDE,
            CALL_SET_WINDOW_TITLE
        };

    // ----------------------------------------------------------------------------------------
        
        struct ebh_param
        {
            std::string text;
            std::string title;
        };

        static void error_box_helper(void* param)
        {
            ebh_param& p = *reinterpret_cast<ebh_param*>(param);

            MessageBox (NULL, TEXT (p.text.c_str()), 
                        p.title.c_str(), MB_OK|MB_ICONERROR|MB_SYSTEMMODAL 
                        ); 
            delete &p;
        }

        static void error_box (
            const char* title,
            const char* text,
            bool nonblocking = false
        )
        {
            try
            {
                if (nonblocking)
                {
                    ebh_param* param = new ebh_param;
                    param->text = text;
                    param->title = title;
                    dlib::create_new_thread(error_box_helper,param);
                }
                else
                {
                    MessageBox (NULL, TEXT (text), 
                                title, MB_OK|MB_ICONERROR|MB_SYSTEMMODAL 
                                ); 
                }
            }
            catch (...)
            {
                // we are totally screwed if this happens so just quit
                exit(0);
            }
        }

    // ----------------------------------------------------------------------------------------

        static bool map_keys (
            unsigned long keycode,
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

            if (keycode <= '9' && keycode >= '0')
            {
                result = keycode;
                if (shift)
                {
                    switch (result)
                    {
                    case '0': result = ')'; break;
                    case '1': result = '!'; break;
                    case '2': result = '@'; break;
                    case '3': result = '#'; break;
                    case '4': result = '$'; break;
                    case '5': result = '%'; break;
                    case '6': result = '^'; break;
                    case '7': result = '&'; break;
                    case '8': result = '*'; break;
                    case '9': result = '('; break;
                    }
                }
            }
            else if (keycode <= 'Z' && keycode >= 'A')
            {
                result = keycode;

                // make the result lower case if we need to.
                if (shift && caps || !caps && !shift)
                    result = result - 'A' + 'a';               
            }
            else
            {
                switch (keycode)
                {
                case VK_BACK:   
                    is_printable = false;
                    result = base_window::KEY_BACKSPACE; 
                    break;

                case VK_SHIFT:
                    is_printable = false;
                    result = base_window::KEY_SHIFT;
                    break;

                case VK_CONTROL:
                    is_printable = false;
                    result = base_window::KEY_CTRL;
                    break;

                case VK_MENU:
                    is_printable = false;
                    result = base_window::KEY_ALT;
                    break;

                case VK_PAUSE:
                    is_printable = false;
                    result = base_window::KEY_PAUSE;
                    break;

                case VK_CAPITAL:
                    is_printable = false;
                    result = base_window::KEY_CAPS_LOCK;
                    break;

                case VK_ESCAPE:
                    is_printable = false;
                    result = base_window::KEY_ESC;
                    break;

                case VK_PRIOR:
                    is_printable = false;
                    result = base_window::KEY_PAGE_UP;
                    break;

                case VK_NEXT:
                    is_printable = false;
                    result = base_window::KEY_PAGE_DOWN;
                    break;

                case VK_END:
                    is_printable = false;
                    result = base_window::KEY_END;
                    break;

                case VK_HOME:
                    is_printable = false;
                    result = base_window::KEY_HOME;
                    break;

                case VK_LEFT:
                    is_printable = false;
                    result = base_window::KEY_LEFT;
                    break;

                case VK_RIGHT:
                    is_printable = false;
                    result = base_window::KEY_RIGHT;
                    break;

                case VK_UP:
                    is_printable = false;
                    result = base_window::KEY_UP;
                    break;

                case VK_DOWN:
                    is_printable = false;
                    result = base_window::KEY_DOWN;
                    break;

                case VK_INSERT:
                    is_printable = false;
                    result = base_window::KEY_INSERT;
                    break;

                case VK_DELETE:
                    is_printable = false;
                    result = base_window::KEY_DELETE;
                    break;

                case 0x91:
                    is_printable = false;
                    result = base_window::KEY_SCROLL_LOCK;
                    break;

                case VK_F1:
                    is_printable = false;
                    result = base_window::KEY_F1;
                    break;

                case VK_F2:
                    is_printable = false;
                    result = base_window::KEY_F2;
                    break;

                case VK_F3:
                    is_printable = false;
                    result = base_window::KEY_F3;
                    break;

                case VK_F4:
                    is_printable = false;
                    result = base_window::KEY_F4;
                    break;

                case VK_F5:
                    is_printable = false;
                    result = base_window::KEY_F5;
                    break;

                case VK_F6:
                    is_printable = false;
                    result = base_window::KEY_F6;
                    break;

                case VK_F7:
                    is_printable = false;
                    result = base_window::KEY_F7;
                    break;

                case VK_F8:
                    is_printable = false;
                    result = base_window::KEY_F8;
                    break;

                case VK_F9:
                    is_printable = false;
                    result = base_window::KEY_F9;
                    break;

                case VK_F10:
                    is_printable = false;
                    result = base_window::KEY_F10;
                    break;

                case VK_F11:
                    is_printable = false;
                    result = base_window::KEY_F11;
                    break;

                case VK_F12:
                    is_printable = false;
                    result = base_window::KEY_F12;
                    break;
      

                case VK_SPACE:  result = ' ';  break;                
                case VK_TAB:    result = '\t'; break;
                case VK_RETURN: result = '\n'; break;
                case VK_NUMPAD0:  result = '0';  break;
                case VK_NUMPAD1:  result = '1';  break;
                case VK_NUMPAD2:  result = '2';  break;
                case VK_NUMPAD3:  result = '3';  break;
                case VK_NUMPAD4:  result = '4';  break;
                case VK_NUMPAD5:  result = '5';  break;
                case VK_NUMPAD6:  result = '6';  break;
                case VK_NUMPAD7:  result = '7';  break;
                case VK_NUMPAD8:  result = '8';  break;
                case VK_NUMPAD9:  result = '9';  break;

                case VK_MULTIPLY:   result = '*';  break;
                case VK_ADD:        result = '+';  break;
                case VK_SUBTRACT:   result = '-';  break;
                case VK_DECIMAL:    result = '.';  break;
                case VK_DIVIDE:     result = '/';  break;

                case VK_OEM_1:
                    if (shift)  result = ':';
                    else        result = ';';
                    break;

                case VK_OEM_PLUS:
                    if (shift)  result = '+';
                    else        result = '=';
                    break;

                case VK_OEM_COMMA:
                    if (shift)  result = '<';
                    else        result = ',';
                    break;

                case VK_OEM_MINUS:
                    if (shift)  result = '_';
                    else        result = '-';
                    break;

                case VK_OEM_PERIOD:
                    if (shift)  result = '>';
                    else        result = '.';
                    break;

                case VK_OEM_2:
                    if (shift)  result = '?';
                    else        result = '/';
                    break;

                case VK_OEM_3:
                    if (shift)  result = '~';
                    else        result = '`';
                    break;

                case VK_OEM_4:
                    if (shift)  result = '{';
                    else        result = '[';
                    break;

                case VK_OEM_5:
                    if (shift)  result = '|';
                    else        result = '\\';
                    break;

                case VK_OEM_6:
                    if (shift)  result = '}';
                    else        result = ']';
                    break;

                case VK_OEM_7:
                    if (shift)  result = '"';
                    else        result = '\'';
                    break;

                default:
                    return false;
                }
            }

            return true;
        }

    // ------------------------------------------------------------------------------------

        LRESULT CALLBACK WndProc (  
            HWND hwnd, 
            UINT message, 
            WPARAM wParam, 
            LPARAM lParam
        )
        {        
            using namespace gui_core_kernel_1_globals;
            queue_of_user_events user_events_temp;
            // Make the event processing thread have a priority slightly above normal.
            // This makes the GUI smother if you do heavy processing in other threads.
            HANDLE hand = OpenThread(THREAD_ALL_ACCESS,FALSE,GetCurrentThreadId());
            SetThreadPriority(hand,THREAD_PRIORITY_ABOVE_NORMAL);
            CloseHandle(hand);

            auto_mutex M(window_table.get_mutex());

            try
            {
                bool is_double = false;
                unsigned long btn = base_window::NONE;

                switch (message)
                {
                    case WM_USER+QUIT_PROGRAM:
                        if (hwnd == helper_window)
                        {                            
                            quit_windows_loop = true;
                            PostQuitMessage(0); 
                        }
                        return 0;

                    case WM_USER+DESTROY_WINDOW:
                        if (hwnd == helper_window)
                        {                            
                            DestroyWindow((HWND)wParam);
                        }
                        return 0;

                    case WM_USER+CALL_MOVE_WINDOW:
                        if (hwnd == helper_window)
                        {
                            MoveWindow(
                                move_window_hwnd,
                                move_window_x,
                                move_window_y,
                                move_window_width,
                                move_window_height,
                                TRUE);
                            move_window_done = true;
                            et_signaler.broadcast();
                        }
                        return 0;

                    case WM_USER+USER_EVENTS_READY:
                        if (hwnd == helper_window)
                        {
                            // this is the signal to look in the user_events queue 
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
                        return 0;

                    case WM_USER+SET_ACTIVE_WINDOW:
                        if (hwnd == helper_window)
                        {                            
                            SetActiveWindow((HWND)wParam);
                        }
                        return 0;

                    case WM_USER+SHOW_WINDOW_SHOW:
                        if (hwnd == helper_window)
                        {                            
                            ShowWindow((HWND)wParam,SW_SHOW);
                        }
                        return 0;

                    case WM_USER+SHOW_WINDOW_HIDE:
                        if (hwnd == helper_window)
                        {                            
                            ShowWindow((HWND)wParam,SW_HIDE);
                        }
                        return 0;

                    case WM_USER+CALL_SET_WINDOW_TITLE:
                        if (hwnd == helper_window)
                        {                            
                            SetWindowText((HWND)wParam,window_title.c_str());
                            set_window_title_done = true;
                            et_signaler.broadcast();
                        }
                        return 0;


                    case WM_USER+CREATE_WINDOW:
                        if (hwnd == helper_window)
                        {                 

                            // if this is stupposed to be a popup window then do the popup window thing
                            if (dwStyle == WS_CHILD)
                            {
                                new_window = CreateWindowEx (WS_EX_TOOLWINDOW|WS_EX_TOPMOST,window_class_name, "",
                                    dwStyle,
                                    CW_USEDEFAULT, CW_USEDEFAULT,
                                    CW_USEDEFAULT, CW_USEDEFAULT,
                                    helper_window, NULL, hInstance, NULL);
                                SetParent(new_window,NULL);
                            }
                            else
                            {
                                new_window = CreateWindow (window_class_name, "",
                                    dwStyle,
                                    CW_USEDEFAULT, CW_USEDEFAULT,
                                    CW_USEDEFAULT, CW_USEDEFAULT,
                                    NULL, NULL, hInstance, NULL);
                            }
                            // use the helper_window to indicate that CreateWindow failed
                            if (new_window == NULL)
                                new_window = helper_window;
                            et_signaler.broadcast();
                        }
                        return 0;

                    case WM_SYSKEYDOWN:
                    case WM_KEYDOWN:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;


                            bool shift = ((GetKeyState(VK_SHIFT)&0x8000)!=0);
                            bool ctrl = ((GetKeyState(VK_CONTROL)&0x8000)!=0);
                            bool caps = ((GetKeyState(VK_CAPITAL)&0x0001)!=0);

                            bool is_printable;
                            unsigned long result;

                            if (map_keys(wParam,shift,caps,result,is_printable))
                            {
                                // signal the keyboard event
                                win->on_keydown(result,is_printable,shift,ctrl);
                            }
                           
                        }
                        break;

                    case WM_SETFOCUS:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;

                            // signal that the window is gaining focus 
                            win->on_focus_gained();
                        }
                        break;

                    case WM_KILLFOCUS:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;

                            // signal that the window is gaining focus 
                            win->on_focus_lost();
                        }
                        break;

                    case WM_SIZE:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;


                            // signal that the window has been resized
                            win->on_window_resized();
                           
                        }
                        return 0;

                    case WM_MOVE:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;


                            // signal that the window has moved 
                            win->on_window_moved();
                           
                        }
                        return 0;

                    case WM_MOUSELEAVE:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;


                            // signal that the mouse has left the window
                            if (win->mouse_in)
                            {
                                win->on_mouse_leave();
                                win->mouse_in = false;
                            }
                           
                        }
                        return 0;

                    case WM_MOUSEWHEEL:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;


                            // signal the mouse wheel event
                            if (GET_WHEEL_DELTA_WPARAM(wParam) > 0)
                            {
                                win->on_wheel_up();                                
                            }
                            else
                            {
                                win->on_wheel_down();
                            }
                           
                        }
                        return 0;

                    case WM_LBUTTONUP:
                        btn = base_window::LEFT;
                    case WM_MBUTTONUP:
                        if (btn == base_window::NONE)
                            btn = base_window::MIDDLE;
                    case WM_RBUTTONUP:
                        if (btn == base_window::NONE)
                            btn = base_window::RIGHT;
                        {        
                            // release the mouse capture if the user isn't holding any
                            // other mouse buttons
                            if (!((wParam & MK_LBUTTON) | (wParam & MK_MBUTTON) | (wParam & MK_RBUTTON)))
                                ReleaseCapture();

                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;

                            unsigned long state = 0;
                            if (wParam & MK_CONTROL)
                                state |= base_window::CONTROL;
                            if (wParam & MK_LBUTTON)
                                state |= base_window::LEFT;
                            if (wParam & MK_MBUTTON)
                                state |= base_window::MIDDLE;
                            if (wParam & MK_RBUTTON)
                                state |= base_window::RIGHT;
                            if (wParam & MK_SHIFT)
                                state |= base_window::SHIFT;
                            
                            // remove the clicked button from the state
                            state &= (~btn);

                            // signal the mouse click
                            win->on_mouse_up(btn,state,GET_X_LPARAM(lParam),GET_Y_LPARAM(lParam));
                           
                        }
                        return 0;

                    case WM_LBUTTONDBLCLK:
                        btn = base_window::LEFT;
                    case WM_MBUTTONDBLCLK:
                        if (btn == base_window::NONE)
                            btn = base_window::MIDDLE;
                    case WM_RBUTTONDBLCLK:
                        if (btn == base_window::NONE)
                            btn = base_window::RIGHT;
                        is_double = true;
                    case WM_LBUTTONDOWN:
                        if (btn == base_window::NONE)
                            btn = base_window::LEFT;
                    case WM_MBUTTONDOWN:
                        if (btn == base_window::NONE)
                            btn = base_window::MIDDLE;
                    case WM_RBUTTONDOWN:
                        if (btn == base_window::NONE)
                            btn = base_window::RIGHT;
                        {
                            SetCapture(hwnd);


                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;
                            
                            unsigned long state = 0;
                            if (wParam & MK_CONTROL)
                                state |= base_window::CONTROL;
                            if (wParam & MK_LBUTTON)
                                state |= base_window::LEFT;
                            if (wParam & MK_MBUTTON)
                                state |= base_window::MIDDLE;
                            if (wParam & MK_RBUTTON)
                                state |= base_window::RIGHT;
                            if (wParam & MK_SHIFT)
                                state |= base_window::SHIFT;

                            // remove the clicked button from the state
                            state &= (~btn);

                            // signal the mouse click
                            win->on_mouse_down(btn,state,GET_X_LPARAM(lParam),GET_Y_LPARAM(lParam),is_double);
                           
                        }
                        return 0;

                    case WM_MOUSEMOVE:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;
                            
                            unsigned long state = 0;
                            bool mouse_button_down = false;
                            if (wParam & MK_CONTROL)
                                state |= base_window::CONTROL;
                            if (wParam & MK_LBUTTON)
                            {
                                state |= base_window::LEFT;
                                mouse_button_down = true;
                            }
                            if (wParam & MK_MBUTTON)
                            {
                                mouse_button_down = true;
                                state |= base_window::MIDDLE;
                            }
                            if (wParam & MK_RBUTTON)
                            {
                                state |= base_window::RIGHT;
                                mouse_button_down = true;
                            }
                            if (wParam & MK_SHIFT)
                                state |= base_window::SHIFT;

                            // signal the mouse movement
                            win->on_mouse_move(state,GET_X_LPARAM(lParam),GET_Y_LPARAM(lParam));
                           

                            // The following block of code checks if the mouse is moving
                            // into or out of the window.
                            if (mouse_button_down == false)
                            {
                                // if there isn't any mouse button down then the fact that
                                // we are getting a mouse move message means it is in the
                                // window
                                if (win->mouse_in == false)
                                {
                                    win->on_mouse_enter();
                                    win->mouse_in = true;

                                    // set the tracker for the mouse
                                    TRACKMOUSEEVENT tm;
                                    tm.hwndTrack = hwnd;
                                    tm.cbSize = sizeof(tm);
                                    tm.dwFlags = TME_LEAVE;
                                    _TrackMouseEvent(&tm);
                                }
                            }
                            else if (win->mouse_in)
                            {
                                // check if the mouse is currently outside the window
                                const long mouse_x = GET_X_LPARAM(lParam);
                                const long mouse_y = GET_Y_LPARAM(lParam);
                                if (mouse_x < 0 || mouse_y < 0)
                                {
                                    // the mouse is not in the window
                                    win->mouse_in = false;
                                    win->on_mouse_leave();
                                }
                                else
                                {
                                    unsigned long width, height;
                                    win->get_size(width,height);  
                                    if (mouse_x >= static_cast<long>(width) || 
                                        mouse_y >= static_cast<long>(height))
                                    {
                                        // the mouse is not in the window
                                        win->mouse_in = false;
                                        win->on_mouse_leave();
                                    }
                                }
                            }
                            else if (win->mouse_in == false)
                            {
                                // at this point we know that the mouse is moving around
                                // with some of its buttons down.  So it might be outside the window.
                                // get the window size and see if the mouse is outside
                                // it.
                                const long mouse_x = GET_X_LPARAM(lParam);
                                const long mouse_y = GET_Y_LPARAM(lParam);
                                unsigned long width, height;
                                win->get_size(width,height);  
                                if (mouse_x < static_cast<long>(width) && 
                                    mouse_y < static_cast<long>(height) &&
                                    mouse_x >= 0 &&
                                    mouse_y >= 0)
                                {
                                    // The mouse has gone inside the window
                                    win->mouse_in = true;
                                    win->on_mouse_enter();

                                    // set the tracker for the mouse
                                    TRACKMOUSEEVENT tm;
                                    tm.hwndTrack = hwnd;
                                    tm.cbSize = sizeof(tm);
                                    tm.dwFlags = TME_LEAVE;
                                    _TrackMouseEvent(&tm);
                                }
                               
                            }


                        }
                        return 0;

                    case WM_PAINT :
                        {     

                            PAINTSTRUCT ps;
                            HDC   hdc = NULL;

                            hdc = BeginPaint (hwnd, &ps) ;

                            try
                            {
                                base_window** win_ = window_table[hwnd];
                                base_window* win;
                                if (win_)
                                    win = *win_;
                                else
                                    break;




                                LONG x = ps.rcPaint.left;
                                LONG y = ps.rcPaint.top;
                                LONG width = ps.rcPaint.right - x;
                                LONG height = ps.rcPaint.bottom - y;
                        
                                if (width != 0 && height != 0)
                                {

                                    BITMAPINFO bmap_info;
                                    bmap_info.bmiColors[0].rgbBlue = 0;
                                    bmap_info.bmiColors[0].rgbGreen = 0;
                                    bmap_info.bmiColors[0].rgbRed = 0;
                                    bmap_info.bmiColors[0].rgbReserved = 0;
                                    bmap_info.bmiHeader.biSize = sizeof(bmap_info.bmiHeader);
                                    bmap_info.bmiHeader.biWidth = width;
                                    bmap_info.bmiHeader.biHeight = -1*height;
                                    bmap_info.bmiHeader.biPlanes = 1;
                                    bmap_info.bmiHeader.biBitCount = 24;
                                    bmap_info.bmiHeader.biCompression = BI_RGB;
                                    bmap_info.bmiHeader.biSizeImage = 0;
                                    bmap_info.bmiHeader.biXPelsPerMeter = 0;
                                    bmap_info.bmiHeader.biYPelsPerMeter = 0;
                                    bmap_info.bmiHeader.biClrUsed = 0;
                                    bmap_info.bmiHeader.biClrImportant = 0;


                                    unsigned char* bitmap ;
                                    LONG size;
                                    LONG padding = 0;
                                    if ((width*3)%sizeof(LONG) != 0)
                                    {
                                        padding = sizeof(LONG) - (width*3)%sizeof(LONG);
                                        size = (width*3+padding)*height;
                                        bitmap = new unsigned char[size];                         
                                    }
                                    else
                                    {
                                        size = width*height*3;
                                        bitmap = new unsigned char[size];
                                    }

                                    canvas bits(bitmap,padding,x,y,x+width-1,y+height-1);



                                    win->paint(bits);


                                    
                                    SetDIBitsToDevice (
                                        hdc,
                                        ps.rcPaint.left,
                                        ps.rcPaint.top,
                                        width,
                                        height,
                                        0,
                                        0,
                                        0,
                                        height,
                                        bitmap,
                                        &bmap_info,
                                        DIB_RGB_COLORS
                                        );
                                }

                                EndPaint (hwnd, &ps) ;    

                            }
                            catch (...)
                            {
                                // make sure EndPaint is called even if an exception
                                // is thrown.
                                if (hdc != NULL)
                                    EndPaint (hwnd, &ps);    
                                throw;
                            }
                        }   
                        return 0 ;

                    case  WM_ERASEBKGND:
                        return 1;




                    case WM_CLOSE:
                        {
                            base_window** win_ = window_table[hwnd];
                            base_window* win;
                            if (win_)
                                win = *win_;
                            else
                                break;
                            
  
                            // signal that the window is being closed                                
                            if (win->on_window_close() == base_window::DO_NOT_CLOSE_WINDOW)
                            {
                                ASSERT(win->has_been_destroyed == false,
                                    "\tYou called close_window() inside the on_window_close() event but" 
                                    << "\n\tthen returned DO_NOT_CLOSE_WINDOW.  You can do one or the other but not both."
                                    << "\n\tthis:     " << win 
                                    );
                                // this happens if the on_window_close() callback
                                // tells us to ignore the close event.  
                                return 0;
                            }
                            else
                            {
                                if (window_table[hwnd])
                                {
                                    window_table.destroy(hwnd);
                                    win->has_been_destroyed = true;
                                }
                                else
                                {
                                    // in this case the window must have self destructed by
                                    // calling delete this;
                                    return 0;
                                }
                            }
                          
                        }
                        return DefWindowProc (hwnd, message, wParam, lParam);


                    default:
                        break;

                } // switch (message)

            
            }
            catch (std::exception& e)
            {
                error_box("Exception thrown in event handler",e.what());
                end_program();
            }
            catch (...)
            {
                error_box("Exception thrown in event handler","Unknown Exception type.");
                end_program();
            }

            return DefWindowProc (hwnd, message, wParam, lParam) ;

        }

    // ----------------------------------------------------------------------------------------

        void show_window (
            HWND hwnd
        )
        {
            using namespace gui_core_kernel_1_globals;
            PostMessage(helper_window,WM_USER+SHOW_WINDOW_SHOW,(WPARAM)hwnd,0);
        }

    // ----------------------------------------------------------------------------------------

        void hide_window (
            HWND hwnd
        )
        {
            using namespace gui_core_kernel_1_globals;
            PostMessage(helper_window,WM_USER+SHOW_WINDOW_HIDE,(WPARAM)hwnd,0);
        }

    // ----------------------------------------------------------------------------------------

        void give_window_focus (
            HWND hwnd
        )
        {
            using namespace gui_core_kernel_1_globals;
            PostMessage(helper_window,WM_USER+SET_ACTIVE_WINDOW,(WPARAM)hwnd,0);
        }

    // ----------------------------------------------------------------------------------------

        void destroy_window (
            HWND hwnd
        )
        {
            using namespace gui_core_kernel_1_globals;
            PostMessage(helper_window,WM_USER+DESTROY_WINDOW,(WPARAM)hwnd,0);
        }

    // ----------------------------------------------------------------------------------------

        HWND make_window (
            DWORD dwStyle_
        )
        {   
            using namespace gui_core_kernel_1_globals;
            // if we are running in the event handling thread then just call
            // CreateWindow directly
            if (get_thread_id() == gui_core_kernel_1_globals::event_thread_id)
            {
                // if this is stupposed to be a popup window then do the popup window thing
                if (dwStyle_ == WS_CHILD)
                {
                    HWND tmp = CreateWindowEx (WS_EX_TOOLWINDOW|WS_EX_TOPMOST, window_class_name, "",
                            dwStyle_,
                            CW_USEDEFAULT, CW_USEDEFAULT,
                            CW_USEDEFAULT, CW_USEDEFAULT,
                            helper_window, NULL, hInstance, NULL);
                    SetParent(tmp,NULL);
                    return tmp;
                }
                else
                {
                    return CreateWindow (window_class_name, "",
                            dwStyle_,
                            CW_USEDEFAULT, CW_USEDEFAULT,
                            CW_USEDEFAULT, CW_USEDEFAULT,
                            NULL, NULL, hInstance, NULL);
                }
            }
            else
            {
                auto_mutex M(window_table.get_mutex());
                // wait for our chance to make a new window request
                while (request_new_window)
                    et_signaler.wait();


                dwStyle = dwStyle_;
                if (PostMessage(helper_window,WM_USER+CREATE_WINDOW,0,0)==0)
                {
                    throw gui_error("Unable to schedule function for execution in event handling thread.");
                } 

                // wait for our request to be serviced
                while (new_window == NULL)
                    et_signaler.wait();

                HWND temp = new_window;
                new_window = NULL;
                request_new_window = false;
                et_signaler.broadcast();

                // if make_window() returns the helper_window then it means it failed
                // to make a new window
                if (temp == helper_window)
                    temp = NULL;

                return temp;
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
                delete &a;
            }
            catch (std::exception& e)
            {
                error_box("Exception Thrown",e.what());
                end_program();
            }
            catch (...)
            {
                error_box("Exception Thrown","Unknown exception type thrown.");
                end_program();
            }
        }

    }

// ----------------------------------------------------------------------------------------

    void end_program (
    )
    {   
        using namespace gui_core_kernel_1_globals;
        
        if (PostMessage(helper_window,WM_USER+QUIT_PROGRAM,0,0)==0)
        {
            throw gui_error("Unable to schedule function for execution in event handling thread.");
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

        const LONG block1 = (blue<<24) | (red<<16) | (green<<8) | blue;
        const LONG block2 = (green<<24) | (blue<<16) | (red<<8) | green;
        const LONG block3 = (red<<24) | (green<<16) | (blue<<8) | red;

        // remember that row_width is a multiple of 4 because windows
        // requires that all bitmaps have row widths that are multiples of 4.
        unsigned long size = row_width/4;
        for (unsigned long i = 0; i < height_; ++i)
        {
            unsigned long padding = size%3;                
            LONG* start = reinterpret_cast<LONG*>(bits+row_width*i);
            LONG* end = reinterpret_cast<LONG*>(start) + size - padding;
            while (start != end)
            {
                *start = block1;
                ++start;
                *start = block2;
                ++start;
                *start = block3;
                ++start;
            }
            if (padding)
            {
                *start = block1;
                ++start;
                --padding;
            }
            if (padding)
            {
                *start = block2;
            }
        }
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    trigger_user_event (
        void* p,
        int i
    )
    {
        using namespace gui_core_kernel_1_globals;

        user_event_type e;
        e.w = hwnd;
        e.p = p;
        e.i = i;
        {
            auto_mutex M(user_events.get_mutex());
            user_events.enqueue(e);
        }
        
        if (PostMessage(helper_window,WM_USER+USER_EVENTS_READY,0,0)==0)
        {
            throw gui_error("Unable to schedule function for execution in event handling thread.");
        } 
    }

// ----------------------------------------------------------------------------------------

    base_window::
    base_window (
        bool resizable,
        bool undecorated 
    ) :
        has_been_destroyed(false),
        wm(gui_core_kernel_1_globals::window_table.get_mutex())
    {
        ASSERT(!(undecorated == true && resizable == true),
            "\tbase_window::base_window()"
            << "\n\tThere is no such thing as an undecorated window that is resizable by the user."
            << "\n\tthis:     " << this
            );

        if (resizable)   
            style = WS_OVERLAPPEDWINDOW;                
        else if (undecorated)
            style = WS_CHILD;
        else
            style = WS_OVERLAPPEDWINDOW ^ WS_THICKFRAME ^ WS_MAXIMIZEBOX;

        hwnd = gui_core_kernel_1_globals::make_window(style);                

        if (hwnd == NULL)
            throw gui_error("unable to create base_window");

        auto_mutex M(wm);

        mouse_in = false;

        HWND temp = hwnd;
        base_window* ttemp = this;
        gui_core_kernel_1_globals::window_table.add(temp,ttemp);
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
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    close_window (
    )
    {
        auto_mutex M(wm);
        if (has_been_destroyed == false)
        {
            // do this just to make sure no one tries to call this window's
            // calbacks.
            gui_core_kernel_1_globals::window_table.destroy(hwnd);
            gui_core_kernel_1_globals::destroy_window(hwnd);
            hwnd = 0;
            has_been_destroyed = true;
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
        const std::string& title
    )
    {
        ASSERT(is_closed() == false,
            "\tvoid base_window::set_title"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );
        using namespace gui_core_kernel_1_globals;
        
        // call the SetWindowText function with our arguments.  We 
        // have to do this because the SetWindowText() apparently blocks
        // until something happens in the event thread so we have to 
        // do this to avoid possible deadlocks.
        auto_mutex M(wm);
        while (set_window_title_done == false)
            et_signaler.wait();
        window_title = title;
        set_window_title_done = false;

        if (PostMessage(helper_window,WM_USER+CALL_SET_WINDOW_TITLE,(WPARAM)hwnd,0)==0)
        {
            throw gui_error("Unable to schedule SetWindowText function for execution in event handling thread.");
        } 
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
        using namespace gui_core_kernel_1_globals;
        show_window(hwnd);
        if (style != WS_CHILD)
            give_window_focus(hwnd);
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    hide(
    )    
    {
        ASSERT(is_closed() == false,
            "\tvoid base_window::hide"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );
        using namespace gui_core_kernel_1_globals;
        hide_window(hwnd);
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    set_size (
        int width_,
        int height_
    )
    {
        using namespace gui_core_kernel_1_globals;
        ASSERT(is_closed() == false,
            "\tvoid base_window::set_size"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            << "\n\twidth:    " << width_ 
            << "\n\theight:   " << height_ 
            );
        auto_mutex M(wm);
        // wait for any MoveWindow calls to finish
        while (move_window_done == false)
            et_signaler.wait();

        RECT info;
        GetWindowRect(hwnd,&info);

        int x = info.left;
        int y = info.top;
        int width;
        int height;

        RECT rect;
        rect.top = 0;
        rect.left = 0;
        rect.bottom = height_;
        rect.right = width_;
        AdjustWindowRectEx(&rect,style,FALSE,0);
        
        width = abs(rect.right - rect.left);
        height = abs(rect.bottom - rect.top);

        // call the MoveWindow function with our arguments.  We 
        // have to do this because the MoveWindow() apparently blocks
        // until something happens in the event thread so we have to 
        // do this to avoid possible deadlocks.
        move_window_hwnd = hwnd;
        move_window_x = x;
        move_window_y = y;
        move_window_width = width;
        move_window_height = height;
        move_window_done = false;

        if (PostMessage(helper_window,WM_USER+CALL_MOVE_WINDOW,0,0)==0)
        {
            throw gui_error("Unable to schedule MoveWindow function for execution in event handling thread.");
        } 

    }

// ----------------------------------------------------------------------------------------

    void base_window::
    set_pos (
        long x_,
        long y_
    )
    {
        using namespace gui_core_kernel_1_globals;
        ASSERT(is_closed() == false,
            "\tvoid base_window::set_pos"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            << "\n\tx:        " << x_ 
            << "\n\ty:        " << y_ 
            );
        auto_mutex M(wm);
        // wait for any MoveWindow calls to finish
        while (move_window_done == false)
            et_signaler.wait();

        RECT info;
        GetWindowRect(hwnd,&info);
        int width = info.right - info.left;
        int height = info.bottom - info.top;



        // call the MoveWindow function with our arguments.  We 
        // have to do this because the MoveWindow() apparently blocks
        // until something happens in the event thread so we have to 
        // do this to avoid possible deadlocks.
        move_window_hwnd = hwnd;
        move_window_x = x_;
        move_window_y = y_;
        move_window_width = width;
        move_window_height = height;
        move_window_done = false;

        if (PostMessage(helper_window,WM_USER+CALL_MOVE_WINDOW,0,0)==0)
        {
            throw gui_error("Unable to schedule MoveWindow function for execution in event handling thread.");
        } 
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    get_pos (
        long& x_,
        long& y_
    )
    {
        ASSERT(is_closed() == false,
            "\tvoid base_window::get_pos"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );
        POINT p;
        p.x = 0;
        p.y = 0;
        ClientToScreen(hwnd,&p);

        x_ = p.x;
        y_ = p.y;
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    get_size (
        unsigned long& width,
        unsigned long& height
    ) const
    {
        ASSERT(is_closed() == false,
            "\tvoid base_window::get_size"
            << "\n\tYou can't do this to a window that has been closed."
            << "\n\tthis:     " << this
            );

        RECT r;
        GetClientRect(hwnd,&r);

        width = r.right - r.left;
        height = r.bottom - r.top;
    }

// ----------------------------------------------------------------------------------------

    void base_window::
    invalidate_rectangle (
        const rectangle& rect
    )
    {
        if (rect.is_empty() == false && !has_been_destroyed)
        {
            RECT info;
            info.top = rect.top();
            info.left = rect.left();
            info.right = rect.right()+1;
            info.bottom = rect.bottom()+1;

            InvalidateRect(hwnd,&info,FALSE);
        }
    }

// ----------------------------------------------------------------------------------------

    void put_on_clipboard (
        const std::string& str
    )
    {
        using namespace gui_core_kernel_1_globals;
        using namespace std;
        if (OpenClipboard(helper_window))
        {
            EmptyClipboard();
            auto_mutex M(window_table.get_mutex());

            const unsigned long newlines = count(str.begin(),str.end(),'\n');

            HGLOBAL mem = GlobalAlloc(GMEM_MOVEABLE,str.size()+newlines+1);
            if (mem != NULL)
            {
                char* buf = reinterpret_cast<char*>(GlobalLock(mem));

                if (buf != NULL)
                {
                    // copy str into buf while also replacing all the \n with \r\n
                    for (string::size_type i = 0; i < str.size(); ++i)
                    {
                        if (str[i] != '\n')
                        {
                            *buf = str[i];
                            ++buf;
                        }
                        else
                        {
                            *buf = '\r';
                            ++buf;
                            *buf = '\n';
                            ++buf;
                        }
                    }
                    *buf = '\0';
                    GlobalUnlock(mem);
                    SetClipboardData(CF_TEXT,mem);
                }
            }
            CloseClipboard();
        }
    }

// ----------------------------------------------------------------------------------------

    void get_from_clipboard (
        std::string& str
    )
    {
        using namespace gui_core_kernel_1_globals;
        using namespace std;
        auto_mutex M(window_table.get_mutex());
        if (OpenClipboard(helper_window))
        {

            HANDLE data = GetClipboardData(CF_TEXT);
            if (data != NULL)
            {
                char* buf = reinterpret_cast<char*>(GlobalLock(data));
                if (buf != 0)
                {
                    str.clear();

                    // copy the data from buf into str while also removing any '\r' 
                    // characters.
                    while (*buf != '\0')
                    {
                        if (*buf != '\r')
                            str += *buf;
                        ++buf;
                    }

                    GlobalUnlock(data);
                }
                else
                {
                    Beep(500,500);
                }
            }

            CloseClipboard();
        }
    }

// ----------------------------------------------------------------------------------------

}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // the real WinMain function
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE ,
                PSTR cmds , int )
{
    using namespace dlib::gui_core_kernel_1_globals;

    try
    {

        dlib::gui_core_kernel_1_globals::hInstance = hInstance;


        // register the main window class
        WNDCLASS     wndclass ;

        wndclass.style         = CS_DBLCLKS;
        wndclass.lpfnWndProc   = dlib::gui_core_kernel_1_globals::WndProc ;
        wndclass.cbClsExtra    = 0 ;
        wndclass.cbWndExtra    = 0 ;
        wndclass.hInstance     = hInstance ;
        wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
        wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
        wndclass.hbrBackground = 0;
        wndclass.lpszMenuName  = NULL ;
        wndclass.lpszClassName = window_class_name ;

        if (!RegisterClass (&wndclass))  
        {
            error_box("Error","Error registering window");
            return 1;
        }


        // make the helper window that is used to trigger events in the
        // event handler loop from other threads
        helper_window = CreateWindow(window_class_name,"",WS_DISABLED,0,0,0,0,HWND_MESSAGE,NULL,hInstance,NULL);
        if (helper_window == NULL)
        {
            error_box("Error","Error gathering needed resources");
            return 1;         
        }
        



        dlib::queue<std::string>::kernel_1a_c qos;
        std::istringstream sin(cmds);
        std::string temp;
        char** argv;

        // parse the command line
        int ch = sin.get();
        while (ch != EOF)
        {
            // accumulate until we see another "
            if (ch == '"')
            {
                ch = sin.get();
                while (ch != '"' && ch != EOF)
                {
                    temp += ch;
                    ch = sin.get();
                }
            }
            else if (ch == ' ' || ch == '\n' || ch == '\t')
            {
                if (temp.size())
                    qos.enqueue(temp);
            }
            else
            {
                temp += ch;
            }

            ch = sin.get();
        }
        if (temp.size())
            qos.enqueue(temp);


        int argc = qos.size()+1;
        argv = new char*[argc];

        // put in argv[0]
        argv[0] = new char[5000];
        GetModuleFileName(NULL,argv[0],5000);


        // fill argv with the arguments
        for (int i = 1; i < argc; ++i)
        {
            qos.dequeue(temp);

            argv[i] = new char[temp.size()+1];
            // copy string into argv[i]
            for (std::string::size_type j = 0; j < temp.size(); ++j)
            {
                argv[i][j] = temp[j];
            }
            argv[i][temp.size()] = '\0';
        }

                


        // call the entry point
        winmain_helper_args* a = new winmain_helper_args;
        a->argc = argc;
        a->argv = argv;        
        if (dlib::create_new_thread(winmain_helper,a) == false)
        {
            error_box("Error","Unable to create needed thread.");
            return 1;
        }

        // start the event handler loop.   
        /*
            A note about this quit_windows_loop thing.  If the user is holding 
            the mouse button down on the title bar of a window it will cause
            the PostQuitMessage() function to be ignored!!  This extra bool 
            is a work around to prevent that from happening.
        */
        MSG msg;
        while (GetMessage (&msg, NULL, 0, 0) && 
            quit_windows_loop == false)
        {
            TranslateMessage (&msg) ;
            DispatchMessage (&msg) ;
        } 

        // free the memory used for argv
        for (int i = 0; i < argc; ++i)
            delete [] argv[i];
        delete [] argv;

        return 0;
    }
    catch (std::exception& e)
    {
        error_box("Exception Thrown",e.what());
    }
    catch (...)
    {
        error_box("Exception Thrown","Unknown exception type thrown.");
    }

    return 0;

}

#endif // WIN32

#endif // DLIB_GUI_CORE_KERNEL_1_CPp_

