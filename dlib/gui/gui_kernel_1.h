// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_GUI_KERNEl_1_
#define DLIB_GUI_KERNEl_1_


#include <string>

#define NOMINMAX // prevent windows from messing with std::min and std::max
#define _WINSOCKAPI_   /* Prevent inclusion of winsock.h in windows.h */


#include <windows.h>
#include <windowsx.h>
#include <winuser.h>
#include <commctrl.h>

#ifndef _USE_THE_OLD_GUI_API_
#error The MS Windows only gui api is deprecated.  You should use the new stuff based on the gui_core component.  But if you really want to use this one you can #define _USE_THE_OLD_GUI_API_
#endif

#ifdef _MSC_VER
// Disable the following warnings for Visual Studio
//
// this is to disable the "'this' : used in base member initializer list"
// warning you get from some of the GUI objects since all the objects
// require that their parent class be passed into their constructor. 
// In the case of my code what I am doing is safe so this warning can be ignored.
//
// The last two warnings have to do with converting points to and from the LONG
// type.  But both these types are 32 bits in windows so it is fine.
#pragma warning(disable : 4355; disable: 4244; disable: 4312)
#endif 


// this is the real entry point to a windows program
int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow);

/*
    IMPLEMENTATION NOTES:
        all objects add a pointer to themselves to the windows they created using the command
        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));
        This pointer is used to look up the appropriate object in the windows procedure

        any object that wishes to be notified of windows events
        must implement the action function defined in object.




*/


namespace dlib
{

// ----------------------------------------------------------------------------------------

    namespace gui_kernel_1_globals
    {

        extern HINSTANCE hInstance;
        extern LOGFONT lf; 
        extern HFONT font;

        LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;
        LRESULT CALLBACK splitterWinProc_1 (HWND, UINT, WPARAM, LPARAM) ;
    }

// ----------------------------------------------------------------------------------------


    void start_program (
    );

// -----------------

    void end_program (
    );

// -----------------

    void message_box (
        const std::string& title,
        const std::string& text,
        bool require_answer = false
    );

// -----------------

    bool question_box (
        const std::string& title,
        const std::string& text,
        bool require_answer = false
    );

// -----------------

    bool select_directory_box (
        std::string& dir_name
    );

// -----------------

    bool open_file_box (
        std::string& file_name
    );

// -----------------

    bool save_file_box (
        std::string& file_name
    );

// -----------------

    template <
        typename queue  // must be an implementation of queue/queue_kernel_abstract.h
        >
    bool open_multiple_files_box (
        queue& file_names
    )
    {

        file_names.clear();
        OPENFILENAME ofn;       // common dialog box structure
        char szFile[10096];       // buffer for file name
        szFile[0] = '\0';

        // Initialize OPENFILENAME
        ZeroMemory(&ofn, sizeof(OPENFILENAME));
        ofn.lStructSize = sizeof(OPENFILENAME);
        ofn.hwndOwner = NULL;
        ofn.lpstrFile = szFile;
        ofn.nMaxFile = sizeof(szFile);
        ofn.lpstrFilter = "All\0*.*\0Text\0*.TXT\0";
        ofn.nFilterIndex = 1;
        ofn.lpstrFileTitle = NULL;
        ofn.nMaxFileTitle = 0;
        ofn.lpstrInitialDir = NULL;
        ofn.Flags = OFN_PATHMUSTEXIST | OFN_ALLOWMULTISELECT |OFN_EXPLORER | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;

        // Display the Open dialog box. 
        if (GetOpenFileName(&ofn)==TRUE)
        {
            char* buf = szFile;
            std::string temp;
            std::string path = szFile;
            

            // find start of first file name
            while (*(buf++) != 0); 
            while (*buf != 0)
            {
                temp = path + '\\' + buf;
                file_names.enqueue(temp);
                // find start of next string
                while (*(buf++) != 0);                
            } 
            if (file_names.size() == 0)
                file_names.enqueue(path);
            return true;
        }
        else
        {
            return false;
        }        
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class object
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class window;  // forward declaration for window
    class object
    {
        /*!
            DEFAULT VALUE
                height == CW_USEDEFAULT
                width == CW_USEDEFAULT
                x == CW_USEDEFAULT
                y == CW_USEDEFAULT

            CONVENTION
                hwnd == handel to the object represented by this
                height == the height of this
                width == the width of this
                x == the x position of this
                y == the y position of this
                pwindow == handel to the parent window of this
             
        !*/
    
        // the following friend statements allow these windows procedures to
        // call the action function for the object object when an event occurs
        friend LRESULT CALLBACK gui_kernel_1_globals::WndProc (HWND, UINT, WPARAM, LPARAM);
        friend LRESULT CALLBACK gui_kernel_1_globals::splitterWinProc_1 (HWND, UINT, WPARAM, LPARAM) ;

    public:

        object (
            window& w
        );

        virtual ~object (
        )=0;

        virtual void set_size (
            int width,
            int height
        );

        virtual void set_position (
            int x,
            int y
        );

        virtual void show (
        );

        virtual void hide (
        );

        virtual void enable (
        );

        virtual void disable (
        );        

        virtual void set_focus (
        );

    private:

        virtual LRESULT action (
            HWND hwnd, UINT message , WPARAM wParam, LPARAM lParam
        ){ return DefWindowProc (hwnd, message, wParam, lParam); }
        /*!
            requires
                is called when a message is sent that should be handeled by the object
                the first parameter is the hwnd, the second is the message, the third 
                is wParam and the fourth is the lParam for the event
        !*/

    protected:
        HWND hwnd;
        HWND pwindow;


        int height;
        int width;
        int x;
        int y;

    private:
        object(object&);        // copy constructor
        object& operator=(object&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class window
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class window
    {

        friend class object;
        friend LRESULT CALLBACK gui_kernel_1_globals::WndProc (HWND, UINT, WPARAM, LPARAM);

        int x, y, width, height;
        HWND hwnd;

    public:

        window (
        );

        window (
            bool resizable
        );

        virtual ~window (
        );

        window (
            const std::string& title
        );

        void set_title (
            const std::string& title
        );

        void show (
        );

        void hide(
        );

        void minimize (
        );

        void maximize (
        );

        void set_size (
            int width,
            int height
        );

        void set_position (
            int x,
            int y
        );

        void get_client_area (
            int& width,
            int& height
        ) const;

        void set_focus (
        );


    protected:


        virtual void on_window_close(
        ){}

        virtual void on_window_resized(
        ){}

    private:


        window(window&);        // copy constructor
        window& operator=(window&);    // assignment operator

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
            CONVENTION
                p == a reference to the parent window
                if (click_handler != 0)
                    click_handler == a pointer to the event handler for button clicks
        !*/

        parent_window& p; 
        void (parent_window::*click_handler)();

    public:
        button(parent_window& w);

        virtual ~button() {}

        void set_name (
            const std::string& name
        );

        void click (
        );

        void set_click_handler (
            void (parent_window::*event_handler)()
        ) {click_handler = event_handler;}

    private:

 
        LRESULT action (
           HWND, UINT message, WPARAM wParam , LPARAM 
        )
        {
            if (message == WM_COMMAND && (UINT)(wParam>>16) == BN_CLICKED && click_handler)
                (p.*click_handler)();
            return 0;
        }

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
            CONVENTION
                p == a reference to the parent window
                if (click_handler != 0)
                    click_handler == a pointer to the event handler for button clicks
        !*/

        parent_window& p; 
        void (parent_window::*click_handler)();

    public:

        flat_button (
            parent_window& w
        );

        virtual ~flat_button(
        ){}

        void set_name (
            const std::string& name
        );

        void click (
        );

        void set_click_handler (
            void (parent_window::*event_handler)()
        ) { click_handler = event_handler; }

    private:

        LRESULT action (
           HWND, UINT message, WPARAM wParam , LPARAM 
        )
        {
            if (message == WM_COMMAND && (UINT)(wParam>>16) == BN_CLICKED && click_handler)
                (p.*click_handler)();
            return 0;
        }

 
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

        const bool scrollable;

    public:
        text_field(
            parent_window& w,
            bool scrollable_ = false
        );

        virtual ~text_field() {}

        void set_text (
            const std::string& text
        );

        const std::string get_text (
        )const ;


    private:

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

        const bool readonly;

    public:

        virtual ~text_box(        
        ){}

        text_box (
            parent_window& w,
            bool r = false
        );

        void set_text (
            const std::string& text
        );

        const std::string get_text (
        )const ;

        void scroll_to_top (
        );

        void scroll_to_bottom (
        );

    private:
        
        LRESULT action (
           HWND, UINT , WPARAM , LPARAM 
        );

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
    public:
        label (
            parent_window& w
        );

        virtual ~label() {}

        void set_text (
            const std::string& text
        );

        const std::string get_text (
        ) const;

    private:
        
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
            CONVENTION
                p == a reference to the parent window of this object
                if (element_double_clicked_handler != 0) then
                    element_double_clicked_handler == pointer to the handler for double click 
                                                      events
                if (element_selected_handler != 0) then
                    element_selected_handler == pointer to the handler for selection
                                                change events
        !*/

        parent_window& p;
        void (parent_window::*element_double_clicked_handler)(int);
        void (parent_window::*element_selected_handler)(int);

    public:

        list_box(
            parent_window& w
        );

        virtual ~list_box(
        ) {}

        int size(
        )const ;

        const std::string get_element (
            int index
        ) const;

        void insert_element (
            int index,
            const std::string& element
        );

        void remove_element (
            int index
        );

        void clear(
        );

        int get_selected_pos (
        ) const;

        void set_element_double_clicked_handler (
            void (parent_window::*event_handler)(int index)
        ) {element_double_clicked_handler = event_handler;}
       
        void set_element_selected_handler (
            void (parent_window::*event_handler)(int index)
        ) {element_selected_handler = event_handler;}

    private:

        LRESULT action (
           HWND, UINT , WPARAM wParam , LPARAM 
        );

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

    public:

        etched_rectangle(
            parent_window& w
        );

        virtual ~etched_rectangle(
        ){}


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

    public:

        sunken_rectangle(
            parent_window& w
        );

        virtual ~sunken_rectangle(
        ){}

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

    public:

        sunken_3d_rectangle(
            parent_window& w
        );

        virtual ~sunken_3d_rectangle(
        ){}

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

    public:

        named_rectangle(
            parent_window& w
        );

        virtual ~named_rectangle(
        ){}

        void set_name (
            const std::string& name
        );
 
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

    public:

        progress_bar(
            parent_window& w
        );

        virtual ~progress_bar(
            ){}

        void set_range (
            int low,
            int high
        );

        void set_pos (
            int pos
        );

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
            CONVENTION
                p == a reference to the parent window
                if (click_handler != 0)
                    click_handler == a pointer to the event handler for button clicks

                name_on_left == true if the name of the check_box will appear to its left
        !*/

        parent_window& p; 
        void (parent_window::*click_handler)();
        const bool name_on_left;

    public:

        check_box (
            parent_window& w,
            bool name_on_left_ = false
        );

        virtual ~check_box(
        ) {}

        void set_name (
            const std::string& name
        );

        bool is_checked (
        )const ;

        void set_checked (
        );

        void set_unchecked (
        );


        void set_click_handler (
            void (parent_window::*event_handler)()
        ) { click_handler = event_handler; }

    private:

        LRESULT action (
           HWND, UINT message, WPARAM wParam , LPARAM 
        )
        {
            if (message == WM_COMMAND && (UINT)(wParam>>16) == BN_CLICKED && click_handler)
                (p.*click_handler)();
            return 0;
        }

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
            CONVENTION
                p == a reference to the parent window
                if (click_handler != 0)
                    click_handler == a pointer to the event handler for button clicks

                name_on_left == true if the name of the check_box will appear to its left
        !*/

        parent_window& p; 
        void (parent_window::*click_handler)();

        const bool name_on_left;

    public:
        radio_button (
            parent_window& w,
            bool name_on_left_ = false
        );

        virtual ~radio_button(
        ) {}

        void set_name (
            const std::string& name
        );

        bool is_checked (
        ) const;

        void set_checked (
        );

        void set_unchecked (
        );


        void set_click_handler (
            void (parent_window::*event_handler)()
        ) { click_handler = event_handler; }

    private:
        LRESULT action (
           HWND, UINT message, WPARAM wParam , LPARAM 
        )
        {
            if (message == WM_COMMAND &&(UINT)(wParam>>16) == BN_CLICKED && click_handler)
                (p.*click_handler)();
            return 0;
        }

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

    public:
        password_field(
            parent_window& w
        );

        virtual ~password_field(
        ){}

        const std::string get_text (
        ) const;


    private:

        // restricted functions
        password_field(password_field&);        // copy constructor
        password_field& operator=(password_field&);    // assignment operator
    };

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
            CONVENTION
                p == a reference to the parent window
                if (move_handler != 0) then
                    move_handler == pointer to the handler for move events
                mouse_captured == true if the mouse is over the client area of the splitter
                                  and we have captured it

                the slider will not allow itself to slide outside the range start to end
        !*/

        parent_window& p;
        void (parent_window::*move_handler)();
        bool mouse_captured;
        int start, end;

    public:
        splitter(
            parent_window& w
        );

        virtual ~splitter(
        ){}

        void get_position (
            int& x_,
            int& y_
        ) const { x_ = x; y_ = y; }

        void set_range (
            int start_,
            int end_
        ){ start = start_; end = end_; }

        void set_move_handler (
            void (parent_window::*event_handler)()
        ) {move_handler = event_handler;}

    private:
       

        LRESULT action (
           HWND, UINT , WPARAM , LPARAM 
        );

        // restricted functions
        splitter(splitter&);        // copy constructor
        splitter& operator=(splitter&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class button function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    button<parent_window>::
    button (
        parent_window& w
    ) :
        object(w),
        p(w),
        click_handler(0)
    {    
        // create the logon button
        hwnd = CreateWindow (
                    "button","", WS_VISIBLE|WS_CHILD|BS_MULTILINE,
                    x,y,width,
                    height,pwindow,0,gui_kernel_1_globals::hInstance,
                    NULL
                    );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    } 

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void button<parent_window>::
    set_name (
        const std::string& name
    )
    {
        SetWindowText(hwnd,name.c_str());
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void button<parent_window>::
    click (
    )
    {
        int wParam = BN_CLICKED;
        wParam <<= 16;
        SendMessage(pwindow,WM_COMMAND,wParam,(LPARAM)hwnd);
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class flat_button function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    flat_button<parent_window>::
    flat_button (
        parent_window& w
    ) :
        object(w),
        click_handler(0),
        p(w)
    {
        // create the logon button
        hwnd = CreateWindow (
                    "button","", WS_VISIBLE|WS_CHILD|BS_FLAT|BS_MULTILINE,
                    x,y,width,
                    height,pwindow,0,gui_kernel_1_globals::hInstance,
                    NULL
                    );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void flat_button<parent_window>::
    set_name (
        const std::string& name
    )
    {
        SetWindowText(hwnd,name.c_str());
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void flat_button<parent_window>::
    click (
    )
    {
        int wParam = BN_CLICKED;
        wParam <<= 16;
        SendMessage(pwindow,WM_COMMAND,wParam,(LPARAM)hwnd);
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class text_field function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void text_field<parent_window>::
    set_text (
        const std::string& text
    )
    {
        Edit_SetText(hwnd,text.c_str());
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    const std::string text_field<parent_window>::
    get_text (
    ) const
    {

        int length = Edit_GetTextLength(hwnd);

        std::string temp;
        if (length > 0)
        {
            char* buf = new char[length+1];
            length = Edit_GetText(hwnd,buf,length+1);
            buf[length] = '\0';
            temp = buf;
            delete [] buf;
        }
        return temp;

    }

// ----------------------------------------------------------------------------------------   

    template <
        typename parent_window
        >
    text_field<parent_window>::
    text_field (
        parent_window& w,
        bool scrollable_
    ) :
        object(w),
        scrollable(scrollable_)   
    {
        
        // create the logon button
        hwnd = CreateWindowEx (  WS_EX_CLIENTEDGE,
                "edit","", WS_CHILD|WS_VISIBLE |(scrollable ? ES_AUTOHSCROLL : 0),
                    x,y,width,
                    height,pwindow,0,gui_kernel_1_globals::hInstance,
                    NULL
                    );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class text_box function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void text_box<parent_window>::
    set_text (
        const std::string& text
    )
    {
        std::string temp;
        char ch;
        for (std::string::size_type i = 0; i < text.size(); ++i)
        {
            ch = text[i];
            if (ch == '\n')
            {
                temp += "\r\n";
            }
            else
            {
                temp += ch;
            }
        }
        Edit_SetText(hwnd,temp.c_str());
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void text_box<parent_window>::
    scroll_to_bottom (
    )
    {
        int lines = Edit_GetLineCount(hwnd);
        Edit_Scroll(hwnd,lines,SB_LINEDOWN);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void text_box<parent_window>::
    scroll_to_top (
    )
    {
        int lines = Edit_GetLineCount(hwnd);
        Edit_Scroll(hwnd,lines,SB_LINEUP);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    const std::string text_box<parent_window>::
    get_text (
    ) const
    {

        int length = Edit_GetTextLength(hwnd);

        std::string temp;
        if (length > 0)
        {
            char* buf = new char[length+1];
            length = Edit_GetText(hwnd,buf,length+1);
            buf[length] = '\0';
            
            // copy buf into temp while replacing "\r\n" with "\n"
            char* pbuf = buf;
            char ch = *pbuf;
            while (ch)
            {
                ++pbuf;
                if (ch == '\r' && *pbuf == '\n')
                {
                    temp += '\n';
                    ++pbuf;
                }
                else
                    temp += ch;

                ch = *pbuf;
            } // end of the block that copies buf to temp

            delete [] buf;
        }
        return temp;
    }

// ----------------------------------------------------------------------------------------   

    template <
        typename parent_window
        >
    LRESULT text_box<parent_window>::
    action (
        HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam
    )
    {
        if (message == WM_CTLCOLORSTATIC && readonly)
        {            
            return (LRESULT)(GetStockObject(WHITE_BRUSH)); 
        }
        return DefWindowProc (hwnd, message, wParam, lParam);
    }

// ----------------------------------------------------------------------------------------   

    template <
        typename parent_window
        >
    text_box<parent_window>::
    text_box (
        parent_window& w,
        bool r
    ) : 
        object(w),
        readonly(r) 
    {


        if (readonly)
        {
            // create the logon button
            hwnd = CreateWindowEx (WS_EX_CLIENTEDGE,
                        "edit","", 
                        WS_CHILD|WS_VISIBLE | ES_READONLY | WS_VSCROLL | 
                        ES_MULTILINE | ES_AUTOVSCROLL,
                        x,y,width,
                        height,pwindow,0,gui_kernel_1_globals::hInstance,
                        NULL
                        );
        }
        else
        {
            // create the logon button
            hwnd = CreateWindowEx (WS_EX_CLIENTEDGE,
                        "edit","", 
                        WS_CHILD|WS_VISIBLE | WS_VSCROLL | ES_MULTILINE | ES_AUTOVSCROLL,
                        x,y,width,
                        height,pwindow,0,gui_kernel_1_globals::hInstance,
                        NULL
                        );
        }
        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

 
        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class label function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    
    template <
        typename parent_window
        >
    void label<parent_window>::
    set_text (
        const std::string& text
    )
    {        
        Static_SetText(hwnd,text.c_str());
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    const std::string label<parent_window>::
    get_text (
    ) const
    {
        char buf[300];
        Static_GetText(hwnd,buf,sizeof(buf));
        return std::string(buf);
    }

// ----------------------------------------------------------------------------------------
        
    template <
        typename parent_window
        >
    label<parent_window>::
    label (
        parent_window& w
    ) :
        object(w)
    {
        // create the logon button
        hwnd = CreateWindow (
                    "static","", WS_CHILD|WS_VISIBLE,
                    x,y,width,
                    height,pwindow,0,gui_kernel_1_globals::hInstance,
                    NULL
                    );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }


// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class list_box function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    
    template <
        typename parent_window
        >
    int list_box<parent_window>::
    get_selected_pos (
    ) const
    {
        int value = SendMessage(hwnd,LB_GETCURSEL,0,0);
        if (value == LB_ERR)
            return -1;
        else
            return value;
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void list_box<parent_window>::
    remove_element (
        int index
    )
    {
        ListBox_DeleteString(hwnd,index);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void list_box<parent_window>::
    clear(
    )
    {
        ListBox_ResetContent(hwnd);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    int list_box<parent_window>::
    size(
    ) const
    {
        return ListBox_GetCount(hwnd);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    const std::string list_box<parent_window>::
    get_element (
        int index
    ) const
    {
        int length = ListBox_GetTextLen(hwnd,index);

        std::string temp;
        if (length > 0)
        {
            char* buf = new char[length+1];
            length = ListBox_GetText(hwnd,index,buf);
            buf[length] = '\0';
            temp = buf;
            delete [] buf;
        }
        return temp;        
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void list_box<parent_window>::
    insert_element (
        int index,
        const std::string& element
    )
    {
        ListBox_InsertString(hwnd,index,element.c_str());
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    list_box<parent_window>::
    list_box (
        parent_window& w
    ) :
        object(w),
        p(w),
        element_double_clicked_handler(0),
        element_selected_handler(0)
    { 

        // create the logon button
        hwnd = CreateWindowEx (WS_EX_CLIENTEDGE,
                    "listbox","", 
                    WS_CHILD | WS_VISIBLE | LBS_NOTIFY | WS_VSCROLL | WS_BORDER ,
                    x,y,width,
                    height,pwindow,0,gui_kernel_1_globals::hInstance,
                    NULL
                    );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    LRESULT list_box<parent_window>::
    action (
        HWND, UINT message, WPARAM wParam, LPARAM 
    )
    {
        if (message == WM_COMMAND)
        {
            if ( ((UINT)(wParam>>16)) == LBN_DBLCLK)
            {
                if (element_double_clicked_handler != 0)
                {
                    int index = SendMessage(hwnd,LB_GETCURSEL,0,0);
                    (p.*element_double_clicked_handler)(index);
                }
            }
            else if ( ((UINT)(wParam>>16)) ==  LBN_SELCHANGE)
            {
                if (element_selected_handler != 0)
                {
                    int index = SendMessage(hwnd,LB_GETCURSEL,0,0);
                    (p.*element_selected_handler)(index);
                }
            }
        }

         return 0;
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class etched_rectange function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    
    template <
        typename parent_window
        >
    etched_rectangle<parent_window>::
    etched_rectangle (
        parent_window& w
    ) :
        object(w)
    {

        // create the logon button
        hwnd = CreateWindow (
                "static", NULL, SS_ETCHEDFRAME|WS_VISIBLE|WS_CHILD , 
                x,y,width,height,pwindow,0,gui_kernel_1_globals::hInstance,NULL
                );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class sunken_rectangle function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    
    template <
        typename parent_window
        >
    sunken_rectangle<parent_window>::
    sunken_rectangle (
        parent_window& w
    ) :
        object(w)
    {

        // create the logon button
        hwnd = CreateWindow (
                "static", NULL, SS_SUNKEN|WS_VISIBLE|WS_CHILD , 
                x,y,width,height,pwindow,0,gui_kernel_1_globals::hInstance,NULL
                );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class sunken_3d_rectangle function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    
    template <
        typename parent_window
        >
    sunken_3d_rectangle<parent_window>::
    sunken_3d_rectangle (
        parent_window& w
    ) :
        object(w)
    {

        // create the logon button
        hwnd = CreateWindowEx (WS_EX_CLIENTEDGE,
                "static", NULL, WS_VISIBLE|WS_CHILD , 
                x,y,width,height,pwindow,0,gui_kernel_1_globals::hInstance,NULL
                );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class named_rectangle function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    
    template <
        typename parent_window
        >
    named_rectangle<parent_window>::
    named_rectangle (
        parent_window& w
    ) :
        object(w)
    {
        // create the logon button
        hwnd = CreateWindow (
                "button", NULL, BS_GROUPBOX|WS_VISIBLE|WS_CHILD , 
                x,y,width,height,pwindow,0,gui_kernel_1_globals::hInstance,NULL
                );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void named_rectangle<parent_window>::
    set_name (
        const std::string& name
    )
    {
        SetWindowText(hwnd,name.c_str());
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class progress_bar function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
   
    template <
        typename parent_window
        >
    void progress_bar<parent_window>::
    set_range (
        int low,
        int high
    )
    {
        SendMessage(hwnd, PBM_SETRANGE, 0, MAKELPARAM(low, high));
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void progress_bar<parent_window>::
    set_pos (
        int pos
    )
    {
        SendMessage(hwnd, PBM_SETPOS, pos, 0);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    progress_bar<parent_window>::
    progress_bar (
        parent_window& w
    ) :
        object(w)
    {
        // create the logon button
        hwnd = CreateWindow (
                "msctls_progress32", NULL, WS_VISIBLE|WS_CHILD | PBS_SMOOTH, 
                x,y,width,height,pwindow,0,gui_kernel_1_globals::hInstance,NULL
                );
        SendMessage(hwnd, PBM_SETRANGE, 0, MAKELPARAM(0, 100));

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class check_box function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    check_box<parent_window>::
    check_box (
        parent_window& w,
        bool name_on_left_
    ) :
        object(w),
        name_on_left(name_on_left_),
        p(w),
        click_handler(0)
    {
        if (name_on_left)
        {
            // create the logon button
            hwnd = CreateWindow (
                        "button","", WS_VISIBLE|WS_CHILD|BS_AUTOCHECKBOX|BS_LEFTTEXT,
                        x,y,width,
                        height,pwindow,0,gui_kernel_1_globals::hInstance,
                        NULL
                        );


        }
        else
        {
            // create the logon button
            hwnd = CreateWindow (
                        "button","", WS_VISIBLE|WS_CHILD|BS_AUTOCHECKBOX,
                        x,y,width,
                        height,pwindow,0,gui_kernel_1_globals::hInstance,
                        NULL
                        );
        }

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void check_box<parent_window>::
    set_checked (
    )
    {
        Button_SetCheck(hwnd,TRUE);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void check_box<parent_window>::
    set_unchecked (
    )
    {
        Button_SetCheck(hwnd,FALSE);
    }
    
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void check_box<parent_window>::
    set_name (
        const std::string& name
    )
    {
        SetWindowText(hwnd,name.c_str());
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    bool check_box<parent_window>::
    is_checked (
    ) const
    {
        if (Button_GetCheck(hwnd) == BST_CHECKED)
            return true;
        else
            return false;
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class radio_button function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    radio_button<parent_window>::
    radio_button (
        parent_window& w,
        bool name_on_left_
    ) :
        object(w),
        name_on_left(name_on_left_),
        p(w),
        click_handler(0)
    {
        if (name_on_left)
        {
            // create the logon button
            hwnd = CreateWindow (
                        "button","", WS_VISIBLE|WS_CHILD|BS_RADIOBUTTON|BS_LEFTTEXT,
                        x,y,width,
                        height,pwindow,0,gui_kernel_1_globals::hInstance,
                        NULL
                        );


        }
        else
        {
            // create the logon button
            hwnd = CreateWindow (
                        "button","", WS_VISIBLE|WS_CHILD|BS_RADIOBUTTON,
                        x,y,width,
                        height,pwindow,0,gui_kernel_1_globals::hInstance,
                        NULL
                        );
        }

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void radio_button<parent_window>::
    set_checked (
    )
    {
        Button_SetCheck(hwnd,TRUE);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void radio_button<parent_window>::
    set_unchecked (
    )
    {
        Button_SetCheck(hwnd,FALSE);
    }
    
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    void radio_button<parent_window>::
    set_name (
        const std::string& name
    )
    {
        SetWindowText(hwnd,name.c_str());
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    bool radio_button<parent_window>::
    is_checked (
    ) const
    {
        if (Button_GetCheck(hwnd) == BST_CHECKED)
            return true;
        else
            return false;
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class password_field function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    const std::string password_field<parent_window>::
    get_text (
    ) const
    {

        int length = Edit_GetTextLength(hwnd);

        std::string temp;
        if (length > 0)
        {
            char* buf = new char[length+1];
            length = Edit_GetText(hwnd,buf,length+1);
            buf[length] = '\0';
            temp = buf;
            delete [] buf;
        }
        return temp;

    }

// ----------------------------------------------------------------------------------------   

    template <
        typename parent_window
        >
    password_field<parent_window>::
    password_field (
        password_field& w
    ) :
        object(w)
    {
        // create the logon button
        hwnd = CreateWindowEx (  WS_EX_CLIENTEDGE,
                    "edit","", WS_CHILD|WS_VISIBLE|ES_PASSWORD ,
                    x,y,width,
                    height,pwindow,0,gui_kernel_1_globals::hInstance,
                    NULL
                    );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));

        SendMessage(hwnd, WM_SETFONT,
            (WPARAM) gui_kernel_1_globals::font,
            MAKELPARAM(TRUE,0) );
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class splitter function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    splitter<parent_window>::
    splitter (
        parent_window& w
    ) :
        object(w),
        mouse_captured(false),
        p(w),
        start(0),
        end(0),
        move_handler(0)
    {
        // create the logon button
        hwnd = CreateWindow (
                    "q3m40698qfv67gb6cd344dm9fwv3847n","", WS_CHILD|WS_VISIBLE,
                    x,y,width,
                    height,pwindow,0,gui_kernel_1_globals::hInstance,
                    NULL
                    );

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG)((LONG_PTR)this));
    }

// ----------------------------------------------------------------------------------------

    template <
        typename parent_window
        >
    LRESULT splitter<parent_window>::
    action (
        HWND, UINT message, WPARAM wParam , LPARAM lParam
    )
    {
     
        switch (message)
        {

        case WM_LBUTTONUP:
            {
                int xPos = GET_X_LPARAM(lParam); 
                int yPos = GET_Y_LPARAM(lParam); 
                
                // if the mouse is no longer in our window and the left
                // mouse button is not down then let it go and we own the
                // mouse then let it go and change the cursor back
                if ( wParam != MK_LBUTTON && mouse_captured &&
                     !(xPos >= x && yPos >= y && xPos-width <= x && yPos-height <= y))
                {
                    ReleaseCapture();
                    mouse_captured = false;
                }
                return 0;
            }

        case WM_MOUSEMOVE:
            {       
                int xPos = GET_X_LPARAM(lParam); 
                int yPos = GET_Y_LPARAM(lParam); 
                
                // if we haven't captured the mouse then do so
                if ( !mouse_captured)
                {
                    SetCapture(hwnd);
                    mouse_captured = true;
                    //change the cursor
                    HCURSOR hCur;
                    if (width > height)
                        hCur = LoadCursor(NULL,IDC_SIZENS);
                    else
                        hCur = LoadCursor(NULL,IDC_SIZEWE);
                    SetCursor(hCur);
                }
                // if the mouse is no longer in our window and the left
                // mouse button is not down then let it go
                else if ( wParam != MK_LBUTTON && 
                     !(xPos >= x && yPos >= y && xPos-width <= x && yPos-height <= y))
                {
                    ReleaseCapture();
                    mouse_captured = false;
                }
                else if (wParam == MK_LBUTTON )
                {
                    if (width > height)
                    {
                        // if it is being dragged outside the range then do nothing
                        if (yPos+y < start )
                            y = start;
                        else if ( yPos+y > end)
                            y = end;                           
                        else
                            y += yPos;   
                    }
                    else
                    {
                        // if it is being dragged outside the range then do nothing
                        if (xPos+x < start )
                            x = start;
                        else if ( xPos+x > end)
                            x = end;                           
                        else
                            x += xPos;  
                    }
                    MoveWindow(hwnd,x,y,width,height,TRUE);
                    if (move_handler)
                        (p.*move_handler)();
                }
                return 0;
            }

            
            
        }
        return DefWindowProc (hwnd, message, wParam, lParam) ;


    }


}


#ifdef NO_MAKEFILE
#include "gui_kernel_1.cpp"
#endif

#endif // DLIB_GUI_KERNEl_1_

