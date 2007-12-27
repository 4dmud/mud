// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#include "../platform.h"

#ifdef WIN32

#include "gui_kernel_1.h"

void winmain(int argc, char** argv);

#include <sstream>
#include <shlobj.h>



namespace dlib
{

// ----------------------------------------------------------------------------------------

    void end_program (
    )
    /*!
        closes all windows and causes the program to terminate
    !*/
    {
        PostQuitMessage (0) ;
    }

// -----------------

    void message_box (
        const std::string& title,
        const std::string& text,
        bool require_answer 
    )
    {
        MessageBox (NULL, TEXT (text.c_str()), 
                      title.c_str(), MB_OK|MB_ICONINFORMATION|
                      (require_answer ? MB_TASKMODAL : 0)) ;
 
    }

// -----------------

    bool question_box (
        const std::string& title,
        const std::string& text,
        bool require_answer 
    )
    {
        if ( MessageBox (NULL, TEXT (text.c_str()), 
                      title.c_str(), MB_YESNO|MB_ICONQUESTION |
                      (require_answer ? MB_TASKMODAL : 0)
                      ) 
                      == IDYES)
        {
            return true;
        }
        else
        {
            return false;
        }
    }


// -----------------

    bool select_directory_box (
        std::string& dir_name
    )
    {
        BROWSEINFO bi;

        bi.hwndOwner = NULL;
        bi.lpszTitle = NULL;
        bi.pidlRoot = NULL;
        char buf[MAX_PATH];
        bi.pszDisplayName = buf;
        bi.iImage = NULL;
        bi.lpfn = NULL;
        bi.ulFlags = 0;

        ITEMIDLIST* list;

        list = SHBrowseForFolder(&bi); 
        if (list == NULL)
            return false;

        if (SHGetPathFromIDList(list,buf) == TRUE)
        {
            try { dir_name = buf; }
            catch (...) { return false;}
            return true;
        }
        else
        {
            return false;
        }

    }

// -----------------

    bool open_file_box (
        std::string& file_name
    )
    {
        OPENFILENAME ofn;       // common dialog box structure
        char szFile[4096];       // buffer for file name
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
        ofn.Flags = OFN_PATHMUSTEXIST |OFN_EXPLORER | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;

        // Display the Open dialog box. 
        if (GetOpenFileName(&ofn)==TRUE)
        {
            file_name = szFile;
            return true;
        }
        else
        {
            return false;
        }
    }

// -----------------

    bool save_file_box (
        std::string& file_name
    )
    {
        OPENFILENAME ofn;       // common dialog box structure
        char szFile[4096];       // buffer for file name
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
        ofn.Flags = OFN_PATHMUSTEXIST |OFN_EXPLORER;

        // Display the Open dialog box. 
        if (GetSaveFileName(&ofn)==TRUE)
        {
            file_name = szFile;
            return true;
        }
        else
        {
            return false;
        }
    }

// -----------------

    void start_program (
    )
    {
        MSG msg ;

        while (GetMessage (&msg, NULL, 0, 0))
            {
            TranslateMessage (&msg) ;
            DispatchMessage (&msg) ;
            }        

    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // the windows procedure and hInstance
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    namespace gui_kernel_1_globals
    {
        HINSTANCE hInstance;

        LOGFONT lf; 
        HFONT font;

        LRESULT CALLBACK WndProc (  
            HWND hwnd, 
            UINT message, 
            WPARAM wParam, 
            LPARAM lParam
        )
        {



            PAINTSTRUCT ps;
            

            switch (message)
            {

            case WM_CTLCOLORSTATIC:
                {
                    object* obj = reinterpret_cast<object*>(GetWindowLongPtr((HWND)lParam,GWLP_USERDATA));
                    if (obj != 0)
                        return obj->action(hwnd,message,wParam,lParam);
                }
                return DefWindowProc (hwnd, message, wParam, lParam);

            case WM_CREATE:


                return 0 ;


            case WM_COMMAND:
                {
                    object* obj = ((object*)GetWindowLongPtr((HWND)lParam,GWLP_USERDATA));
                    if (obj != 0)
                        return obj->action(hwnd,message,wParam,lParam);
                }
                return 0;            

            case WM_PAINT :
                {
                    BeginPaint (hwnd, &ps) ;
                    EndPaint (hwnd, &ps) ;    
                }   
                return 0 ;

            case WM_SIZE:
                {
                    window* win = reinterpret_cast<window*> (
                        GetWindowLongPtr(hwnd,GWLP_USERDATA)
                        );

                    RECT info;
                    GetWindowRect(hwnd,&info);

                    win->x = info.left;
                    win->y = info.top;
                    win->width = info.right - win->x;
                    win->height = info.bottom - win->y;

                    // signal that the window has been resized
                    win->on_window_resized();

                }
                return 0;


            case WM_DESTROY :
                {
                    window* win = reinterpret_cast<window*> (
                        GetWindowLongPtr(hwnd,GWLP_USERDATA)
                        );


                    // signal that the window is being closed
                    win->hwnd = 0;
                    win->on_window_close();
                }
                return DefWindowProc (hwnd, message, wParam, lParam);
            }
            return DefWindowProc (hwnd, message, wParam, lParam) ;



        }

    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class object function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    object::
    object (
        window& w
    ) :
            pwindow(w.hwnd), 
            width(CW_USEDEFAULT),
            height(CW_USEDEFAULT),
            x(CW_USEDEFAULT),
            y(CW_USEDEFAULT)
    {}

// ----------------------------------------------------------------------------------------

    object::
    ~object (
    ) 
    {
        // we don't need to do anything here like call DestroyWindow(hwnd)
        // because this window object will be destroyed along with its parent
        // window when the parent window is destroyed
    }

// ----------------------------------------------------------------------------------------

    void object::
    set_size (
        int width_,
        int height_
    )
    {
        width = width_;
        height = height_;
        MoveWindow(hwnd,x,y,width,height,TRUE);
    }

// ----------------------------------------------------------------------------------------

    void object::
    set_position (
        int x_,
        int y_
    )
    {
        x = x_;
        y = y_;
        MoveWindow(hwnd,x,y,width,height,TRUE);
    }

// ----------------------------------------------------------------------------------------

    void object::
    show (
    )
    {
        ShowWindow (hwnd, SW_SHOW) ;
    }

// ----------------------------------------------------------------------------------------

    void object::
    hide (
    )
    {
        ShowWindow (hwnd, SW_HIDE) ;
    }

// ----------------------------------------------------------------------------------------

    void object::
    enable (
    )
    {
        EnableWindow(hwnd,TRUE);
    }

// ----------------------------------------------------------------------------------------

    void object::
    disable (
    )
    {
        EnableWindow(hwnd,FALSE);
    }

// ----------------------------------------------------------------------------------------

    void object::
    set_focus (
    )
    {
        SetFocus(hwnd);
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // class window function definitions
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    window::
    window (
        bool resizable
    )
    {
        if (resizable)
        {
            // create the window
            hwnd = CreateWindow ("40gMn7wG39N0g46mw3G76nwK03m6sFg", TEXT (""),
                                WS_OVERLAPPEDWINDOW,
                                CW_USEDEFAULT, CW_USEDEFAULT,
                                CW_USEDEFAULT, CW_USEDEFAULT,
                                NULL, NULL, gui_kernel_1_globals::hInstance, NULL) ;
        }
        else
        {
            // create the window
            hwnd = CreateWindow ("40gMn7wG39N0g46mw3G76nwK03m6sFg", TEXT (""),
                                WS_OVERLAPPEDWINDOW^ WS_THICKFRAME ^ WS_MAXIMIZEBOX,
                                CW_USEDEFAULT, CW_USEDEFAULT,
                                CW_USEDEFAULT, CW_USEDEFAULT,
                                NULL, NULL, gui_kernel_1_globals::hInstance, NULL) ;
 
        }


        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG_PTR)this);

 
        RECT info;
        GetWindowRect(hwnd,&info);

        x = info.left;
        y = info.top;
        width = info.right - x;
        height = info.bottom - y;
    }

// ----------------------------------------------------------------------------------------

    window::
    window (
    )
    {
        // create the window
        hwnd = CreateWindow ("40gMn7wG39N0g46mw3G76nwK03m6sFg", TEXT (""),
                            WS_OVERLAPPEDWINDOW^ WS_THICKFRAME ^ WS_MAXIMIZEBOX,
                            CW_USEDEFAULT, CW_USEDEFAULT,
                            CW_USEDEFAULT, CW_USEDEFAULT,
                            NULL, NULL, gui_kernel_1_globals::hInstance, NULL) ;

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG_PTR)this);


        RECT info;
        GetWindowRect(hwnd,&info);

        x = info.left;
        y = info.top;
        width = info.right - x;
        height = info.bottom - y;
    }

// ----------------------------------------------------------------------------------------

    window::
    ~window (
    )
    {
        if (hwnd)
        {
            DestroyWindow(hwnd);
            hwnd = 0;
        }        
    }

// ----------------------------------------------------------------------------------------

    window::
    window (
        const std::string& title
    )
    {
        // create the window
        hwnd = CreateWindow ("40gMn7wG39N0g46mw3G76nwK03m6sFg", title.c_str(),
                            WS_OVERLAPPEDWINDOW,
                            CW_USEDEFAULT, CW_USEDEFAULT,
                            CW_USEDEFAULT, CW_USEDEFAULT,
                            NULL, NULL, gui_kernel_1_globals::hInstance, NULL) ;

        SetWindowLongPtr(hwnd,GWLP_USERDATA,(LONG_PTR)this);

        RECT info;
        GetWindowRect(hwnd,&info);

        x = info.left;
        y = info.top;
        width = info.right - x;
        height = info.bottom - y;

    }

// ----------------------------------------------------------------------------------------

    void window::
    set_focus (
    )
    {
        ShowWindow (hwnd, SW_RESTORE) ;
        SetFocus(hwnd);
    }

// ----------------------------------------------------------------------------------------

    void window::
    show (
    )
    {
        ShowWindow (hwnd, SW_SHOW) ;
    }

// ----------------------------------------------------------------------------------------

    void window::
    hide (
    )
    {
        ShowWindow (hwnd, SW_HIDE) ;
    }

// ----------------------------------------------------------------------------------------

    void window::
    minimize (
    )
    {
        ShowWindow (hwnd, SW_MINIMIZE) ;
    }

// ----------------------------------------------------------------------------------------

    void window::
    maximize (
    )
    {
        ShowWindow (hwnd, SW_MAXIMIZE) ;
    }

// ----------------------------------------------------------------------------------------

    void window::
    set_size (
        int width_,
        int height_
    )
    {
        width = width_;
        height = height_;
        MoveWindow(hwnd,x,y,width,height,TRUE);
    }

// ----------------------------------------------------------------------------------------

    void window::
    set_position (
        int x_,
        int y_
    )
    {
        x = x_;
        y = y_;
        MoveWindow(hwnd,x,y,width,height,TRUE);
    }

// ----------------------------------------------------------------------------------------

    void window::
    set_title (
        const std::string& title
    )
    {
        SetWindowText(hwnd,title.c_str());
    }

// ----------------------------------------------------------------------------------------

    void window::
    get_client_area (
        int& width,
        int& height
    ) const
    {
        RECT r;
        GetClientRect(hwnd,&r);

        width = r.right - r.left;
        height = r.bottom - r.top;
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    // all this function does is pass the event on to the correct
    // splitter object where it is handeled in the action() function
    namespace gui_kernel_1_globals
    {
        LRESULT CALLBACK splitterWinProc_1 (
            HWND hwnd, 
            UINT message, 
            WPARAM wParam, 
            LPARAM lParam
        ) 
        {
            PAINTSTRUCT ps;
            switch (message)
            {
                case WM_LBUTTONUP:
                case WM_MOUSEMOVE:
                    {
                        object* obj = ((object*)GetWindowLongPtr(hwnd,GWLP_USERDATA));
                        if (obj != 0)
                            return obj->action(hwnd,message,wParam,lParam);
                    }
                    return 0;   

                case WM_PAINT :
                    {
                        BeginPaint (hwnd, &ps) ;
                        EndPaint (hwnd, &ps) ;    
                    }   
                    return 0 ;
            }
            return DefWindowProc (hwnd, message, wParam, lParam) ;
        }
    }
}

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // the real WinMain function
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE ,
                    PSTR cmds , int )
    {
        dlib::gui_kernel_1_globals::hInstance = hInstance;

        

        // register the main window class
        TCHAR szAppName[] = TEXT ("40gMn7wG39N0g46mw3G76nwK03m6sFg") ;
        WNDCLASS     wndclass ;

        wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
        wndclass.lpfnWndProc   = dlib::gui_kernel_1_globals::WndProc ;
        wndclass.cbClsExtra    = 0 ;
        wndclass.cbWndExtra    = 0 ;
        wndclass.hInstance     = hInstance ;
        wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
        wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
        wndclass.hbrBackground = (HBRUSH) COLOR_BACKGROUND +1 ;
        wndclass.lpszMenuName  = NULL ;
        wndclass.lpszClassName = szAppName ;

        if (!RegisterClass (&wndclass))  
        {
            MessageBox (NULL, TEXT ("error registering window"), 
                        szAppName, MB_ICONERROR) ;
            return 0 ;
        }






        // register the class used with the splitter object
        TCHAR aszAppName[] = TEXT ("q3m40698qfv67gb6cd344dm9fwv3847n") ;
        WNDCLASS     awndclass ;

        awndclass.style         = CS_HREDRAW | CS_VREDRAW ;
        awndclass.lpfnWndProc   = dlib::gui_kernel_1_globals::splitterWinProc_1 ;
        awndclass.cbClsExtra    = 0 ;
        awndclass.cbWndExtra    = 0 ;
        awndclass.hInstance     = hInstance ;
        awndclass.hIcon         = NULL ;
        awndclass.hCursor       = LoadCursor (NULL, IDC_SIZEALL) ;
        awndclass.hbrBackground = (HBRUSH) COLOR_BACKGROUND +1 ;
        awndclass.lpszMenuName  = NULL ;
        awndclass.lpszClassName = aszAppName ;

        if (!RegisterClass (&awndclass))  
        {
            MessageBox (NULL, TEXT ("error registering window"), 
                        aszAppName, MB_ICONERROR) ;
            return 0 ;
        }




        std::istringstream sin(cmds);
        std::string temp;
        int argc = 0;
        char** argv;

        // find the number of arguments we have
        while (sin)
        {
            ++argc;
            sin >> temp;
        }


        argv = new char*[argc];
        sin.clear();
        sin.str(cmds);

        // put in argv[0]
        argv[0] = new char[5000];
        GetModuleFileName(NULL,argv[0],5000);



        // fill argv with the arguments
        for (int i = 1; i < argc; ++i)
        {
            sin >> temp;


            argv[i] = new char[temp.size()+1];
            // copy string into argv[i]
            for (std::string::size_type j = 0; j < temp.size(); ++j)
            {
                argv[i][j] = temp[j];
            }
            argv[i][temp.size()] = '\0';
        }



         

        // set up the front which will be used by the program
        char tmp[32] = "MS Sans Serif";
        dlib::gui_kernel_1_globals::lf.lfHeight   = 16 ;
        dlib::gui_kernel_1_globals::lf.lfWeight     = FW_NORMAL ;

        for (int i = 0; i < 32; ++i)
            dlib::gui_kernel_1_globals::lf.lfFaceName[i] = tmp[i];

        dlib::gui_kernel_1_globals::lf.lfWidth = 0; 
        dlib::gui_kernel_1_globals::lf.lfEscapement = 0; 
        dlib::gui_kernel_1_globals::lf.lfOrientation= 0; 
        dlib::gui_kernel_1_globals::lf.lfItalic = FALSE; 
        dlib::gui_kernel_1_globals::lf.lfUnderline = FALSE; 
        dlib::gui_kernel_1_globals::lf.lfStrikeOut = FALSE; 
        dlib::gui_kernel_1_globals::lf.lfCharSet = 0; 
        dlib::gui_kernel_1_globals::lf.lfOutPrecision = 0; 
        dlib::gui_kernel_1_globals::lf.lfClipPrecision = 0; 
        dlib::gui_kernel_1_globals::lf.lfQuality = 0; 
        dlib::gui_kernel_1_globals::lf.lfPitchAndFamily = 0; 




        dlib::gui_kernel_1_globals::font = CreateFontIndirect(&dlib::gui_kernel_1_globals::lf);

        // register common controls used
        tagINITCOMMONCONTROLSEX initstuff;
        initstuff.dwSize = sizeof(tagINITCOMMONCONTROLSEX);
        initstuff.dwICC = ICC_PROGRESS_CLASS;
        InitCommonControlsEx(&initstuff);
                


        // call the entry point
        winmain(argc, argv);

        DeleteObject(dlib::gui_kernel_1_globals::font);

        // free the memory used for argv
        for (int i = 0; i < argc; ++i)
            delete [] argv[i];
        delete [] argv;
    

        return 0;

    }

// ----------------------------------------------------------------------------------------

#endif // WIN32

