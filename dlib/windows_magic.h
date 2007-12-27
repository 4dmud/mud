// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_WINDOWS_MAGIc_ 
#define DLIB_WINDOWS_MAGIc_ 

// This file contains all the magical #defines you have to setup  before you
// include the windows header files.  

#ifndef NOMINMAX
#define NOMINMAX // prevent windows from messing with std::min and std::max
#endif

#ifndef _WINSOCKAPI_
#define _WINSOCKAPI_   /* Prevent inclusion of winsock.h in windows.h */
#endif

// This is something stupid you have to do to make visual studio include the right
// stuff.  I don't really know what the deal is with this.
#if _WIN32_WINNT < 0x0500
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0500
#endif

#endif // DLIB_WINDOWS_MAGIc_

