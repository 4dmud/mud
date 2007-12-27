// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_GUi_
#define DLIB_GUi_

#include "platform.h"



#ifdef WIN32
#include "gui/windows.h"

#define SOME_GUI_IMPLEMENTATION_IS_BEING_USED
#endif






#ifdef SOME_GUI_IMPLEMENTATION_IS_BEING_USED
#include "gui/text_box_window.h"
#include "gui/window_factory.h"
#endif 


#endif // DLIB_GUi_

