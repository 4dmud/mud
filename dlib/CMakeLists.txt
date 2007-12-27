#
# This is a CMake makefile.  You can find the cmake utility and
# information about it at http://www.cmake.org
#

# setting this makes CMake allow normal looking IF ELSE statements
SET(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS true)


# make the default build type Debug 
IF(NOT CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE Debug CACHE STRING
      "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
      FORCE)
ENDIF()

# Make sure ENABLE_ASSERTS is defined for debug builds
if (NOT CMAKE_CXX_FLAGS_DEBUG MATCHES "-DENABLE_ASSERTS")
   set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DENABLE_ASSERTS" 
      CACHE STRING "Flags used by the compiler during debug builds." 
      FORCE)
endif ()


OPTION(DLIB_ISO_CPP_ONLY "Enable this if you don't want to compile any non-ISO C++ code (i.e. you don't use any of the API Wrappers)" OFF)


add_library(dlib_console EXCLUDE_FROM_ALL all_console.cpp )


IF (DLIB_ISO_CPP_ONLY)
   ADD_DEFINITIONS(-DDLIB_ISO_CPP_ONLY)	
ELSE ()

   # we want to link to the right stuff depending on our platform.  
   IF (WIN32) ###############################################################################
      set (dlib_console_link ws2_32)
      add_library(dlib_gui EXCLUDE_FROM_ALL all_gui.cpp)
      TARGET_LINK_LIBRARIES(dlib_gui comctl32 ws2_32 )
   ELSEIF(APPLE) ############################################################################
      FIND_LIBRARY(pthreadlib pthread)
      set (dlib_needed_libraries ${pthreadlib})

      FIND_LIBRARY(xlib X11)
      # make sure X11 is in the include path
      FIND_PATH(xlib_path Xlib.h
         PATHS 
         /Developer/SDKs/MacOSX10.4u.sdk/usr/X11R6/include
         PATH_SUFFIXES X11
         )
      if (xlib AND xlib_path)
         add_library(dlib_gui EXCLUDE_FROM_ALL all_gui.cpp)
         GET_FILENAME_COMPONENT(x11_path ${xlib_path} PATH CACHE)
         INCLUDE_DIRECTORIES(${x11_path})
         TARGET_LINK_LIBRARIES(dlib_gui ${dlib_needed_libraries} ${xlib} )
      else()
         message(" ****** DLIB_GUI TARGET DISABLED BECAUSE X11 DEVELOPMENT LIBRARIES NOT FOUND ******")
      endif()

      set(dlib_console_link ${dlib_needed_libraries} )

      MARK_AS_ADVANCED(pthreadlib xlib xlib_path x11_path)
   ELSE () ##################################################################################
      FIND_LIBRARY(pthreadlib pthread)
      set (dlib_needed_libraries ${pthreadlib})

      # link to the nsl library if it exists.  this is something you need sometimes 
      FIND_LIBRARY(nsllib nsl)
      if (nsllib)
         set (dlib_needed_libraries ${dlib_needed_libraries} ${nsllib})
      endif ()

      # link to the socket library if it exists.  this is something you need on solaris
      FIND_LIBRARY(socketlib socket)
      if (socketlib)
         set (dlib_needed_libraries ${dlib_needed_libraries} ${socketlib})
      endif ()

      INCLUDE(FindX11)
      if (X11_FOUND)
         add_library(dlib_gui EXCLUDE_FROM_ALL all_gui.cpp)
         INCLUDE_DIRECTORIES(${X11_INCLUDE_DIR})
         TARGET_LINK_LIBRARIES(dlib_gui ${dlib_needed_libraries} ${X11_LIBRARIES} )
      else()
         message(" ****** DLIB_GUI TARGET DISABLED BECAUSE X11 DEVELOPMENT LIBRARIES NOT FOUND ******")
      endif()

      set(dlib_console_link ${dlib_needed_libraries} )

      MARK_AS_ADVANCED(nsllib pthreadlib socketlib)
   ENDIF () #################################################################################

   TARGET_LINK_LIBRARIES(dlib_console ${dlib_console_link} )

ENDIF ()


#test for some things that really should be true about the compiler
INCLUDE(TestForSTDNamespace)
INCLUDE(TestForANSIStreamHeaders)


