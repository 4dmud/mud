 
                             dlib C++ library

This is a collection of various classes. Sockets, threading, GUI, 
and directory browsing API. Also many container classes and other 
miscellaneous items such as a big integer and data compression objects.  

Documentation:  
  There is separate HTML (or Windows help files) documentation for
  this library available online and in downloadable archives at 
  http://dclib.sourceforge.net

Install Stuff:
  The first thing to note is that there isn't anything to configure. 
  You just extract the library somewhere, make sure it is in your 
  include path and add its cpp files to your project. If you don't 
  feel like making a project or makefile and your program only contains 
  a single translation unit (i.e. a single cpp file) you can #define 
  NO_MAKEFILE at the top of your program. Doing this will cause all 
  the .cpp files from the library to be included in the headers so 
  you don't have to link them in or anything like that. Or if you 
  don't want to do this you will need to compile the .cpp files and 
  link them in yourself. The easiest way to do this is to compile 
  and link either dlib/all_console.cpp or dlib/all_gui.cpp depending 
  on whether you intend to make a console application or one with a 
  graphical user interface (using the gui_core and gui_widgets 
  components).

  An example makefile that uses this library can be found here: 
  dlib/test/makefile. It is the makefile used to build the regression 
  test suite for this library. There is also a CMake makefile that 
  builds the regression test suite at dlib/test/CMakeLists.txt and 
  another CMake makefile that builds all the example programs at 
  dlib/examples/CMakeLists.txt 

  For further information see the documentation at 
  http://dclib.sourceforge.net

The license for this library can be found in LICENSE.txt


Version: 15.5
Subversion Revision Number: 1452
