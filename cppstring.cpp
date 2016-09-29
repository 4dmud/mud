//
// C++ Implementation: string
//
// Description:
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "config.h"
#include "sysdep.h"
string vformat(const char *fmt, va_list argPtr);


string format(const char* fmt ...) {
  va_list argList;
  va_start(argList, fmt);
  string result = vformat(fmt, argList);
  va_end(argList);

  return result;
}


string vformat(const char *fmt, va_list argPtr) {
    // We draw the line at a 1MB string.
  const int maxSize = 1000000;

    // If the string is less than 161 characters,
    // allocate it on the stack because this saves
    // the malloc/free time.
  const int bufSize = 161;
  char stackBuffer[bufSize];

  int attemptedSize = bufSize - 1;

  int numChars = vsnprintf(stackBuffer, attemptedSize, fmt, argPtr);

  if (numChars >= 0) {
        // Got it on the first try.
    return string(stackBuffer);
  }

    // Now use the heap.
  char* heapBuffer = NULL;

  while ((numChars == -1) && (attemptedSize < maxSize)) {
        // Try a bigger size
    attemptedSize *= 2;
    heapBuffer = (char*)realloc(heapBuffer, attemptedSize + 1);
    numChars = vsnprintf(heapBuffer, attemptedSize, fmt, argPtr);
  }

  string result = string(heapBuffer);

  free(heapBuffer);

  return result;

}
