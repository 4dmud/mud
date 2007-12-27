// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_ALL_CONSOLe_
#define DLIB_ALL_CONSOLe_

// ISO C++ code
#include "base64/base64_kernel_1.cpp"
#include "bigint/bigint_kernel_1.cpp"
#include "bigint/bigint_kernel_2.cpp"
#include "bit_stream/bit_stream_kernel_1.cpp"
#include "entropy_decoder/entropy_decoder_kernel_1.cpp"
#include "entropy_decoder/entropy_decoder_kernel_2.cpp"
#include "entropy_encoder/entropy_encoder_kernel_1.cpp"
#include "entropy_encoder/entropy_encoder_kernel_2.cpp"
#include "error.cpp"
#include "md5/md5_kernel_1.cpp"
#include "pixel.cpp"
#include "rand/rand_kernel_1.cpp"
#include "rand/rand_kernel_2.cpp"
#include "tokenizer/tokenizer_kernel_1.cpp"

#ifndef DLIB_ISO_CPP_ONLY
// Code that depends on OS specific APIs
#include "dir_nav/dir_nav_kernel_1.cpp"
#include "dir_nav/dir_nav_kernel_2.cpp"
#include "linker/linker_kernel_1.cpp"
#include "logger/extra_logger_headers.cpp"
#include "logger/logger_kernel_1.cpp"
#include "logger/logger_config_file.cpp"
#include "misc_api/misc_api_kernel_1.cpp"
#include "misc_api/misc_api_kernel_2.cpp"
#include "sockets/sockets_extensions.cpp"
#include "sockets/sockets_kernel_1.cpp"
#include "sockets/sockets_kernel_2.cpp"
#include "sockstreambuf/sockstreambuf_kernel_1.cpp"
#include "sockstreambuf/sockstreambuf_kernel_2.cpp"
#include "threads/multithreaded_object_extension.cpp"
#include "threads/threaded_object_extension.cpp"
#include "threads/threads_kernel_1.cpp"
#include "threads/threads_kernel_2.cpp"
#include "threads/threads_kernel_shared.cpp"
#include "timer/timer_kernel_2.cpp"
#endif // DLIB_ISO_CPP_ONLY

#endif // DLIB_ALL_CONSOLe_

