# This is the makefile used to build the dlib C++ library's regression test suite
# on Debian Linux using the gcc compiler.

# this is the name of the output executable
TARGET = test 

# these are the compile time flags passed to gcc
CFLAGS = -O3 -ggdb  -DDEBUG  -I ../..  -Wall

# These are the link time flags passed to gcc
LFLAGS = -lpthread   -lnsl 

# The name of the compiler.  If you only have one version of
# gcc installed then you probably want to change this to just g++ 
CC = nice g++-4.1

####################################################
####################################################
#  Here we list all the cpp files we want to compile

SRC = main.cpp
SRC += tester.cpp

SRC += ../all_console.cpp 

SRC += array2d.cpp
SRC += array.cpp
SRC += base64.cpp
SRC += bigint.cpp
SRC += binary_search_tree_kernel_1a.cpp
SRC += binary_search_tree_kernel_2a.cpp
SRC += binary_search_tree_mm1.cpp
SRC += binary_search_tree_mm2.cpp
SRC += cmd_line_parser.cpp
SRC += cmd_line_parser_wchar_t.cpp
SRC += compress_stream.cpp
SRC += conditioning_class.cpp
SRC += conditioning_class_c.cpp
SRC += config_reader.cpp
SRC += entropy_coder.cpp
SRC += entropy_encoder_model.cpp
SRC += hash_map.cpp
SRC += hash_set.cpp
SRC += hash_table.cpp
SRC += lz77_buffer.cpp
SRC += map.cpp
SRC += matrix.cpp
SRC += md5.cpp
SRC += member_function_pointer.cpp
SRC += multithreaded_object.cpp
SRC += pipe.cpp
SRC += queue.cpp
SRC += rand.cpp
SRC += reference_counter.cpp
SRC += sequence.cpp
SRC += set.cpp
SRC += sliding_buffer.cpp
SRC += sockets.cpp
SRC += sockstreambuf.cpp
SRC += stack.cpp
SRC += static_map.cpp
SRC += static_set.cpp
SRC += threads.cpp
SRC += timer.cpp
SRC += tokenizer.cpp


####################################################

TMP = $(SRC:.cpp=.o)
OBJ = $(TMP:.c=.o)

$(TARGET): $(OBJ) 
	@echo Linking $@
	@$(CC) $(LFLAGS) $(OBJ) -o $@
	@echo Build Complete

.cpp.o: $<
	@echo Compiling $<
	@$(CC) -c $(CFLAGS) $< -o $@

clean:
	@rm -f $(OBJ) $(TARGET)
	@echo All object files and binaries removed

dep: 
	@echo Running makedepend
	@makedepend -- $(CFLAGS) -- $(SRC) 2> /dev/null 
	@echo Completed makedepend

###############################################################################
##########  Stuff from makedepend                                         #####
##########  type make dep at the command line to rebuild the dependencies #####
##########  Also, DON'T edit the contents of this file beyond this line.  #####
###############################################################################

# DO NOT DELETE

main.o: ../../dlib/cmd_line_parser.h
main.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_1.h ../algs.h
main.o: ../platform.h ../assert.h ../error.h ../interfaces/enumerable.h
main.o: ../interfaces/cmd_line_parser_option.h ../assert.h ../string.h
main.o: ../string/string.h ../error.h ../string/string_abstract.h
main.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_c.h
main.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_abstract.h
main.o: ../../dlib/cmd_line_parser/cmd_line_parser_print_1.h
main.o: ../../dlib/cmd_line_parser/cmd_line_parser_print_abstract.h
main.o: ../../dlib/cmd_line_parser/cmd_line_parser_check_1.h
main.o: ../../dlib/cmd_line_parser/cmd_line_parser_check_c.h ../../dlib/map.h
main.o: ../../dlib/map/map_kernel_1.h ../../dlib/map/map_kernel_abstract.h
main.o: ../interfaces/map_pair.h ../interfaces/remover.h ../serialize.h
main.o: ../algs.h ../uintn.h ../interfaces/enumerable.h
main.o: ../interfaces/map_pair.h ../memory_manager.h
main.o: ../memory_manager/memory_manager_kernel_1.h
main.o: ../memory_manager/memory_manager_kernel_abstract.h
main.o: ../memory_manager/memory_manager_kernel_2.h
main.o: ../memory_manager/memory_manager_kernel_3.h
main.o: ../memory_manager/memory_manager_kernel_2.h
main.o: ../binary_search_tree/binary_search_tree_kernel_2.h
main.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
main.o: ../../dlib/map/map_kernel_c.h binary_search_tree.h
main.o: ../../dlib/memory_manager_global.h
main.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
main.o: ../memory_manager/memory_manager_kernel_abstract.h
main.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
main.o: ../../dlib/memory_manager.h ../../dlib/memory_manager_stateless.h
main.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
main.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
main.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
main.o: ../threads.h ../threads/threads_kernel.h ../platform.h
main.o: ../threads/posix.h ../threads/threads_kernel_2.h
main.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
main.o: /usr/include/features.h /usr/include/sys/cdefs.h
main.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
main.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
main.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
main.o: /usr/include/time.h /usr/include/bits/sched.h /usr/include/signal.h
main.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
main.o: /usr/include/bits/setjmp.h /usr/include/errno.h
main.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
main.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
main.o: /usr/include/asm-generic/errno.h
main.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
main.o: /usr/include/bits/time.h /usr/include/sys/select.h
main.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
main.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
main.o: ../threads/rmutex_extension.h ../threads/rmutex_extension_abstract.h
main.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
main.o: ../binary_search_tree/binary_search_tree_kernel_1.h
main.o: ../binary_search_tree/binary_search_tree_kernel_2.h
main.o: ../binary_search_tree/binary_search_tree_kernel_c.h
main.o: ../member_function_pointer.h
main.o: ../member_function_pointer/member_function_pointer_kernel_1.h
main.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
main.o: ../member_function_pointer/member_function_pointer_kernel_c.h
main.o: ../queue.h ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
main.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
main.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
main.o: ../threads/auto_mutex_extension.h ../threads/auto_unlock_extension.h
main.o: ../threads/auto_unlock_extension_abstract.h
main.o: ../threads/create_new_thread_extension.h
main.o: ../threads/create_new_thread_extension_abstract.h
main.o: ../threads/multithreaded_object_extension.h
main.o: ../threads/multithreaded_object_extension_abstract.h
main.o: ../threads/rsignaler_extension.h
main.o: ../threads/rsignaler_extension_abstract.h ../map.h
main.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
main.o: ../threads/threaded_object_extension.h
main.o: ../threads/threaded_object_extension_abstract.h
main.o: ../threads/thread_specific_data_extension.h
main.o: ../threads/thread_specific_data_extension_abstract.h
main.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
main.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h ../misc_api.h
main.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
main.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
main.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
main.o: ../set/set_kernel_c.h ../set/set_compare_1.h
main.o: ../set/set_compare_abstract.h
main.o: ../../dlib/logger/logger_kernel_abstract.h
main.o: ../../dlib/logger/extra_logger_headers.h
main.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
main.o: ../../dlib/algs.h ../../dlib/sequence.h
main.o: ../../dlib/sequence/sequence_kernel_1.h
main.o: ../../dlib/sequence/sequence_kernel_abstract.h
main.o: ../../dlib/sequence/sequence_kernel_2.h
main.o: ../../dlib/sequence/sequence_kernel_c.h
main.o: ../../dlib/sequence/sequence_compare_1.h
main.o: ../../dlib/sequence/sequence_compare_abstract.h
main.o: ../../dlib/sequence/sequence_sort_1.h
main.o: ../../dlib/sequence/sequence_sort_abstract.h
main.o: ../../dlib/sequence/sequence_sort_2.h ../sort.h ../../dlib/string.h
tester.o: tester.h ../../dlib/map.h ../../dlib/logger.h
tester.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
tester.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
tester.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
tester.o: /usr/include/pthread.h /usr/include/features.h
tester.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
tester.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
tester.o: /usr/include/sched.h /usr/include/bits/types.h
tester.o: /usr/include/bits/typesizes.h /usr/include/time.h
tester.o: /usr/include/bits/sched.h /usr/include/signal.h
tester.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
tester.o: /usr/include/bits/setjmp.h /usr/include/errno.h
tester.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
tester.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
tester.o: /usr/include/asm-generic/errno.h
tester.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
tester.o: /usr/include/bits/time.h /usr/include/sys/select.h
tester.o: /usr/include/bits/select.h ../algs.h ../platform.h ../assert.h
tester.o: ../error.h ../threads/threads_kernel_shared.h
tester.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
tester.o: ../threads/rmutex_extension.h
tester.o: ../threads/rmutex_extension_abstract.h
tester.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
tester.o: ../binary_search_tree/binary_search_tree_kernel_1.h
tester.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
tester.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
tester.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
tester.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
tester.o: ../binary_search_tree/binary_search_tree_kernel_2.h
tester.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
tester.o: ../../dlib/memory_manager.h ../member_function_pointer.h
tester.o: ../member_function_pointer/member_function_pointer_kernel_1.h
tester.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
tester.o: ../member_function_pointer/member_function_pointer_kernel_c.h
tester.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
tester.o: ../memory_manager/memory_manager_kernel_abstract.h
tester.o: ../memory_manager/memory_manager_kernel_2.h
tester.o: ../memory_manager/memory_manager_kernel_3.h
tester.o: ../memory_manager/memory_manager_kernel_2.h
tester.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
tester.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
tester.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
tester.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
tester.o: ../threads/auto_mutex_extension.h
tester.o: ../threads/auto_unlock_extension.h
tester.o: ../threads/auto_unlock_extension_abstract.h
tester.o: ../threads/create_new_thread_extension.h
tester.o: ../threads/create_new_thread_extension_abstract.h
tester.o: ../threads/multithreaded_object_extension.h
tester.o: ../threads/multithreaded_object_extension_abstract.h
tester.o: ../threads/rsignaler_extension.h
tester.o: ../threads/rsignaler_extension_abstract.h ../map.h
tester.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
tester.o: ../threads/threaded_object_extension.h
tester.o: ../threads/threaded_object_extension_abstract.h
tester.o: ../threads/thread_specific_data_extension.h
tester.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
tester.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
tester.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
tester.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
tester.o: ../set/set_kernel_c.h binary_search_tree.h
tester.o: ../../dlib/memory_manager_global.h
tester.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
tester.o: ../memory_manager/memory_manager_kernel_abstract.h
tester.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
tester.o: ../../dlib/memory_manager_stateless.h
tester.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
tester.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
tester.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
tester.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
tester.o: ../set/set_compare_abstract.h
tester.o: ../../dlib/logger/logger_kernel_abstract.h
tester.o: ../../dlib/logger/extra_logger_headers.h
tester.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
tester.o: ../../dlib/algs.h
../all_console.o: ../base64/base64_kernel_1.cpp ../base64/base64_kernel_1.h
../all_console.o: ../algs.h ../platform.h ../assert.h ../error.h
../all_console.o: ../bigint/bigint_kernel_1.cpp ../bigint/bigint_kernel_1.h
../all_console.o: ../bigint/bigint_kernel_abstract.h ../serialize.h ../algs.h
../all_console.o: ../uintn.h ../interfaces/enumerable.h
../all_console.o: ../interfaces/map_pair.h ../uintn.h
../all_console.o: ../bigint/bigint_kernel_2.cpp ../bigint/bigint_kernel_2.h
../all_console.o: ../bit_stream/bit_stream_kernel_1.cpp
../all_console.o: ../bit_stream/bit_stream_kernel_1.h
../all_console.o: ../bit_stream/bit_stream_kernel_abstract.h
../all_console.o: ../entropy_decoder/entropy_decoder_kernel_1.cpp
../all_console.o: ../entropy_decoder/entropy_decoder_kernel_1.h
../all_console.o: ../entropy_decoder/entropy_decoder_kernel_abstract.h
../all_console.o: ../assert.h ../entropy_decoder/entropy_decoder_kernel_2.cpp
../all_console.o: ../entropy_decoder/entropy_decoder_kernel_2.h
../all_console.o: ../entropy_encoder/entropy_encoder_kernel_1.cpp
../all_console.o: ../entropy_encoder/entropy_encoder_kernel_1.h
../all_console.o: ../entropy_encoder/entropy_encoder_kernel_abstract.h
../all_console.o: ../entropy_encoder/entropy_encoder_kernel_2.cpp
../all_console.o: ../entropy_encoder/entropy_encoder_kernel_2.h ../error.cpp
../all_console.o: ../md5/md5_kernel_1.cpp ../md5/md5_kernel_1.h
../all_console.o: ../md5/md5_kernel_abstract.h ../pixel.cpp ../pixel.h
../all_console.o: ../serialize.h ../rand/rand_kernel_1.cpp ../md5.h
../all_console.o: ../md5/md5_kernel_1.h ../rand/rand_kernel_1.h
../all_console.o: ../rand/rand_kernel_abstract.h ../rand/rand_kernel_2.cpp
../all_console.o: ../rand/rand_kernel_2.h ../tokenizer/tokenizer_kernel_1.cpp
../all_console.o: ../tokenizer/tokenizer_kernel_1.h
../all_console.o: ../tokenizer/tokenizer_kernel_abstract.h
../all_console.o: ../dir_nav/dir_nav_kernel_1.cpp ../platform.h
../all_console.o: ../dir_nav/dir_nav_kernel_2.cpp
../all_console.o: ../dir_nav/dir_nav_kernel_2.h
../all_console.o: ../dir_nav/dir_nav_kernel_abstract.h
../all_console.o: /usr/include/sys/types.h /usr/include/features.h
../all_console.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
../all_console.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
../all_console.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
../all_console.o: /usr/include/time.h /usr/include/endian.h
../all_console.o: /usr/include/bits/endian.h /usr/include/sys/select.h
../all_console.o: /usr/include/bits/select.h /usr/include/bits/sigset.h
../all_console.o: /usr/include/bits/time.h /usr/include/sys/sysmacros.h
../all_console.o: /usr/include/bits/pthreadtypes.h /usr/include/dirent.h
../all_console.o: /usr/include/bits/dirent.h /usr/include/bits/posix1_lim.h
../all_console.o: /usr/include/bits/local_lim.h /usr/include/linux/limits.h
../all_console.o: /usr/include/libgen.h /usr/include/unistd.h
../all_console.o: /usr/include/bits/posix_opt.h /usr/include/bits/confname.h
../all_console.o: /usr/include/getopt.h /usr/include/sys/stat.h
../all_console.o: /usr/include/bits/stat.h /usr/include/errno.h
../all_console.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
../all_console.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
../all_console.o: /usr/include/asm-generic/errno.h
../all_console.o: /usr/include/asm-generic/errno-base.h /usr/include/stdlib.h
../all_console.o: /usr/include/alloca.h ../linker/linker_kernel_1.cpp
../all_console.o: ../linker/linker_kernel_1.h
../all_console.o: ../linker/linker_kernel_abstract.h ../threads.h
../all_console.o: ../threads/threads_kernel.h ../threads/posix.h
../all_console.o: ../threads/threads_kernel_2.h
../all_console.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
../all_console.o: /usr/include/sched.h /usr/include/bits/sched.h
../all_console.o: /usr/include/signal.h /usr/include/bits/setjmp.h
../all_console.o: /usr/include/sys/time.h ../threads/threads_kernel_shared.h
../all_console.o: ../threads/auto_mutex_extension.h
../all_console.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
../all_console.o: ../threads/rmutex_extension_abstract.h
../all_console.o: ../threads/auto_mutex_extension_abstract.h
../all_console.o: ../binary_search_tree.h
../all_console.o: ../binary_search_tree/binary_search_tree_kernel_1.h
../all_console.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
../all_console.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
../all_console.o: ../interfaces/remover.h
../all_console.o: ../binary_search_tree/binary_search_tree_kernel_2.h
../all_console.o: ../binary_search_tree/binary_search_tree_kernel_c.h
../all_console.o: ../../dlib/memory_manager.h ../member_function_pointer.h
../all_console.o: ../member_function_pointer/member_function_pointer_kernel_1.h
../all_console.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
../all_console.o: ../member_function_pointer/member_function_pointer_kernel_c.h
../all_console.o: ../memory_manager.h
../all_console.o: ../memory_manager/memory_manager_kernel_1.h
../all_console.o: ../memory_manager/memory_manager_kernel_abstract.h
../all_console.o: ../memory_manager/memory_manager_kernel_2.h
../all_console.o: ../memory_manager/memory_manager_kernel_3.h
../all_console.o: ../memory_manager/memory_manager_kernel_2.h
../all_console.o: ../binary_search_tree/binary_search_tree_kernel_2.h
../all_console.o: ../queue.h ../queue/queue_kernel_1.h
../all_console.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
../all_console.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
../all_console.o: ../queue/queue_sort_abstract.h
../all_console.o: ../threads/auto_mutex_extension.h
../all_console.o: ../threads/auto_unlock_extension.h
../all_console.o: ../threads/auto_unlock_extension_abstract.h
../all_console.o: ../threads/create_new_thread_extension.h
../all_console.o: ../threads/create_new_thread_extension_abstract.h
../all_console.o: ../threads/multithreaded_object_extension.h
../all_console.o: ../threads/multithreaded_object_extension_abstract.h
../all_console.o: ../threads/rsignaler_extension.h
../all_console.o: ../threads/rsignaler_extension_abstract.h ../map.h
../all_console.o: ../threads/rmutex_extension.h
../all_console.o: ../threads/rsignaler_extension.h
../all_console.o: ../threads/threaded_object_extension.h
../all_console.o: ../threads/threaded_object_extension_abstract.h
../all_console.o: ../threads/thread_specific_data_extension.h
../all_console.o: ../threads/thread_specific_data_extension_abstract.h
../all_console.o: ../sockets.h ../sockets/posix.h
../all_console.o: ../sockets/sockets_kernel_2.h
../all_console.o: ../sockets/sockets_kernel_abstract.h
../all_console.o: /usr/include/sys/socket.h /usr/include/sys/uio.h
../all_console.o: /usr/include/bits/uio.h /usr/include/bits/socket.h
../all_console.o: /usr/include/limits.h /usr/include/bits/posix2_lim.h
../all_console.o: /usr/include/bits/sockaddr.h /usr/include/asm/socket.h
../all_console.o: /usr/include/asm-i386/socket.h /usr/include/asm/sockios.h
../all_console.o: /usr/include/asm-i386/sockios.h /usr/include/arpa/inet.h
../all_console.o: /usr/include/netinet/in.h /usr/include/stdint.h
../all_console.o: /usr/include/bits/wchar.h /usr/include/bits/in.h
../all_console.o: /usr/include/bits/byteswap.h /usr/include/inttypes.h
../all_console.o: /usr/include/netdb.h /usr/include/rpc/netdb.h
../all_console.o: /usr/include/bits/netdb.h /usr/include/sys/param.h
../all_console.o: /usr/include/linux/param.h /usr/include/asm/param.h
../all_console.o: /usr/include/asm-i386/param.h
../all_console.o: ../sockets/sockets_extensions.h
../all_console.o: ../sockets/sockets_extensions_abstract.h
../all_console.o: ../logger/extra_logger_headers.cpp
../all_console.o: ../logger/extra_logger_headers.h
../all_console.o: ../../dlib/logger/logger_kernel_abstract.h
../all_console.o: ../../dlib/logger/logger_kernel_1.h
../all_console.o: ../logger/logger_kernel_1.cpp
../all_console.o: ../misc_api/misc_api_kernel_1.cpp
../all_console.o: ../misc_api/misc_api_kernel_2.cpp
../all_console.o: ../misc_api/misc_api_kernel_2.h
../all_console.o: ../misc_api/misc_api_kernel_abstract.h
../all_console.o: ../sockets/sockets_extensions.cpp ../error.h
../all_console.o: ../sockets/sockets_extensions.h ../timer.h
../all_console.o: ../timer/timer_kernel_1.h ../misc_api.h ../misc_api/posix.h
../all_console.o: ../timer/timer_kernel_abstract.h ../timer/timer_kernel_2.h
../all_console.o: ../timeout.h ../timeout/timeout_kernel_1.h
../all_console.o: ../timeout/timeout_kernel_abstract.h
../all_console.o: ../sockets/sockets_kernel_1.cpp
../all_console.o: ../sockets/sockets_kernel_2.cpp /usr/include/fcntl.h
../all_console.o: /usr/include/bits/fcntl.h
../all_console.o: ../sockstreambuf/sockstreambuf_kernel_1.cpp
../all_console.o: ../sockstreambuf/sockstreambuf_kernel_1.h
../all_console.o: ../sockstreambuf/sockstreambuf_kernel_abstract.h
../all_console.o: ../sockstreambuf/sockstreambuf_kernel_2.cpp
../all_console.o: ../sockstreambuf/sockstreambuf_kernel_2.h
../all_console.o: ../threads/multithreaded_object_extension.cpp
../all_console.o: ../threads/multithreaded_object_extension.h
../all_console.o: ../threads/threaded_object_extension.cpp
../all_console.o: ../threads/threaded_object_extension.h
../all_console.o: ../threads/threads_kernel_1.cpp
../all_console.o: ../threads/threads_kernel_2.cpp
../all_console.o: ../threads/threads_kernel_shared.cpp
../all_console.o: ../timer/timer_kernel_2.cpp ../timer/timer_kernel_2.h
array2d.o: ../../dlib/interfaces/enumerable.h ../../dlib/array2d.h
array2d.o: ../../dlib/array2d/array2d_kernel_1.h
array2d.o: ../../dlib/array2d/array2d_kernel_abstract.h ../algs.h
array2d.o: ../platform.h ../assert.h ../error.h ../interfaces/enumerable.h
array2d.o: ../serialize.h ../algs.h ../uintn.h ../interfaces/enumerable.h
array2d.o: ../interfaces/map_pair.h ../memory_manager.h
array2d.o: ../memory_manager/memory_manager_kernel_1.h
array2d.o: ../memory_manager/memory_manager_kernel_abstract.h ../assert.h
array2d.o: ../memory_manager/memory_manager_kernel_2.h
array2d.o: ../memory_manager/memory_manager_kernel_3.h
array2d.o: ../memory_manager/memory_manager_kernel_2.h
array2d.o: ../binary_search_tree/binary_search_tree_kernel_2.h
array2d.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
array2d.o: ../interfaces/map_pair.h ../interfaces/remover.h
array2d.o: ../../dlib/array2d/array2d_kernel_c.h ../../dlib/memory_manager.h
array2d.o: tester.h ../../dlib/map.h ../../dlib/logger.h
array2d.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
array2d.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
array2d.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
array2d.o: /usr/include/pthread.h /usr/include/features.h
array2d.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
array2d.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
array2d.o: /usr/include/sched.h /usr/include/bits/types.h
array2d.o: /usr/include/bits/typesizes.h /usr/include/time.h
array2d.o: /usr/include/bits/sched.h /usr/include/signal.h
array2d.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
array2d.o: /usr/include/bits/setjmp.h /usr/include/errno.h
array2d.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
array2d.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
array2d.o: /usr/include/asm-generic/errno.h
array2d.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
array2d.o: /usr/include/bits/time.h /usr/include/sys/select.h
array2d.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
array2d.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
array2d.o: ../threads/rmutex_extension.h
array2d.o: ../threads/rmutex_extension_abstract.h
array2d.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
array2d.o: ../binary_search_tree/binary_search_tree_kernel_1.h
array2d.o: ../binary_search_tree/binary_search_tree_kernel_2.h
array2d.o: ../binary_search_tree/binary_search_tree_kernel_c.h
array2d.o: ../member_function_pointer.h
array2d.o: ../member_function_pointer/member_function_pointer_kernel_1.h
array2d.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
array2d.o: ../member_function_pointer/member_function_pointer_kernel_c.h
array2d.o: ../queue.h ../queue/queue_kernel_1.h
array2d.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
array2d.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
array2d.o: ../queue/queue_sort_abstract.h ../threads/auto_mutex_extension.h
array2d.o: ../threads/auto_unlock_extension.h
array2d.o: ../threads/auto_unlock_extension_abstract.h
array2d.o: ../threads/create_new_thread_extension.h
array2d.o: ../threads/create_new_thread_extension_abstract.h
array2d.o: ../threads/multithreaded_object_extension.h
array2d.o: ../threads/multithreaded_object_extension_abstract.h
array2d.o: ../threads/rsignaler_extension.h
array2d.o: ../threads/rsignaler_extension_abstract.h ../map.h
array2d.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
array2d.o: ../threads/threaded_object_extension.h
array2d.o: ../threads/threaded_object_extension_abstract.h
array2d.o: ../threads/thread_specific_data_extension.h
array2d.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
array2d.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
array2d.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
array2d.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
array2d.o: ../set/set_kernel_c.h binary_search_tree.h
array2d.o: ../../dlib/memory_manager_global.h
array2d.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
array2d.o: ../memory_manager/memory_manager_kernel_abstract.h
array2d.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
array2d.o: ../../dlib/memory_manager_stateless.h
array2d.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
array2d.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
array2d.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
array2d.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
array2d.o: ../set/set_compare_abstract.h
array2d.o: ../../dlib/logger/logger_kernel_abstract.h
array2d.o: ../../dlib/logger/extra_logger_headers.h
array2d.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
array2d.o: ../../dlib/algs.h
array.o: ../../dlib/interfaces/enumerable.h ../../dlib/array.h
array.o: ../../dlib/array/array_kernel_1.h
array.o: ../../dlib/array/array_kernel_abstract.h ../interfaces/enumerable.h
array.o: ../algs.h ../platform.h ../assert.h ../error.h ../serialize.h
array.o: ../algs.h ../uintn.h ../interfaces/enumerable.h
array.o: ../interfaces/map_pair.h ../memory_manager.h
array.o: ../memory_manager/memory_manager_kernel_1.h
array.o: ../memory_manager/memory_manager_kernel_abstract.h ../assert.h
array.o: ../memory_manager/memory_manager_kernel_2.h
array.o: ../memory_manager/memory_manager_kernel_3.h
array.o: ../memory_manager/memory_manager_kernel_2.h
array.o: ../binary_search_tree/binary_search_tree_kernel_2.h
array.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
array.o: ../interfaces/map_pair.h ../interfaces/remover.h
array.o: ../../dlib/array/array_kernel_2.h ../../dlib/array/array_kernel_c.h
array.o: ../../dlib/array/array_sort_1.h
array.o: ../../dlib/array/array_sort_abstract.h ../sort.h
array.o: ../../dlib/array/array_sort_2.h ../../dlib/array/array_expand_1.h
array.o: ../../dlib/memory_manager.h tester.h ../../dlib/map.h
array.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h ../threads.h
array.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
array.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
array.o: /usr/include/pthread.h /usr/include/features.h
array.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
array.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
array.o: /usr/include/sched.h /usr/include/bits/types.h
array.o: /usr/include/bits/typesizes.h /usr/include/time.h
array.o: /usr/include/bits/sched.h /usr/include/signal.h
array.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
array.o: /usr/include/bits/setjmp.h /usr/include/errno.h
array.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
array.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
array.o: /usr/include/asm-generic/errno.h
array.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
array.o: /usr/include/bits/time.h /usr/include/sys/select.h
array.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
array.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
array.o: ../threads/rmutex_extension.h ../threads/rmutex_extension_abstract.h
array.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
array.o: ../binary_search_tree/binary_search_tree_kernel_1.h
array.o: ../binary_search_tree/binary_search_tree_kernel_2.h
array.o: ../binary_search_tree/binary_search_tree_kernel_c.h
array.o: ../member_function_pointer.h
array.o: ../member_function_pointer/member_function_pointer_kernel_1.h
array.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
array.o: ../member_function_pointer/member_function_pointer_kernel_c.h
array.o: ../queue.h ../queue/queue_kernel_1.h
array.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
array.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
array.o: ../queue/queue_sort_abstract.h ../threads/auto_mutex_extension.h
array.o: ../threads/auto_unlock_extension.h
array.o: ../threads/auto_unlock_extension_abstract.h
array.o: ../threads/create_new_thread_extension.h
array.o: ../threads/create_new_thread_extension_abstract.h
array.o: ../threads/multithreaded_object_extension.h
array.o: ../threads/multithreaded_object_extension_abstract.h
array.o: ../threads/rsignaler_extension.h
array.o: ../threads/rsignaler_extension_abstract.h ../map.h
array.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
array.o: ../threads/threaded_object_extension.h
array.o: ../threads/threaded_object_extension_abstract.h
array.o: ../threads/thread_specific_data_extension.h
array.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
array.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
array.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
array.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
array.o: ../set/set_kernel_c.h binary_search_tree.h
array.o: ../../dlib/memory_manager_global.h
array.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
array.o: ../memory_manager/memory_manager_kernel_abstract.h
array.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
array.o: ../../dlib/memory_manager_stateless.h
array.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
array.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
array.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
array.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
array.o: ../set/set_compare_abstract.h
array.o: ../../dlib/logger/logger_kernel_abstract.h
array.o: ../../dlib/logger/extra_logger_headers.h
array.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
array.o: ../../dlib/algs.h
base64.o: ../../dlib/base64.h ../../dlib/base64/base64_kernel_1.h ../algs.h
base64.o: ../platform.h ../assert.h ../error.h tester.h ../../dlib/map.h
base64.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
base64.o: ../threads.h ../threads/threads_kernel.h ../platform.h
base64.o: ../threads/posix.h ../threads/threads_kernel_2.h
base64.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
base64.o: /usr/include/features.h /usr/include/sys/cdefs.h
base64.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
base64.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
base64.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
base64.o: /usr/include/time.h /usr/include/bits/sched.h /usr/include/signal.h
base64.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
base64.o: /usr/include/bits/setjmp.h /usr/include/errno.h
base64.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
base64.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
base64.o: /usr/include/asm-generic/errno.h
base64.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
base64.o: /usr/include/bits/time.h /usr/include/sys/select.h
base64.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
base64.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
base64.o: ../threads/rmutex_extension.h
base64.o: ../threads/rmutex_extension_abstract.h
base64.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
base64.o: ../binary_search_tree/binary_search_tree_kernel_1.h
base64.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
base64.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
base64.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
base64.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
base64.o: ../binary_search_tree/binary_search_tree_kernel_2.h
base64.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
base64.o: ../../dlib/memory_manager.h ../member_function_pointer.h
base64.o: ../member_function_pointer/member_function_pointer_kernel_1.h
base64.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
base64.o: ../member_function_pointer/member_function_pointer_kernel_c.h
base64.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
base64.o: ../memory_manager/memory_manager_kernel_abstract.h
base64.o: ../memory_manager/memory_manager_kernel_2.h
base64.o: ../memory_manager/memory_manager_kernel_3.h
base64.o: ../memory_manager/memory_manager_kernel_2.h
base64.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
base64.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
base64.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
base64.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
base64.o: ../threads/auto_mutex_extension.h
base64.o: ../threads/auto_unlock_extension.h
base64.o: ../threads/auto_unlock_extension_abstract.h
base64.o: ../threads/create_new_thread_extension.h
base64.o: ../threads/create_new_thread_extension_abstract.h
base64.o: ../threads/multithreaded_object_extension.h
base64.o: ../threads/multithreaded_object_extension_abstract.h
base64.o: ../threads/rsignaler_extension.h
base64.o: ../threads/rsignaler_extension_abstract.h ../map.h
base64.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
base64.o: ../threads/threaded_object_extension.h
base64.o: ../threads/threaded_object_extension_abstract.h
base64.o: ../threads/thread_specific_data_extension.h
base64.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
base64.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
base64.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
base64.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
base64.o: ../set/set_kernel_c.h binary_search_tree.h
base64.o: ../../dlib/memory_manager_global.h
base64.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
base64.o: ../memory_manager/memory_manager_kernel_abstract.h
base64.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
base64.o: ../../dlib/memory_manager_stateless.h
base64.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
base64.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
base64.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
base64.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
base64.o: ../set/set_compare_abstract.h
base64.o: ../../dlib/logger/logger_kernel_abstract.h
base64.o: ../../dlib/logger/extra_logger_headers.h
base64.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
base64.o: ../../dlib/algs.h
bigint.o: ../../dlib/bigint.h ../../dlib/bigint/bigint_kernel_1.h
bigint.o: ../bigint/bigint_kernel_abstract.h ../algs.h ../platform.h
bigint.o: ../assert.h ../error.h ../serialize.h ../algs.h ../uintn.h
bigint.o: ../interfaces/enumerable.h ../interfaces/map_pair.h ../uintn.h
bigint.o: ../../dlib/bigint/bigint_kernel_2.h
bigint.o: ../../dlib/bigint/bigint_kernel_c.h ../assert.h tester.h
bigint.o: ../../dlib/map.h ../../dlib/logger.h
bigint.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
bigint.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
bigint.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
bigint.o: /usr/include/pthread.h /usr/include/features.h
bigint.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
bigint.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
bigint.o: /usr/include/sched.h /usr/include/bits/types.h
bigint.o: /usr/include/bits/typesizes.h /usr/include/time.h
bigint.o: /usr/include/bits/sched.h /usr/include/signal.h
bigint.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
bigint.o: /usr/include/bits/setjmp.h /usr/include/errno.h
bigint.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
bigint.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
bigint.o: /usr/include/asm-generic/errno.h
bigint.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
bigint.o: /usr/include/bits/time.h /usr/include/sys/select.h
bigint.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
bigint.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
bigint.o: ../threads/rmutex_extension.h
bigint.o: ../threads/rmutex_extension_abstract.h
bigint.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
bigint.o: ../binary_search_tree/binary_search_tree_kernel_1.h
bigint.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
bigint.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
bigint.o: ../interfaces/remover.h
bigint.o: ../binary_search_tree/binary_search_tree_kernel_2.h
bigint.o: ../binary_search_tree/binary_search_tree_kernel_c.h
bigint.o: ../../dlib/memory_manager.h ../member_function_pointer.h
bigint.o: ../member_function_pointer/member_function_pointer_kernel_1.h
bigint.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
bigint.o: ../member_function_pointer/member_function_pointer_kernel_c.h
bigint.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
bigint.o: ../memory_manager/memory_manager_kernel_abstract.h
bigint.o: ../memory_manager/memory_manager_kernel_2.h
bigint.o: ../memory_manager/memory_manager_kernel_3.h
bigint.o: ../memory_manager/memory_manager_kernel_2.h
bigint.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
bigint.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
bigint.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
bigint.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
bigint.o: ../threads/auto_mutex_extension.h
bigint.o: ../threads/auto_unlock_extension.h
bigint.o: ../threads/auto_unlock_extension_abstract.h
bigint.o: ../threads/create_new_thread_extension.h
bigint.o: ../threads/create_new_thread_extension_abstract.h
bigint.o: ../threads/multithreaded_object_extension.h
bigint.o: ../threads/multithreaded_object_extension_abstract.h
bigint.o: ../threads/rsignaler_extension.h
bigint.o: ../threads/rsignaler_extension_abstract.h ../map.h
bigint.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
bigint.o: ../threads/threaded_object_extension.h
bigint.o: ../threads/threaded_object_extension_abstract.h
bigint.o: ../threads/thread_specific_data_extension.h
bigint.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
bigint.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
bigint.o: ../misc_api/misc_api_kernel_abstract.h ../set.h
bigint.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
bigint.o: ../set/set_kernel_c.h binary_search_tree.h
bigint.o: ../../dlib/memory_manager_global.h
bigint.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
bigint.o: ../memory_manager/memory_manager_kernel_abstract.h
bigint.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
bigint.o: ../../dlib/memory_manager_stateless.h
bigint.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
bigint.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
bigint.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
bigint.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
bigint.o: ../set/set_compare_abstract.h
bigint.o: ../../dlib/logger/logger_kernel_abstract.h
bigint.o: ../../dlib/logger/extra_logger_headers.h
bigint.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
bigint.o: ../../dlib/algs.h
binary_search_tree_kernel_1a.o: ../../dlib/memory_manager_global.h
binary_search_tree_kernel_1a.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
binary_search_tree_kernel_1a.o: ../algs.h ../platform.h ../assert.h
binary_search_tree_kernel_1a.o: ../error.h
binary_search_tree_kernel_1a.o: ../memory_manager/memory_manager_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../../dlib/memory_manager.h
binary_search_tree_kernel_1a.o: ../../dlib/memory_manager_stateless.h
binary_search_tree_kernel_1a.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
binary_search_tree_kernel_1a.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
binary_search_tree_kernel_1a.o: ../threads.h ../threads/threads_kernel.h
binary_search_tree_kernel_1a.o: ../platform.h ../threads/posix.h
binary_search_tree_kernel_1a.o: ../threads/threads_kernel_2.h
binary_search_tree_kernel_1a.o: ../threads/threads_kernel_abstract.h
binary_search_tree_kernel_1a.o: /usr/include/pthread.h
binary_search_tree_kernel_1a.o: /usr/include/features.h
binary_search_tree_kernel_1a.o: /usr/include/sys/cdefs.h
binary_search_tree_kernel_1a.o: /usr/include/bits/wordsize.h
binary_search_tree_kernel_1a.o: /usr/include/gnu/stubs.h
binary_search_tree_kernel_1a.o: /usr/include/gnu/stubs-32.h
binary_search_tree_kernel_1a.o: /usr/include/sched.h
binary_search_tree_kernel_1a.o: /usr/include/bits/types.h
binary_search_tree_kernel_1a.o: /usr/include/bits/typesizes.h
binary_search_tree_kernel_1a.o: /usr/include/time.h /usr/include/bits/sched.h
binary_search_tree_kernel_1a.o: /usr/include/signal.h
binary_search_tree_kernel_1a.o: /usr/include/bits/sigset.h
binary_search_tree_kernel_1a.o: /usr/include/bits/pthreadtypes.h
binary_search_tree_kernel_1a.o: /usr/include/bits/setjmp.h
binary_search_tree_kernel_1a.o: /usr/include/errno.h
binary_search_tree_kernel_1a.o: /usr/include/bits/errno.h
binary_search_tree_kernel_1a.o: /usr/include/linux/errno.h
binary_search_tree_kernel_1a.o: /usr/include/asm/errno.h
binary_search_tree_kernel_1a.o: /usr/include/asm-i386/errno.h
binary_search_tree_kernel_1a.o: /usr/include/asm-generic/errno.h
binary_search_tree_kernel_1a.o: /usr/include/asm-generic/errno-base.h
binary_search_tree_kernel_1a.o: /usr/include/sys/time.h
binary_search_tree_kernel_1a.o: /usr/include/bits/time.h
binary_search_tree_kernel_1a.o: /usr/include/sys/select.h
binary_search_tree_kernel_1a.o: /usr/include/bits/select.h
binary_search_tree_kernel_1a.o: ../threads/threads_kernel_shared.h
binary_search_tree_kernel_1a.o: ../threads/auto_mutex_extension.h
binary_search_tree_kernel_1a.o: ../threads/threads_kernel.h
binary_search_tree_kernel_1a.o: ../threads/rmutex_extension.h
binary_search_tree_kernel_1a.o: ../threads/rmutex_extension_abstract.h
binary_search_tree_kernel_1a.o: ../threads/auto_mutex_extension_abstract.h
binary_search_tree_kernel_1a.o: ../binary_search_tree.h
binary_search_tree_kernel_1a.o: ../binary_search_tree/binary_search_tree_kernel_1.h
binary_search_tree_kernel_1a.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../interfaces/map_pair.h
binary_search_tree_kernel_1a.o: ../interfaces/enumerable.h
binary_search_tree_kernel_1a.o: ../interfaces/remover.h ../serialize.h
binary_search_tree_kernel_1a.o: ../algs.h ../uintn.h
binary_search_tree_kernel_1a.o: ../interfaces/enumerable.h
binary_search_tree_kernel_1a.o: ../interfaces/map_pair.h
binary_search_tree_kernel_1a.o: ../binary_search_tree/binary_search_tree_kernel_2.h
binary_search_tree_kernel_1a.o: ../binary_search_tree/binary_search_tree_kernel_c.h
binary_search_tree_kernel_1a.o: ../assert.h ../member_function_pointer.h
binary_search_tree_kernel_1a.o: ../member_function_pointer/member_function_pointer_kernel_1.h
binary_search_tree_kernel_1a.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../member_function_pointer/member_function_pointer_kernel_c.h
binary_search_tree_kernel_1a.o: ../memory_manager.h
binary_search_tree_kernel_1a.o: ../memory_manager/memory_manager_kernel_1.h
binary_search_tree_kernel_1a.o: ../memory_manager/memory_manager_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../memory_manager/memory_manager_kernel_2.h
binary_search_tree_kernel_1a.o: ../memory_manager/memory_manager_kernel_3.h
binary_search_tree_kernel_1a.o: ../memory_manager/memory_manager_kernel_2.h
binary_search_tree_kernel_1a.o: ../binary_search_tree/binary_search_tree_kernel_2.h
binary_search_tree_kernel_1a.o: ../queue.h ../queue/queue_kernel_1.h
binary_search_tree_kernel_1a.o: ../queue/queue_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../queue/queue_kernel_2.h
binary_search_tree_kernel_1a.o: ../queue/queue_kernel_c.h
binary_search_tree_kernel_1a.o: ../queue/queue_sort_1.h
binary_search_tree_kernel_1a.o: ../queue/queue_sort_abstract.h
binary_search_tree_kernel_1a.o: ../threads/auto_mutex_extension.h
binary_search_tree_kernel_1a.o: ../threads/auto_unlock_extension.h
binary_search_tree_kernel_1a.o: ../threads/auto_unlock_extension_abstract.h
binary_search_tree_kernel_1a.o: ../threads/create_new_thread_extension.h
binary_search_tree_kernel_1a.o: ../threads/create_new_thread_extension_abstract.h
binary_search_tree_kernel_1a.o: ../threads/multithreaded_object_extension.h
binary_search_tree_kernel_1a.o: ../threads/multithreaded_object_extension_abstract.h
binary_search_tree_kernel_1a.o: ../threads/rsignaler_extension.h
binary_search_tree_kernel_1a.o: ../threads/rsignaler_extension_abstract.h
binary_search_tree_kernel_1a.o: ../map.h ../threads/rmutex_extension.h
binary_search_tree_kernel_1a.o: ../threads/rsignaler_extension.h
binary_search_tree_kernel_1a.o: ../threads/threaded_object_extension.h
binary_search_tree_kernel_1a.o: ../threads/threaded_object_extension_abstract.h
binary_search_tree_kernel_1a.o: ../threads/thread_specific_data_extension.h
binary_search_tree_kernel_1a.o: ../threads/thread_specific_data_extension_abstract.h
binary_search_tree_kernel_1a.o: ../../dlib/binary_search_tree.h tester.h
binary_search_tree_kernel_1a.o: ../../dlib/map.h ../../dlib/logger.h
binary_search_tree_kernel_1a.o: ../../dlib/logger/logger_kernel_1.h
binary_search_tree_kernel_1a.o: ../misc_api.h ../misc_api/posix.h
binary_search_tree_kernel_1a.o: ../misc_api/misc_api_kernel_2.h
binary_search_tree_kernel_1a.o: ../misc_api/misc_api_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../uintn.h ../set.h ../set/set_kernel_1.h
binary_search_tree_kernel_1a.o: ../set/set_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../set/set_kernel_c.h binary_search_tree.h
binary_search_tree_kernel_1a.o: ../set/set_compare_1.h
binary_search_tree_kernel_1a.o: ../set/set_compare_abstract.h
binary_search_tree_kernel_1a.o: ../../dlib/logger/logger_kernel_abstract.h
binary_search_tree_kernel_1a.o: ../../dlib/logger/extra_logger_headers.h
binary_search_tree_kernel_1a.o: ../../dlib/logger/logger_kernel_1.h
binary_search_tree_kernel_1a.o: ../../dlib/assert.h ../../dlib/algs.h
binary_search_tree_kernel_2a.o: ../../dlib/memory_manager_global.h
binary_search_tree_kernel_2a.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
binary_search_tree_kernel_2a.o: ../algs.h ../platform.h ../assert.h
binary_search_tree_kernel_2a.o: ../error.h
binary_search_tree_kernel_2a.o: ../memory_manager/memory_manager_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../../dlib/memory_manager.h
binary_search_tree_kernel_2a.o: ../../dlib/memory_manager_stateless.h
binary_search_tree_kernel_2a.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
binary_search_tree_kernel_2a.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
binary_search_tree_kernel_2a.o: ../threads.h ../threads/threads_kernel.h
binary_search_tree_kernel_2a.o: ../platform.h ../threads/posix.h
binary_search_tree_kernel_2a.o: ../threads/threads_kernel_2.h
binary_search_tree_kernel_2a.o: ../threads/threads_kernel_abstract.h
binary_search_tree_kernel_2a.o: /usr/include/pthread.h
binary_search_tree_kernel_2a.o: /usr/include/features.h
binary_search_tree_kernel_2a.o: /usr/include/sys/cdefs.h
binary_search_tree_kernel_2a.o: /usr/include/bits/wordsize.h
binary_search_tree_kernel_2a.o: /usr/include/gnu/stubs.h
binary_search_tree_kernel_2a.o: /usr/include/gnu/stubs-32.h
binary_search_tree_kernel_2a.o: /usr/include/sched.h
binary_search_tree_kernel_2a.o: /usr/include/bits/types.h
binary_search_tree_kernel_2a.o: /usr/include/bits/typesizes.h
binary_search_tree_kernel_2a.o: /usr/include/time.h /usr/include/bits/sched.h
binary_search_tree_kernel_2a.o: /usr/include/signal.h
binary_search_tree_kernel_2a.o: /usr/include/bits/sigset.h
binary_search_tree_kernel_2a.o: /usr/include/bits/pthreadtypes.h
binary_search_tree_kernel_2a.o: /usr/include/bits/setjmp.h
binary_search_tree_kernel_2a.o: /usr/include/errno.h
binary_search_tree_kernel_2a.o: /usr/include/bits/errno.h
binary_search_tree_kernel_2a.o: /usr/include/linux/errno.h
binary_search_tree_kernel_2a.o: /usr/include/asm/errno.h
binary_search_tree_kernel_2a.o: /usr/include/asm-i386/errno.h
binary_search_tree_kernel_2a.o: /usr/include/asm-generic/errno.h
binary_search_tree_kernel_2a.o: /usr/include/asm-generic/errno-base.h
binary_search_tree_kernel_2a.o: /usr/include/sys/time.h
binary_search_tree_kernel_2a.o: /usr/include/bits/time.h
binary_search_tree_kernel_2a.o: /usr/include/sys/select.h
binary_search_tree_kernel_2a.o: /usr/include/bits/select.h
binary_search_tree_kernel_2a.o: ../threads/threads_kernel_shared.h
binary_search_tree_kernel_2a.o: ../threads/auto_mutex_extension.h
binary_search_tree_kernel_2a.o: ../threads/threads_kernel.h
binary_search_tree_kernel_2a.o: ../threads/rmutex_extension.h
binary_search_tree_kernel_2a.o: ../threads/rmutex_extension_abstract.h
binary_search_tree_kernel_2a.o: ../threads/auto_mutex_extension_abstract.h
binary_search_tree_kernel_2a.o: ../binary_search_tree.h
binary_search_tree_kernel_2a.o: ../binary_search_tree/binary_search_tree_kernel_1.h
binary_search_tree_kernel_2a.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../interfaces/map_pair.h
binary_search_tree_kernel_2a.o: ../interfaces/enumerable.h
binary_search_tree_kernel_2a.o: ../interfaces/remover.h ../serialize.h
binary_search_tree_kernel_2a.o: ../algs.h ../uintn.h
binary_search_tree_kernel_2a.o: ../interfaces/enumerable.h
binary_search_tree_kernel_2a.o: ../interfaces/map_pair.h
binary_search_tree_kernel_2a.o: ../binary_search_tree/binary_search_tree_kernel_2.h
binary_search_tree_kernel_2a.o: ../binary_search_tree/binary_search_tree_kernel_c.h
binary_search_tree_kernel_2a.o: ../assert.h ../member_function_pointer.h
binary_search_tree_kernel_2a.o: ../member_function_pointer/member_function_pointer_kernel_1.h
binary_search_tree_kernel_2a.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../member_function_pointer/member_function_pointer_kernel_c.h
binary_search_tree_kernel_2a.o: ../memory_manager.h
binary_search_tree_kernel_2a.o: ../memory_manager/memory_manager_kernel_1.h
binary_search_tree_kernel_2a.o: ../memory_manager/memory_manager_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../memory_manager/memory_manager_kernel_2.h
binary_search_tree_kernel_2a.o: ../memory_manager/memory_manager_kernel_3.h
binary_search_tree_kernel_2a.o: ../memory_manager/memory_manager_kernel_2.h
binary_search_tree_kernel_2a.o: ../binary_search_tree/binary_search_tree_kernel_2.h
binary_search_tree_kernel_2a.o: ../queue.h ../queue/queue_kernel_1.h
binary_search_tree_kernel_2a.o: ../queue/queue_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../queue/queue_kernel_2.h
binary_search_tree_kernel_2a.o: ../queue/queue_kernel_c.h
binary_search_tree_kernel_2a.o: ../queue/queue_sort_1.h
binary_search_tree_kernel_2a.o: ../queue/queue_sort_abstract.h
binary_search_tree_kernel_2a.o: ../threads/auto_mutex_extension.h
binary_search_tree_kernel_2a.o: ../threads/auto_unlock_extension.h
binary_search_tree_kernel_2a.o: ../threads/auto_unlock_extension_abstract.h
binary_search_tree_kernel_2a.o: ../threads/create_new_thread_extension.h
binary_search_tree_kernel_2a.o: ../threads/create_new_thread_extension_abstract.h
binary_search_tree_kernel_2a.o: ../threads/multithreaded_object_extension.h
binary_search_tree_kernel_2a.o: ../threads/multithreaded_object_extension_abstract.h
binary_search_tree_kernel_2a.o: ../threads/rsignaler_extension.h
binary_search_tree_kernel_2a.o: ../threads/rsignaler_extension_abstract.h
binary_search_tree_kernel_2a.o: ../map.h ../threads/rmutex_extension.h
binary_search_tree_kernel_2a.o: ../threads/rsignaler_extension.h
binary_search_tree_kernel_2a.o: ../threads/threaded_object_extension.h
binary_search_tree_kernel_2a.o: ../threads/threaded_object_extension_abstract.h
binary_search_tree_kernel_2a.o: ../threads/thread_specific_data_extension.h
binary_search_tree_kernel_2a.o: ../threads/thread_specific_data_extension_abstract.h
binary_search_tree_kernel_2a.o: ../../dlib/binary_search_tree.h tester.h
binary_search_tree_kernel_2a.o: ../../dlib/map.h ../../dlib/logger.h
binary_search_tree_kernel_2a.o: ../../dlib/logger/logger_kernel_1.h
binary_search_tree_kernel_2a.o: ../misc_api.h ../misc_api/posix.h
binary_search_tree_kernel_2a.o: ../misc_api/misc_api_kernel_2.h
binary_search_tree_kernel_2a.o: ../misc_api/misc_api_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../uintn.h ../set.h ../set/set_kernel_1.h
binary_search_tree_kernel_2a.o: ../set/set_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../set/set_kernel_c.h binary_search_tree.h
binary_search_tree_kernel_2a.o: ../set/set_compare_1.h
binary_search_tree_kernel_2a.o: ../set/set_compare_abstract.h
binary_search_tree_kernel_2a.o: ../../dlib/logger/logger_kernel_abstract.h
binary_search_tree_kernel_2a.o: ../../dlib/logger/extra_logger_headers.h
binary_search_tree_kernel_2a.o: ../../dlib/logger/logger_kernel_1.h
binary_search_tree_kernel_2a.o: ../../dlib/assert.h ../../dlib/algs.h
binary_search_tree_mm1.o: ../../dlib/memory_manager_global.h
binary_search_tree_mm1.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
binary_search_tree_mm1.o: ../algs.h ../platform.h ../assert.h ../error.h
binary_search_tree_mm1.o: ../memory_manager/memory_manager_kernel_abstract.h
binary_search_tree_mm1.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
binary_search_tree_mm1.o: ../../dlib/memory_manager.h
binary_search_tree_mm1.o: ../../dlib/memory_manager_stateless.h
binary_search_tree_mm1.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
binary_search_tree_mm1.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
binary_search_tree_mm1.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
binary_search_tree_mm1.o: ../threads.h ../threads/threads_kernel.h
binary_search_tree_mm1.o: ../platform.h ../threads/posix.h
binary_search_tree_mm1.o: ../threads/threads_kernel_2.h
binary_search_tree_mm1.o: ../threads/threads_kernel_abstract.h
binary_search_tree_mm1.o: /usr/include/pthread.h /usr/include/features.h
binary_search_tree_mm1.o: /usr/include/sys/cdefs.h
binary_search_tree_mm1.o: /usr/include/bits/wordsize.h
binary_search_tree_mm1.o: /usr/include/gnu/stubs.h
binary_search_tree_mm1.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
binary_search_tree_mm1.o: /usr/include/bits/types.h
binary_search_tree_mm1.o: /usr/include/bits/typesizes.h /usr/include/time.h
binary_search_tree_mm1.o: /usr/include/bits/sched.h /usr/include/signal.h
binary_search_tree_mm1.o: /usr/include/bits/sigset.h
binary_search_tree_mm1.o: /usr/include/bits/pthreadtypes.h
binary_search_tree_mm1.o: /usr/include/bits/setjmp.h /usr/include/errno.h
binary_search_tree_mm1.o: /usr/include/bits/errno.h
binary_search_tree_mm1.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
binary_search_tree_mm1.o: /usr/include/asm-i386/errno.h
binary_search_tree_mm1.o: /usr/include/asm-generic/errno.h
binary_search_tree_mm1.o: /usr/include/asm-generic/errno-base.h
binary_search_tree_mm1.o: /usr/include/sys/time.h /usr/include/bits/time.h
binary_search_tree_mm1.o: /usr/include/sys/select.h
binary_search_tree_mm1.o: /usr/include/bits/select.h
binary_search_tree_mm1.o: ../threads/threads_kernel_shared.h
binary_search_tree_mm1.o: ../threads/auto_mutex_extension.h
binary_search_tree_mm1.o: ../threads/threads_kernel.h
binary_search_tree_mm1.o: ../threads/rmutex_extension.h
binary_search_tree_mm1.o: ../threads/rmutex_extension_abstract.h
binary_search_tree_mm1.o: ../threads/auto_mutex_extension_abstract.h
binary_search_tree_mm1.o: ../binary_search_tree.h
binary_search_tree_mm1.o: ../binary_search_tree/binary_search_tree_kernel_1.h
binary_search_tree_mm1.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
binary_search_tree_mm1.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
binary_search_tree_mm1.o: ../interfaces/remover.h ../serialize.h ../algs.h
binary_search_tree_mm1.o: ../uintn.h ../interfaces/enumerable.h
binary_search_tree_mm1.o: ../interfaces/map_pair.h
binary_search_tree_mm1.o: ../binary_search_tree/binary_search_tree_kernel_2.h
binary_search_tree_mm1.o: ../binary_search_tree/binary_search_tree_kernel_c.h
binary_search_tree_mm1.o: ../assert.h ../member_function_pointer.h
binary_search_tree_mm1.o: ../member_function_pointer/member_function_pointer_kernel_1.h
binary_search_tree_mm1.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
binary_search_tree_mm1.o: ../member_function_pointer/member_function_pointer_kernel_c.h
binary_search_tree_mm1.o: ../memory_manager.h
binary_search_tree_mm1.o: ../memory_manager/memory_manager_kernel_1.h
binary_search_tree_mm1.o: ../memory_manager/memory_manager_kernel_abstract.h
binary_search_tree_mm1.o: ../memory_manager/memory_manager_kernel_2.h
binary_search_tree_mm1.o: ../memory_manager/memory_manager_kernel_3.h
binary_search_tree_mm1.o: ../memory_manager/memory_manager_kernel_2.h
binary_search_tree_mm1.o: ../binary_search_tree/binary_search_tree_kernel_2.h
binary_search_tree_mm1.o: ../queue.h ../queue/queue_kernel_1.h
binary_search_tree_mm1.o: ../queue/queue_kernel_abstract.h
binary_search_tree_mm1.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
binary_search_tree_mm1.o: ../queue/queue_sort_1.h
binary_search_tree_mm1.o: ../queue/queue_sort_abstract.h
binary_search_tree_mm1.o: ../threads/auto_mutex_extension.h
binary_search_tree_mm1.o: ../threads/auto_unlock_extension.h
binary_search_tree_mm1.o: ../threads/auto_unlock_extension_abstract.h
binary_search_tree_mm1.o: ../threads/create_new_thread_extension.h
binary_search_tree_mm1.o: ../threads/create_new_thread_extension_abstract.h
binary_search_tree_mm1.o: ../threads/multithreaded_object_extension.h
binary_search_tree_mm1.o: ../threads/multithreaded_object_extension_abstract.h
binary_search_tree_mm1.o: ../threads/rsignaler_extension.h
binary_search_tree_mm1.o: ../threads/rsignaler_extension_abstract.h ../map.h
binary_search_tree_mm1.o: ../threads/rmutex_extension.h
binary_search_tree_mm1.o: ../threads/rsignaler_extension.h
binary_search_tree_mm1.o: ../threads/threaded_object_extension.h
binary_search_tree_mm1.o: ../threads/threaded_object_extension_abstract.h
binary_search_tree_mm1.o: ../threads/thread_specific_data_extension.h
binary_search_tree_mm1.o: ../threads/thread_specific_data_extension_abstract.h
binary_search_tree_mm1.o: ../../dlib/binary_search_tree.h tester.h
binary_search_tree_mm1.o: ../../dlib/map.h ../../dlib/logger.h
binary_search_tree_mm1.o: ../../dlib/logger/logger_kernel_1.h ../misc_api.h
binary_search_tree_mm1.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
binary_search_tree_mm1.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
binary_search_tree_mm1.o: ../set.h ../set/set_kernel_1.h
binary_search_tree_mm1.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
binary_search_tree_mm1.o: binary_search_tree.h ../set/set_compare_1.h
binary_search_tree_mm1.o: ../set/set_compare_abstract.h
binary_search_tree_mm1.o: ../../dlib/logger/logger_kernel_abstract.h
binary_search_tree_mm1.o: ../../dlib/logger/extra_logger_headers.h
binary_search_tree_mm1.o: ../../dlib/logger/logger_kernel_1.h
binary_search_tree_mm1.o: ../../dlib/assert.h ../../dlib/algs.h
binary_search_tree_mm2.o: ../../dlib/memory_manager_global.h
binary_search_tree_mm2.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
binary_search_tree_mm2.o: ../algs.h ../platform.h ../assert.h ../error.h
binary_search_tree_mm2.o: ../memory_manager/memory_manager_kernel_abstract.h
binary_search_tree_mm2.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
binary_search_tree_mm2.o: ../../dlib/memory_manager.h
binary_search_tree_mm2.o: ../../dlib/memory_manager_stateless.h
binary_search_tree_mm2.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
binary_search_tree_mm2.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
binary_search_tree_mm2.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
binary_search_tree_mm2.o: ../threads.h ../threads/threads_kernel.h
binary_search_tree_mm2.o: ../platform.h ../threads/posix.h
binary_search_tree_mm2.o: ../threads/threads_kernel_2.h
binary_search_tree_mm2.o: ../threads/threads_kernel_abstract.h
binary_search_tree_mm2.o: /usr/include/pthread.h /usr/include/features.h
binary_search_tree_mm2.o: /usr/include/sys/cdefs.h
binary_search_tree_mm2.o: /usr/include/bits/wordsize.h
binary_search_tree_mm2.o: /usr/include/gnu/stubs.h
binary_search_tree_mm2.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
binary_search_tree_mm2.o: /usr/include/bits/types.h
binary_search_tree_mm2.o: /usr/include/bits/typesizes.h /usr/include/time.h
binary_search_tree_mm2.o: /usr/include/bits/sched.h /usr/include/signal.h
binary_search_tree_mm2.o: /usr/include/bits/sigset.h
binary_search_tree_mm2.o: /usr/include/bits/pthreadtypes.h
binary_search_tree_mm2.o: /usr/include/bits/setjmp.h /usr/include/errno.h
binary_search_tree_mm2.o: /usr/include/bits/errno.h
binary_search_tree_mm2.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
binary_search_tree_mm2.o: /usr/include/asm-i386/errno.h
binary_search_tree_mm2.o: /usr/include/asm-generic/errno.h
binary_search_tree_mm2.o: /usr/include/asm-generic/errno-base.h
binary_search_tree_mm2.o: /usr/include/sys/time.h /usr/include/bits/time.h
binary_search_tree_mm2.o: /usr/include/sys/select.h
binary_search_tree_mm2.o: /usr/include/bits/select.h
binary_search_tree_mm2.o: ../threads/threads_kernel_shared.h
binary_search_tree_mm2.o: ../threads/auto_mutex_extension.h
binary_search_tree_mm2.o: ../threads/threads_kernel.h
binary_search_tree_mm2.o: ../threads/rmutex_extension.h
binary_search_tree_mm2.o: ../threads/rmutex_extension_abstract.h
binary_search_tree_mm2.o: ../threads/auto_mutex_extension_abstract.h
binary_search_tree_mm2.o: ../binary_search_tree.h
binary_search_tree_mm2.o: ../binary_search_tree/binary_search_tree_kernel_1.h
binary_search_tree_mm2.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
binary_search_tree_mm2.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
binary_search_tree_mm2.o: ../interfaces/remover.h ../serialize.h ../algs.h
binary_search_tree_mm2.o: ../uintn.h ../interfaces/enumerable.h
binary_search_tree_mm2.o: ../interfaces/map_pair.h
binary_search_tree_mm2.o: ../binary_search_tree/binary_search_tree_kernel_2.h
binary_search_tree_mm2.o: ../binary_search_tree/binary_search_tree_kernel_c.h
binary_search_tree_mm2.o: ../assert.h ../member_function_pointer.h
binary_search_tree_mm2.o: ../member_function_pointer/member_function_pointer_kernel_1.h
binary_search_tree_mm2.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
binary_search_tree_mm2.o: ../member_function_pointer/member_function_pointer_kernel_c.h
binary_search_tree_mm2.o: ../memory_manager.h
binary_search_tree_mm2.o: ../memory_manager/memory_manager_kernel_1.h
binary_search_tree_mm2.o: ../memory_manager/memory_manager_kernel_abstract.h
binary_search_tree_mm2.o: ../memory_manager/memory_manager_kernel_2.h
binary_search_tree_mm2.o: ../memory_manager/memory_manager_kernel_3.h
binary_search_tree_mm2.o: ../memory_manager/memory_manager_kernel_2.h
binary_search_tree_mm2.o: ../binary_search_tree/binary_search_tree_kernel_2.h
binary_search_tree_mm2.o: ../queue.h ../queue/queue_kernel_1.h
binary_search_tree_mm2.o: ../queue/queue_kernel_abstract.h
binary_search_tree_mm2.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
binary_search_tree_mm2.o: ../queue/queue_sort_1.h
binary_search_tree_mm2.o: ../queue/queue_sort_abstract.h
binary_search_tree_mm2.o: ../threads/auto_mutex_extension.h
binary_search_tree_mm2.o: ../threads/auto_unlock_extension.h
binary_search_tree_mm2.o: ../threads/auto_unlock_extension_abstract.h
binary_search_tree_mm2.o: ../threads/create_new_thread_extension.h
binary_search_tree_mm2.o: ../threads/create_new_thread_extension_abstract.h
binary_search_tree_mm2.o: ../threads/multithreaded_object_extension.h
binary_search_tree_mm2.o: ../threads/multithreaded_object_extension_abstract.h
binary_search_tree_mm2.o: ../threads/rsignaler_extension.h
binary_search_tree_mm2.o: ../threads/rsignaler_extension_abstract.h ../map.h
binary_search_tree_mm2.o: ../threads/rmutex_extension.h
binary_search_tree_mm2.o: ../threads/rsignaler_extension.h
binary_search_tree_mm2.o: ../threads/threaded_object_extension.h
binary_search_tree_mm2.o: ../threads/threaded_object_extension_abstract.h
binary_search_tree_mm2.o: ../threads/thread_specific_data_extension.h
binary_search_tree_mm2.o: ../threads/thread_specific_data_extension_abstract.h
binary_search_tree_mm2.o: ../../dlib/binary_search_tree.h tester.h
binary_search_tree_mm2.o: ../../dlib/map.h ../../dlib/logger.h
binary_search_tree_mm2.o: ../../dlib/logger/logger_kernel_1.h ../misc_api.h
binary_search_tree_mm2.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
binary_search_tree_mm2.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
binary_search_tree_mm2.o: ../set.h ../set/set_kernel_1.h
binary_search_tree_mm2.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
binary_search_tree_mm2.o: binary_search_tree.h ../set/set_compare_1.h
binary_search_tree_mm2.o: ../set/set_compare_abstract.h
binary_search_tree_mm2.o: ../../dlib/logger/logger_kernel_abstract.h
binary_search_tree_mm2.o: ../../dlib/logger/extra_logger_headers.h
binary_search_tree_mm2.o: ../../dlib/logger/logger_kernel_1.h
binary_search_tree_mm2.o: ../../dlib/assert.h ../../dlib/algs.h
cmd_line_parser.o: ../../dlib/string.h ../../dlib/cmd_line_parser.h
cmd_line_parser.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_1.h
cmd_line_parser.o: ../algs.h ../platform.h ../assert.h ../error.h
cmd_line_parser.o: ../interfaces/enumerable.h
cmd_line_parser.o: ../interfaces/cmd_line_parser_option.h ../assert.h
cmd_line_parser.o: ../string.h ../string/string.h ../error.h
cmd_line_parser.o: ../string/string_abstract.h
cmd_line_parser.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_c.h
cmd_line_parser.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_abstract.h
cmd_line_parser.o: ../../dlib/cmd_line_parser/cmd_line_parser_print_1.h
cmd_line_parser.o: ../../dlib/cmd_line_parser/cmd_line_parser_print_abstract.h
cmd_line_parser.o: ../../dlib/cmd_line_parser/cmd_line_parser_check_1.h
cmd_line_parser.o: ../../dlib/cmd_line_parser/cmd_line_parser_check_c.h
cmd_line_parser.o: ../../dlib/map.h ../../dlib/map/map_kernel_1.h
cmd_line_parser.o: ../../dlib/map/map_kernel_abstract.h
cmd_line_parser.o: ../interfaces/map_pair.h ../interfaces/remover.h
cmd_line_parser.o: ../serialize.h ../algs.h ../uintn.h
cmd_line_parser.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
cmd_line_parser.o: ../memory_manager.h
cmd_line_parser.o: ../memory_manager/memory_manager_kernel_1.h
cmd_line_parser.o: ../memory_manager/memory_manager_kernel_abstract.h
cmd_line_parser.o: ../memory_manager/memory_manager_kernel_2.h
cmd_line_parser.o: ../memory_manager/memory_manager_kernel_3.h
cmd_line_parser.o: ../memory_manager/memory_manager_kernel_2.h
cmd_line_parser.o: ../binary_search_tree/binary_search_tree_kernel_2.h
cmd_line_parser.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
cmd_line_parser.o: ../../dlib/map/map_kernel_c.h binary_search_tree.h
cmd_line_parser.o: ../../dlib/memory_manager_global.h
cmd_line_parser.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
cmd_line_parser.o: ../memory_manager/memory_manager_kernel_abstract.h
cmd_line_parser.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
cmd_line_parser.o: ../../dlib/memory_manager.h
cmd_line_parser.o: ../../dlib/memory_manager_stateless.h
cmd_line_parser.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
cmd_line_parser.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
cmd_line_parser.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
cmd_line_parser.o: ../threads.h ../threads/threads_kernel.h ../platform.h
cmd_line_parser.o: ../threads/posix.h ../threads/threads_kernel_2.h
cmd_line_parser.o: ../threads/threads_kernel_abstract.h
cmd_line_parser.o: /usr/include/pthread.h /usr/include/features.h
cmd_line_parser.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
cmd_line_parser.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
cmd_line_parser.o: /usr/include/sched.h /usr/include/bits/types.h
cmd_line_parser.o: /usr/include/bits/typesizes.h /usr/include/time.h
cmd_line_parser.o: /usr/include/bits/sched.h /usr/include/signal.h
cmd_line_parser.o: /usr/include/bits/sigset.h
cmd_line_parser.o: /usr/include/bits/pthreadtypes.h
cmd_line_parser.o: /usr/include/bits/setjmp.h /usr/include/errno.h
cmd_line_parser.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
cmd_line_parser.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
cmd_line_parser.o: /usr/include/asm-generic/errno.h
cmd_line_parser.o: /usr/include/asm-generic/errno-base.h
cmd_line_parser.o: /usr/include/sys/time.h /usr/include/bits/time.h
cmd_line_parser.o: /usr/include/sys/select.h /usr/include/bits/select.h
cmd_line_parser.o: ../threads/threads_kernel_shared.h
cmd_line_parser.o: ../threads/auto_mutex_extension.h
cmd_line_parser.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
cmd_line_parser.o: ../threads/rmutex_extension_abstract.h
cmd_line_parser.o: ../threads/auto_mutex_extension_abstract.h
cmd_line_parser.o: ../binary_search_tree.h
cmd_line_parser.o: ../binary_search_tree/binary_search_tree_kernel_1.h
cmd_line_parser.o: ../binary_search_tree/binary_search_tree_kernel_2.h
cmd_line_parser.o: ../binary_search_tree/binary_search_tree_kernel_c.h
cmd_line_parser.o: ../member_function_pointer.h
cmd_line_parser.o: ../member_function_pointer/member_function_pointer_kernel_1.h
cmd_line_parser.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
cmd_line_parser.o: ../member_function_pointer/member_function_pointer_kernel_c.h
cmd_line_parser.o: ../queue.h ../queue/queue_kernel_1.h
cmd_line_parser.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
cmd_line_parser.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
cmd_line_parser.o: ../queue/queue_sort_abstract.h
cmd_line_parser.o: ../threads/auto_mutex_extension.h
cmd_line_parser.o: ../threads/auto_unlock_extension.h
cmd_line_parser.o: ../threads/auto_unlock_extension_abstract.h
cmd_line_parser.o: ../threads/create_new_thread_extension.h
cmd_line_parser.o: ../threads/create_new_thread_extension_abstract.h
cmd_line_parser.o: ../threads/multithreaded_object_extension.h
cmd_line_parser.o: ../threads/multithreaded_object_extension_abstract.h
cmd_line_parser.o: ../threads/rsignaler_extension.h
cmd_line_parser.o: ../threads/rsignaler_extension_abstract.h ../map.h
cmd_line_parser.o: ../threads/rmutex_extension.h
cmd_line_parser.o: ../threads/rsignaler_extension.h
cmd_line_parser.o: ../threads/threaded_object_extension.h
cmd_line_parser.o: ../threads/threaded_object_extension_abstract.h
cmd_line_parser.o: ../threads/thread_specific_data_extension.h
cmd_line_parser.o: ../threads/thread_specific_data_extension_abstract.h
cmd_line_parser.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
cmd_line_parser.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
cmd_line_parser.o: ../misc_api.h ../misc_api/posix.h
cmd_line_parser.o: ../misc_api/misc_api_kernel_2.h
cmd_line_parser.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
cmd_line_parser.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
cmd_line_parser.o: ../set/set_kernel_c.h ../set/set_compare_1.h
cmd_line_parser.o: ../set/set_compare_abstract.h
cmd_line_parser.o: ../../dlib/logger/logger_kernel_abstract.h
cmd_line_parser.o: ../../dlib/logger/extra_logger_headers.h
cmd_line_parser.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
cmd_line_parser.o: ../../dlib/algs.h ../../dlib/sequence.h
cmd_line_parser.o: ../../dlib/sequence/sequence_kernel_1.h
cmd_line_parser.o: ../../dlib/sequence/sequence_kernel_abstract.h
cmd_line_parser.o: ../../dlib/sequence/sequence_kernel_2.h
cmd_line_parser.o: ../../dlib/sequence/sequence_kernel_c.h
cmd_line_parser.o: ../../dlib/sequence/sequence_compare_1.h
cmd_line_parser.o: ../../dlib/sequence/sequence_compare_abstract.h
cmd_line_parser.o: ../../dlib/sequence/sequence_sort_1.h
cmd_line_parser.o: ../../dlib/sequence/sequence_sort_abstract.h
cmd_line_parser.o: ../../dlib/sequence/sequence_sort_2.h ../sort.h
cmd_line_parser.o: cmd_line_parser.h
cmd_line_parser_wchar_t.o: ../../dlib/string.h ../../dlib/cmd_line_parser.h
cmd_line_parser_wchar_t.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_1.h
cmd_line_parser_wchar_t.o: ../algs.h ../platform.h ../assert.h ../error.h
cmd_line_parser_wchar_t.o: ../interfaces/enumerable.h
cmd_line_parser_wchar_t.o: ../interfaces/cmd_line_parser_option.h ../assert.h
cmd_line_parser_wchar_t.o: ../string.h ../string/string.h ../error.h
cmd_line_parser_wchar_t.o: ../string/string_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_c.h
cmd_line_parser_wchar_t.o: ../../dlib/cmd_line_parser/cmd_line_parser_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/cmd_line_parser/cmd_line_parser_print_1.h
cmd_line_parser_wchar_t.o: ../../dlib/cmd_line_parser/cmd_line_parser_print_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/cmd_line_parser/cmd_line_parser_check_1.h
cmd_line_parser_wchar_t.o: ../../dlib/cmd_line_parser/cmd_line_parser_check_c.h
cmd_line_parser_wchar_t.o: ../../dlib/map.h ../../dlib/map/map_kernel_1.h
cmd_line_parser_wchar_t.o: ../../dlib/map/map_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../interfaces/map_pair.h ../interfaces/remover.h
cmd_line_parser_wchar_t.o: ../serialize.h ../algs.h ../uintn.h
cmd_line_parser_wchar_t.o: ../interfaces/enumerable.h
cmd_line_parser_wchar_t.o: ../interfaces/map_pair.h ../memory_manager.h
cmd_line_parser_wchar_t.o: ../memory_manager/memory_manager_kernel_1.h
cmd_line_parser_wchar_t.o: ../memory_manager/memory_manager_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../memory_manager/memory_manager_kernel_2.h
cmd_line_parser_wchar_t.o: ../memory_manager/memory_manager_kernel_3.h
cmd_line_parser_wchar_t.o: ../memory_manager/memory_manager_kernel_2.h
cmd_line_parser_wchar_t.o: ../binary_search_tree/binary_search_tree_kernel_2.h
cmd_line_parser_wchar_t.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/map/map_kernel_c.h binary_search_tree.h
cmd_line_parser_wchar_t.o: ../../dlib/memory_manager_global.h
cmd_line_parser_wchar_t.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
cmd_line_parser_wchar_t.o: ../memory_manager/memory_manager_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/memory_manager.h
cmd_line_parser_wchar_t.o: ../../dlib/memory_manager_stateless.h
cmd_line_parser_wchar_t.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
cmd_line_parser_wchar_t.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
cmd_line_parser_wchar_t.o: ../threads.h ../threads/threads_kernel.h
cmd_line_parser_wchar_t.o: ../platform.h ../threads/posix.h
cmd_line_parser_wchar_t.o: ../threads/threads_kernel_2.h
cmd_line_parser_wchar_t.o: ../threads/threads_kernel_abstract.h
cmd_line_parser_wchar_t.o: /usr/include/pthread.h /usr/include/features.h
cmd_line_parser_wchar_t.o: /usr/include/sys/cdefs.h
cmd_line_parser_wchar_t.o: /usr/include/bits/wordsize.h
cmd_line_parser_wchar_t.o: /usr/include/gnu/stubs.h
cmd_line_parser_wchar_t.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
cmd_line_parser_wchar_t.o: /usr/include/bits/types.h
cmd_line_parser_wchar_t.o: /usr/include/bits/typesizes.h /usr/include/time.h
cmd_line_parser_wchar_t.o: /usr/include/bits/sched.h /usr/include/signal.h
cmd_line_parser_wchar_t.o: /usr/include/bits/sigset.h
cmd_line_parser_wchar_t.o: /usr/include/bits/pthreadtypes.h
cmd_line_parser_wchar_t.o: /usr/include/bits/setjmp.h /usr/include/errno.h
cmd_line_parser_wchar_t.o: /usr/include/bits/errno.h
cmd_line_parser_wchar_t.o: /usr/include/linux/errno.h
cmd_line_parser_wchar_t.o: /usr/include/asm/errno.h
cmd_line_parser_wchar_t.o: /usr/include/asm-i386/errno.h
cmd_line_parser_wchar_t.o: /usr/include/asm-generic/errno.h
cmd_line_parser_wchar_t.o: /usr/include/asm-generic/errno-base.h
cmd_line_parser_wchar_t.o: /usr/include/sys/time.h /usr/include/bits/time.h
cmd_line_parser_wchar_t.o: /usr/include/sys/select.h
cmd_line_parser_wchar_t.o: /usr/include/bits/select.h
cmd_line_parser_wchar_t.o: ../threads/threads_kernel_shared.h
cmd_line_parser_wchar_t.o: ../threads/auto_mutex_extension.h
cmd_line_parser_wchar_t.o: ../threads/threads_kernel.h
cmd_line_parser_wchar_t.o: ../threads/rmutex_extension.h
cmd_line_parser_wchar_t.o: ../threads/rmutex_extension_abstract.h
cmd_line_parser_wchar_t.o: ../threads/auto_mutex_extension_abstract.h
cmd_line_parser_wchar_t.o: ../binary_search_tree.h
cmd_line_parser_wchar_t.o: ../binary_search_tree/binary_search_tree_kernel_1.h
cmd_line_parser_wchar_t.o: ../binary_search_tree/binary_search_tree_kernel_2.h
cmd_line_parser_wchar_t.o: ../binary_search_tree/binary_search_tree_kernel_c.h
cmd_line_parser_wchar_t.o: ../member_function_pointer.h
cmd_line_parser_wchar_t.o: ../member_function_pointer/member_function_pointer_kernel_1.h
cmd_line_parser_wchar_t.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../member_function_pointer/member_function_pointer_kernel_c.h
cmd_line_parser_wchar_t.o: ../queue.h ../queue/queue_kernel_1.h
cmd_line_parser_wchar_t.o: ../queue/queue_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../queue/queue_kernel_2.h
cmd_line_parser_wchar_t.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
cmd_line_parser_wchar_t.o: ../queue/queue_sort_abstract.h
cmd_line_parser_wchar_t.o: ../threads/auto_mutex_extension.h
cmd_line_parser_wchar_t.o: ../threads/auto_unlock_extension.h
cmd_line_parser_wchar_t.o: ../threads/auto_unlock_extension_abstract.h
cmd_line_parser_wchar_t.o: ../threads/create_new_thread_extension.h
cmd_line_parser_wchar_t.o: ../threads/create_new_thread_extension_abstract.h
cmd_line_parser_wchar_t.o: ../threads/multithreaded_object_extension.h
cmd_line_parser_wchar_t.o: ../threads/multithreaded_object_extension_abstract.h
cmd_line_parser_wchar_t.o: ../threads/rsignaler_extension.h
cmd_line_parser_wchar_t.o: ../threads/rsignaler_extension_abstract.h ../map.h
cmd_line_parser_wchar_t.o: ../threads/rmutex_extension.h
cmd_line_parser_wchar_t.o: ../threads/rsignaler_extension.h
cmd_line_parser_wchar_t.o: ../threads/threaded_object_extension.h
cmd_line_parser_wchar_t.o: ../threads/threaded_object_extension_abstract.h
cmd_line_parser_wchar_t.o: ../threads/thread_specific_data_extension.h
cmd_line_parser_wchar_t.o: ../threads/thread_specific_data_extension_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/binary_search_tree.h tester.h
cmd_line_parser_wchar_t.o: ../../dlib/map.h ../../dlib/logger.h
cmd_line_parser_wchar_t.o: ../../dlib/logger/logger_kernel_1.h ../misc_api.h
cmd_line_parser_wchar_t.o: ../misc_api/posix.h
cmd_line_parser_wchar_t.o: ../misc_api/misc_api_kernel_2.h
cmd_line_parser_wchar_t.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
cmd_line_parser_wchar_t.o: ../set.h ../set/set_kernel_1.h
cmd_line_parser_wchar_t.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
cmd_line_parser_wchar_t.o: ../set/set_compare_1.h
cmd_line_parser_wchar_t.o: ../set/set_compare_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/logger/logger_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/logger/extra_logger_headers.h
cmd_line_parser_wchar_t.o: ../../dlib/logger/logger_kernel_1.h
cmd_line_parser_wchar_t.o: ../../dlib/assert.h ../../dlib/algs.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_kernel_1.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_kernel_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_kernel_2.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_kernel_c.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_compare_1.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_compare_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_sort_1.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_sort_abstract.h
cmd_line_parser_wchar_t.o: ../../dlib/sequence/sequence_sort_2.h ../sort.h
cmd_line_parser_wchar_t.o: cmd_line_parser.h
compress_stream.o: ../../dlib/compress_stream.h
compress_stream.o: ../../dlib/compress_stream/compress_stream_kernel_1.h
compress_stream.o: ../algs.h ../platform.h ../assert.h ../error.h
compress_stream.o: ../../dlib/compress_stream/compress_stream_kernel_abstract.h
compress_stream.o: ../../dlib/compress_stream/compress_stream_kernel_2.h
compress_stream.o: ../../dlib/compress_stream/compress_stream_kernel_3.h
compress_stream.o: ../assert.h conditioning_class.h
compress_stream.o: ../../dlib/conditioning_class.h
compress_stream.o: ../../dlib/conditioning_class/conditioning_class_kernel_1.h
compress_stream.o: ../../dlib/conditioning_class/conditioning_class_kernel_abstract.h
compress_stream.o: ../../dlib/conditioning_class/conditioning_class_kernel_2.h
compress_stream.o: ../../dlib/conditioning_class/conditioning_class_kernel_3.h
compress_stream.o: ../../dlib/conditioning_class/conditioning_class_kernel_4.h
compress_stream.o: ../../dlib/conditioning_class/conditioning_class_kernel_c.h
compress_stream.o: ../../dlib/memory_manager.h tester.h ../../dlib/map.h
compress_stream.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
compress_stream.o: ../threads.h ../threads/threads_kernel.h ../platform.h
compress_stream.o: ../threads/posix.h ../threads/threads_kernel_2.h
compress_stream.o: ../threads/threads_kernel_abstract.h
compress_stream.o: /usr/include/pthread.h /usr/include/features.h
compress_stream.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
compress_stream.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
compress_stream.o: /usr/include/sched.h /usr/include/bits/types.h
compress_stream.o: /usr/include/bits/typesizes.h /usr/include/time.h
compress_stream.o: /usr/include/bits/sched.h /usr/include/signal.h
compress_stream.o: /usr/include/bits/sigset.h
compress_stream.o: /usr/include/bits/pthreadtypes.h
compress_stream.o: /usr/include/bits/setjmp.h /usr/include/errno.h
compress_stream.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
compress_stream.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
compress_stream.o: /usr/include/asm-generic/errno.h
compress_stream.o: /usr/include/asm-generic/errno-base.h
compress_stream.o: /usr/include/sys/time.h /usr/include/bits/time.h
compress_stream.o: /usr/include/sys/select.h /usr/include/bits/select.h
compress_stream.o: ../threads/threads_kernel_shared.h
compress_stream.o: ../threads/auto_mutex_extension.h
compress_stream.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
compress_stream.o: ../threads/rmutex_extension_abstract.h
compress_stream.o: ../threads/auto_mutex_extension_abstract.h
compress_stream.o: ../binary_search_tree.h
compress_stream.o: ../binary_search_tree/binary_search_tree_kernel_1.h
compress_stream.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
compress_stream.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
compress_stream.o: ../interfaces/remover.h ../serialize.h ../algs.h
compress_stream.o: ../uintn.h ../interfaces/enumerable.h
compress_stream.o: ../interfaces/map_pair.h
compress_stream.o: ../binary_search_tree/binary_search_tree_kernel_2.h
compress_stream.o: ../binary_search_tree/binary_search_tree_kernel_c.h
compress_stream.o: ../member_function_pointer.h
compress_stream.o: ../member_function_pointer/member_function_pointer_kernel_1.h
compress_stream.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
compress_stream.o: ../member_function_pointer/member_function_pointer_kernel_c.h
compress_stream.o: ../memory_manager.h
compress_stream.o: ../memory_manager/memory_manager_kernel_1.h
compress_stream.o: ../memory_manager/memory_manager_kernel_abstract.h
compress_stream.o: ../memory_manager/memory_manager_kernel_2.h
compress_stream.o: ../memory_manager/memory_manager_kernel_3.h
compress_stream.o: ../memory_manager/memory_manager_kernel_2.h
compress_stream.o: ../binary_search_tree/binary_search_tree_kernel_2.h
compress_stream.o: ../queue.h ../queue/queue_kernel_1.h
compress_stream.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
compress_stream.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
compress_stream.o: ../queue/queue_sort_abstract.h
compress_stream.o: ../threads/auto_mutex_extension.h
compress_stream.o: ../threads/auto_unlock_extension.h
compress_stream.o: ../threads/auto_unlock_extension_abstract.h
compress_stream.o: ../threads/create_new_thread_extension.h
compress_stream.o: ../threads/create_new_thread_extension_abstract.h
compress_stream.o: ../threads/multithreaded_object_extension.h
compress_stream.o: ../threads/multithreaded_object_extension_abstract.h
compress_stream.o: ../threads/rsignaler_extension.h
compress_stream.o: ../threads/rsignaler_extension_abstract.h ../map.h
compress_stream.o: ../threads/rmutex_extension.h
compress_stream.o: ../threads/rsignaler_extension.h
compress_stream.o: ../threads/threaded_object_extension.h
compress_stream.o: ../threads/threaded_object_extension_abstract.h
compress_stream.o: ../threads/thread_specific_data_extension.h
compress_stream.o: ../threads/thread_specific_data_extension_abstract.h
compress_stream.o: ../misc_api.h ../misc_api/posix.h
compress_stream.o: ../misc_api/misc_api_kernel_2.h
compress_stream.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
compress_stream.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
compress_stream.o: ../set/set_kernel_c.h binary_search_tree.h
compress_stream.o: ../../dlib/memory_manager_global.h
compress_stream.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
compress_stream.o: ../memory_manager/memory_manager_kernel_abstract.h
compress_stream.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
compress_stream.o: ../../dlib/memory_manager_stateless.h
compress_stream.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
compress_stream.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
compress_stream.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
compress_stream.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
compress_stream.o: ../set/set_compare_abstract.h
compress_stream.o: ../../dlib/logger/logger_kernel_abstract.h
compress_stream.o: ../../dlib/logger/extra_logger_headers.h
compress_stream.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
compress_stream.o: ../../dlib/algs.h ../../dlib/entropy_encoder.h
compress_stream.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_1.h
compress_stream.o: ../entropy_encoder/entropy_encoder_kernel_abstract.h
compress_stream.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_2.h
compress_stream.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_c.h
compress_stream.o: ../../dlib/entropy_decoder.h
compress_stream.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_1.h
compress_stream.o: ../entropy_decoder/entropy_decoder_kernel_abstract.h
compress_stream.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_2.h
compress_stream.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_c.h
compress_stream.o: ../../dlib/entropy_encoder_model.h
compress_stream.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_1.h
compress_stream.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_abstract.h
compress_stream.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_2.h
compress_stream.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_3.h
compress_stream.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_4.h
compress_stream.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_5.h
compress_stream.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_6.h
compress_stream.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_c.h
compress_stream.o: ../../dlib/sliding_buffer.h
compress_stream.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_1.h
compress_stream.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_abstract.h
compress_stream.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_c.h
compress_stream.o: ../../dlib/entropy_decoder_model.h
compress_stream.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_1.h
compress_stream.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_abstract.h
compress_stream.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_2.h
compress_stream.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_3.h
compress_stream.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_4.h
compress_stream.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_5.h
compress_stream.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_6.h
compress_stream.o: ../../dlib/lz77_buffer.h
compress_stream.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_1.h
compress_stream.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_abstract.h
compress_stream.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_2.h
compress_stream.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_c.h
compress_stream.o: ../../dlib/lzp_buffer.h
compress_stream.o: ../../dlib/lzp_buffer/lzp_buffer_kernel_1.h
compress_stream.o: ../../dlib/lzp_buffer/lzp_buffer_kernel_abstract.h
compress_stream.o: ../../dlib/lzp_buffer/lzp_buffer_kernel_2.h
compress_stream.o: ../../dlib/lzp_buffer/lzp_buffer_kernel_c.h
compress_stream.o: ../../dlib/crc32.h ../../dlib/crc32/crc32_kernel_1.h
compress_stream.o: ../../dlib/crc32/crc32_kernel_abstract.h
conditioning_class.o: ../../dlib/conditioning_class.h
conditioning_class.o: ../../dlib/conditioning_class/conditioning_class_kernel_1.h
conditioning_class.o: ../../dlib/conditioning_class/conditioning_class_kernel_abstract.h
conditioning_class.o: ../assert.h ../algs.h ../platform.h ../assert.h
conditioning_class.o: ../error.h
conditioning_class.o: ../../dlib/conditioning_class/conditioning_class_kernel_2.h
conditioning_class.o: ../../dlib/conditioning_class/conditioning_class_kernel_3.h
conditioning_class.o: ../../dlib/conditioning_class/conditioning_class_kernel_4.h
conditioning_class.o: ../../dlib/conditioning_class/conditioning_class_kernel_c.h
conditioning_class.o: ../../dlib/memory_manager.h tester.h ../../dlib/map.h
conditioning_class.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
conditioning_class.o: ../threads.h ../threads/threads_kernel.h ../platform.h
conditioning_class.o: ../threads/posix.h ../threads/threads_kernel_2.h
conditioning_class.o: ../threads/threads_kernel_abstract.h
conditioning_class.o: /usr/include/pthread.h /usr/include/features.h
conditioning_class.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
conditioning_class.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
conditioning_class.o: /usr/include/sched.h /usr/include/bits/types.h
conditioning_class.o: /usr/include/bits/typesizes.h /usr/include/time.h
conditioning_class.o: /usr/include/bits/sched.h /usr/include/signal.h
conditioning_class.o: /usr/include/bits/sigset.h
conditioning_class.o: /usr/include/bits/pthreadtypes.h
conditioning_class.o: /usr/include/bits/setjmp.h /usr/include/errno.h
conditioning_class.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
conditioning_class.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
conditioning_class.o: /usr/include/asm-generic/errno.h
conditioning_class.o: /usr/include/asm-generic/errno-base.h
conditioning_class.o: /usr/include/sys/time.h /usr/include/bits/time.h
conditioning_class.o: /usr/include/sys/select.h /usr/include/bits/select.h
conditioning_class.o: ../threads/threads_kernel_shared.h
conditioning_class.o: ../threads/auto_mutex_extension.h
conditioning_class.o: ../threads/threads_kernel.h
conditioning_class.o: ../threads/rmutex_extension.h
conditioning_class.o: ../threads/rmutex_extension_abstract.h
conditioning_class.o: ../threads/auto_mutex_extension_abstract.h
conditioning_class.o: ../binary_search_tree.h
conditioning_class.o: ../binary_search_tree/binary_search_tree_kernel_1.h
conditioning_class.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
conditioning_class.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
conditioning_class.o: ../interfaces/remover.h ../serialize.h ../algs.h
conditioning_class.o: ../uintn.h ../interfaces/enumerable.h
conditioning_class.o: ../interfaces/map_pair.h
conditioning_class.o: ../binary_search_tree/binary_search_tree_kernel_2.h
conditioning_class.o: ../binary_search_tree/binary_search_tree_kernel_c.h
conditioning_class.o: ../member_function_pointer.h
conditioning_class.o: ../member_function_pointer/member_function_pointer_kernel_1.h
conditioning_class.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
conditioning_class.o: ../member_function_pointer/member_function_pointer_kernel_c.h
conditioning_class.o: ../memory_manager.h
conditioning_class.o: ../memory_manager/memory_manager_kernel_1.h
conditioning_class.o: ../memory_manager/memory_manager_kernel_abstract.h
conditioning_class.o: ../memory_manager/memory_manager_kernel_2.h
conditioning_class.o: ../memory_manager/memory_manager_kernel_3.h
conditioning_class.o: ../memory_manager/memory_manager_kernel_2.h
conditioning_class.o: ../binary_search_tree/binary_search_tree_kernel_2.h
conditioning_class.o: ../queue.h ../queue/queue_kernel_1.h
conditioning_class.o: ../queue/queue_kernel_abstract.h
conditioning_class.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
conditioning_class.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
conditioning_class.o: ../threads/auto_mutex_extension.h
conditioning_class.o: ../threads/auto_unlock_extension.h
conditioning_class.o: ../threads/auto_unlock_extension_abstract.h
conditioning_class.o: ../threads/create_new_thread_extension.h
conditioning_class.o: ../threads/create_new_thread_extension_abstract.h
conditioning_class.o: ../threads/multithreaded_object_extension.h
conditioning_class.o: ../threads/multithreaded_object_extension_abstract.h
conditioning_class.o: ../threads/rsignaler_extension.h
conditioning_class.o: ../threads/rsignaler_extension_abstract.h ../map.h
conditioning_class.o: ../threads/rmutex_extension.h
conditioning_class.o: ../threads/rsignaler_extension.h
conditioning_class.o: ../threads/threaded_object_extension.h
conditioning_class.o: ../threads/threaded_object_extension_abstract.h
conditioning_class.o: ../threads/thread_specific_data_extension.h
conditioning_class.o: ../threads/thread_specific_data_extension_abstract.h
conditioning_class.o: ../misc_api.h ../misc_api/posix.h
conditioning_class.o: ../misc_api/misc_api_kernel_2.h
conditioning_class.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
conditioning_class.o: ../set.h ../set/set_kernel_1.h
conditioning_class.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
conditioning_class.o: binary_search_tree.h ../../dlib/memory_manager_global.h
conditioning_class.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
conditioning_class.o: ../memory_manager/memory_manager_kernel_abstract.h
conditioning_class.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
conditioning_class.o: ../../dlib/memory_manager_stateless.h
conditioning_class.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
conditioning_class.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
conditioning_class.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
conditioning_class.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
conditioning_class.o: ../set/set_compare_abstract.h
conditioning_class.o: ../../dlib/logger/logger_kernel_abstract.h
conditioning_class.o: ../../dlib/logger/extra_logger_headers.h
conditioning_class.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
conditioning_class.o: ../../dlib/algs.h conditioning_class.h
conditioning_class_c.o: ../../dlib/conditioning_class.h
conditioning_class_c.o: ../../dlib/conditioning_class/conditioning_class_kernel_1.h
conditioning_class_c.o: ../../dlib/conditioning_class/conditioning_class_kernel_abstract.h
conditioning_class_c.o: ../assert.h ../algs.h ../platform.h ../assert.h
conditioning_class_c.o: ../error.h
conditioning_class_c.o: ../../dlib/conditioning_class/conditioning_class_kernel_2.h
conditioning_class_c.o: ../../dlib/conditioning_class/conditioning_class_kernel_3.h
conditioning_class_c.o: ../../dlib/conditioning_class/conditioning_class_kernel_4.h
conditioning_class_c.o: ../../dlib/conditioning_class/conditioning_class_kernel_c.h
conditioning_class_c.o: ../../dlib/memory_manager.h tester.h ../../dlib/map.h
conditioning_class_c.o: ../../dlib/logger.h
conditioning_class_c.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
conditioning_class_c.o: ../threads/threads_kernel.h ../platform.h
conditioning_class_c.o: ../threads/posix.h ../threads/threads_kernel_2.h
conditioning_class_c.o: ../threads/threads_kernel_abstract.h
conditioning_class_c.o: /usr/include/pthread.h /usr/include/features.h
conditioning_class_c.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
conditioning_class_c.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
conditioning_class_c.o: /usr/include/sched.h /usr/include/bits/types.h
conditioning_class_c.o: /usr/include/bits/typesizes.h /usr/include/time.h
conditioning_class_c.o: /usr/include/bits/sched.h /usr/include/signal.h
conditioning_class_c.o: /usr/include/bits/sigset.h
conditioning_class_c.o: /usr/include/bits/pthreadtypes.h
conditioning_class_c.o: /usr/include/bits/setjmp.h /usr/include/errno.h
conditioning_class_c.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
conditioning_class_c.o: /usr/include/asm/errno.h
conditioning_class_c.o: /usr/include/asm-i386/errno.h
conditioning_class_c.o: /usr/include/asm-generic/errno.h
conditioning_class_c.o: /usr/include/asm-generic/errno-base.h
conditioning_class_c.o: /usr/include/sys/time.h /usr/include/bits/time.h
conditioning_class_c.o: /usr/include/sys/select.h /usr/include/bits/select.h
conditioning_class_c.o: ../threads/threads_kernel_shared.h
conditioning_class_c.o: ../threads/auto_mutex_extension.h
conditioning_class_c.o: ../threads/threads_kernel.h
conditioning_class_c.o: ../threads/rmutex_extension.h
conditioning_class_c.o: ../threads/rmutex_extension_abstract.h
conditioning_class_c.o: ../threads/auto_mutex_extension_abstract.h
conditioning_class_c.o: ../binary_search_tree.h
conditioning_class_c.o: ../binary_search_tree/binary_search_tree_kernel_1.h
conditioning_class_c.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
conditioning_class_c.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
conditioning_class_c.o: ../interfaces/remover.h ../serialize.h ../algs.h
conditioning_class_c.o: ../uintn.h ../interfaces/enumerable.h
conditioning_class_c.o: ../interfaces/map_pair.h
conditioning_class_c.o: ../binary_search_tree/binary_search_tree_kernel_2.h
conditioning_class_c.o: ../binary_search_tree/binary_search_tree_kernel_c.h
conditioning_class_c.o: ../member_function_pointer.h
conditioning_class_c.o: ../member_function_pointer/member_function_pointer_kernel_1.h
conditioning_class_c.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
conditioning_class_c.o: ../member_function_pointer/member_function_pointer_kernel_c.h
conditioning_class_c.o: ../memory_manager.h
conditioning_class_c.o: ../memory_manager/memory_manager_kernel_1.h
conditioning_class_c.o: ../memory_manager/memory_manager_kernel_abstract.h
conditioning_class_c.o: ../memory_manager/memory_manager_kernel_2.h
conditioning_class_c.o: ../memory_manager/memory_manager_kernel_3.h
conditioning_class_c.o: ../memory_manager/memory_manager_kernel_2.h
conditioning_class_c.o: ../binary_search_tree/binary_search_tree_kernel_2.h
conditioning_class_c.o: ../queue.h ../queue/queue_kernel_1.h
conditioning_class_c.o: ../queue/queue_kernel_abstract.h
conditioning_class_c.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
conditioning_class_c.o: ../queue/queue_sort_1.h
conditioning_class_c.o: ../queue/queue_sort_abstract.h
conditioning_class_c.o: ../threads/auto_mutex_extension.h
conditioning_class_c.o: ../threads/auto_unlock_extension.h
conditioning_class_c.o: ../threads/auto_unlock_extension_abstract.h
conditioning_class_c.o: ../threads/create_new_thread_extension.h
conditioning_class_c.o: ../threads/create_new_thread_extension_abstract.h
conditioning_class_c.o: ../threads/multithreaded_object_extension.h
conditioning_class_c.o: ../threads/multithreaded_object_extension_abstract.h
conditioning_class_c.o: ../threads/rsignaler_extension.h
conditioning_class_c.o: ../threads/rsignaler_extension_abstract.h ../map.h
conditioning_class_c.o: ../threads/rmutex_extension.h
conditioning_class_c.o: ../threads/rsignaler_extension.h
conditioning_class_c.o: ../threads/threaded_object_extension.h
conditioning_class_c.o: ../threads/threaded_object_extension_abstract.h
conditioning_class_c.o: ../threads/thread_specific_data_extension.h
conditioning_class_c.o: ../threads/thread_specific_data_extension_abstract.h
conditioning_class_c.o: ../misc_api.h ../misc_api/posix.h
conditioning_class_c.o: ../misc_api/misc_api_kernel_2.h
conditioning_class_c.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
conditioning_class_c.o: ../set.h ../set/set_kernel_1.h
conditioning_class_c.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
conditioning_class_c.o: binary_search_tree.h
conditioning_class_c.o: ../../dlib/memory_manager_global.h
conditioning_class_c.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
conditioning_class_c.o: ../memory_manager/memory_manager_kernel_abstract.h
conditioning_class_c.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
conditioning_class_c.o: ../../dlib/memory_manager_stateless.h
conditioning_class_c.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
conditioning_class_c.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
conditioning_class_c.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
conditioning_class_c.o: ../../dlib/binary_search_tree.h
conditioning_class_c.o: ../set/set_compare_1.h ../set/set_compare_abstract.h
conditioning_class_c.o: ../../dlib/logger/logger_kernel_abstract.h
conditioning_class_c.o: ../../dlib/logger/extra_logger_headers.h
conditioning_class_c.o: ../../dlib/logger/logger_kernel_1.h
conditioning_class_c.o: ../../dlib/assert.h ../../dlib/algs.h
conditioning_class_c.o: conditioning_class.h
config_reader.o: ../../dlib/config_reader.h
config_reader.o: ../../dlib/config_reader/config_reader_kernel_1.h
config_reader.o: ../../dlib/config_reader/config_reader_kernel_abstract.h
config_reader.o: ../algs.h ../platform.h ../assert.h ../error.h
config_reader.o: ../interfaces/enumerable.h ../../dlib/map.h
config_reader.o: ../../dlib/map/map_kernel_1.h
config_reader.o: ../../dlib/map/map_kernel_abstract.h
config_reader.o: ../interfaces/map_pair.h ../interfaces/remover.h
config_reader.o: ../serialize.h ../algs.h ../uintn.h
config_reader.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
config_reader.o: ../memory_manager.h
config_reader.o: ../memory_manager/memory_manager_kernel_1.h
config_reader.o: ../memory_manager/memory_manager_kernel_abstract.h
config_reader.o: ../assert.h ../memory_manager/memory_manager_kernel_2.h
config_reader.o: ../memory_manager/memory_manager_kernel_3.h
config_reader.o: ../memory_manager/memory_manager_kernel_2.h
config_reader.o: ../binary_search_tree/binary_search_tree_kernel_2.h
config_reader.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
config_reader.o: ../../dlib/map/map_kernel_c.h binary_search_tree.h
config_reader.o: ../../dlib/memory_manager_global.h
config_reader.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
config_reader.o: ../memory_manager/memory_manager_kernel_abstract.h
config_reader.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
config_reader.o: ../../dlib/memory_manager.h
config_reader.o: ../../dlib/memory_manager_stateless.h
config_reader.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
config_reader.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
config_reader.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
config_reader.o: ../threads.h ../threads/threads_kernel.h ../platform.h
config_reader.o: ../threads/posix.h ../threads/threads_kernel_2.h
config_reader.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
config_reader.o: /usr/include/features.h /usr/include/sys/cdefs.h
config_reader.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
config_reader.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
config_reader.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
config_reader.o: /usr/include/time.h /usr/include/bits/sched.h
config_reader.o: /usr/include/signal.h /usr/include/bits/sigset.h
config_reader.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
config_reader.o: /usr/include/errno.h /usr/include/bits/errno.h
config_reader.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
config_reader.o: /usr/include/asm-i386/errno.h
config_reader.o: /usr/include/asm-generic/errno.h
config_reader.o: /usr/include/asm-generic/errno-base.h
config_reader.o: /usr/include/sys/time.h /usr/include/bits/time.h
config_reader.o: /usr/include/sys/select.h /usr/include/bits/select.h
config_reader.o: ../threads/threads_kernel_shared.h
config_reader.o: ../threads/auto_mutex_extension.h
config_reader.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
config_reader.o: ../threads/rmutex_extension_abstract.h
config_reader.o: ../threads/auto_mutex_extension_abstract.h
config_reader.o: ../binary_search_tree.h
config_reader.o: ../binary_search_tree/binary_search_tree_kernel_1.h
config_reader.o: ../binary_search_tree/binary_search_tree_kernel_2.h
config_reader.o: ../binary_search_tree/binary_search_tree_kernel_c.h
config_reader.o: ../member_function_pointer.h
config_reader.o: ../member_function_pointer/member_function_pointer_kernel_1.h
config_reader.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
config_reader.o: ../member_function_pointer/member_function_pointer_kernel_c.h
config_reader.o: ../queue.h ../queue/queue_kernel_1.h
config_reader.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
config_reader.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
config_reader.o: ../queue/queue_sort_abstract.h
config_reader.o: ../threads/auto_mutex_extension.h
config_reader.o: ../threads/auto_unlock_extension.h
config_reader.o: ../threads/auto_unlock_extension_abstract.h
config_reader.o: ../threads/create_new_thread_extension.h
config_reader.o: ../threads/create_new_thread_extension_abstract.h
config_reader.o: ../threads/multithreaded_object_extension.h
config_reader.o: ../threads/multithreaded_object_extension_abstract.h
config_reader.o: ../threads/rsignaler_extension.h
config_reader.o: ../threads/rsignaler_extension_abstract.h ../map.h
config_reader.o: ../threads/rmutex_extension.h
config_reader.o: ../threads/rsignaler_extension.h
config_reader.o: ../threads/threaded_object_extension.h
config_reader.o: ../threads/threaded_object_extension_abstract.h
config_reader.o: ../threads/thread_specific_data_extension.h
config_reader.o: ../threads/thread_specific_data_extension_abstract.h
config_reader.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
config_reader.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
config_reader.o: ../misc_api.h ../misc_api/posix.h
config_reader.o: ../misc_api/misc_api_kernel_2.h
config_reader.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
config_reader.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
config_reader.o: ../set/set_kernel_c.h ../set/set_compare_1.h
config_reader.o: ../set/set_compare_abstract.h
config_reader.o: ../../dlib/logger/logger_kernel_abstract.h
config_reader.o: ../../dlib/logger/extra_logger_headers.h
config_reader.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
config_reader.o: ../../dlib/algs.h ../../dlib/tokenizer.h
config_reader.o: ../../dlib/tokenizer/tokenizer_kernel_1.h
config_reader.o: ../tokenizer/tokenizer_kernel_abstract.h
config_reader.o: ../../dlib/tokenizer/tokenizer_kernel_c.h
config_reader.o: ../../dlib/config_reader/config_reader_thread_safe_1.h
config_reader.o: ../../dlib/config_reader/config_reader_thread_safe_abstract.h
entropy_coder.o: ../../dlib/entropy_encoder.h
entropy_coder.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_1.h
entropy_coder.o: ../algs.h ../platform.h ../assert.h ../error.h
entropy_coder.o: ../entropy_encoder/entropy_encoder_kernel_abstract.h
entropy_coder.o: ../uintn.h
entropy_coder.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_2.h
entropy_coder.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_c.h
entropy_coder.o: ../assert.h ../../dlib/entropy_decoder.h
entropy_coder.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_1.h
entropy_coder.o: ../entropy_decoder/entropy_decoder_kernel_abstract.h
entropy_coder.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_2.h
entropy_coder.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_c.h
entropy_coder.o: tester.h ../../dlib/map.h ../../dlib/logger.h
entropy_coder.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
entropy_coder.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
entropy_coder.o: ../threads/threads_kernel_2.h
entropy_coder.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
entropy_coder.o: /usr/include/features.h /usr/include/sys/cdefs.h
entropy_coder.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
entropy_coder.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
entropy_coder.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
entropy_coder.o: /usr/include/time.h /usr/include/bits/sched.h
entropy_coder.o: /usr/include/signal.h /usr/include/bits/sigset.h
entropy_coder.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
entropy_coder.o: /usr/include/errno.h /usr/include/bits/errno.h
entropy_coder.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
entropy_coder.o: /usr/include/asm-i386/errno.h
entropy_coder.o: /usr/include/asm-generic/errno.h
entropy_coder.o: /usr/include/asm-generic/errno-base.h
entropy_coder.o: /usr/include/sys/time.h /usr/include/bits/time.h
entropy_coder.o: /usr/include/sys/select.h /usr/include/bits/select.h
entropy_coder.o: ../threads/threads_kernel_shared.h
entropy_coder.o: ../threads/auto_mutex_extension.h
entropy_coder.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
entropy_coder.o: ../threads/rmutex_extension_abstract.h
entropy_coder.o: ../threads/auto_mutex_extension_abstract.h
entropy_coder.o: ../binary_search_tree.h
entropy_coder.o: ../binary_search_tree/binary_search_tree_kernel_1.h
entropy_coder.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
entropy_coder.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
entropy_coder.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
entropy_coder.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
entropy_coder.o: ../binary_search_tree/binary_search_tree_kernel_2.h
entropy_coder.o: ../binary_search_tree/binary_search_tree_kernel_c.h
entropy_coder.o: ../../dlib/memory_manager.h ../member_function_pointer.h
entropy_coder.o: ../member_function_pointer/member_function_pointer_kernel_1.h
entropy_coder.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
entropy_coder.o: ../member_function_pointer/member_function_pointer_kernel_c.h
entropy_coder.o: ../memory_manager.h
entropy_coder.o: ../memory_manager/memory_manager_kernel_1.h
entropy_coder.o: ../memory_manager/memory_manager_kernel_abstract.h
entropy_coder.o: ../memory_manager/memory_manager_kernel_2.h
entropy_coder.o: ../memory_manager/memory_manager_kernel_3.h
entropy_coder.o: ../memory_manager/memory_manager_kernel_2.h
entropy_coder.o: ../binary_search_tree/binary_search_tree_kernel_2.h
entropy_coder.o: ../queue.h ../queue/queue_kernel_1.h
entropy_coder.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
entropy_coder.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
entropy_coder.o: ../queue/queue_sort_abstract.h
entropy_coder.o: ../threads/auto_mutex_extension.h
entropy_coder.o: ../threads/auto_unlock_extension.h
entropy_coder.o: ../threads/auto_unlock_extension_abstract.h
entropy_coder.o: ../threads/create_new_thread_extension.h
entropy_coder.o: ../threads/create_new_thread_extension_abstract.h
entropy_coder.o: ../threads/multithreaded_object_extension.h
entropy_coder.o: ../threads/multithreaded_object_extension_abstract.h
entropy_coder.o: ../threads/rsignaler_extension.h
entropy_coder.o: ../threads/rsignaler_extension_abstract.h ../map.h
entropy_coder.o: ../threads/rmutex_extension.h
entropy_coder.o: ../threads/rsignaler_extension.h
entropy_coder.o: ../threads/threaded_object_extension.h
entropy_coder.o: ../threads/threaded_object_extension_abstract.h
entropy_coder.o: ../threads/thread_specific_data_extension.h
entropy_coder.o: ../threads/thread_specific_data_extension_abstract.h
entropy_coder.o: ../misc_api.h ../misc_api/posix.h
entropy_coder.o: ../misc_api/misc_api_kernel_2.h
entropy_coder.o: ../misc_api/misc_api_kernel_abstract.h ../set.h
entropy_coder.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
entropy_coder.o: ../set/set_kernel_c.h binary_search_tree.h
entropy_coder.o: ../../dlib/memory_manager_global.h
entropy_coder.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
entropy_coder.o: ../memory_manager/memory_manager_kernel_abstract.h
entropy_coder.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
entropy_coder.o: ../../dlib/memory_manager_stateless.h
entropy_coder.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
entropy_coder.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
entropy_coder.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
entropy_coder.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
entropy_coder.o: ../set/set_compare_abstract.h
entropy_coder.o: ../../dlib/logger/logger_kernel_abstract.h
entropy_coder.o: ../../dlib/logger/extra_logger_headers.h
entropy_coder.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
entropy_coder.o: ../../dlib/algs.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_1.h
entropy_encoder_model.o: ../algs.h ../platform.h ../assert.h ../error.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_abstract.h
entropy_encoder_model.o: ../assert.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_2.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_3.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_4.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_5.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_6.h
entropy_encoder_model.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_c.h
entropy_encoder_model.o: conditioning_class.h ../../dlib/conditioning_class.h
entropy_encoder_model.o: ../../dlib/conditioning_class/conditioning_class_kernel_1.h
entropy_encoder_model.o: ../../dlib/conditioning_class/conditioning_class_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/conditioning_class/conditioning_class_kernel_2.h
entropy_encoder_model.o: ../../dlib/conditioning_class/conditioning_class_kernel_3.h
entropy_encoder_model.o: ../../dlib/conditioning_class/conditioning_class_kernel_4.h
entropy_encoder_model.o: ../../dlib/conditioning_class/conditioning_class_kernel_c.h
entropy_encoder_model.o: ../../dlib/memory_manager.h tester.h
entropy_encoder_model.o: ../../dlib/map.h ../../dlib/logger.h
entropy_encoder_model.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
entropy_encoder_model.o: ../threads/threads_kernel.h ../platform.h
entropy_encoder_model.o: ../threads/posix.h ../threads/threads_kernel_2.h
entropy_encoder_model.o: ../threads/threads_kernel_abstract.h
entropy_encoder_model.o: /usr/include/pthread.h /usr/include/features.h
entropy_encoder_model.o: /usr/include/sys/cdefs.h
entropy_encoder_model.o: /usr/include/bits/wordsize.h
entropy_encoder_model.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
entropy_encoder_model.o: /usr/include/sched.h /usr/include/bits/types.h
entropy_encoder_model.o: /usr/include/bits/typesizes.h /usr/include/time.h
entropy_encoder_model.o: /usr/include/bits/sched.h /usr/include/signal.h
entropy_encoder_model.o: /usr/include/bits/sigset.h
entropy_encoder_model.o: /usr/include/bits/pthreadtypes.h
entropy_encoder_model.o: /usr/include/bits/setjmp.h /usr/include/errno.h
entropy_encoder_model.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
entropy_encoder_model.o: /usr/include/asm/errno.h
entropy_encoder_model.o: /usr/include/asm-i386/errno.h
entropy_encoder_model.o: /usr/include/asm-generic/errno.h
entropy_encoder_model.o: /usr/include/asm-generic/errno-base.h
entropy_encoder_model.o: /usr/include/sys/time.h /usr/include/bits/time.h
entropy_encoder_model.o: /usr/include/sys/select.h /usr/include/bits/select.h
entropy_encoder_model.o: ../threads/threads_kernel_shared.h
entropy_encoder_model.o: ../threads/auto_mutex_extension.h
entropy_encoder_model.o: ../threads/threads_kernel.h
entropy_encoder_model.o: ../threads/rmutex_extension.h
entropy_encoder_model.o: ../threads/rmutex_extension_abstract.h
entropy_encoder_model.o: ../threads/auto_mutex_extension_abstract.h
entropy_encoder_model.o: ../binary_search_tree.h
entropy_encoder_model.o: ../binary_search_tree/binary_search_tree_kernel_1.h
entropy_encoder_model.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
entropy_encoder_model.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
entropy_encoder_model.o: ../interfaces/remover.h ../serialize.h ../algs.h
entropy_encoder_model.o: ../uintn.h ../interfaces/enumerable.h
entropy_encoder_model.o: ../interfaces/map_pair.h
entropy_encoder_model.o: ../binary_search_tree/binary_search_tree_kernel_2.h
entropy_encoder_model.o: ../binary_search_tree/binary_search_tree_kernel_c.h
entropy_encoder_model.o: ../member_function_pointer.h
entropy_encoder_model.o: ../member_function_pointer/member_function_pointer_kernel_1.h
entropy_encoder_model.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
entropy_encoder_model.o: ../member_function_pointer/member_function_pointer_kernel_c.h
entropy_encoder_model.o: ../memory_manager.h
entropy_encoder_model.o: ../memory_manager/memory_manager_kernel_1.h
entropy_encoder_model.o: ../memory_manager/memory_manager_kernel_abstract.h
entropy_encoder_model.o: ../memory_manager/memory_manager_kernel_2.h
entropy_encoder_model.o: ../memory_manager/memory_manager_kernel_3.h
entropy_encoder_model.o: ../memory_manager/memory_manager_kernel_2.h
entropy_encoder_model.o: ../binary_search_tree/binary_search_tree_kernel_2.h
entropy_encoder_model.o: ../queue.h ../queue/queue_kernel_1.h
entropy_encoder_model.o: ../queue/queue_kernel_abstract.h
entropy_encoder_model.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
entropy_encoder_model.o: ../queue/queue_sort_1.h
entropy_encoder_model.o: ../queue/queue_sort_abstract.h
entropy_encoder_model.o: ../threads/auto_mutex_extension.h
entropy_encoder_model.o: ../threads/auto_unlock_extension.h
entropy_encoder_model.o: ../threads/auto_unlock_extension_abstract.h
entropy_encoder_model.o: ../threads/create_new_thread_extension.h
entropy_encoder_model.o: ../threads/create_new_thread_extension_abstract.h
entropy_encoder_model.o: ../threads/multithreaded_object_extension.h
entropy_encoder_model.o: ../threads/multithreaded_object_extension_abstract.h
entropy_encoder_model.o: ../threads/rsignaler_extension.h
entropy_encoder_model.o: ../threads/rsignaler_extension_abstract.h ../map.h
entropy_encoder_model.o: ../threads/rmutex_extension.h
entropy_encoder_model.o: ../threads/rsignaler_extension.h
entropy_encoder_model.o: ../threads/threaded_object_extension.h
entropy_encoder_model.o: ../threads/threaded_object_extension_abstract.h
entropy_encoder_model.o: ../threads/thread_specific_data_extension.h
entropy_encoder_model.o: ../threads/thread_specific_data_extension_abstract.h
entropy_encoder_model.o: ../misc_api.h ../misc_api/posix.h
entropy_encoder_model.o: ../misc_api/misc_api_kernel_2.h
entropy_encoder_model.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
entropy_encoder_model.o: ../set.h ../set/set_kernel_1.h
entropy_encoder_model.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
entropy_encoder_model.o: binary_search_tree.h
entropy_encoder_model.o: ../../dlib/memory_manager_global.h
entropy_encoder_model.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
entropy_encoder_model.o: ../memory_manager/memory_manager_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/memory_manager_stateless.h
entropy_encoder_model.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
entropy_encoder_model.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
entropy_encoder_model.o: ../../dlib/binary_search_tree.h
entropy_encoder_model.o: ../set/set_compare_1.h ../set/set_compare_abstract.h
entropy_encoder_model.o: ../../dlib/logger/logger_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/logger/extra_logger_headers.h
entropy_encoder_model.o: ../../dlib/logger/logger_kernel_1.h
entropy_encoder_model.o: ../../dlib/assert.h ../../dlib/algs.h
entropy_encoder_model.o: ../../dlib/sliding_buffer.h
entropy_encoder_model.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_1.h
entropy_encoder_model.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_c.h
entropy_encoder_model.o: ../../dlib/entropy_decoder_model.h
entropy_encoder_model.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_1.h
entropy_encoder_model.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_2.h
entropy_encoder_model.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_3.h
entropy_encoder_model.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_4.h
entropy_encoder_model.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_5.h
entropy_encoder_model.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_6.h
entropy_encoder_model.o: ../../dlib/entropy_encoder.h
entropy_encoder_model.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_1.h
entropy_encoder_model.o: ../entropy_encoder/entropy_encoder_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_2.h
entropy_encoder_model.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_c.h
entropy_encoder_model.o: ../../dlib/entropy_decoder.h
entropy_encoder_model.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_1.h
entropy_encoder_model.o: ../entropy_decoder/entropy_decoder_kernel_abstract.h
entropy_encoder_model.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_2.h
entropy_encoder_model.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_c.h
hash_map.o: ../../dlib/hash_map.h ../../dlib/hash_map/hash_map_kernel_1.h
hash_map.o: ../../dlib/hash_map/hash_map_kernel_abstract.h ../algs.h
hash_map.o: ../platform.h ../assert.h ../error.h
hash_map.o: ../general_hash/general_hash.h ../interfaces/enumerable.h
hash_map.o: ../interfaces/map_pair.h ../interfaces/remover.h ../assert.h
hash_map.o: ../serialize.h ../algs.h ../uintn.h ../interfaces/enumerable.h
hash_map.o: ../interfaces/map_pair.h ../memory_manager.h
hash_map.o: ../memory_manager/memory_manager_kernel_1.h
hash_map.o: ../memory_manager/memory_manager_kernel_abstract.h
hash_map.o: ../memory_manager/memory_manager_kernel_2.h
hash_map.o: ../memory_manager/memory_manager_kernel_3.h
hash_map.o: ../memory_manager/memory_manager_kernel_2.h
hash_map.o: ../binary_search_tree/binary_search_tree_kernel_2.h
hash_map.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
hash_map.o: ../../dlib/hash_map/hash_map_kernel_c.h ../../dlib/hash_table.h
hash_map.o: ../../dlib/hash_table/hash_table_kernel_1.h
hash_map.o: ../../dlib/hash_table/hash_table_kernel_abstract.h
hash_map.o: ../../dlib/hash_table/hash_table_kernel_2.h
hash_map.o: ../../dlib/hash_table/hash_table_kernel_c.h
hash_map.o: ../../dlib/memory_manager.h binary_search_tree.h
hash_map.o: ../../dlib/memory_manager_global.h
hash_map.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
hash_map.o: ../memory_manager/memory_manager_kernel_abstract.h
hash_map.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
hash_map.o: ../../dlib/memory_manager_stateless.h
hash_map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
hash_map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
hash_map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
hash_map.o: ../threads.h ../threads/threads_kernel.h ../platform.h
hash_map.o: ../threads/posix.h ../threads/threads_kernel_2.h
hash_map.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
hash_map.o: /usr/include/features.h /usr/include/sys/cdefs.h
hash_map.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
hash_map.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
hash_map.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
hash_map.o: /usr/include/time.h /usr/include/bits/sched.h
hash_map.o: /usr/include/signal.h /usr/include/bits/sigset.h
hash_map.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
hash_map.o: /usr/include/errno.h /usr/include/bits/errno.h
hash_map.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
hash_map.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
hash_map.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
hash_map.o: /usr/include/bits/time.h /usr/include/sys/select.h
hash_map.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
hash_map.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
hash_map.o: ../threads/rmutex_extension.h
hash_map.o: ../threads/rmutex_extension_abstract.h
hash_map.o: ../threads/auto_mutex_extension_abstract.h
hash_map.o: ../binary_search_tree.h
hash_map.o: ../binary_search_tree/binary_search_tree_kernel_1.h
hash_map.o: ../binary_search_tree/binary_search_tree_kernel_2.h
hash_map.o: ../binary_search_tree/binary_search_tree_kernel_c.h
hash_map.o: ../member_function_pointer.h
hash_map.o: ../member_function_pointer/member_function_pointer_kernel_1.h
hash_map.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
hash_map.o: ../member_function_pointer/member_function_pointer_kernel_c.h
hash_map.o: ../queue.h ../queue/queue_kernel_1.h
hash_map.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
hash_map.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
hash_map.o: ../queue/queue_sort_abstract.h ../threads/auto_mutex_extension.h
hash_map.o: ../threads/auto_unlock_extension.h
hash_map.o: ../threads/auto_unlock_extension_abstract.h
hash_map.o: ../threads/create_new_thread_extension.h
hash_map.o: ../threads/create_new_thread_extension_abstract.h
hash_map.o: ../threads/multithreaded_object_extension.h
hash_map.o: ../threads/multithreaded_object_extension_abstract.h
hash_map.o: ../threads/rsignaler_extension.h
hash_map.o: ../threads/rsignaler_extension_abstract.h ../map.h
hash_map.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
hash_map.o: ../threads/threaded_object_extension.h
hash_map.o: ../threads/threaded_object_extension_abstract.h
hash_map.o: ../threads/thread_specific_data_extension.h
hash_map.o: ../threads/thread_specific_data_extension_abstract.h
hash_map.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
hash_map.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
hash_map.o: ../misc_api.h ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
hash_map.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
hash_map.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
hash_map.o: ../set/set_kernel_c.h ../set/set_compare_1.h
hash_map.o: ../set/set_compare_abstract.h
hash_map.o: ../../dlib/logger/logger_kernel_abstract.h
hash_map.o: ../../dlib/logger/extra_logger_headers.h
hash_map.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
hash_map.o: ../../dlib/algs.h
hash_set.o: ../../dlib/hash_set.h ../../dlib/hash_set/hash_set_kernel_1.h
hash_set.o: ../../dlib/hash_set/hash_set_kernel_abstract.h ../algs.h
hash_set.o: ../platform.h ../assert.h ../error.h
hash_set.o: ../general_hash/general_hash.h ../interfaces/enumerable.h
hash_set.o: ../interfaces/remover.h ../assert.h ../serialize.h ../algs.h
hash_set.o: ../uintn.h ../interfaces/enumerable.h ../interfaces/map_pair.h
hash_set.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
hash_set.o: ../memory_manager/memory_manager_kernel_abstract.h
hash_set.o: ../memory_manager/memory_manager_kernel_2.h
hash_set.o: ../memory_manager/memory_manager_kernel_3.h
hash_set.o: ../memory_manager/memory_manager_kernel_2.h
hash_set.o: ../binary_search_tree/binary_search_tree_kernel_2.h
hash_set.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
hash_set.o: ../interfaces/map_pair.h ../../dlib/hash_set/hash_set_kernel_c.h
hash_set.o: ../../dlib/hash_table.h
hash_set.o: ../../dlib/hash_table/hash_table_kernel_1.h
hash_set.o: ../../dlib/hash_table/hash_table_kernel_abstract.h
hash_set.o: ../../dlib/hash_table/hash_table_kernel_2.h
hash_set.o: ../../dlib/hash_table/hash_table_kernel_c.h
hash_set.o: ../../dlib/memory_manager.h binary_search_tree.h
hash_set.o: ../../dlib/memory_manager_global.h
hash_set.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
hash_set.o: ../memory_manager/memory_manager_kernel_abstract.h
hash_set.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
hash_set.o: ../../dlib/memory_manager_stateless.h
hash_set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
hash_set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
hash_set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
hash_set.o: ../threads.h ../threads/threads_kernel.h ../platform.h
hash_set.o: ../threads/posix.h ../threads/threads_kernel_2.h
hash_set.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
hash_set.o: /usr/include/features.h /usr/include/sys/cdefs.h
hash_set.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
hash_set.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
hash_set.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
hash_set.o: /usr/include/time.h /usr/include/bits/sched.h
hash_set.o: /usr/include/signal.h /usr/include/bits/sigset.h
hash_set.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
hash_set.o: /usr/include/errno.h /usr/include/bits/errno.h
hash_set.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
hash_set.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
hash_set.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
hash_set.o: /usr/include/bits/time.h /usr/include/sys/select.h
hash_set.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
hash_set.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
hash_set.o: ../threads/rmutex_extension.h
hash_set.o: ../threads/rmutex_extension_abstract.h
hash_set.o: ../threads/auto_mutex_extension_abstract.h
hash_set.o: ../binary_search_tree.h
hash_set.o: ../binary_search_tree/binary_search_tree_kernel_1.h
hash_set.o: ../binary_search_tree/binary_search_tree_kernel_2.h
hash_set.o: ../binary_search_tree/binary_search_tree_kernel_c.h
hash_set.o: ../member_function_pointer.h
hash_set.o: ../member_function_pointer/member_function_pointer_kernel_1.h
hash_set.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
hash_set.o: ../member_function_pointer/member_function_pointer_kernel_c.h
hash_set.o: ../queue.h ../queue/queue_kernel_1.h
hash_set.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
hash_set.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
hash_set.o: ../queue/queue_sort_abstract.h ../threads/auto_mutex_extension.h
hash_set.o: ../threads/auto_unlock_extension.h
hash_set.o: ../threads/auto_unlock_extension_abstract.h
hash_set.o: ../threads/create_new_thread_extension.h
hash_set.o: ../threads/create_new_thread_extension_abstract.h
hash_set.o: ../threads/multithreaded_object_extension.h
hash_set.o: ../threads/multithreaded_object_extension_abstract.h
hash_set.o: ../threads/rsignaler_extension.h
hash_set.o: ../threads/rsignaler_extension_abstract.h ../map.h
hash_set.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
hash_set.o: ../threads/threaded_object_extension.h
hash_set.o: ../threads/threaded_object_extension_abstract.h
hash_set.o: ../threads/thread_specific_data_extension.h
hash_set.o: ../threads/thread_specific_data_extension_abstract.h
hash_set.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
hash_set.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
hash_set.o: ../misc_api.h ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
hash_set.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
hash_set.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
hash_set.o: ../set/set_kernel_c.h ../set/set_compare_1.h
hash_set.o: ../set/set_compare_abstract.h
hash_set.o: ../../dlib/logger/logger_kernel_abstract.h
hash_set.o: ../../dlib/logger/extra_logger_headers.h
hash_set.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
hash_set.o: ../../dlib/algs.h
hash_table.o: ../../dlib/hash_table.h
hash_table.o: ../../dlib/hash_table/hash_table_kernel_1.h
hash_table.o: ../../dlib/hash_table/hash_table_kernel_abstract.h
hash_table.o: ../general_hash/general_hash.h ../algs.h ../platform.h
hash_table.o: ../assert.h ../error.h ../interfaces/map_pair.h
hash_table.o: ../interfaces/enumerable.h ../interfaces/remover.h ../assert.h
hash_table.o: ../serialize.h ../algs.h ../uintn.h ../interfaces/enumerable.h
hash_table.o: ../interfaces/map_pair.h ../memory_manager.h
hash_table.o: ../memory_manager/memory_manager_kernel_1.h
hash_table.o: ../memory_manager/memory_manager_kernel_abstract.h
hash_table.o: ../memory_manager/memory_manager_kernel_2.h
hash_table.o: ../memory_manager/memory_manager_kernel_3.h
hash_table.o: ../memory_manager/memory_manager_kernel_2.h
hash_table.o: ../binary_search_tree/binary_search_tree_kernel_2.h
hash_table.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
hash_table.o: ../../dlib/hash_table/hash_table_kernel_2.h
hash_table.o: ../../dlib/hash_table/hash_table_kernel_c.h
hash_table.o: ../../dlib/memory_manager.h binary_search_tree.h
hash_table.o: ../../dlib/memory_manager_global.h
hash_table.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
hash_table.o: ../memory_manager/memory_manager_kernel_abstract.h
hash_table.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
hash_table.o: ../../dlib/memory_manager_stateless.h
hash_table.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
hash_table.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
hash_table.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
hash_table.o: ../threads.h ../threads/threads_kernel.h ../platform.h
hash_table.o: ../threads/posix.h ../threads/threads_kernel_2.h
hash_table.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
hash_table.o: /usr/include/features.h /usr/include/sys/cdefs.h
hash_table.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
hash_table.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
hash_table.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
hash_table.o: /usr/include/time.h /usr/include/bits/sched.h
hash_table.o: /usr/include/signal.h /usr/include/bits/sigset.h
hash_table.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
hash_table.o: /usr/include/errno.h /usr/include/bits/errno.h
hash_table.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
hash_table.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
hash_table.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
hash_table.o: /usr/include/bits/time.h /usr/include/sys/select.h
hash_table.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
hash_table.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
hash_table.o: ../threads/rmutex_extension.h
hash_table.o: ../threads/rmutex_extension_abstract.h
hash_table.o: ../threads/auto_mutex_extension_abstract.h
hash_table.o: ../binary_search_tree.h
hash_table.o: ../binary_search_tree/binary_search_tree_kernel_1.h
hash_table.o: ../binary_search_tree/binary_search_tree_kernel_2.h
hash_table.o: ../binary_search_tree/binary_search_tree_kernel_c.h
hash_table.o: ../member_function_pointer.h
hash_table.o: ../member_function_pointer/member_function_pointer_kernel_1.h
hash_table.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
hash_table.o: ../member_function_pointer/member_function_pointer_kernel_c.h
hash_table.o: ../queue.h ../queue/queue_kernel_1.h
hash_table.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
hash_table.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
hash_table.o: ../queue/queue_sort_abstract.h
hash_table.o: ../threads/auto_mutex_extension.h
hash_table.o: ../threads/auto_unlock_extension.h
hash_table.o: ../threads/auto_unlock_extension_abstract.h
hash_table.o: ../threads/create_new_thread_extension.h
hash_table.o: ../threads/create_new_thread_extension_abstract.h
hash_table.o: ../threads/multithreaded_object_extension.h
hash_table.o: ../threads/multithreaded_object_extension_abstract.h
hash_table.o: ../threads/rsignaler_extension.h
hash_table.o: ../threads/rsignaler_extension_abstract.h ../map.h
hash_table.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
hash_table.o: ../threads/threaded_object_extension.h
hash_table.o: ../threads/threaded_object_extension_abstract.h
hash_table.o: ../threads/thread_specific_data_extension.h
hash_table.o: ../threads/thread_specific_data_extension_abstract.h
hash_table.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
hash_table.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
hash_table.o: ../misc_api.h ../misc_api/posix.h
hash_table.o: ../misc_api/misc_api_kernel_2.h
hash_table.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
hash_table.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
hash_table.o: ../set/set_kernel_c.h ../set/set_compare_1.h
hash_table.o: ../set/set_compare_abstract.h
hash_table.o: ../../dlib/logger/logger_kernel_abstract.h
hash_table.o: ../../dlib/logger/extra_logger_headers.h
hash_table.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
hash_table.o: ../../dlib/algs.h
lz77_buffer.o: ../../dlib/sliding_buffer.h
lz77_buffer.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_1.h
lz77_buffer.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_abstract.h
lz77_buffer.o: ../algs.h ../platform.h ../assert.h ../error.h
lz77_buffer.o: ../interfaces/enumerable.h ../serialize.h ../algs.h ../uintn.h
lz77_buffer.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
lz77_buffer.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_c.h
lz77_buffer.o: ../assert.h ../../dlib/lz77_buffer.h
lz77_buffer.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_1.h
lz77_buffer.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_abstract.h
lz77_buffer.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_2.h
lz77_buffer.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_c.h
lz77_buffer.o: ../../dlib/sliding_buffer.h tester.h ../../dlib/map.h
lz77_buffer.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
lz77_buffer.o: ../threads.h ../threads/threads_kernel.h ../platform.h
lz77_buffer.o: ../threads/posix.h ../threads/threads_kernel_2.h
lz77_buffer.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
lz77_buffer.o: /usr/include/features.h /usr/include/sys/cdefs.h
lz77_buffer.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
lz77_buffer.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
lz77_buffer.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
lz77_buffer.o: /usr/include/time.h /usr/include/bits/sched.h
lz77_buffer.o: /usr/include/signal.h /usr/include/bits/sigset.h
lz77_buffer.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
lz77_buffer.o: /usr/include/errno.h /usr/include/bits/errno.h
lz77_buffer.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
lz77_buffer.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
lz77_buffer.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
lz77_buffer.o: /usr/include/bits/time.h /usr/include/sys/select.h
lz77_buffer.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
lz77_buffer.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
lz77_buffer.o: ../threads/rmutex_extension.h
lz77_buffer.o: ../threads/rmutex_extension_abstract.h
lz77_buffer.o: ../threads/auto_mutex_extension_abstract.h
lz77_buffer.o: ../binary_search_tree.h
lz77_buffer.o: ../binary_search_tree/binary_search_tree_kernel_1.h
lz77_buffer.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
lz77_buffer.o: ../interfaces/map_pair.h ../interfaces/remover.h
lz77_buffer.o: ../binary_search_tree/binary_search_tree_kernel_2.h
lz77_buffer.o: ../binary_search_tree/binary_search_tree_kernel_c.h
lz77_buffer.o: ../../dlib/memory_manager.h ../member_function_pointer.h
lz77_buffer.o: ../member_function_pointer/member_function_pointer_kernel_1.h
lz77_buffer.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
lz77_buffer.o: ../member_function_pointer/member_function_pointer_kernel_c.h
lz77_buffer.o: ../memory_manager.h
lz77_buffer.o: ../memory_manager/memory_manager_kernel_1.h
lz77_buffer.o: ../memory_manager/memory_manager_kernel_abstract.h
lz77_buffer.o: ../memory_manager/memory_manager_kernel_2.h
lz77_buffer.o: ../memory_manager/memory_manager_kernel_3.h
lz77_buffer.o: ../memory_manager/memory_manager_kernel_2.h
lz77_buffer.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
lz77_buffer.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
lz77_buffer.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
lz77_buffer.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
lz77_buffer.o: ../threads/auto_mutex_extension.h
lz77_buffer.o: ../threads/auto_unlock_extension.h
lz77_buffer.o: ../threads/auto_unlock_extension_abstract.h
lz77_buffer.o: ../threads/create_new_thread_extension.h
lz77_buffer.o: ../threads/create_new_thread_extension_abstract.h
lz77_buffer.o: ../threads/multithreaded_object_extension.h
lz77_buffer.o: ../threads/multithreaded_object_extension_abstract.h
lz77_buffer.o: ../threads/rsignaler_extension.h
lz77_buffer.o: ../threads/rsignaler_extension_abstract.h ../map.h
lz77_buffer.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
lz77_buffer.o: ../threads/threaded_object_extension.h
lz77_buffer.o: ../threads/threaded_object_extension_abstract.h
lz77_buffer.o: ../threads/thread_specific_data_extension.h
lz77_buffer.o: ../threads/thread_specific_data_extension_abstract.h
lz77_buffer.o: ../misc_api.h ../misc_api/posix.h
lz77_buffer.o: ../misc_api/misc_api_kernel_2.h
lz77_buffer.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
lz77_buffer.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
lz77_buffer.o: ../set/set_kernel_c.h binary_search_tree.h
lz77_buffer.o: ../../dlib/memory_manager_global.h
lz77_buffer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
lz77_buffer.o: ../memory_manager/memory_manager_kernel_abstract.h
lz77_buffer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
lz77_buffer.o: ../../dlib/memory_manager_stateless.h
lz77_buffer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
lz77_buffer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
lz77_buffer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
lz77_buffer.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
lz77_buffer.o: ../set/set_compare_abstract.h
lz77_buffer.o: ../../dlib/logger/logger_kernel_abstract.h
lz77_buffer.o: ../../dlib/logger/extra_logger_headers.h
lz77_buffer.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
lz77_buffer.o: ../../dlib/algs.h
map.o: ../../dlib/map.h tester.h ../../dlib/logger.h
map.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
map.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
map.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
map.o: /usr/include/pthread.h /usr/include/features.h
map.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
map.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
map.o: /usr/include/sched.h /usr/include/bits/types.h
map.o: /usr/include/bits/typesizes.h /usr/include/time.h
map.o: /usr/include/bits/sched.h /usr/include/signal.h
map.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
map.o: /usr/include/bits/setjmp.h /usr/include/errno.h
map.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
map.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
map.o: /usr/include/asm-generic/errno.h /usr/include/asm-generic/errno-base.h
map.o: /usr/include/sys/time.h /usr/include/bits/time.h
map.o: /usr/include/sys/select.h /usr/include/bits/select.h ../algs.h
map.o: ../platform.h ../assert.h ../error.h
map.o: ../threads/threads_kernel_shared.h ../threads/auto_mutex_extension.h
map.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
map.o: ../threads/rmutex_extension_abstract.h
map.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
map.o: ../binary_search_tree/binary_search_tree_kernel_1.h
map.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
map.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
map.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
map.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
map.o: ../binary_search_tree/binary_search_tree_kernel_2.h
map.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
map.o: ../../dlib/memory_manager.h ../member_function_pointer.h
map.o: ../member_function_pointer/member_function_pointer_kernel_1.h
map.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
map.o: ../member_function_pointer/member_function_pointer_kernel_c.h
map.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
map.o: ../memory_manager/memory_manager_kernel_abstract.h
map.o: ../memory_manager/memory_manager_kernel_2.h
map.o: ../memory_manager/memory_manager_kernel_3.h
map.o: ../memory_manager/memory_manager_kernel_2.h
map.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
map.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
map.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
map.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
map.o: ../threads/auto_mutex_extension.h ../threads/auto_unlock_extension.h
map.o: ../threads/auto_unlock_extension_abstract.h
map.o: ../threads/create_new_thread_extension.h
map.o: ../threads/create_new_thread_extension_abstract.h
map.o: ../threads/multithreaded_object_extension.h
map.o: ../threads/multithreaded_object_extension_abstract.h
map.o: ../threads/rsignaler_extension.h
map.o: ../threads/rsignaler_extension_abstract.h ../map.h
map.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
map.o: ../threads/threaded_object_extension.h
map.o: ../threads/threaded_object_extension_abstract.h
map.o: ../threads/thread_specific_data_extension.h
map.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
map.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
map.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
map.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
map.o: ../set/set_kernel_c.h binary_search_tree.h
map.o: ../../dlib/memory_manager_global.h
map.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
map.o: ../memory_manager/memory_manager_kernel_abstract.h
map.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
map.o: ../../dlib/memory_manager_stateless.h
map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
map.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
map.o: ../set/set_compare_abstract.h
map.o: ../../dlib/logger/logger_kernel_abstract.h
map.o: ../../dlib/logger/extra_logger_headers.h
map.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
map.o: ../../dlib/algs.h
matrix.o: ../../dlib/matrix.h ../../dlib/matrix/matrix.h
matrix.o: ../../dlib/matrix/matrix_abstract.h ../algs.h ../platform.h
matrix.o: ../assert.h ../error.h ../serialize.h ../algs.h ../uintn.h
matrix.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
matrix.o: ../../dlib/matrix/matrix_utilities.h
matrix.o: ../../dlib/matrix/matrix_utilities_abstract.h
matrix.o: ../../dlib/matrix/matrix.h tester.h ../../dlib/map.h
matrix.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
matrix.o: ../threads.h ../threads/threads_kernel.h ../platform.h
matrix.o: ../threads/posix.h ../threads/threads_kernel_2.h
matrix.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
matrix.o: /usr/include/features.h /usr/include/sys/cdefs.h
matrix.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
matrix.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
matrix.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
matrix.o: /usr/include/time.h /usr/include/bits/sched.h /usr/include/signal.h
matrix.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
matrix.o: /usr/include/bits/setjmp.h /usr/include/errno.h
matrix.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
matrix.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
matrix.o: /usr/include/asm-generic/errno.h
matrix.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
matrix.o: /usr/include/bits/time.h /usr/include/sys/select.h
matrix.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
matrix.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
matrix.o: ../threads/rmutex_extension.h
matrix.o: ../threads/rmutex_extension_abstract.h
matrix.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
matrix.o: ../binary_search_tree/binary_search_tree_kernel_1.h
matrix.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
matrix.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
matrix.o: ../interfaces/remover.h
matrix.o: ../binary_search_tree/binary_search_tree_kernel_2.h
matrix.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
matrix.o: ../../dlib/memory_manager.h ../member_function_pointer.h
matrix.o: ../member_function_pointer/member_function_pointer_kernel_1.h
matrix.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
matrix.o: ../member_function_pointer/member_function_pointer_kernel_c.h
matrix.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
matrix.o: ../memory_manager/memory_manager_kernel_abstract.h
matrix.o: ../memory_manager/memory_manager_kernel_2.h
matrix.o: ../memory_manager/memory_manager_kernel_3.h
matrix.o: ../memory_manager/memory_manager_kernel_2.h
matrix.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
matrix.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
matrix.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
matrix.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
matrix.o: ../threads/auto_mutex_extension.h
matrix.o: ../threads/auto_unlock_extension.h
matrix.o: ../threads/auto_unlock_extension_abstract.h
matrix.o: ../threads/create_new_thread_extension.h
matrix.o: ../threads/create_new_thread_extension_abstract.h
matrix.o: ../threads/multithreaded_object_extension.h
matrix.o: ../threads/multithreaded_object_extension_abstract.h
matrix.o: ../threads/rsignaler_extension.h
matrix.o: ../threads/rsignaler_extension_abstract.h ../map.h
matrix.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
matrix.o: ../threads/threaded_object_extension.h
matrix.o: ../threads/threaded_object_extension_abstract.h
matrix.o: ../threads/thread_specific_data_extension.h
matrix.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
matrix.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
matrix.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
matrix.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
matrix.o: ../set/set_kernel_c.h binary_search_tree.h
matrix.o: ../../dlib/memory_manager_global.h
matrix.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
matrix.o: ../memory_manager/memory_manager_kernel_abstract.h
matrix.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
matrix.o: ../../dlib/memory_manager_stateless.h
matrix.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
matrix.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
matrix.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
matrix.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
matrix.o: ../set/set_compare_abstract.h
matrix.o: ../../dlib/logger/logger_kernel_abstract.h
matrix.o: ../../dlib/logger/extra_logger_headers.h
matrix.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
matrix.o: ../../dlib/algs.h
md5.o: ../../dlib/md5.h ../md5/md5_kernel_1.h tester.h ../../dlib/map.h
md5.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h ../threads.h
md5.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
md5.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
md5.o: /usr/include/pthread.h /usr/include/features.h
md5.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
md5.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
md5.o: /usr/include/sched.h /usr/include/bits/types.h
md5.o: /usr/include/bits/typesizes.h /usr/include/time.h
md5.o: /usr/include/bits/sched.h /usr/include/signal.h
md5.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
md5.o: /usr/include/bits/setjmp.h /usr/include/errno.h
md5.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
md5.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
md5.o: /usr/include/asm-generic/errno.h /usr/include/asm-generic/errno-base.h
md5.o: /usr/include/sys/time.h /usr/include/bits/time.h
md5.o: /usr/include/sys/select.h /usr/include/bits/select.h ../algs.h
md5.o: ../platform.h ../assert.h ../error.h
md5.o: ../threads/threads_kernel_shared.h ../threads/auto_mutex_extension.h
md5.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
md5.o: ../threads/rmutex_extension_abstract.h
md5.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
md5.o: ../binary_search_tree/binary_search_tree_kernel_1.h
md5.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
md5.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
md5.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
md5.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
md5.o: ../binary_search_tree/binary_search_tree_kernel_2.h
md5.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
md5.o: ../../dlib/memory_manager.h ../member_function_pointer.h
md5.o: ../member_function_pointer/member_function_pointer_kernel_1.h
md5.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
md5.o: ../member_function_pointer/member_function_pointer_kernel_c.h
md5.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
md5.o: ../memory_manager/memory_manager_kernel_abstract.h
md5.o: ../memory_manager/memory_manager_kernel_2.h
md5.o: ../memory_manager/memory_manager_kernel_3.h
md5.o: ../memory_manager/memory_manager_kernel_2.h
md5.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
md5.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
md5.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
md5.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
md5.o: ../threads/auto_mutex_extension.h ../threads/auto_unlock_extension.h
md5.o: ../threads/auto_unlock_extension_abstract.h
md5.o: ../threads/create_new_thread_extension.h
md5.o: ../threads/create_new_thread_extension_abstract.h
md5.o: ../threads/multithreaded_object_extension.h
md5.o: ../threads/multithreaded_object_extension_abstract.h
md5.o: ../threads/rsignaler_extension.h
md5.o: ../threads/rsignaler_extension_abstract.h ../map.h
md5.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
md5.o: ../threads/threaded_object_extension.h
md5.o: ../threads/threaded_object_extension_abstract.h
md5.o: ../threads/thread_specific_data_extension.h
md5.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
md5.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
md5.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
md5.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
md5.o: ../set/set_kernel_c.h binary_search_tree.h
md5.o: ../../dlib/memory_manager_global.h
md5.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
md5.o: ../memory_manager/memory_manager_kernel_abstract.h
md5.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
md5.o: ../../dlib/memory_manager_stateless.h
md5.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
md5.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
md5.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
md5.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
md5.o: ../set/set_compare_abstract.h
md5.o: ../../dlib/logger/logger_kernel_abstract.h
md5.o: ../../dlib/logger/extra_logger_headers.h
md5.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
md5.o: ../../dlib/algs.h
member_function_pointer.o: ../../dlib/member_function_pointer.h
member_function_pointer.o: ../member_function_pointer/member_function_pointer_kernel_1.h
member_function_pointer.o: ../algs.h ../platform.h ../assert.h ../error.h
member_function_pointer.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
member_function_pointer.o: ../member_function_pointer/member_function_pointer_kernel_c.h
member_function_pointer.o: ../assert.h tester.h ../../dlib/map.h
member_function_pointer.o: ../../dlib/logger.h
member_function_pointer.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
member_function_pointer.o: ../threads/threads_kernel.h ../platform.h
member_function_pointer.o: ../threads/posix.h ../threads/threads_kernel_2.h
member_function_pointer.o: ../threads/threads_kernel_abstract.h
member_function_pointer.o: /usr/include/pthread.h /usr/include/features.h
member_function_pointer.o: /usr/include/sys/cdefs.h
member_function_pointer.o: /usr/include/bits/wordsize.h
member_function_pointer.o: /usr/include/gnu/stubs.h
member_function_pointer.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
member_function_pointer.o: /usr/include/bits/types.h
member_function_pointer.o: /usr/include/bits/typesizes.h /usr/include/time.h
member_function_pointer.o: /usr/include/bits/sched.h /usr/include/signal.h
member_function_pointer.o: /usr/include/bits/sigset.h
member_function_pointer.o: /usr/include/bits/pthreadtypes.h
member_function_pointer.o: /usr/include/bits/setjmp.h /usr/include/errno.h
member_function_pointer.o: /usr/include/bits/errno.h
member_function_pointer.o: /usr/include/linux/errno.h
member_function_pointer.o: /usr/include/asm/errno.h
member_function_pointer.o: /usr/include/asm-i386/errno.h
member_function_pointer.o: /usr/include/asm-generic/errno.h
member_function_pointer.o: /usr/include/asm-generic/errno-base.h
member_function_pointer.o: /usr/include/sys/time.h /usr/include/bits/time.h
member_function_pointer.o: /usr/include/sys/select.h
member_function_pointer.o: /usr/include/bits/select.h
member_function_pointer.o: ../threads/threads_kernel_shared.h
member_function_pointer.o: ../threads/auto_mutex_extension.h
member_function_pointer.o: ../threads/threads_kernel.h
member_function_pointer.o: ../threads/rmutex_extension.h
member_function_pointer.o: ../threads/rmutex_extension_abstract.h
member_function_pointer.o: ../threads/auto_mutex_extension_abstract.h
member_function_pointer.o: ../binary_search_tree.h
member_function_pointer.o: ../binary_search_tree/binary_search_tree_kernel_1.h
member_function_pointer.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
member_function_pointer.o: ../interfaces/map_pair.h
member_function_pointer.o: ../interfaces/enumerable.h ../interfaces/remover.h
member_function_pointer.o: ../serialize.h ../algs.h ../uintn.h
member_function_pointer.o: ../interfaces/enumerable.h
member_function_pointer.o: ../interfaces/map_pair.h
member_function_pointer.o: ../binary_search_tree/binary_search_tree_kernel_2.h
member_function_pointer.o: ../binary_search_tree/binary_search_tree_kernel_c.h
member_function_pointer.o: ../../dlib/memory_manager.h
member_function_pointer.o: ../member_function_pointer.h ../memory_manager.h
member_function_pointer.o: ../memory_manager/memory_manager_kernel_1.h
member_function_pointer.o: ../memory_manager/memory_manager_kernel_abstract.h
member_function_pointer.o: ../memory_manager/memory_manager_kernel_2.h
member_function_pointer.o: ../memory_manager/memory_manager_kernel_3.h
member_function_pointer.o: ../memory_manager/memory_manager_kernel_2.h
member_function_pointer.o: ../binary_search_tree/binary_search_tree_kernel_2.h
member_function_pointer.o: ../queue.h ../queue/queue_kernel_1.h
member_function_pointer.o: ../queue/queue_kernel_abstract.h
member_function_pointer.o: ../queue/queue_kernel_2.h
member_function_pointer.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
member_function_pointer.o: ../queue/queue_sort_abstract.h
member_function_pointer.o: ../threads/auto_mutex_extension.h
member_function_pointer.o: ../threads/auto_unlock_extension.h
member_function_pointer.o: ../threads/auto_unlock_extension_abstract.h
member_function_pointer.o: ../threads/create_new_thread_extension.h
member_function_pointer.o: ../threads/create_new_thread_extension_abstract.h
member_function_pointer.o: ../threads/multithreaded_object_extension.h
member_function_pointer.o: ../threads/multithreaded_object_extension_abstract.h
member_function_pointer.o: ../threads/rsignaler_extension.h
member_function_pointer.o: ../threads/rsignaler_extension_abstract.h ../map.h
member_function_pointer.o: ../threads/rmutex_extension.h
member_function_pointer.o: ../threads/rsignaler_extension.h
member_function_pointer.o: ../threads/threaded_object_extension.h
member_function_pointer.o: ../threads/threaded_object_extension_abstract.h
member_function_pointer.o: ../threads/thread_specific_data_extension.h
member_function_pointer.o: ../threads/thread_specific_data_extension_abstract.h
member_function_pointer.o: ../misc_api.h ../misc_api/posix.h
member_function_pointer.o: ../misc_api/misc_api_kernel_2.h
member_function_pointer.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
member_function_pointer.o: ../set.h ../set/set_kernel_1.h
member_function_pointer.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
member_function_pointer.o: binary_search_tree.h
member_function_pointer.o: ../../dlib/memory_manager_global.h
member_function_pointer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
member_function_pointer.o: ../memory_manager/memory_manager_kernel_abstract.h
member_function_pointer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
member_function_pointer.o: ../../dlib/memory_manager_stateless.h
member_function_pointer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
member_function_pointer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
member_function_pointer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
member_function_pointer.o: ../../dlib/binary_search_tree.h
member_function_pointer.o: ../set/set_compare_1.h
member_function_pointer.o: ../set/set_compare_abstract.h
member_function_pointer.o: ../../dlib/logger/logger_kernel_abstract.h
member_function_pointer.o: ../../dlib/logger/extra_logger_headers.h
member_function_pointer.o: ../../dlib/logger/logger_kernel_1.h
member_function_pointer.o: ../../dlib/assert.h ../../dlib/algs.h
multithreaded_object.o: ../../dlib/threads.h ../threads/threads_kernel.h
multithreaded_object.o: ../platform.h ../threads/posix.h
multithreaded_object.o: ../threads/threads_kernel_2.h
multithreaded_object.o: ../threads/threads_kernel_abstract.h
multithreaded_object.o: /usr/include/pthread.h /usr/include/features.h
multithreaded_object.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
multithreaded_object.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
multithreaded_object.o: /usr/include/sched.h /usr/include/bits/types.h
multithreaded_object.o: /usr/include/bits/typesizes.h /usr/include/time.h
multithreaded_object.o: /usr/include/bits/sched.h /usr/include/signal.h
multithreaded_object.o: /usr/include/bits/sigset.h
multithreaded_object.o: /usr/include/bits/pthreadtypes.h
multithreaded_object.o: /usr/include/bits/setjmp.h /usr/include/errno.h
multithreaded_object.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
multithreaded_object.o: /usr/include/asm/errno.h
multithreaded_object.o: /usr/include/asm-i386/errno.h
multithreaded_object.o: /usr/include/asm-generic/errno.h
multithreaded_object.o: /usr/include/asm-generic/errno-base.h
multithreaded_object.o: /usr/include/sys/time.h /usr/include/bits/time.h
multithreaded_object.o: /usr/include/sys/select.h /usr/include/bits/select.h
multithreaded_object.o: ../algs.h ../platform.h ../assert.h ../error.h
multithreaded_object.o: ../threads/threads_kernel_shared.h
multithreaded_object.o: ../threads/auto_mutex_extension.h
multithreaded_object.o: ../threads/threads_kernel.h
multithreaded_object.o: ../threads/rmutex_extension.h
multithreaded_object.o: ../threads/rmutex_extension_abstract.h
multithreaded_object.o: ../threads/auto_mutex_extension_abstract.h
multithreaded_object.o: ../binary_search_tree.h
multithreaded_object.o: ../binary_search_tree/binary_search_tree_kernel_1.h
multithreaded_object.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
multithreaded_object.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
multithreaded_object.o: ../interfaces/remover.h ../serialize.h ../algs.h
multithreaded_object.o: ../uintn.h ../interfaces/enumerable.h
multithreaded_object.o: ../interfaces/map_pair.h
multithreaded_object.o: ../binary_search_tree/binary_search_tree_kernel_2.h
multithreaded_object.o: ../binary_search_tree/binary_search_tree_kernel_c.h
multithreaded_object.o: ../assert.h ../../dlib/memory_manager.h
multithreaded_object.o: ../member_function_pointer.h
multithreaded_object.o: ../member_function_pointer/member_function_pointer_kernel_1.h
multithreaded_object.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
multithreaded_object.o: ../member_function_pointer/member_function_pointer_kernel_c.h
multithreaded_object.o: ../memory_manager.h
multithreaded_object.o: ../memory_manager/memory_manager_kernel_1.h
multithreaded_object.o: ../memory_manager/memory_manager_kernel_abstract.h
multithreaded_object.o: ../memory_manager/memory_manager_kernel_2.h
multithreaded_object.o: ../memory_manager/memory_manager_kernel_3.h
multithreaded_object.o: ../memory_manager/memory_manager_kernel_2.h
multithreaded_object.o: ../binary_search_tree/binary_search_tree_kernel_2.h
multithreaded_object.o: ../queue.h ../queue/queue_kernel_1.h
multithreaded_object.o: ../queue/queue_kernel_abstract.h
multithreaded_object.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
multithreaded_object.o: ../queue/queue_sort_1.h
multithreaded_object.o: ../queue/queue_sort_abstract.h
multithreaded_object.o: ../threads/auto_mutex_extension.h
multithreaded_object.o: ../threads/auto_unlock_extension.h
multithreaded_object.o: ../threads/auto_unlock_extension_abstract.h
multithreaded_object.o: ../threads/create_new_thread_extension.h
multithreaded_object.o: ../threads/create_new_thread_extension_abstract.h
multithreaded_object.o: ../threads.h
multithreaded_object.o: ../threads/multithreaded_object_extension.h
multithreaded_object.o: ../threads/multithreaded_object_extension_abstract.h
multithreaded_object.o: ../threads/rsignaler_extension.h
multithreaded_object.o: ../threads/rsignaler_extension_abstract.h ../map.h
multithreaded_object.o: ../threads/rmutex_extension.h
multithreaded_object.o: ../threads/rsignaler_extension.h
multithreaded_object.o: ../threads/threaded_object_extension.h
multithreaded_object.o: ../threads/threaded_object_extension_abstract.h
multithreaded_object.o: ../threads/thread_specific_data_extension.h
multithreaded_object.o: ../threads/thread_specific_data_extension_abstract.h
multithreaded_object.o: tester.h ../../dlib/map.h ../../dlib/logger.h
multithreaded_object.o: ../../dlib/logger/logger_kernel_1.h ../misc_api.h
multithreaded_object.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
multithreaded_object.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
multithreaded_object.o: ../set.h ../set/set_kernel_1.h
multithreaded_object.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
multithreaded_object.o: binary_search_tree.h
multithreaded_object.o: ../../dlib/memory_manager_global.h
multithreaded_object.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
multithreaded_object.o: ../memory_manager/memory_manager_kernel_abstract.h
multithreaded_object.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
multithreaded_object.o: ../../dlib/memory_manager_stateless.h
multithreaded_object.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
multithreaded_object.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
multithreaded_object.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
multithreaded_object.o: ../../dlib/binary_search_tree.h
multithreaded_object.o: ../set/set_compare_1.h ../set/set_compare_abstract.h
multithreaded_object.o: ../../dlib/logger/logger_kernel_abstract.h
multithreaded_object.o: ../../dlib/logger/extra_logger_headers.h
multithreaded_object.o: ../../dlib/logger/logger_kernel_1.h
multithreaded_object.o: ../../dlib/assert.h ../../dlib/algs.h
pipe.o: ../../dlib/misc_api.h ../platform.h ../misc_api/posix.h
pipe.o: ../misc_api/misc_api_kernel_2.h
pipe.o: ../misc_api/misc_api_kernel_abstract.h ../algs.h ../assert.h
pipe.o: ../error.h ../uintn.h ../../dlib/pipe.h
pipe.o: ../../dlib/pipe/pipe_kernel_1.h ../threads.h
pipe.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
pipe.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
pipe.o: /usr/include/pthread.h /usr/include/features.h
pipe.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
pipe.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
pipe.o: /usr/include/sched.h /usr/include/bits/types.h
pipe.o: /usr/include/bits/typesizes.h /usr/include/time.h
pipe.o: /usr/include/bits/sched.h /usr/include/signal.h
pipe.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
pipe.o: /usr/include/bits/setjmp.h /usr/include/errno.h
pipe.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
pipe.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
pipe.o: /usr/include/asm-generic/errno.h
pipe.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
pipe.o: /usr/include/bits/time.h /usr/include/sys/select.h
pipe.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
pipe.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
pipe.o: ../threads/rmutex_extension.h ../threads/rmutex_extension_abstract.h
pipe.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
pipe.o: ../binary_search_tree/binary_search_tree_kernel_1.h
pipe.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
pipe.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
pipe.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
pipe.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
pipe.o: ../binary_search_tree/binary_search_tree_kernel_2.h
pipe.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
pipe.o: ../../dlib/memory_manager.h ../member_function_pointer.h
pipe.o: ../member_function_pointer/member_function_pointer_kernel_1.h
pipe.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
pipe.o: ../member_function_pointer/member_function_pointer_kernel_c.h
pipe.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
pipe.o: ../memory_manager/memory_manager_kernel_abstract.h
pipe.o: ../memory_manager/memory_manager_kernel_2.h
pipe.o: ../memory_manager/memory_manager_kernel_3.h
pipe.o: ../memory_manager/memory_manager_kernel_2.h
pipe.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
pipe.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
pipe.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
pipe.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
pipe.o: ../threads/auto_mutex_extension.h ../threads/auto_unlock_extension.h
pipe.o: ../threads/auto_unlock_extension_abstract.h
pipe.o: ../threads/create_new_thread_extension.h
pipe.o: ../threads/create_new_thread_extension_abstract.h
pipe.o: ../threads/multithreaded_object_extension.h
pipe.o: ../threads/multithreaded_object_extension_abstract.h
pipe.o: ../threads/rsignaler_extension.h
pipe.o: ../threads/rsignaler_extension_abstract.h ../map.h
pipe.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
pipe.o: ../threads/threaded_object_extension.h
pipe.o: ../threads/threaded_object_extension_abstract.h
pipe.o: ../threads/thread_specific_data_extension.h
pipe.o: ../threads/thread_specific_data_extension_abstract.h
pipe.o: ../../dlib/pipe/pipe_kernel_abstract.h tester.h ../../dlib/map.h
pipe.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h ../misc_api.h
pipe.o: ../set.h ../set/set_kernel_1.h ../set/set_kernel_abstract.h
pipe.o: ../set/set_kernel_c.h binary_search_tree.h
pipe.o: ../../dlib/memory_manager_global.h
pipe.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
pipe.o: ../memory_manager/memory_manager_kernel_abstract.h
pipe.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
pipe.o: ../../dlib/memory_manager_stateless.h
pipe.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
pipe.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
pipe.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
pipe.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
pipe.o: ../set/set_compare_abstract.h
pipe.o: ../../dlib/logger/logger_kernel_abstract.h
pipe.o: ../../dlib/logger/extra_logger_headers.h
pipe.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
pipe.o: ../../dlib/algs.h
queue.o: ../../dlib/queue.h ../queue/queue_kernel_1.h
queue.o: ../queue/queue_kernel_abstract.h ../algs.h ../platform.h ../assert.h
queue.o: ../error.h ../interfaces/enumerable.h ../interfaces/remover.h
queue.o: ../serialize.h ../algs.h ../uintn.h ../interfaces/enumerable.h
queue.o: ../interfaces/map_pair.h ../memory_manager.h
queue.o: ../memory_manager/memory_manager_kernel_1.h
queue.o: ../memory_manager/memory_manager_kernel_abstract.h ../assert.h
queue.o: ../memory_manager/memory_manager_kernel_2.h
queue.o: ../memory_manager/memory_manager_kernel_3.h
queue.o: ../memory_manager/memory_manager_kernel_2.h
queue.o: ../binary_search_tree/binary_search_tree_kernel_2.h
queue.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
queue.o: ../interfaces/map_pair.h ../queue/queue_kernel_2.h
queue.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
queue.o: ../queue/queue_sort_abstract.h ../../dlib/memory_manager.h tester.h
queue.o: ../../dlib/map.h ../../dlib/logger.h
queue.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
queue.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
queue.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
queue.o: /usr/include/pthread.h /usr/include/features.h
queue.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
queue.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
queue.o: /usr/include/sched.h /usr/include/bits/types.h
queue.o: /usr/include/bits/typesizes.h /usr/include/time.h
queue.o: /usr/include/bits/sched.h /usr/include/signal.h
queue.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
queue.o: /usr/include/bits/setjmp.h /usr/include/errno.h
queue.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
queue.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
queue.o: /usr/include/asm-generic/errno.h
queue.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
queue.o: /usr/include/bits/time.h /usr/include/sys/select.h
queue.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
queue.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
queue.o: ../threads/rmutex_extension.h ../threads/rmutex_extension_abstract.h
queue.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
queue.o: ../binary_search_tree/binary_search_tree_kernel_1.h
queue.o: ../binary_search_tree/binary_search_tree_kernel_2.h
queue.o: ../binary_search_tree/binary_search_tree_kernel_c.h
queue.o: ../member_function_pointer.h
queue.o: ../member_function_pointer/member_function_pointer_kernel_1.h
queue.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
queue.o: ../member_function_pointer/member_function_pointer_kernel_c.h
queue.o: ../queue.h ../threads/auto_mutex_extension.h
queue.o: ../threads/auto_unlock_extension.h
queue.o: ../threads/auto_unlock_extension_abstract.h
queue.o: ../threads/create_new_thread_extension.h
queue.o: ../threads/create_new_thread_extension_abstract.h
queue.o: ../threads/multithreaded_object_extension.h
queue.o: ../threads/multithreaded_object_extension_abstract.h
queue.o: ../threads/rsignaler_extension.h
queue.o: ../threads/rsignaler_extension_abstract.h ../map.h
queue.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
queue.o: ../threads/threaded_object_extension.h
queue.o: ../threads/threaded_object_extension_abstract.h
queue.o: ../threads/thread_specific_data_extension.h
queue.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
queue.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
queue.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
queue.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
queue.o: ../set/set_kernel_c.h binary_search_tree.h
queue.o: ../../dlib/memory_manager_global.h
queue.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
queue.o: ../memory_manager/memory_manager_kernel_abstract.h
queue.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
queue.o: ../../dlib/memory_manager_stateless.h
queue.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
queue.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
queue.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
queue.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
queue.o: ../set/set_compare_abstract.h
queue.o: ../../dlib/logger/logger_kernel_abstract.h
queue.o: ../../dlib/logger/extra_logger_headers.h
queue.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
queue.o: ../../dlib/algs.h
rand.o: ../../dlib/rand.h ../../dlib/rand/rand_kernel_1.h ../algs.h
rand.o: ../platform.h ../assert.h ../error.h ../rand/rand_kernel_abstract.h
rand.o: ../../dlib/rand/rand_kernel_2.h ../algs.h
rand.o: ../../dlib/compress_stream.h
rand.o: ../../dlib/compress_stream/compress_stream_kernel_1.h
rand.o: ../../dlib/compress_stream/compress_stream_kernel_abstract.h
rand.o: ../../dlib/compress_stream/compress_stream_kernel_2.h
rand.o: ../../dlib/compress_stream/compress_stream_kernel_3.h ../assert.h
rand.o: conditioning_class.h ../../dlib/conditioning_class.h
rand.o: ../../dlib/conditioning_class/conditioning_class_kernel_1.h
rand.o: ../../dlib/conditioning_class/conditioning_class_kernel_abstract.h
rand.o: ../../dlib/conditioning_class/conditioning_class_kernel_2.h
rand.o: ../../dlib/conditioning_class/conditioning_class_kernel_3.h
rand.o: ../../dlib/conditioning_class/conditioning_class_kernel_4.h
rand.o: ../../dlib/conditioning_class/conditioning_class_kernel_c.h
rand.o: ../../dlib/memory_manager.h tester.h ../../dlib/map.h
rand.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h ../threads.h
rand.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
rand.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
rand.o: /usr/include/pthread.h /usr/include/features.h
rand.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
rand.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
rand.o: /usr/include/sched.h /usr/include/bits/types.h
rand.o: /usr/include/bits/typesizes.h /usr/include/time.h
rand.o: /usr/include/bits/sched.h /usr/include/signal.h
rand.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
rand.o: /usr/include/bits/setjmp.h /usr/include/errno.h
rand.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
rand.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
rand.o: /usr/include/asm-generic/errno.h
rand.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
rand.o: /usr/include/bits/time.h /usr/include/sys/select.h
rand.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
rand.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
rand.o: ../threads/rmutex_extension.h ../threads/rmutex_extension_abstract.h
rand.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
rand.o: ../binary_search_tree/binary_search_tree_kernel_1.h
rand.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
rand.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
rand.o: ../interfaces/remover.h ../serialize.h ../uintn.h
rand.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
rand.o: ../binary_search_tree/binary_search_tree_kernel_2.h
rand.o: ../binary_search_tree/binary_search_tree_kernel_c.h
rand.o: ../member_function_pointer.h
rand.o: ../member_function_pointer/member_function_pointer_kernel_1.h
rand.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
rand.o: ../member_function_pointer/member_function_pointer_kernel_c.h
rand.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
rand.o: ../memory_manager/memory_manager_kernel_abstract.h
rand.o: ../memory_manager/memory_manager_kernel_2.h
rand.o: ../memory_manager/memory_manager_kernel_3.h
rand.o: ../memory_manager/memory_manager_kernel_2.h
rand.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
rand.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
rand.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
rand.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
rand.o: ../threads/auto_mutex_extension.h ../threads/auto_unlock_extension.h
rand.o: ../threads/auto_unlock_extension_abstract.h
rand.o: ../threads/create_new_thread_extension.h
rand.o: ../threads/create_new_thread_extension_abstract.h
rand.o: ../threads/multithreaded_object_extension.h
rand.o: ../threads/multithreaded_object_extension_abstract.h
rand.o: ../threads/rsignaler_extension.h
rand.o: ../threads/rsignaler_extension_abstract.h ../map.h
rand.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
rand.o: ../threads/threaded_object_extension.h
rand.o: ../threads/threaded_object_extension_abstract.h
rand.o: ../threads/thread_specific_data_extension.h
rand.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
rand.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
rand.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
rand.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
rand.o: ../set/set_kernel_c.h binary_search_tree.h
rand.o: ../../dlib/memory_manager_global.h
rand.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
rand.o: ../memory_manager/memory_manager_kernel_abstract.h
rand.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
rand.o: ../../dlib/memory_manager_stateless.h
rand.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
rand.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
rand.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
rand.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
rand.o: ../set/set_compare_abstract.h
rand.o: ../../dlib/logger/logger_kernel_abstract.h
rand.o: ../../dlib/logger/extra_logger_headers.h
rand.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
rand.o: ../../dlib/algs.h ../../dlib/entropy_encoder.h
rand.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_1.h
rand.o: ../entropy_encoder/entropy_encoder_kernel_abstract.h
rand.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_2.h
rand.o: ../../dlib/entropy_encoder/entropy_encoder_kernel_c.h
rand.o: ../../dlib/entropy_decoder.h
rand.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_1.h
rand.o: ../entropy_decoder/entropy_decoder_kernel_abstract.h
rand.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_2.h
rand.o: ../../dlib/entropy_decoder/entropy_decoder_kernel_c.h
rand.o: ../../dlib/entropy_encoder_model.h
rand.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_1.h
rand.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_abstract.h
rand.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_2.h
rand.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_3.h
rand.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_4.h
rand.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_5.h
rand.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_6.h
rand.o: ../../dlib/entropy_encoder_model/entropy_encoder_model_kernel_c.h
rand.o: ../../dlib/sliding_buffer.h
rand.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_1.h
rand.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_abstract.h
rand.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_c.h
rand.o: ../../dlib/entropy_decoder_model.h
rand.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_1.h
rand.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_abstract.h
rand.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_2.h
rand.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_3.h
rand.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_4.h
rand.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_5.h
rand.o: ../../dlib/entropy_decoder_model/entropy_decoder_model_kernel_6.h
rand.o: ../../dlib/lz77_buffer.h
rand.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_1.h
rand.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_abstract.h
rand.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_2.h
rand.o: ../../dlib/lz77_buffer/lz77_buffer_kernel_c.h ../../dlib/lzp_buffer.h
rand.o: ../../dlib/lzp_buffer/lzp_buffer_kernel_1.h
rand.o: ../../dlib/lzp_buffer/lzp_buffer_kernel_abstract.h
rand.o: ../../dlib/lzp_buffer/lzp_buffer_kernel_2.h
rand.o: ../../dlib/lzp_buffer/lzp_buffer_kernel_c.h ../../dlib/crc32.h
rand.o: ../../dlib/crc32/crc32_kernel_1.h
rand.o: ../../dlib/crc32/crc32_kernel_abstract.h
reference_counter.o: ../../dlib/reference_counter.h
reference_counter.o: ../../dlib/reference_counter/reference_counter_kernel_1.h
reference_counter.o: ../../dlib/reference_counter/reference_counter_kernel_abstract.h
reference_counter.o: ../algs.h ../platform.h ../assert.h ../error.h ../algs.h
reference_counter.o: tester.h ../../dlib/map.h ../../dlib/logger.h
reference_counter.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
reference_counter.o: ../threads/threads_kernel.h ../platform.h
reference_counter.o: ../threads/posix.h ../threads/threads_kernel_2.h
reference_counter.o: ../threads/threads_kernel_abstract.h
reference_counter.o: /usr/include/pthread.h /usr/include/features.h
reference_counter.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
reference_counter.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
reference_counter.o: /usr/include/sched.h /usr/include/bits/types.h
reference_counter.o: /usr/include/bits/typesizes.h /usr/include/time.h
reference_counter.o: /usr/include/bits/sched.h /usr/include/signal.h
reference_counter.o: /usr/include/bits/sigset.h
reference_counter.o: /usr/include/bits/pthreadtypes.h
reference_counter.o: /usr/include/bits/setjmp.h /usr/include/errno.h
reference_counter.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
reference_counter.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
reference_counter.o: /usr/include/asm-generic/errno.h
reference_counter.o: /usr/include/asm-generic/errno-base.h
reference_counter.o: /usr/include/sys/time.h /usr/include/bits/time.h
reference_counter.o: /usr/include/sys/select.h /usr/include/bits/select.h
reference_counter.o: ../threads/threads_kernel_shared.h
reference_counter.o: ../threads/auto_mutex_extension.h
reference_counter.o: ../threads/threads_kernel.h
reference_counter.o: ../threads/rmutex_extension.h
reference_counter.o: ../threads/rmutex_extension_abstract.h
reference_counter.o: ../threads/auto_mutex_extension_abstract.h
reference_counter.o: ../binary_search_tree.h
reference_counter.o: ../binary_search_tree/binary_search_tree_kernel_1.h
reference_counter.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
reference_counter.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
reference_counter.o: ../interfaces/remover.h ../serialize.h ../uintn.h
reference_counter.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
reference_counter.o: ../binary_search_tree/binary_search_tree_kernel_2.h
reference_counter.o: ../binary_search_tree/binary_search_tree_kernel_c.h
reference_counter.o: ../assert.h ../../dlib/memory_manager.h
reference_counter.o: ../member_function_pointer.h
reference_counter.o: ../member_function_pointer/member_function_pointer_kernel_1.h
reference_counter.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
reference_counter.o: ../member_function_pointer/member_function_pointer_kernel_c.h
reference_counter.o: ../memory_manager.h
reference_counter.o: ../memory_manager/memory_manager_kernel_1.h
reference_counter.o: ../memory_manager/memory_manager_kernel_abstract.h
reference_counter.o: ../memory_manager/memory_manager_kernel_2.h
reference_counter.o: ../memory_manager/memory_manager_kernel_3.h
reference_counter.o: ../memory_manager/memory_manager_kernel_2.h
reference_counter.o: ../binary_search_tree/binary_search_tree_kernel_2.h
reference_counter.o: ../queue.h ../queue/queue_kernel_1.h
reference_counter.o: ../queue/queue_kernel_abstract.h
reference_counter.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
reference_counter.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
reference_counter.o: ../threads/auto_mutex_extension.h
reference_counter.o: ../threads/auto_unlock_extension.h
reference_counter.o: ../threads/auto_unlock_extension_abstract.h
reference_counter.o: ../threads/create_new_thread_extension.h
reference_counter.o: ../threads/create_new_thread_extension_abstract.h
reference_counter.o: ../threads/multithreaded_object_extension.h
reference_counter.o: ../threads/multithreaded_object_extension_abstract.h
reference_counter.o: ../threads/rsignaler_extension.h
reference_counter.o: ../threads/rsignaler_extension_abstract.h ../map.h
reference_counter.o: ../threads/rmutex_extension.h
reference_counter.o: ../threads/rsignaler_extension.h
reference_counter.o: ../threads/threaded_object_extension.h
reference_counter.o: ../threads/threaded_object_extension_abstract.h
reference_counter.o: ../threads/thread_specific_data_extension.h
reference_counter.o: ../threads/thread_specific_data_extension_abstract.h
reference_counter.o: ../misc_api.h ../misc_api/posix.h
reference_counter.o: ../misc_api/misc_api_kernel_2.h
reference_counter.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
reference_counter.o: ../set.h ../set/set_kernel_1.h
reference_counter.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
reference_counter.o: binary_search_tree.h ../../dlib/memory_manager_global.h
reference_counter.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
reference_counter.o: ../memory_manager/memory_manager_kernel_abstract.h
reference_counter.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
reference_counter.o: ../../dlib/memory_manager_stateless.h
reference_counter.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
reference_counter.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
reference_counter.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
reference_counter.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
reference_counter.o: ../set/set_compare_abstract.h
reference_counter.o: ../../dlib/logger/logger_kernel_abstract.h
reference_counter.o: ../../dlib/logger/extra_logger_headers.h
reference_counter.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
reference_counter.o: ../../dlib/algs.h
sequence.o: ../../dlib/sequence.h ../../dlib/sequence/sequence_kernel_1.h
sequence.o: ../../dlib/sequence/sequence_kernel_abstract.h ../algs.h
sequence.o: ../platform.h ../assert.h ../error.h ../interfaces/enumerable.h
sequence.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
sequence.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
sequence.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
sequence.o: ../memory_manager/memory_manager_kernel_abstract.h ../assert.h
sequence.o: ../memory_manager/memory_manager_kernel_2.h
sequence.o: ../memory_manager/memory_manager_kernel_3.h
sequence.o: ../memory_manager/memory_manager_kernel_2.h
sequence.o: ../binary_search_tree/binary_search_tree_kernel_2.h
sequence.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
sequence.o: ../interfaces/map_pair.h ../../dlib/sequence/sequence_kernel_2.h
sequence.o: ../../dlib/sequence/sequence_kernel_c.h
sequence.o: ../../dlib/sequence/sequence_compare_1.h
sequence.o: ../../dlib/sequence/sequence_compare_abstract.h
sequence.o: ../../dlib/sequence/sequence_sort_1.h
sequence.o: ../../dlib/sequence/sequence_sort_abstract.h
sequence.o: ../../dlib/sequence/sequence_sort_2.h ../sort.h
sequence.o: ../../dlib/memory_manager.h tester.h ../../dlib/map.h
sequence.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
sequence.o: ../threads.h ../threads/threads_kernel.h ../platform.h
sequence.o: ../threads/posix.h ../threads/threads_kernel_2.h
sequence.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
sequence.o: /usr/include/features.h /usr/include/sys/cdefs.h
sequence.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
sequence.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
sequence.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
sequence.o: /usr/include/time.h /usr/include/bits/sched.h
sequence.o: /usr/include/signal.h /usr/include/bits/sigset.h
sequence.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
sequence.o: /usr/include/errno.h /usr/include/bits/errno.h
sequence.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
sequence.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
sequence.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
sequence.o: /usr/include/bits/time.h /usr/include/sys/select.h
sequence.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
sequence.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
sequence.o: ../threads/rmutex_extension.h
sequence.o: ../threads/rmutex_extension_abstract.h
sequence.o: ../threads/auto_mutex_extension_abstract.h
sequence.o: ../binary_search_tree.h
sequence.o: ../binary_search_tree/binary_search_tree_kernel_1.h
sequence.o: ../binary_search_tree/binary_search_tree_kernel_2.h
sequence.o: ../binary_search_tree/binary_search_tree_kernel_c.h
sequence.o: ../member_function_pointer.h
sequence.o: ../member_function_pointer/member_function_pointer_kernel_1.h
sequence.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
sequence.o: ../member_function_pointer/member_function_pointer_kernel_c.h
sequence.o: ../queue.h ../queue/queue_kernel_1.h
sequence.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
sequence.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
sequence.o: ../queue/queue_sort_abstract.h ../threads/auto_mutex_extension.h
sequence.o: ../threads/auto_unlock_extension.h
sequence.o: ../threads/auto_unlock_extension_abstract.h
sequence.o: ../threads/create_new_thread_extension.h
sequence.o: ../threads/create_new_thread_extension_abstract.h
sequence.o: ../threads/multithreaded_object_extension.h
sequence.o: ../threads/multithreaded_object_extension_abstract.h
sequence.o: ../threads/rsignaler_extension.h
sequence.o: ../threads/rsignaler_extension_abstract.h ../map.h
sequence.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
sequence.o: ../threads/threaded_object_extension.h
sequence.o: ../threads/threaded_object_extension_abstract.h
sequence.o: ../threads/thread_specific_data_extension.h
sequence.o: ../threads/thread_specific_data_extension_abstract.h
sequence.o: ../misc_api.h ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
sequence.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
sequence.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
sequence.o: ../set/set_kernel_c.h binary_search_tree.h
sequence.o: ../../dlib/memory_manager_global.h
sequence.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
sequence.o: ../memory_manager/memory_manager_kernel_abstract.h
sequence.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
sequence.o: ../../dlib/memory_manager_stateless.h
sequence.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
sequence.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
sequence.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
sequence.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
sequence.o: ../set/set_compare_abstract.h
sequence.o: ../../dlib/logger/logger_kernel_abstract.h
sequence.o: ../../dlib/logger/extra_logger_headers.h
sequence.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
sequence.o: ../../dlib/algs.h
set.o: ../../dlib/set.h ../set/set_kernel_1.h ../set/set_kernel_abstract.h
set.o: ../algs.h ../platform.h ../assert.h ../error.h
set.o: ../interfaces/enumerable.h ../interfaces/remover.h ../serialize.h
set.o: ../algs.h ../uintn.h ../interfaces/enumerable.h
set.o: ../interfaces/map_pair.h ../memory_manager.h
set.o: ../memory_manager/memory_manager_kernel_1.h
set.o: ../memory_manager/memory_manager_kernel_abstract.h ../assert.h
set.o: ../memory_manager/memory_manager_kernel_2.h
set.o: ../memory_manager/memory_manager_kernel_3.h
set.o: ../memory_manager/memory_manager_kernel_2.h
set.o: ../binary_search_tree/binary_search_tree_kernel_2.h
set.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
set.o: ../interfaces/map_pair.h ../set/set_kernel_c.h binary_search_tree.h
set.o: ../../dlib/memory_manager_global.h
set.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
set.o: ../memory_manager/memory_manager_kernel_abstract.h
set.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
set.o: ../../dlib/memory_manager.h ../../dlib/memory_manager_stateless.h
set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
set.o: ../threads.h ../threads/threads_kernel.h ../platform.h
set.o: ../threads/posix.h ../threads/threads_kernel_2.h
set.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
set.o: /usr/include/features.h /usr/include/sys/cdefs.h
set.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
set.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
set.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
set.o: /usr/include/time.h /usr/include/bits/sched.h /usr/include/signal.h
set.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
set.o: /usr/include/bits/setjmp.h /usr/include/errno.h
set.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
set.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
set.o: /usr/include/asm-generic/errno.h /usr/include/asm-generic/errno-base.h
set.o: /usr/include/sys/time.h /usr/include/bits/time.h
set.o: /usr/include/sys/select.h /usr/include/bits/select.h
set.o: ../threads/threads_kernel_shared.h ../threads/auto_mutex_extension.h
set.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
set.o: ../threads/rmutex_extension_abstract.h
set.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
set.o: ../binary_search_tree/binary_search_tree_kernel_1.h
set.o: ../binary_search_tree/binary_search_tree_kernel_2.h
set.o: ../binary_search_tree/binary_search_tree_kernel_c.h
set.o: ../member_function_pointer.h
set.o: ../member_function_pointer/member_function_pointer_kernel_1.h
set.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
set.o: ../member_function_pointer/member_function_pointer_kernel_c.h
set.o: ../queue.h ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
set.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
set.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
set.o: ../threads/auto_mutex_extension.h ../threads/auto_unlock_extension.h
set.o: ../threads/auto_unlock_extension_abstract.h
set.o: ../threads/create_new_thread_extension.h
set.o: ../threads/create_new_thread_extension_abstract.h
set.o: ../threads/multithreaded_object_extension.h
set.o: ../threads/multithreaded_object_extension_abstract.h
set.o: ../threads/rsignaler_extension.h
set.o: ../threads/rsignaler_extension_abstract.h ../map.h
set.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
set.o: ../threads/threaded_object_extension.h
set.o: ../threads/threaded_object_extension_abstract.h
set.o: ../threads/thread_specific_data_extension.h
set.o: ../threads/thread_specific_data_extension_abstract.h
set.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
set.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h ../misc_api.h
set.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
set.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
set.o: ../set/set_compare_1.h ../set/set_compare_abstract.h
set.o: ../../dlib/logger/logger_kernel_abstract.h
set.o: ../../dlib/logger/extra_logger_headers.h
set.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
set.o: ../../dlib/algs.h
sliding_buffer.o: ../../dlib/sliding_buffer.h
sliding_buffer.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_1.h
sliding_buffer.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_abstract.h
sliding_buffer.o: ../algs.h ../platform.h ../assert.h ../error.h
sliding_buffer.o: ../interfaces/enumerable.h ../serialize.h ../algs.h
sliding_buffer.o: ../uintn.h ../interfaces/enumerable.h
sliding_buffer.o: ../interfaces/map_pair.h
sliding_buffer.o: ../../dlib/sliding_buffer/sliding_buffer_kernel_c.h
sliding_buffer.o: ../assert.h tester.h ../../dlib/map.h ../../dlib/logger.h
sliding_buffer.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
sliding_buffer.o: ../threads/threads_kernel.h ../platform.h
sliding_buffer.o: ../threads/posix.h ../threads/threads_kernel_2.h
sliding_buffer.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
sliding_buffer.o: /usr/include/features.h /usr/include/sys/cdefs.h
sliding_buffer.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
sliding_buffer.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
sliding_buffer.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
sliding_buffer.o: /usr/include/time.h /usr/include/bits/sched.h
sliding_buffer.o: /usr/include/signal.h /usr/include/bits/sigset.h
sliding_buffer.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
sliding_buffer.o: /usr/include/errno.h /usr/include/bits/errno.h
sliding_buffer.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
sliding_buffer.o: /usr/include/asm-i386/errno.h
sliding_buffer.o: /usr/include/asm-generic/errno.h
sliding_buffer.o: /usr/include/asm-generic/errno-base.h
sliding_buffer.o: /usr/include/sys/time.h /usr/include/bits/time.h
sliding_buffer.o: /usr/include/sys/select.h /usr/include/bits/select.h
sliding_buffer.o: ../threads/threads_kernel_shared.h
sliding_buffer.o: ../threads/auto_mutex_extension.h
sliding_buffer.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
sliding_buffer.o: ../threads/rmutex_extension_abstract.h
sliding_buffer.o: ../threads/auto_mutex_extension_abstract.h
sliding_buffer.o: ../binary_search_tree.h
sliding_buffer.o: ../binary_search_tree/binary_search_tree_kernel_1.h
sliding_buffer.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
sliding_buffer.o: ../interfaces/map_pair.h ../interfaces/remover.h
sliding_buffer.o: ../binary_search_tree/binary_search_tree_kernel_2.h
sliding_buffer.o: ../binary_search_tree/binary_search_tree_kernel_c.h
sliding_buffer.o: ../../dlib/memory_manager.h ../member_function_pointer.h
sliding_buffer.o: ../member_function_pointer/member_function_pointer_kernel_1.h
sliding_buffer.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
sliding_buffer.o: ../member_function_pointer/member_function_pointer_kernel_c.h
sliding_buffer.o: ../memory_manager.h
sliding_buffer.o: ../memory_manager/memory_manager_kernel_1.h
sliding_buffer.o: ../memory_manager/memory_manager_kernel_abstract.h
sliding_buffer.o: ../memory_manager/memory_manager_kernel_2.h
sliding_buffer.o: ../memory_manager/memory_manager_kernel_3.h
sliding_buffer.o: ../memory_manager/memory_manager_kernel_2.h
sliding_buffer.o: ../binary_search_tree/binary_search_tree_kernel_2.h
sliding_buffer.o: ../queue.h ../queue/queue_kernel_1.h
sliding_buffer.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
sliding_buffer.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
sliding_buffer.o: ../queue/queue_sort_abstract.h
sliding_buffer.o: ../threads/auto_mutex_extension.h
sliding_buffer.o: ../threads/auto_unlock_extension.h
sliding_buffer.o: ../threads/auto_unlock_extension_abstract.h
sliding_buffer.o: ../threads/create_new_thread_extension.h
sliding_buffer.o: ../threads/create_new_thread_extension_abstract.h
sliding_buffer.o: ../threads/multithreaded_object_extension.h
sliding_buffer.o: ../threads/multithreaded_object_extension_abstract.h
sliding_buffer.o: ../threads/rsignaler_extension.h
sliding_buffer.o: ../threads/rsignaler_extension_abstract.h ../map.h
sliding_buffer.o: ../threads/rmutex_extension.h
sliding_buffer.o: ../threads/rsignaler_extension.h
sliding_buffer.o: ../threads/threaded_object_extension.h
sliding_buffer.o: ../threads/threaded_object_extension_abstract.h
sliding_buffer.o: ../threads/thread_specific_data_extension.h
sliding_buffer.o: ../threads/thread_specific_data_extension_abstract.h
sliding_buffer.o: ../misc_api.h ../misc_api/posix.h
sliding_buffer.o: ../misc_api/misc_api_kernel_2.h
sliding_buffer.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
sliding_buffer.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
sliding_buffer.o: ../set/set_kernel_c.h binary_search_tree.h
sliding_buffer.o: ../../dlib/memory_manager_global.h
sliding_buffer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
sliding_buffer.o: ../memory_manager/memory_manager_kernel_abstract.h
sliding_buffer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
sliding_buffer.o: ../../dlib/memory_manager_stateless.h
sliding_buffer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
sliding_buffer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
sliding_buffer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
sliding_buffer.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
sliding_buffer.o: ../set/set_compare_abstract.h
sliding_buffer.o: ../../dlib/logger/logger_kernel_abstract.h
sliding_buffer.o: ../../dlib/logger/extra_logger_headers.h
sliding_buffer.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
sliding_buffer.o: ../../dlib/algs.h
sockets.o: ../../dlib/sockets.h ../platform.h ../sockets/posix.h
sockets.o: ../sockets/sockets_kernel_2.h ../platform.h
sockets.o: ../sockets/sockets_kernel_abstract.h /usr/include/sys/types.h
sockets.o: /usr/include/features.h /usr/include/sys/cdefs.h
sockets.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
sockets.o: /usr/include/gnu/stubs-32.h /usr/include/bits/types.h
sockets.o: /usr/include/bits/typesizes.h /usr/include/time.h
sockets.o: /usr/include/endian.h /usr/include/bits/endian.h
sockets.o: /usr/include/sys/select.h /usr/include/bits/select.h
sockets.o: /usr/include/bits/sigset.h /usr/include/bits/time.h
sockets.o: /usr/include/sys/sysmacros.h /usr/include/bits/pthreadtypes.h
sockets.o: /usr/include/sys/socket.h /usr/include/sys/uio.h
sockets.o: /usr/include/bits/uio.h /usr/include/bits/socket.h
sockets.o: /usr/include/limits.h /usr/include/bits/posix1_lim.h
sockets.o: /usr/include/bits/local_lim.h /usr/include/linux/limits.h
sockets.o: /usr/include/bits/posix2_lim.h /usr/include/bits/sockaddr.h
sockets.o: /usr/include/asm/socket.h /usr/include/asm-i386/socket.h
sockets.o: /usr/include/asm/sockios.h /usr/include/asm-i386/sockios.h
sockets.o: /usr/include/errno.h /usr/include/bits/errno.h
sockets.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
sockets.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
sockets.o: /usr/include/asm-generic/errno-base.h /usr/include/arpa/inet.h
sockets.o: /usr/include/netinet/in.h /usr/include/stdint.h
sockets.o: /usr/include/bits/wchar.h /usr/include/bits/in.h
sockets.o: /usr/include/bits/byteswap.h /usr/include/signal.h
sockets.o: /usr/include/inttypes.h /usr/include/netdb.h
sockets.o: /usr/include/rpc/netdb.h /usr/include/bits/netdb.h
sockets.o: /usr/include/unistd.h /usr/include/bits/posix_opt.h
sockets.o: /usr/include/bits/confname.h /usr/include/getopt.h
sockets.o: /usr/include/sys/param.h /usr/include/linux/param.h
sockets.o: /usr/include/asm/param.h /usr/include/asm-i386/param.h
sockets.o: ../threads.h ../threads/threads_kernel.h ../threads/posix.h
sockets.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
sockets.o: /usr/include/pthread.h /usr/include/sched.h
sockets.o: /usr/include/bits/sched.h /usr/include/bits/setjmp.h
sockets.o: /usr/include/sys/time.h ../algs.h ../assert.h ../error.h
sockets.o: ../threads/threads_kernel_shared.h
sockets.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
sockets.o: ../threads/rmutex_extension.h
sockets.o: ../threads/rmutex_extension_abstract.h
sockets.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
sockets.o: ../binary_search_tree/binary_search_tree_kernel_1.h
sockets.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
sockets.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
sockets.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
sockets.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
sockets.o: ../binary_search_tree/binary_search_tree_kernel_2.h
sockets.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
sockets.o: ../../dlib/memory_manager.h ../member_function_pointer.h
sockets.o: ../member_function_pointer/member_function_pointer_kernel_1.h
sockets.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
sockets.o: ../member_function_pointer/member_function_pointer_kernel_c.h
sockets.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
sockets.o: ../memory_manager/memory_manager_kernel_abstract.h
sockets.o: ../memory_manager/memory_manager_kernel_2.h
sockets.o: ../memory_manager/memory_manager_kernel_3.h
sockets.o: ../memory_manager/memory_manager_kernel_2.h
sockets.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
sockets.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
sockets.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
sockets.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
sockets.o: ../threads/auto_mutex_extension.h
sockets.o: ../threads/auto_unlock_extension.h
sockets.o: ../threads/auto_unlock_extension_abstract.h
sockets.o: ../threads/create_new_thread_extension.h
sockets.o: ../threads/create_new_thread_extension_abstract.h
sockets.o: ../threads/multithreaded_object_extension.h
sockets.o: ../threads/multithreaded_object_extension_abstract.h
sockets.o: ../threads/rsignaler_extension.h
sockets.o: ../threads/rsignaler_extension_abstract.h ../map.h
sockets.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
sockets.o: ../threads/threaded_object_extension.h
sockets.o: ../threads/threaded_object_extension_abstract.h
sockets.o: ../threads/thread_specific_data_extension.h
sockets.o: ../threads/thread_specific_data_extension_abstract.h
sockets.o: ../sockets/sockets_extensions.h ../sockets.h
sockets.o: ../sockets/sockets_extensions_abstract.h ../../dlib/server.h
sockets.o: ../../dlib/server/server_kernel_1.h
sockets.o: ../../dlib/server/server_kernel_abstract.h ../logger.h
sockets.o: ../../dlib/logger/logger_kernel_1.h ../misc_api.h
sockets.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
sockets.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
sockets.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
sockets.o: ../set/set_kernel_c.h binary_search_tree.h
sockets.o: ../../dlib/memory_manager_global.h
sockets.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
sockets.o: ../memory_manager/memory_manager_kernel_abstract.h
sockets.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
sockets.o: ../../dlib/memory_manager_stateless.h
sockets.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
sockets.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
sockets.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
sockets.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
sockets.o: ../../dlib/logger.h ../../dlib/logger/extra_logger_headers.h
sockets.o: ../../dlib/logger/logger_kernel_abstract.h
sockets.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
sockets.o: ../../dlib/algs.h ../set/set_compare_1.h
sockets.o: ../set/set_compare_abstract.h ../../dlib/server/server_kernel_c.h
sockets.o: ../../dlib/server/server_iostream_1.h
sockets.o: ../../dlib/server/server_iostream_abstract.h
sockets.o: ../../dlib/server/server_http_1.h
sockets.o: ../../dlib/server/server_http_abstract.h ../../dlib/set.h
sockets.o: ../../dlib/sockstreambuf.h
sockets.o: ../../dlib/sockstreambuf/sockstreambuf_kernel_1.h
sockets.o: ../sockstreambuf/sockstreambuf_kernel_abstract.h
sockets.o: ../../dlib/sockstreambuf/sockstreambuf_kernel_2.h ../../dlib/map.h
sockets.o: ../../dlib/map/map_kernel_1.h ../../dlib/map/map_kernel_abstract.h
sockets.o: ../../dlib/map/map_kernel_c.h ../../dlib/queue.h
sockets.o: ../../dlib/misc_api.h
sockstreambuf.o: ../../dlib/sockets.h ../platform.h ../sockets/posix.h
sockstreambuf.o: ../sockets/sockets_kernel_2.h ../platform.h
sockstreambuf.o: ../sockets/sockets_kernel_abstract.h
sockstreambuf.o: /usr/include/sys/types.h /usr/include/features.h
sockstreambuf.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
sockstreambuf.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
sockstreambuf.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
sockstreambuf.o: /usr/include/time.h /usr/include/endian.h
sockstreambuf.o: /usr/include/bits/endian.h /usr/include/sys/select.h
sockstreambuf.o: /usr/include/bits/select.h /usr/include/bits/sigset.h
sockstreambuf.o: /usr/include/bits/time.h /usr/include/sys/sysmacros.h
sockstreambuf.o: /usr/include/bits/pthreadtypes.h /usr/include/sys/socket.h
sockstreambuf.o: /usr/include/sys/uio.h /usr/include/bits/uio.h
sockstreambuf.o: /usr/include/bits/socket.h /usr/include/limits.h
sockstreambuf.o: /usr/include/bits/posix1_lim.h /usr/include/bits/local_lim.h
sockstreambuf.o: /usr/include/linux/limits.h /usr/include/bits/posix2_lim.h
sockstreambuf.o: /usr/include/bits/sockaddr.h /usr/include/asm/socket.h
sockstreambuf.o: /usr/include/asm-i386/socket.h /usr/include/asm/sockios.h
sockstreambuf.o: /usr/include/asm-i386/sockios.h /usr/include/errno.h
sockstreambuf.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
sockstreambuf.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
sockstreambuf.o: /usr/include/asm-generic/errno.h
sockstreambuf.o: /usr/include/asm-generic/errno-base.h
sockstreambuf.o: /usr/include/arpa/inet.h /usr/include/netinet/in.h
sockstreambuf.o: /usr/include/stdint.h /usr/include/bits/wchar.h
sockstreambuf.o: /usr/include/bits/in.h /usr/include/bits/byteswap.h
sockstreambuf.o: /usr/include/signal.h /usr/include/inttypes.h
sockstreambuf.o: /usr/include/netdb.h /usr/include/rpc/netdb.h
sockstreambuf.o: /usr/include/bits/netdb.h /usr/include/unistd.h
sockstreambuf.o: /usr/include/bits/posix_opt.h /usr/include/bits/confname.h
sockstreambuf.o: /usr/include/getopt.h /usr/include/sys/param.h
sockstreambuf.o: /usr/include/linux/param.h /usr/include/asm/param.h
sockstreambuf.o: /usr/include/asm-i386/param.h ../threads.h
sockstreambuf.o: ../threads/threads_kernel.h ../threads/posix.h
sockstreambuf.o: ../threads/threads_kernel_2.h
sockstreambuf.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
sockstreambuf.o: /usr/include/sched.h /usr/include/bits/sched.h
sockstreambuf.o: /usr/include/bits/setjmp.h /usr/include/sys/time.h ../algs.h
sockstreambuf.o: ../assert.h ../error.h ../threads/threads_kernel_shared.h
sockstreambuf.o: ../threads/auto_mutex_extension.h
sockstreambuf.o: ../threads/threads_kernel.h ../threads/rmutex_extension.h
sockstreambuf.o: ../threads/rmutex_extension_abstract.h
sockstreambuf.o: ../threads/auto_mutex_extension_abstract.h
sockstreambuf.o: ../binary_search_tree.h
sockstreambuf.o: ../binary_search_tree/binary_search_tree_kernel_1.h
sockstreambuf.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
sockstreambuf.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
sockstreambuf.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
sockstreambuf.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
sockstreambuf.o: ../binary_search_tree/binary_search_tree_kernel_2.h
sockstreambuf.o: ../binary_search_tree/binary_search_tree_kernel_c.h
sockstreambuf.o: ../assert.h ../../dlib/memory_manager.h
sockstreambuf.o: ../member_function_pointer.h
sockstreambuf.o: ../member_function_pointer/member_function_pointer_kernel_1.h
sockstreambuf.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
sockstreambuf.o: ../member_function_pointer/member_function_pointer_kernel_c.h
sockstreambuf.o: ../memory_manager.h
sockstreambuf.o: ../memory_manager/memory_manager_kernel_1.h
sockstreambuf.o: ../memory_manager/memory_manager_kernel_abstract.h
sockstreambuf.o: ../memory_manager/memory_manager_kernel_2.h
sockstreambuf.o: ../memory_manager/memory_manager_kernel_3.h
sockstreambuf.o: ../memory_manager/memory_manager_kernel_2.h
sockstreambuf.o: ../binary_search_tree/binary_search_tree_kernel_2.h
sockstreambuf.o: ../queue.h ../queue/queue_kernel_1.h
sockstreambuf.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
sockstreambuf.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
sockstreambuf.o: ../queue/queue_sort_abstract.h
sockstreambuf.o: ../threads/auto_mutex_extension.h
sockstreambuf.o: ../threads/auto_unlock_extension.h
sockstreambuf.o: ../threads/auto_unlock_extension_abstract.h
sockstreambuf.o: ../threads/create_new_thread_extension.h
sockstreambuf.o: ../threads/create_new_thread_extension_abstract.h
sockstreambuf.o: ../threads/multithreaded_object_extension.h
sockstreambuf.o: ../threads/multithreaded_object_extension_abstract.h
sockstreambuf.o: ../threads/rsignaler_extension.h
sockstreambuf.o: ../threads/rsignaler_extension_abstract.h ../map.h
sockstreambuf.o: ../threads/rmutex_extension.h
sockstreambuf.o: ../threads/rsignaler_extension.h
sockstreambuf.o: ../threads/threaded_object_extension.h
sockstreambuf.o: ../threads/threaded_object_extension_abstract.h
sockstreambuf.o: ../threads/thread_specific_data_extension.h
sockstreambuf.o: ../threads/thread_specific_data_extension_abstract.h
sockstreambuf.o: ../sockets/sockets_extensions.h ../sockets.h
sockstreambuf.o: ../sockets/sockets_extensions_abstract.h
sockstreambuf.o: ../../dlib/misc_api.h ../misc_api/posix.h
sockstreambuf.o: ../misc_api/misc_api_kernel_2.h
sockstreambuf.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
sockstreambuf.o: ../../dlib/sockstreambuf.h
sockstreambuf.o: ../../dlib/sockstreambuf/sockstreambuf_kernel_1.h
sockstreambuf.o: ../sockstreambuf/sockstreambuf_kernel_abstract.h
sockstreambuf.o: ../../dlib/sockstreambuf/sockstreambuf_kernel_2.h tester.h
sockstreambuf.o: ../../dlib/map.h ../../dlib/logger.h
sockstreambuf.o: ../../dlib/logger/logger_kernel_1.h ../misc_api.h ../set.h
sockstreambuf.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
sockstreambuf.o: ../set/set_kernel_c.h binary_search_tree.h
sockstreambuf.o: ../../dlib/memory_manager_global.h
sockstreambuf.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
sockstreambuf.o: ../memory_manager/memory_manager_kernel_abstract.h
sockstreambuf.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
sockstreambuf.o: ../../dlib/memory_manager_stateless.h
sockstreambuf.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
sockstreambuf.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
sockstreambuf.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
sockstreambuf.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
sockstreambuf.o: ../set/set_compare_abstract.h
sockstreambuf.o: ../../dlib/logger/logger_kernel_abstract.h
sockstreambuf.o: ../../dlib/logger/extra_logger_headers.h
sockstreambuf.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
sockstreambuf.o: ../../dlib/algs.h
stack.o: ../../dlib/stack.h ../../dlib/stack/stack_kernel_1.h
stack.o: ../../dlib/stack/stack_kernel_abstract.h ../algs.h ../platform.h
stack.o: ../assert.h ../error.h ../interfaces/enumerable.h
stack.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
stack.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
stack.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
stack.o: ../memory_manager/memory_manager_kernel_abstract.h ../assert.h
stack.o: ../memory_manager/memory_manager_kernel_2.h
stack.o: ../memory_manager/memory_manager_kernel_3.h
stack.o: ../memory_manager/memory_manager_kernel_2.h
stack.o: ../binary_search_tree/binary_search_tree_kernel_2.h
stack.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
stack.o: ../interfaces/map_pair.h ../../dlib/stack/stack_kernel_c.h
stack.o: ../../dlib/memory_manager.h tester.h ../../dlib/map.h
stack.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h ../threads.h
stack.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
stack.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
stack.o: /usr/include/pthread.h /usr/include/features.h
stack.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
stack.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
stack.o: /usr/include/sched.h /usr/include/bits/types.h
stack.o: /usr/include/bits/typesizes.h /usr/include/time.h
stack.o: /usr/include/bits/sched.h /usr/include/signal.h
stack.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
stack.o: /usr/include/bits/setjmp.h /usr/include/errno.h
stack.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
stack.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
stack.o: /usr/include/asm-generic/errno.h
stack.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
stack.o: /usr/include/bits/time.h /usr/include/sys/select.h
stack.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
stack.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
stack.o: ../threads/rmutex_extension.h ../threads/rmutex_extension_abstract.h
stack.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
stack.o: ../binary_search_tree/binary_search_tree_kernel_1.h
stack.o: ../binary_search_tree/binary_search_tree_kernel_2.h
stack.o: ../binary_search_tree/binary_search_tree_kernel_c.h
stack.o: ../member_function_pointer.h
stack.o: ../member_function_pointer/member_function_pointer_kernel_1.h
stack.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
stack.o: ../member_function_pointer/member_function_pointer_kernel_c.h
stack.o: ../queue.h ../queue/queue_kernel_1.h
stack.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
stack.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
stack.o: ../queue/queue_sort_abstract.h ../threads/auto_mutex_extension.h
stack.o: ../threads/auto_unlock_extension.h
stack.o: ../threads/auto_unlock_extension_abstract.h
stack.o: ../threads/create_new_thread_extension.h
stack.o: ../threads/create_new_thread_extension_abstract.h
stack.o: ../threads/multithreaded_object_extension.h
stack.o: ../threads/multithreaded_object_extension_abstract.h
stack.o: ../threads/rsignaler_extension.h
stack.o: ../threads/rsignaler_extension_abstract.h ../map.h
stack.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
stack.o: ../threads/threaded_object_extension.h
stack.o: ../threads/threaded_object_extension_abstract.h
stack.o: ../threads/thread_specific_data_extension.h
stack.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
stack.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
stack.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
stack.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
stack.o: ../set/set_kernel_c.h binary_search_tree.h
stack.o: ../../dlib/memory_manager_global.h
stack.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
stack.o: ../memory_manager/memory_manager_kernel_abstract.h
stack.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
stack.o: ../../dlib/memory_manager_stateless.h
stack.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
stack.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
stack.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
stack.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
stack.o: ../set/set_compare_abstract.h
stack.o: ../../dlib/logger/logger_kernel_abstract.h
stack.o: ../../dlib/logger/extra_logger_headers.h
stack.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
stack.o: ../../dlib/algs.h
static_map.o: ../../dlib/hash_table.h
static_map.o: ../../dlib/hash_table/hash_table_kernel_1.h
static_map.o: ../../dlib/hash_table/hash_table_kernel_abstract.h
static_map.o: ../general_hash/general_hash.h ../algs.h ../platform.h
static_map.o: ../assert.h ../error.h ../interfaces/map_pair.h
static_map.o: ../interfaces/enumerable.h ../interfaces/remover.h ../assert.h
static_map.o: ../serialize.h ../algs.h ../uintn.h ../interfaces/enumerable.h
static_map.o: ../interfaces/map_pair.h ../memory_manager.h
static_map.o: ../memory_manager/memory_manager_kernel_1.h
static_map.o: ../memory_manager/memory_manager_kernel_abstract.h
static_map.o: ../memory_manager/memory_manager_kernel_2.h
static_map.o: ../memory_manager/memory_manager_kernel_3.h
static_map.o: ../memory_manager/memory_manager_kernel_2.h
static_map.o: ../binary_search_tree/binary_search_tree_kernel_2.h
static_map.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
static_map.o: ../../dlib/hash_table/hash_table_kernel_2.h
static_map.o: ../../dlib/hash_table/hash_table_kernel_c.h
static_map.o: ../../dlib/memory_manager.h binary_search_tree.h
static_map.o: ../../dlib/memory_manager_global.h
static_map.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
static_map.o: ../memory_manager/memory_manager_kernel_abstract.h
static_map.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
static_map.o: ../../dlib/memory_manager_stateless.h
static_map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
static_map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
static_map.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
static_map.o: ../threads.h ../threads/threads_kernel.h ../platform.h
static_map.o: ../threads/posix.h ../threads/threads_kernel_2.h
static_map.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
static_map.o: /usr/include/features.h /usr/include/sys/cdefs.h
static_map.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
static_map.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
static_map.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
static_map.o: /usr/include/time.h /usr/include/bits/sched.h
static_map.o: /usr/include/signal.h /usr/include/bits/sigset.h
static_map.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
static_map.o: /usr/include/errno.h /usr/include/bits/errno.h
static_map.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
static_map.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
static_map.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
static_map.o: /usr/include/bits/time.h /usr/include/sys/select.h
static_map.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
static_map.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
static_map.o: ../threads/rmutex_extension.h
static_map.o: ../threads/rmutex_extension_abstract.h
static_map.o: ../threads/auto_mutex_extension_abstract.h
static_map.o: ../binary_search_tree.h
static_map.o: ../binary_search_tree/binary_search_tree_kernel_1.h
static_map.o: ../binary_search_tree/binary_search_tree_kernel_2.h
static_map.o: ../binary_search_tree/binary_search_tree_kernel_c.h
static_map.o: ../member_function_pointer.h
static_map.o: ../member_function_pointer/member_function_pointer_kernel_1.h
static_map.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
static_map.o: ../member_function_pointer/member_function_pointer_kernel_c.h
static_map.o: ../queue.h ../queue/queue_kernel_1.h
static_map.o: ../queue/queue_kernel_abstract.h ../queue/queue_kernel_2.h
static_map.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
static_map.o: ../queue/queue_sort_abstract.h
static_map.o: ../threads/auto_mutex_extension.h
static_map.o: ../threads/auto_unlock_extension.h
static_map.o: ../threads/auto_unlock_extension_abstract.h
static_map.o: ../threads/create_new_thread_extension.h
static_map.o: ../threads/create_new_thread_extension_abstract.h
static_map.o: ../threads/multithreaded_object_extension.h
static_map.o: ../threads/multithreaded_object_extension_abstract.h
static_map.o: ../threads/rsignaler_extension.h
static_map.o: ../threads/rsignaler_extension_abstract.h ../map.h
static_map.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
static_map.o: ../threads/threaded_object_extension.h
static_map.o: ../threads/threaded_object_extension_abstract.h
static_map.o: ../threads/thread_specific_data_extension.h
static_map.o: ../threads/thread_specific_data_extension_abstract.h
static_map.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
static_map.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
static_map.o: ../misc_api.h ../misc_api/posix.h
static_map.o: ../misc_api/misc_api_kernel_2.h
static_map.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
static_map.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
static_map.o: ../set/set_kernel_c.h ../set/set_compare_1.h
static_map.o: ../set/set_compare_abstract.h
static_map.o: ../../dlib/logger/logger_kernel_abstract.h
static_map.o: ../../dlib/logger/extra_logger_headers.h
static_map.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
static_map.o: ../../dlib/algs.h ../../dlib/static_map.h
static_map.o: ../../dlib/static_map/static_map_kernel_1.h
static_map.o: ../../dlib/static_map/static_map_kernel_abstract.h
static_map.o: ../../dlib/static_map/static_map_kernel_c.h
static_set.o: ../../dlib/queue.h ../queue/queue_kernel_1.h
static_set.o: ../queue/queue_kernel_abstract.h ../algs.h ../platform.h
static_set.o: ../assert.h ../error.h ../interfaces/enumerable.h
static_set.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
static_set.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
static_set.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
static_set.o: ../memory_manager/memory_manager_kernel_abstract.h ../assert.h
static_set.o: ../memory_manager/memory_manager_kernel_2.h
static_set.o: ../memory_manager/memory_manager_kernel_3.h
static_set.o: ../memory_manager/memory_manager_kernel_2.h
static_set.o: ../binary_search_tree/binary_search_tree_kernel_2.h
static_set.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
static_set.o: ../interfaces/map_pair.h ../queue/queue_kernel_2.h
static_set.o: ../queue/queue_kernel_c.h ../queue/queue_sort_1.h
static_set.o: ../queue/queue_sort_abstract.h ../../dlib/memory_manager.h
static_set.o: ../../dlib/static_set.h
static_set.o: ../../dlib/static_set/static_set_kernel_1.h
static_set.o: ../../dlib/static_set/static_set_kernel_abstract.h ../sort.h
static_set.o: ../../dlib/static_set/static_set_kernel_c.h
static_set.o: ../../dlib/static_set/static_set_compare_1.h
static_set.o: ../../dlib/static_set/static_set_compare_abstract.h
static_set.o: ../../dlib/set.h ../set/set_kernel_1.h
static_set.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
static_set.o: binary_search_tree.h ../../dlib/memory_manager_global.h
static_set.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
static_set.o: ../memory_manager/memory_manager_kernel_abstract.h
static_set.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
static_set.o: ../../dlib/memory_manager_stateless.h
static_set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
static_set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
static_set.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
static_set.o: ../threads.h ../threads/threads_kernel.h ../platform.h
static_set.o: ../threads/posix.h ../threads/threads_kernel_2.h
static_set.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
static_set.o: /usr/include/features.h /usr/include/sys/cdefs.h
static_set.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
static_set.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
static_set.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
static_set.o: /usr/include/time.h /usr/include/bits/sched.h
static_set.o: /usr/include/signal.h /usr/include/bits/sigset.h
static_set.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
static_set.o: /usr/include/errno.h /usr/include/bits/errno.h
static_set.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
static_set.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
static_set.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
static_set.o: /usr/include/bits/time.h /usr/include/sys/select.h
static_set.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
static_set.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
static_set.o: ../threads/rmutex_extension.h
static_set.o: ../threads/rmutex_extension_abstract.h
static_set.o: ../threads/auto_mutex_extension_abstract.h
static_set.o: ../binary_search_tree.h
static_set.o: ../binary_search_tree/binary_search_tree_kernel_1.h
static_set.o: ../binary_search_tree/binary_search_tree_kernel_2.h
static_set.o: ../binary_search_tree/binary_search_tree_kernel_c.h
static_set.o: ../member_function_pointer.h
static_set.o: ../member_function_pointer/member_function_pointer_kernel_1.h
static_set.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
static_set.o: ../member_function_pointer/member_function_pointer_kernel_c.h
static_set.o: ../queue.h ../threads/auto_mutex_extension.h
static_set.o: ../threads/auto_unlock_extension.h
static_set.o: ../threads/auto_unlock_extension_abstract.h
static_set.o: ../threads/create_new_thread_extension.h
static_set.o: ../threads/create_new_thread_extension_abstract.h
static_set.o: ../threads/multithreaded_object_extension.h
static_set.o: ../threads/multithreaded_object_extension_abstract.h
static_set.o: ../threads/rsignaler_extension.h
static_set.o: ../threads/rsignaler_extension_abstract.h ../map.h
static_set.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
static_set.o: ../threads/threaded_object_extension.h
static_set.o: ../threads/threaded_object_extension_abstract.h
static_set.o: ../threads/thread_specific_data_extension.h
static_set.o: ../threads/thread_specific_data_extension_abstract.h
static_set.o: ../../dlib/binary_search_tree.h tester.h ../../dlib/map.h
static_set.o: ../../dlib/logger.h ../../dlib/logger/logger_kernel_1.h
static_set.o: ../misc_api.h ../misc_api/posix.h
static_set.o: ../misc_api/misc_api_kernel_2.h
static_set.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
static_set.o: ../set/set_compare_1.h ../set/set_compare_abstract.h
static_set.o: ../../dlib/logger/logger_kernel_abstract.h
static_set.o: ../../dlib/logger/extra_logger_headers.h
static_set.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
static_set.o: ../../dlib/algs.h
threads.o: ../../dlib/misc_api.h ../platform.h ../misc_api/posix.h
threads.o: ../misc_api/misc_api_kernel_2.h
threads.o: ../misc_api/misc_api_kernel_abstract.h ../algs.h ../assert.h
threads.o: ../error.h ../uintn.h ../../dlib/threads.h
threads.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
threads.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
threads.o: /usr/include/pthread.h /usr/include/features.h
threads.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
threads.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
threads.o: /usr/include/sched.h /usr/include/bits/types.h
threads.o: /usr/include/bits/typesizes.h /usr/include/time.h
threads.o: /usr/include/bits/sched.h /usr/include/signal.h
threads.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
threads.o: /usr/include/bits/setjmp.h /usr/include/errno.h
threads.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
threads.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
threads.o: /usr/include/asm-generic/errno.h
threads.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
threads.o: /usr/include/bits/time.h /usr/include/sys/select.h
threads.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
threads.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
threads.o: ../threads/rmutex_extension.h
threads.o: ../threads/rmutex_extension_abstract.h
threads.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
threads.o: ../binary_search_tree/binary_search_tree_kernel_1.h
threads.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
threads.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
threads.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
threads.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
threads.o: ../binary_search_tree/binary_search_tree_kernel_2.h
threads.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
threads.o: ../../dlib/memory_manager.h ../member_function_pointer.h
threads.o: ../member_function_pointer/member_function_pointer_kernel_1.h
threads.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
threads.o: ../member_function_pointer/member_function_pointer_kernel_c.h
threads.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
threads.o: ../memory_manager/memory_manager_kernel_abstract.h
threads.o: ../memory_manager/memory_manager_kernel_2.h
threads.o: ../memory_manager/memory_manager_kernel_3.h
threads.o: ../memory_manager/memory_manager_kernel_2.h
threads.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
threads.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
threads.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
threads.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
threads.o: ../threads/auto_mutex_extension.h
threads.o: ../threads/auto_unlock_extension.h
threads.o: ../threads/auto_unlock_extension_abstract.h
threads.o: ../threads/create_new_thread_extension.h
threads.o: ../threads/create_new_thread_extension_abstract.h ../threads.h
threads.o: ../threads/multithreaded_object_extension.h
threads.o: ../threads/multithreaded_object_extension_abstract.h
threads.o: ../threads/rsignaler_extension.h
threads.o: ../threads/rsignaler_extension_abstract.h ../map.h
threads.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
threads.o: ../threads/threaded_object_extension.h
threads.o: ../threads/threaded_object_extension_abstract.h
threads.o: ../threads/thread_specific_data_extension.h
threads.o: ../threads/thread_specific_data_extension_abstract.h tester.h
threads.o: ../../dlib/map.h ../../dlib/logger.h
threads.o: ../../dlib/logger/logger_kernel_1.h ../misc_api.h ../set.h
threads.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
threads.o: ../set/set_kernel_c.h binary_search_tree.h
threads.o: ../../dlib/memory_manager_global.h
threads.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
threads.o: ../memory_manager/memory_manager_kernel_abstract.h
threads.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
threads.o: ../../dlib/memory_manager_stateless.h
threads.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
threads.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
threads.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
threads.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
threads.o: ../set/set_compare_abstract.h
threads.o: ../../dlib/logger/logger_kernel_abstract.h
threads.o: ../../dlib/logger/extra_logger_headers.h
threads.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
threads.o: ../../dlib/algs.h
timer.o: ../../dlib/timer.h ../timer/timer_kernel_1.h ../threads.h
timer.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
timer.o: ../threads/threads_kernel_2.h ../threads/threads_kernel_abstract.h
timer.o: /usr/include/pthread.h /usr/include/features.h
timer.o: /usr/include/sys/cdefs.h /usr/include/bits/wordsize.h
timer.o: /usr/include/gnu/stubs.h /usr/include/gnu/stubs-32.h
timer.o: /usr/include/sched.h /usr/include/bits/types.h
timer.o: /usr/include/bits/typesizes.h /usr/include/time.h
timer.o: /usr/include/bits/sched.h /usr/include/signal.h
timer.o: /usr/include/bits/sigset.h /usr/include/bits/pthreadtypes.h
timer.o: /usr/include/bits/setjmp.h /usr/include/errno.h
timer.o: /usr/include/bits/errno.h /usr/include/linux/errno.h
timer.o: /usr/include/asm/errno.h /usr/include/asm-i386/errno.h
timer.o: /usr/include/asm-generic/errno.h
timer.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
timer.o: /usr/include/bits/time.h /usr/include/sys/select.h
timer.o: /usr/include/bits/select.h ../algs.h ../platform.h ../assert.h
timer.o: ../error.h ../threads/threads_kernel_shared.h
timer.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
timer.o: ../threads/rmutex_extension.h ../threads/rmutex_extension_abstract.h
timer.o: ../threads/auto_mutex_extension_abstract.h ../binary_search_tree.h
timer.o: ../binary_search_tree/binary_search_tree_kernel_1.h
timer.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
timer.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
timer.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
timer.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
timer.o: ../binary_search_tree/binary_search_tree_kernel_2.h
timer.o: ../binary_search_tree/binary_search_tree_kernel_c.h ../assert.h
timer.o: ../../dlib/memory_manager.h ../member_function_pointer.h
timer.o: ../member_function_pointer/member_function_pointer_kernel_1.h
timer.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
timer.o: ../member_function_pointer/member_function_pointer_kernel_c.h
timer.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
timer.o: ../memory_manager/memory_manager_kernel_abstract.h
timer.o: ../memory_manager/memory_manager_kernel_2.h
timer.o: ../memory_manager/memory_manager_kernel_3.h
timer.o: ../memory_manager/memory_manager_kernel_2.h
timer.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
timer.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
timer.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
timer.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
timer.o: ../threads/auto_mutex_extension.h ../threads/auto_unlock_extension.h
timer.o: ../threads/auto_unlock_extension_abstract.h
timer.o: ../threads/create_new_thread_extension.h
timer.o: ../threads/create_new_thread_extension_abstract.h
timer.o: ../threads/multithreaded_object_extension.h
timer.o: ../threads/multithreaded_object_extension_abstract.h
timer.o: ../threads/rsignaler_extension.h
timer.o: ../threads/rsignaler_extension_abstract.h ../map.h
timer.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
timer.o: ../threads/threaded_object_extension.h
timer.o: ../threads/threaded_object_extension_abstract.h
timer.o: ../threads/thread_specific_data_extension.h
timer.o: ../threads/thread_specific_data_extension_abstract.h ../misc_api.h
timer.o: ../misc_api/posix.h ../misc_api/misc_api_kernel_2.h
timer.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h
timer.o: ../timer/timer_kernel_abstract.h ../timer/timer_kernel_2.h
timer.o: ../../dlib/timeout.h ../timeout/timeout_kernel_1.h
timer.o: ../timeout/timeout_kernel_abstract.h ../timer.h tester.h
timer.o: ../../dlib/map.h ../../dlib/logger.h
timer.o: ../../dlib/logger/logger_kernel_1.h ../set.h ../set/set_kernel_1.h
timer.o: ../set/set_kernel_abstract.h ../set/set_kernel_c.h
timer.o: binary_search_tree.h ../../dlib/memory_manager_global.h
timer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
timer.o: ../memory_manager/memory_manager_kernel_abstract.h
timer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
timer.o: ../../dlib/memory_manager_stateless.h
timer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
timer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
timer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
timer.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
timer.o: ../set/set_compare_abstract.h
timer.o: ../../dlib/logger/logger_kernel_abstract.h
timer.o: ../../dlib/logger/extra_logger_headers.h
timer.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
timer.o: ../../dlib/algs.h
tokenizer.o: ../../dlib/tokenizer.h ../../dlib/tokenizer/tokenizer_kernel_1.h
tokenizer.o: ../algs.h ../platform.h ../assert.h ../error.h
tokenizer.o: ../tokenizer/tokenizer_kernel_abstract.h
tokenizer.o: ../../dlib/tokenizer/tokenizer_kernel_c.h ../assert.h tester.h
tokenizer.o: ../../dlib/map.h ../../dlib/logger.h
tokenizer.o: ../../dlib/logger/logger_kernel_1.h ../threads.h
tokenizer.o: ../threads/threads_kernel.h ../platform.h ../threads/posix.h
tokenizer.o: ../threads/threads_kernel_2.h
tokenizer.o: ../threads/threads_kernel_abstract.h /usr/include/pthread.h
tokenizer.o: /usr/include/features.h /usr/include/sys/cdefs.h
tokenizer.o: /usr/include/bits/wordsize.h /usr/include/gnu/stubs.h
tokenizer.o: /usr/include/gnu/stubs-32.h /usr/include/sched.h
tokenizer.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
tokenizer.o: /usr/include/time.h /usr/include/bits/sched.h
tokenizer.o: /usr/include/signal.h /usr/include/bits/sigset.h
tokenizer.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/setjmp.h
tokenizer.o: /usr/include/errno.h /usr/include/bits/errno.h
tokenizer.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
tokenizer.o: /usr/include/asm-i386/errno.h /usr/include/asm-generic/errno.h
tokenizer.o: /usr/include/asm-generic/errno-base.h /usr/include/sys/time.h
tokenizer.o: /usr/include/bits/time.h /usr/include/sys/select.h
tokenizer.o: /usr/include/bits/select.h ../threads/threads_kernel_shared.h
tokenizer.o: ../threads/auto_mutex_extension.h ../threads/threads_kernel.h
tokenizer.o: ../threads/rmutex_extension.h
tokenizer.o: ../threads/rmutex_extension_abstract.h
tokenizer.o: ../threads/auto_mutex_extension_abstract.h
tokenizer.o: ../binary_search_tree.h
tokenizer.o: ../binary_search_tree/binary_search_tree_kernel_1.h
tokenizer.o: ../binary_search_tree/binary_search_tree_kernel_abstract.h
tokenizer.o: ../interfaces/map_pair.h ../interfaces/enumerable.h
tokenizer.o: ../interfaces/remover.h ../serialize.h ../algs.h ../uintn.h
tokenizer.o: ../interfaces/enumerable.h ../interfaces/map_pair.h
tokenizer.o: ../binary_search_tree/binary_search_tree_kernel_2.h
tokenizer.o: ../binary_search_tree/binary_search_tree_kernel_c.h
tokenizer.o: ../../dlib/memory_manager.h ../member_function_pointer.h
tokenizer.o: ../member_function_pointer/member_function_pointer_kernel_1.h
tokenizer.o: ../member_function_pointer/member_function_pointer_kernel_abstract.h
tokenizer.o: ../member_function_pointer/member_function_pointer_kernel_c.h
tokenizer.o: ../memory_manager.h ../memory_manager/memory_manager_kernel_1.h
tokenizer.o: ../memory_manager/memory_manager_kernel_abstract.h
tokenizer.o: ../memory_manager/memory_manager_kernel_2.h
tokenizer.o: ../memory_manager/memory_manager_kernel_3.h
tokenizer.o: ../memory_manager/memory_manager_kernel_2.h
tokenizer.o: ../binary_search_tree/binary_search_tree_kernel_2.h ../queue.h
tokenizer.o: ../queue/queue_kernel_1.h ../queue/queue_kernel_abstract.h
tokenizer.o: ../queue/queue_kernel_2.h ../queue/queue_kernel_c.h
tokenizer.o: ../queue/queue_sort_1.h ../queue/queue_sort_abstract.h
tokenizer.o: ../threads/auto_mutex_extension.h
tokenizer.o: ../threads/auto_unlock_extension.h
tokenizer.o: ../threads/auto_unlock_extension_abstract.h
tokenizer.o: ../threads/create_new_thread_extension.h
tokenizer.o: ../threads/create_new_thread_extension_abstract.h
tokenizer.o: ../threads/multithreaded_object_extension.h
tokenizer.o: ../threads/multithreaded_object_extension_abstract.h
tokenizer.o: ../threads/rsignaler_extension.h
tokenizer.o: ../threads/rsignaler_extension_abstract.h ../map.h
tokenizer.o: ../threads/rmutex_extension.h ../threads/rsignaler_extension.h
tokenizer.o: ../threads/threaded_object_extension.h
tokenizer.o: ../threads/threaded_object_extension_abstract.h
tokenizer.o: ../threads/thread_specific_data_extension.h
tokenizer.o: ../threads/thread_specific_data_extension_abstract.h
tokenizer.o: ../misc_api.h ../misc_api/posix.h
tokenizer.o: ../misc_api/misc_api_kernel_2.h
tokenizer.o: ../misc_api/misc_api_kernel_abstract.h ../uintn.h ../set.h
tokenizer.o: ../set/set_kernel_1.h ../set/set_kernel_abstract.h
tokenizer.o: ../set/set_kernel_c.h binary_search_tree.h
tokenizer.o: ../../dlib/memory_manager_global.h
tokenizer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_1.h
tokenizer.o: ../memory_manager/memory_manager_kernel_abstract.h
tokenizer.o: ../../dlib/memory_manager_global/memory_manager_global_kernel_abstract.h
tokenizer.o: ../../dlib/memory_manager_stateless.h
tokenizer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_1.h
tokenizer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_abstract.h
tokenizer.o: ../../dlib/memory_manager_stateless/memory_manager_stateless_kernel_2.h
tokenizer.o: ../../dlib/binary_search_tree.h ../set/set_compare_1.h
tokenizer.o: ../set/set_compare_abstract.h
tokenizer.o: ../../dlib/logger/logger_kernel_abstract.h
tokenizer.o: ../../dlib/logger/extra_logger_headers.h
tokenizer.o: ../../dlib/logger/logger_kernel_1.h ../../dlib/assert.h
tokenizer.o: ../../dlib/algs.h
