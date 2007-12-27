// Copyright (C) 2003  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_THREADS_KERNEl_ABSTRACT_
#ifdef DLIB_THREADS_KERNEl_ABSTRACT_

namespace dlib
{

// ----------------------------------------------------------------------------------------

    /*!
        GENERAL COMMENTS
            Unlike the other API wrappers you may make instances of these objects at the
            global scope and call these functions before main() or winmain() have been
            entered (except for get_main_thread_id()).

        PROGRAM TERMINATION
            When the main() function ends (or you call end_program() if this is a gui 
            application) the program will wait until all outstanding threads have 
            terminated.  This means that if you want your program to actually
            be able to terminate you have to ensure that all your threads will eventually
            terminate.  To help you make sure all your threads end you can use the 
            register_program_ending_handler() function.  It allows you to register a member 
            function that will be called after main() has ended (or end_program() is 
            called).  Thus, you can register a function that will somehow tell your 
            threads to end.
            
            Also note that once main() ends (or you call end_program()) C++ will start 
            destructing global and static objects so any threads making use of those 
            resources may get into trouble.  So you probably just want to ensure that
            all your threads are done *before* you try to terminate the program.  But
            if that isn't possible for whatever reason then you can use the 
            register_program_ending_handler() function to notify those threads that it is time
            to end.

        THREAD POOL
            When threads end they go into a global thread pool and each waits there 
            for 30 seconds before timing out and having its resources returned to the 
            operating system.  When create_new_thread() is called it first looks in the
            thread pool to see if there are any threads it can snatch from the pool, if 
            not then it makes a new one.  

            Note that whenever I say something happens when a thread "terminates" or "ends"
            I mean "when it returns to the thread pool."  From the client programmer point
            of view a thread terminates/ends when it returns to the dlib thread pool and you 
            shouldn't and indeed don't need to know when it actually gets its resources
            reclaimed by the operating system.

            If you want to change the timeout to a different value you can #define 
            DLIB_THREAD_POOL_TIMEOUT to whatever value (in milliseconds) that you like.
    !*/

// ----------------------------------------------------------------------------------------

    thread_id_type get_thread_id (
    );
    /*!
        ensures
            - returns a unique id for the calling thread.  Note that while the id is unique 
              among all currently existing threads it may have been used by a previous
              thread that has terminated.
    !*/

// ----------------------------------------------------------------------------------------

    thread_id_type get_main_thread_id (
    );
    /*!
        requires
            - if (main() or winmain() hasn't been entered yet) then
                - this function is being called from the main program thread.
                  (i.e. don't call this function from a thread that you spawn and
                  run before main() or winmain() starts.)
        ensures
            - returns the unique id for the main program thread.  This is the thread
              that executes the main() function.  (i.e. Within the main() function it 
              is the case that get_main_thread_id() == get_thread_id().)
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename T
        >
    void register_thread_end_handler (
        T& obj,
        void (T::*handler)()
    );
    /*!
        requires
            - handler == a valid member function pointer for class T
            - handler does not throw
            - handler does not call register_thread_end_handler()
            - handler does not block
            - get_main_thread_id() != get_thread_id() (i.e. this function is not called
              from the main program thread)
        ensures
            - let ID == the thread id for the thread calling register_thread_end_handler()
            - (obj.*handler)() will be called when the thread with thread id ID is 
              terminating and it will be called from within that terminating thread.  
              (i.e. inside the handler function get_thread_id() == ID == the id of the 
              thread that is terminating. )
            - each call to this function adds another handler that will be called when
              the given thread terminates.  This means that if you call it a bunch of 
              times then you will end up registering multiple handlers (or single 
              handlers multiple times) that will be called when the thread ends.  i.e. 
              if you call register_thread_end_handler() N times for a given thread then
              that thread will receive N calls to its registered handlers.
        throws
            - std::bad_alloc
              If this exception is thrown then the call to this function had no effect.
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename T
        >
    void register_program_ending_handler (
        T& obj,
        void (T::*handler)()
    );
    /*!
        requires
            - handler == a valid member function pointer for class T
            - handler does not throw
            - handler does not call register_thread_end_handler()
            - handler does not call register_program_ending_handler()
            - handler does not block
        ensures
            - (obj.*handler)() will be called after main() has terminated (or after you
              have called end_program() if this is a gui application). 
            - each call to this function adds another handler that will be called at  
              program termination.  This means that if you call it a bunch of 
              times then you will end up registering multiple handlers (or single 
              handlers multiple times).  
        throws
            - std::bad_alloc
              If this exception is thrown then the call to this function had no effect.
    !*/

// ----------------------------------------------------------------------------------------

    bool create_new_thread (
        void (*funct)(void*),
        void* param
    );
    /*!
        ensures
            - creates a new thread for the function pointed to by funct 
            - passes it param as its parameter 
            - returns true upon success and false upon failure to create the new thread
    !*/

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // mutex object
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class mutex
    {
        /*!
            INITIAL VALUE
                mutex is in the unlocked state

            WHAT THIS OBJECT REPRESENTS
                This object represents a mutex intended to be used for synchronous 
                thread control of shared data. When a thread wants to access some 
                shared data it locks out other threads by calling lock() and calls 
                unlock() when it is finished.  
        !*/
    public:

        mutex (
        );
        /*!
            ensures
                - #*this is properly initialized
            throws
                - dlib::thread_error
                    the constructor may throw this exception if there is a problem 
                    gathering resources to create the mutex.
        !*/

        ~mutex (
        );
        /*!
            requires
                - *this is not locked
            ensures
                - all resources allocated by *this have been freed
        !*/

        void lock (
        ) const;
        /*!
            requires
                - the thread calling lock() does not already have a lock on *this
            ensures
                - if (*this is currently locked by another thread) then 
                    - the thread that called lock() on *this is put to sleep until 
                      it becomes available                  
                - if (*this is currently unlocked) then 
                    - #*this becomes locked and the current thread is NOT put to sleep 
                      but now "owns" #*this
        !*/

        void unlock (
        ) const;
        /*!
            ensures
                - if (*this is currently locked and owned by the thread calling unlock) then
                    - #*this is unlocked (i.e. other threads may now lock this object)
                - else
                    - the call to unlock() has no effect
        !*/


    private:
        // restricted functions
        mutex(mutex&);        // copy constructor
        mutex& operator=(mutex&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
    // signaler object
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    class signaler
    {
        /*!

            WHAT THIS OBJECT REPRESENTS
                This object represents an event signaling system for threads.  It gives 
                a thread the ability to wake up other threads that are waiting for a 
                particular signal. 

                Each signaler object is associated with one and only one mutex object.  
                More than one signaler object may be associated with a single mutex
                but a signaler object may only be associated with a single mutex.

                NOTE:
                You must guard against spurious wakeups.  This means that a thread
                might return from a call to wait even if no other thread called
                signal.  This is rare but must be guarded against. 
        !*/
    public:

        signaler (
            const mutex& associated_mutex
        );
        /*!
            ensures
                - #*this is properly initialized 
                - #get_mutex() == associated_mutex
            throws
                - dlib::thread_error
                    the constructor may throw this exception if there is a problem 
                    gathering resources to create the signaler.    
        !*/


        ~signaler (
        );
        /*!
            ensures
                - all resources allocated by *this have been freed
        !*/

        void wait (
        ) const;
        /*!
            requires
                - get_mutex() is locked and owned by the calling thread
            ensures
                - atomically unlocks get_mutex() and blocks the calling thread                      
                - calling thread may wake if another thread calls signal() or broadcast()
                  on *this
                - when wait() returns the calling thread again has a lock on get_mutex()
        !*/

        bool wait_or_timeout (
            unsigned long milliseconds
        ) const;
        /*!
            requires
                - get_mutex() is locked and owned by the calling thread
            ensures
                - atomically unlocks get_mutex() and blocks the calling thread
                - calling thread may wake if another thread calls signal() or broadcast()
                  on *this
                - after the specified number of milliseconds has elapsed the calling thread
                  will wake once get_mutex() is free
                - when wait returns the calling thread again has a lock on get_mutex()

                - returns false if the call to wait_or_timeout timed out 
                - returns true if the call did not time out
        !*/


        void signal (
        ) const;
        /*!
            ensures
                - if (at least one thread is waiting on *this) then
                    - at least one of the waiting threads will wake 
        !*/

        void broadcast (
        ) const;
        /*!
            ensures
                - any and all threads waiting on *this will wake 
        !*/

        const mutex& get_mutex (
        ) const;
        /*!
            ensures
                - returns a const reference to the mutex associated with *this
        !*/

    private:
        // restricted functions
        signaler(signaler&);        // copy constructor
        signaler& operator=(signaler&);    // assignment operator
    };

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_THREADS_KERNEl_ABSTRACT_

