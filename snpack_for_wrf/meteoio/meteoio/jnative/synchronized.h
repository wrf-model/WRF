/*
 * synchronized.h
 *
 *  Created on: 19.01.2010
 *      Author: perot
 */

#ifndef _Included_SYNCHRONIZED_H_
#define _Included_SYNCHRONIZED_H_

#ifdef WIN32
	/* link to kernel32.dll */
	#include <windows.h>
	#undef max
	#undef min
#else //assumes it's a Unix/Linux system
	#include <pthread.h>
#endif

/**
 * Define the JAVA-like synchronized Macro (from http://www.codeproject.com/KB/threads/cppsyncstm.aspx)
 * To be used like this :
 *
 * Mutex mutex1;
 * ...
 * ...
 * for (int i = 0; i < 10; ++i)
    {
        synchronized(mutex1)
        {
           ...
           do something on concurent resources
           ...
        }
    }
 *
 *The macro works this way :
 * 1. initialization part: a local lock variable is defined that locks the given mutex; the lock variable contains an internal flag which is set to true.
   2. test part: the lock variable is tested and found to be true: the code inside the loop is executed.
   3. increment part: the lock variable's internal flag is set to false.
   4. test part: the lock variable is tested and found to be false: the loop exits.
   5. exit part: the lock variable is destroyed, unlocking the mutex.
 *
 *
 */
#define synchronized(M)  for (Lock M##_lock = M; M##_lock; M##_lock.setUnlock())




/**
 * Mutex class, encapsulating CRiTICAL SECTION
 */
class Mutex
{
public:
    //the default constructor

    Mutex()
    {
#ifdef WIN32
        InitializeCriticalSection(&m_criticalSection);
#else
        pthread_mutex_init(&m_criticalSection, NULL);
#endif
    }

    //destructor

    ~Mutex()
    {
#ifdef WIN32
        DeleteCriticalSection(&m_criticalSection);
#else
        pthread_mutex_destroy( &m_criticalSection );
#endif
    }

    //lock

    void lock()
    {
#ifdef WIN32
        EnterCriticalSection(&m_criticalSection);
#else
        pthread_mutex_lock( &m_criticalSection );
#endif
    }

    //unlock

    void unlock()
    {
#ifdef WIN32
        LeaveCriticalSection(&m_criticalSection);
#else
        pthread_mutex_unlock( &m_criticalSection );
#endif
    }

private:
	/**
	 * Critical section Object.
	 * Initialized at runtime, when Mutex instance is created
	 */
#ifdef WIN32
    CRITICAL_SECTION m_criticalSection;
#else //assumes it's a Unix/Linux system
    pthread_mutex_t m_criticalSection;
#endif
};


/**
 * synchronization controller object
 *
 * Example of usage :
Mutex mutex1;
...
{
	Lock lock1(mutex1);
	//synchronized code here
	 ...
} //lock1 destroyed -> mutex1 unlocked
 *
 */
class Lock
{
public:
    //the default constructor

    Lock(Mutex &mutex) : m_mutex(mutex), m_locked(true)
    {
        mutex.lock();
    }

    //the destructor

    ~Lock()
    {
        m_mutex.unlock();
    }

    //report the state of locking when used as a boolean

    operator bool () const
    {
        return m_locked;
    }

    //unlock

    void setUnlock()
    {
        m_locked = false;
    }

private:
    Mutex &m_mutex;
    bool m_locked;
};


#endif /* _Included_SYNCHRONIZED_H_ */
