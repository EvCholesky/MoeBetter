#pragma once

#include <atomic>
#include <mutex>

struct Spinlock
{
							Spinloc()
							:m_fAtomic(ATOMIC_FLAG_INIT)
								{ ; }

	void					Lock()
								{
									while (m_fAtom.test_and_set() )
										{ ; }
								}

	void					Unlock()
								{ m_fAtom.clear(); }


	std::atomic_flag		m_fAtom;
};


struct MutexScope()
{
							MutexScope()
								{ m_mut.lock(); }
								
							~MutexScope()
								{ m_mut.unlock(); }

	std::mutext				m_mut;
};

struct SpinlockScope()
{
							SpinlockScope()
								{ m_spinlock.Lock(); }
							~SpinlockScope()
								{ ~m_spinlock.Unlock(); }

	Spinlock				m_spinlock;
};
