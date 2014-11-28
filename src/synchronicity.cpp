#include <iostream>
#include <string>
#include <boost/interprocess/sync/named_upgradable_mutex.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include <boost/bind.hpp>

#include <R.h>
#include <Rdefines.h>

#include "synchronicity/util.h"
#include "synchronicity/SharedCounter.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace boost::posix_time;

class BoostMutexInfo
{
  public:

    BoostMutexInfo() : 
      _timeout(-1), _name(""), _read(true), _locked(false) {}
    
    virtual ~BoostMutexInfo() {destroy();}
  

    bool init(const std::string &newName)
    {
      _name = newName;
      _counter.init(newName+"_counter");
      return true;
    }

    bool init(const std::string &newName, const long timeout)
    {
      init(newName);
      _timeout = timeout;
      return true;
    }

    // This function must be protected by a semaphore.
    bool destroy()
    {
      if (_counter.get() == 1)
      {
        try
        {
          named_upgradable_mutex::remove( _name.c_str() );
          return true;
        }
        catch(std::exception &e)
        {
          //printf("%s\n", e.what());
          return false;
        }
      }
      return false;
    }
    
    long timeout() const {return _timeout;}

    std::string name() const {return _name;}

    SharedCounter count() const {return _counter;}

    bool is_timed() const {return _timeout!=-1;}
    
    bool& read() {return _read;}
    bool& locked() {return _locked;}

  protected:
    long _timeout;
    std::string _name;
    SharedCounter _counter;
    bool _read;
    bool _locked;    
};
// Use these functions for locking and unlock.

// Note, we don't actually create the mutexes until the first time
// it is locked.

template<typename LockFunctionType>
SEXP boost_lock(const std::string &resourceName, 
  const LockFunctionType &lockFun)
{
  named_upgradable_mutex mutex(open_or_create, resourceName.c_str());
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  try
  {
    lockFun(mutex);
    LOGICAL_DATA(ret)[0] = Rboolean(1);
  }
  catch (std::exception &e)
  {
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    //printf("%s\n", e.what());
  }
  UNPROTECT(1);
  return ret;
}

template<typename TryLockFunctionType>
SEXP boost_try_lock(const std::string &resourceName, 
  const TryLockFunctionType &tryLockFun)
{
  named_upgradable_mutex mutex(open_or_create, resourceName.c_str());
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  try
  {
    LOGICAL_DATA(ret)[0] = tryLockFun(mutex) ? Rboolean(1) : Rboolean(0);
  }
  catch (std::exception &e)
  {
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    //printf("%s\n", e.what());
  }
  UNPROTECT(1);
  return ret;
}

template<typename UnlockFunctionType>
SEXP boost_unlock(const std::string &resourceName, 
  const UnlockFunctionType &unlockFun)
{
  named_upgradable_mutex mutex(open_or_create, resourceName.c_str());
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  try
  {
    unlockFun(mutex);
    LOGICAL_DATA(ret)[0] = Rboolean(1);
  }
  catch (std::exception &e)
  {
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    //printf("%s\n", e.what());
  }
  UNPROTECT(1);
  return ret;
}

ptime to_ptime( const long timeout )
{
  return second_clock::local_time() + seconds( timeout );
}

extern "C"
{

void DestroyBoostMutexInfo( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pbmi = 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  std::string cmName = pbmi->name()+"_counter_mutex";
  named_upgradable_mutex mutex(open_or_create, cmName.c_str());
  delete pbmi;
  R_ClearExternalPtr(mutexInfoAddr);
  named_upgradable_mutex::remove( cmName.c_str() );
}

SEXP CreateBoostMutexInfo( SEXP resourceName, SEXP timeout )
{
  BoostMutexInfo *pbmi = new BoostMutexInfo();
  if (GET_LENGTH(timeout) == 0)
  {
    pbmi->init( RChar2String(resourceName) );
  }
  else 
  {
    pbmi->init( RChar2String(resourceName), 
      static_cast<long>( NUMERIC_VALUE(timeout) ) );
  }
  SEXP address = R_MakeExternalPtr( pbmi, R_NilValue, R_NilValue );
  R_RegisterCFinalizerEx( address, (R_CFinalizer_t)DestroyBoostMutexInfo,
    (Rboolean)TRUE );
  return(address);
}

SEXP GetResourceName( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pbmi = 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  return String2RChar( pbmi->name() );
}

SEXP GetTimeout( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pbmi = 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  if (pbmi->timeout() == -1)
  {
    return NULL_USER_OBJECT;
  }
  SEXP ret = PROTECT(NEW_NUMERIC(1));
  NUMERIC_DATA(ret)[0] = static_cast<double>(pbmi->timeout());
  UNPROTECT(1);
  return ret;
}

SEXP IsRead( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  LOGICAL_DATA(ret)[0] = pmi->read() ? Rboolean(1) : Rboolean(0);
  UNPROTECT(1);
  return(ret);
}

SEXP boost_lock( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  pmi->locked() = true;
  pmi->read() = false;
  if (pmi->is_timed())
  {
    return boost_lock( pmi->name(),
      bind( &named_upgradable_mutex::timed_lock, _1, 
        to_ptime(pmi->timeout()) ) );
  }
  else
  {
    return boost_lock( pmi->name(), bind(&named_upgradable_mutex::lock, _1));
  }
}

SEXP boost_try_lock( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  pmi->locked() = true;
  pmi->read() = false;
  return boost_try_lock( pmi->name(), 
    bind( &named_upgradable_mutex::try_lock, _1 ) );
}

SEXP boost_unlock( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  if (!pmi->locked())
  {
    SEXP ret = PROTECT(NEW_LOGICAL(1));
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    warning("This mutex is already unlocked.");
    UNPROTECT(1);
    return(ret);
  }
  pmi->locked() = false;
  return boost_unlock( pmi->name(), 
    bind( &named_upgradable_mutex::unlock, _1 ) );
}

SEXP boost_lock_shared( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  pmi->locked() = true;
  pmi->read() = true;
  if (pmi->is_timed())
  {
    return boost_lock( pmi->name(),
      bind(&named_upgradable_mutex::timed_lock_sharable, 
        _1, to_ptime(pmi->timeout())) );
  }
  else
  {
    return boost_lock( pmi->name(), 
      bind(&named_upgradable_mutex::lock_sharable, _1));
  }
}

SEXP boost_try_lock_shared( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  pmi->locked() = true;
  pmi->read() = true;
  return boost_lock( pmi->name(),
    bind( &named_upgradable_mutex::try_lock_sharable, _1 ) );
}

SEXP boost_unlock_shared( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  if (!pmi->locked())
  {
    SEXP ret = PROTECT(NEW_LOGICAL(1));
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    warning("This mutex is already unlocked.");
    UNPROTECT(1);
    return(ret);
  }
  pmi->locked() = false;
  return boost_unlock( pmi->name(),
    bind( &named_upgradable_mutex::unlock_sharable, _1 ) );
}

}
