#include <iostream>
#include <string>
#include <boost/interprocess/sync/named_upgradable_mutex.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include <boost/bind.hpp>
#include <boost/date_time/local_time/local_date_time.hpp>

#include <Rcpp.h>

#include "../inst/synchronicity/util.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace boost::posix_time;

class BoostMutexInfo
{
  public:

    BoostMutexInfo() : 
      _timeout(-1), _name(""), _read(true), _locked(false), 
      _pmutex(NULL), _create(true) {}
    
    virtual ~BoostMutexInfo() {destroy();}
  

    bool init(const std::string &newName, const bool create)
    {
      _name = newName;
      _create = create;
      if (_create) 
        _pmutex = new named_upgradable_mutex(create_only, newName.c_str());
      else
        _pmutex = new named_upgradable_mutex(open_only, newName.c_str());
      return true;
    }

    bool init(const std::string &newName, const long timeout, const bool create)
    {
      init(newName, create);
      _timeout = timeout;
      return true;
    }

    bool destroy()
    {
      if (_pmutex) delete _pmutex;
      if (_create)
        named_upgradable_mutex::remove( _name.c_str() );
      return true;
    }
    
    long timeout() const {return _timeout;}

    std::string name() const {return _name;}

    bool is_timed() const {return _timeout!=-1;}
    
    bool& read() {return _read;}
    bool& locked() {return _locked;}
    named_upgradable_mutex& mutex() {return *_pmutex;}

  protected:
    long _timeout;
    std::string _name;
    named_upgradable_mutex *_pmutex;
    bool _read;
    bool _locked;
    bool _create;
};

template<typename MutexType, typename LockFunctionType>
SEXP boost_lock(MutexType &mutex, const LockFunctionType &lockFun)
{
//  named_upgradable_mutex mutex(open_or_create, resourceName.c_str());
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  try
  {
    lockFun(mutex);
    LOGICAL(ret)[0] = Rboolean(1);
  }
  catch (std::exception &e)
  {
    LOGICAL(ret)[0] = Rboolean(0);
    //printf("%s\n", e.what());
  }
  Rf_unprotect(1);
  return ret;
}

template<typename MutexType, typename TryLockFunctionType>
SEXP boost_try_lock(MutexType &mutex, const TryLockFunctionType &tryLockFun)
{
//  named_upgradable_mutex mutex(open_or_create, resourceName.c_str());
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  try
  {
    LOGICAL(ret)[0] = tryLockFun(mutex) ? Rboolean(1) : Rboolean(0);
  }
  catch (std::exception &e)
  {
    LOGICAL(ret)[0] = Rboolean(0);
    //printf("%s\n", e.what());
  }
  Rf_unprotect(1);
  return ret;
}

template<typename MutexType, typename UnlockFunctionType>
SEXP boost_unlock(MutexType &mutex, const UnlockFunctionType &unlockFun)
{
//  named_upgradable_mutex mutex(open_or_create, resourceName.c_str());
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  try
  {
    unlockFun(mutex);
    LOGICAL(ret)[0] = Rboolean(1);
  }
  catch (std::exception &e)
  {
    LOGICAL(ret)[0] = Rboolean(0);
    //printf("%s\n", e.what());
  }
  Rf_unprotect(1);
  return ret;
}

ptime to_ptime( const long timeout )
{
  return second_clock::local_time() + seconds( timeout );
}

void DestroyBoostMutexInfo( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pbmi = 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  delete pbmi;
  R_ClearExternalPtr(mutexInfoAddr);
}

template<bool create>
SEXP GenericCreateBoostMutexInfo( SEXP resourceName, SEXP timeout )
{
  BoostMutexInfo *pbmi = new BoostMutexInfo();
  if (Rf_length(timeout) == 0)
  {
    pbmi->init( RChar2String(resourceName), create );
  }
  else 
  {
    pbmi->init( RChar2String(resourceName), 
      static_cast<long>( REAL(timeout)[0] ), create );
  }
  SEXP address = R_MakeExternalPtr( pbmi, R_NilValue, R_NilValue );
  R_RegisterCFinalizerEx( address, (R_CFinalizer_t)DestroyBoostMutexInfo,
    (Rboolean)TRUE );
  return(address);
}

// [[Rcpp::export]]
SEXP CreateBoostMutexInfo(SEXP resourceName, SEXP timeout ) 
{
  return GenericCreateBoostMutexInfo<true>(resourceName, timeout);
}

// [[Rcpp::export]]
SEXP AttachBoostMutexInfo( SEXP resourceName, SEXP timeout )
{
  return GenericCreateBoostMutexInfo<false>(resourceName, timeout);
}

// [[Rcpp::export]]
SEXP GetResourceName( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pbmi = 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  return String2RChar( pbmi->name() );
}

// [[Rcpp::export]]
SEXP GetTimeout( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pbmi = 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  if (pbmi->timeout() == -1)
  {
    return R_NilValue;
  }
  SEXP ret = Rf_protect(Rf_allocVector(REALSXP, 1));
  REAL(ret)[0] = static_cast<double>(pbmi->timeout());
  Rf_unprotect(1);
  return ret;
}

// [[Rcpp::export]]
SEXP IsRead( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
  LOGICAL(ret)[0] = pmi->read() ? Rboolean(1) : Rboolean(0);
  Rf_unprotect(1);
  return(ret);
}

// [[Rcpp::export]]
SEXP boost_lock( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  pmi->locked() = true;
  pmi->read() = false;
  if (pmi->is_timed())
  {
    return boost_lock( pmi->mutex(),
      bind( &named_upgradable_mutex::timed_lock, _1, 
        to_ptime(pmi->timeout()) ) );
  }
  else
  {
    return boost_lock( pmi->mutex(), bind(&named_upgradable_mutex::lock, _1));
  }
}

// [[Rcpp::export]]
SEXP boost_try_lock( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  pmi->locked() = true;
  pmi->read() = false;
  return boost_try_lock( pmi->mutex(), 
                         bind( &named_upgradable_mutex::try_lock, _1 ));
}

// [[Rcpp::export]]
SEXP boost_unlock( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  if (!pmi->locked())
  {
    SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
    LOGICAL(ret)[0] = Rboolean(0);
    Rf_warning("This mutex is already unlocked.");
    Rf_unprotect(1);
    return(ret);
  }
  pmi->locked() = false;
  return boost_unlock( pmi->mutex(), 
    bind( &named_upgradable_mutex::unlock, _1 ));
}

// [[Rcpp::export]]
SEXP boost_lock_shared( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  pmi->locked() = true;
  pmi->read() = true;
  if (pmi->is_timed())
  {
    return boost_lock( pmi->mutex(),
      bind(&named_upgradable_mutex::timed_lock_sharable, 
        _1, to_ptime(pmi->timeout())));
  }
  else
  {
    return boost_lock( pmi->mutex(), 
      bind(&named_upgradable_mutex::lock_sharable, _1));
  }
}

// [[Rcpp::export]]
SEXP boost_try_lock_shared( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  pmi->locked() = true;
  pmi->read() = true;
  return boost_lock( pmi->mutex(),
    bind( &named_upgradable_mutex::try_lock_sharable, _1 ) );
}

// [[Rcpp::export]]
SEXP boost_unlock_shared( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pmi= 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  if (!pmi->locked())
  {
    SEXP ret = Rf_protect(Rf_allocVector(LGLSXP,1));
    LOGICAL(ret)[0] = Rboolean(0);
    Rf_warning("This mutex is already unlocked.");
    Rf_unprotect(1);
    return(ret);
  }
  pmi->locked() = false;
  return boost_unlock( pmi->mutex(),
    bind( &named_upgradable_mutex::unlock_sharable, _1 ) );
}

