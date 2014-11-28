# MUTEXES


setClass('mutex')
setGeneric('lock', function(m, ...) standardGeneric('lock'))
setGeneric('lock.shared', function(m, ...) standardGeneric('lock.shared'))
setGeneric('unlock', function(m, ...) standardGeneric('unlock'))

setClass('boost.mutex', contains='mutex', 
  representation(isRead='logical', mutexInfoAddr='externalptr'))

setGeneric('read', function(m) standardGeneric('read'))
setMethod('read', signature(m='boost.mutex'), function(m) 
  return(.Call("IsRead", m@mutexInfoAddr)))

setMethod('lock', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=TRUE
    if (!is.logical(block)) stop('The block argument should be logical')
    return(
      .Call(ifelse(block, 'boost_lock', 'boost_try_lock'), m@mutexInfoAddr))
  })
setMethod('lock.shared', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=TRUE
    if (!is.logical(block)) stop('The block argument should be logical')
    return( 
      .Call(ifelse(block, 'boost_lock_shared', 'boost_try_lock_shared'),
        m@mutexInfoAddr))
  })
setMethod('unlock', signature(m='boost.mutex'),
  function(m, ...)
  {
    return(
      .Call( ifelse(read(m), 'boost_unlock_shared', 'boost_unlock'),
        m@mutexInfoAddr) )
  })

setGeneric('shared.name', function(m) standardGeneric('shared.name'))

setMethod('shared.name', signature(m='boost.mutex'), 
  function(m) 
  {
    return(.Call('GetResourceName', m@mutexInfoAddr))
  })

setGeneric('timeout', function(m) standardGeneric('timeout'))

setMethod('timeout', signature(m='boost.mutex'),
  function(m)
  {
    return(.Call('GetTimeout', m@mutexInfoAddr))
#    ret = .Call('GetTimeout', m@mutexInfoAddr)
#    if (length(ret) == 0) ret = Inf
#    return(ret)
  })

setGeneric('is.timed', function(m) standardGeneric('is.timed'))
setMethod('is.timed', signature(m='boost.mutex'),
  function(m)
  {
    return(!is.null(timeout(m)))
  })

# The constructor for a boost.mutex
boost.mutex=function(sharedName=NULL, timeout=NULL)
{
  isRead = TRUE
  if (is.null(sharedName)) 
  {
    sharedName = uuid()
  }
  if (!is.null(timeout) && !is.numeric(timeout))
  {
    stop("The timeout parameter must be numeric.")
  }
  if (is.numeric(timeout) && timeout <= 0)
  {
    stop("You must specify a timeout greater than zero.")
  }
  mutexInfoAddr=.Call('CreateBoostMutexInfo', sharedName, as.double(timeout))
  return(new('boost.mutex', isRead=isRead, mutexInfoAddr=mutexInfoAddr))
}


setClass('descriptor', representation(description='list'))
setGeneric('description', function(x) standardGeneric('description'))
setMethod('description', signature(x='descriptor'),
  function(x) return(x@description))

setClass('boost.mutex.descriptor', contains='descriptor')

setMethod('describe', signature(x='boost.mutex'),
  function(x)
  {
    return(new('boost.mutex.descriptor',
      description=list(shared.name=shared.name(x),
      timeout=timeout(x))))
  })

setGeneric('attach.mutex', function(obj, ...) 
  standardGeneric('attach.mutex'))

setMethod('attach.mutex', signature(obj='character'),
  function(obj, ...)
  {
    path = match.call()[['path']]
    if (is.null(path))
    {
      path <- '.'
    }
    path <- path.expand(path)
    if (basename(obj) != obj)
    {

        warning(paste("Two paths were specified in attach.mutex",
          "The one associated with the file will be used.", sep="  "))
      path <- dirname(obj)
      obj <- basename(obj)
    }
    fileWithPath <- file.path(path, obj)
    fi = file.info(fileWithPath)
    print(dir())
    if (is.na(fi$isdir))
      stop( paste("The file", fileWithPath, "could not be found") )
    if (fi$isdir)
      stop( fileWithPath, "is a directory" )
    info <- dget(fileWithPath)
    return(attach.mutex(info, path=path))
  })

setMethod('attach.mutex', signature(obj='boost.mutex.descriptor'),
  function(obj, ...)
  {
    desc = description(obj)
    return(boost.mutex(sharedName = desc$shared.name,
      timeout = desc$timeout))
  })


