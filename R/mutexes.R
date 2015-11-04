# MUTEXES

#' @export
setClass('mutex')

#' @export
setGeneric('lock', function(m, ...) standardGeneric('lock'))

#' @export
setGeneric('lock.shared', function(m, ...) standardGeneric('lock.shared'))

#' @export
setGeneric('unlock', function(m, ...) standardGeneric('unlock'))

#' @export
setClass('boost.mutex', contains='mutex', 
  representation(isRead='logical', mutexInfoAddr='externalptr'))

#' @export
setGeneric('read', function(m) standardGeneric('read'))

#' @export
setMethod('read', signature(m='boost.mutex'), function(m) 
  return(.Call("IsRead", m@mutexInfoAddr, PACKAGE="synchronicity")))

#' @export
setMethod('lock', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=TRUE
    if (!is.logical(block)) stop('The block argument should be logical')
    block_call = ifelse(block, 'boost_lock', 'boost_try_lock')
    .Call(block_call, m@mutexInfoAddr, PACKAGE="synchronicity")
  })

#' @export
setMethod('lock.shared', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=TRUE
    if (!is.logical(block)) stop('The block argument should be logical')
    block_call = ifelse(block, 'boost_lock_shared', 'boost_try_lock_shared')  
    .Call(block_call, m@mutexInfoAddr, PACKAGE="synchronicity")
  })

#' @export
setMethod('unlock', signature(m='boost.mutex'),
  function(m, ...)
  {
    block_call = ifelse(read(m), 'boost_unlock_shared', 'boost_unlock')
    .Call(block_call, m@mutexInfoAddr, PACKAGE="synchronicity")
  })

#' @export
setGeneric('shared.name', function(m) standardGeneric('shared.name'))

#' @export
setMethod('shared.name', signature(m='boost.mutex'), 
  function(m) 
  {
    .Call('GetResourceName', m@mutexInfoAddr, PACKAGE="synchronicity")
  })

#' @export
setGeneric('timeout', function(m) standardGeneric('timeout'))

#' @export
setMethod('timeout', signature(m='boost.mutex'),
  function(m)
  {
    .Call('GetTimeout', m@mutexInfoAddr, PACKAGE="synchronicity")
#    ret = .Call('GetTimeout', m@mutexInfoAddr)
#    if (length(ret) == 0) ret = Inf
#    return(ret)
  })

#' @export
setGeneric('is.timed', function(m) standardGeneric('is.timed'))

#' @export
setMethod('is.timed', signature(m='boost.mutex'),
  function(m)
  {
    return(!is.null(timeout(m)))
  })

#' @export
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
  mutexInfoAddr=.Call('CreateBoostMutexInfo', sharedName, as.double(timeout),
                      PACKAGE="synchronicity")
  return(new('boost.mutex', isRead=isRead, mutexInfoAddr=mutexInfoAddr))
}


#' @export
setClass('descriptor', representation(description='list'))

#' @export
setGeneric('description', function(x) standardGeneric('description'))

#' @export
setMethod('description', signature(x='descriptor'),
  function(x) return(x@description))

#' @export
setClass('boost.mutex.descriptor', contains='descriptor')

#' @importFrom bigmemory.sri describe
#' @import methods
#' @export
setMethod('describe', signature(x='boost.mutex'),
  function(x)
  {
    return(new('boost.mutex.descriptor',
      description=list(shared.name=shared.name(x),
      timeout=timeout(x))))
  })

#' @export
setGeneric('attach.mutex', function(obj, ...) 
  standardGeneric('attach.mutex'))

#' @export
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

#' @export
setMethod('attach.mutex', signature(obj='boost.mutex.descriptor'),
  function(obj, ...)
  {
    desc = description(obj)
    return(boost.mutex(sharedName = desc$shared.name,
      timeout = desc$timeout))
  })


