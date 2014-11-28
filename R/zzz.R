.onLoad <- function(libname, pkgname)
{
  library.dynam('synchronicity', pkgname, libname);
}

.onUnload <- function(libpath)
{
  library.dynam.unload('synchronicity', libpath);
}
