.First.lib <- function(libname, pkgname) {
    library.dynam("particleTracking", pkgname, libname)
}

### in case we decide to keep the namespace ...
.onLoad <- .First.lib
