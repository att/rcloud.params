param.caps <- NULL
params.QS <- NULL

.onLoad <- function(libname, pkgname)
{
  f <- function(module.name, module.path) {
    path <- system.file("javascript", module.path, package="rcloud.params")
    caps <- rcloud.install.js.module(module.name,
                                     paste(readLines(path), collapse='\n'))
    caps
  }
  param.caps <<- f("rcloud.params", "rcloud.params.js")
  if(!is.null(param.caps)) {
    param.QS <<- param.caps$init()
  }
}
