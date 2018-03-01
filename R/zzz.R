input.caps <- NULL
input.QS <- NULL


.onLoad <- function(libname, pkgname)
{
  f <- function(module.name, module.path) {
    path <- system.file("javascript", module.path, package="rcloud.params")
    caps <- rcloud.install.js.module(module.name,
                                     paste(readLines(path), collapse='\n'))  # Install javascript files on browser-side
    caps
  }
  input.caps <<- f("rcloud.params", "rcloud.params.js")  # is everything on js side eg init, focus, error_highlight, submit_wait() 
  
  if(!is.null(input.caps)) {
    # init calls js queryString which grabs url and splits. init also has a method for updating url
    input.QS <<- input.caps$init()  # $notebook in list with notebook id string
    
  }
  
}
