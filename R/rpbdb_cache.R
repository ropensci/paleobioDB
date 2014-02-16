# Cache mecanism for internal use of the package
# Built on the need for a way to store dynamic runtime private package
# variables
# TODO: look for R native ways

.private_pk_cache <- new.env()

#' Returns a cached variable if it's intializated. If not, 
#' initialize it by calling the function provided
#' 
#' @param varname Name of the variable to retrieve
#' @param setupfun Function to compute the value of the variable the
#' fist time
#
.package_cache_return_or_setup<-function(varname, setupfun)
{
  if(!.package_cache_Has(varname))
  {
  	if(is.function(setupfun)){
  		setupcall <- as.call(list(setupfun))
    } else {
      setupcall <- call(setupfun)
    }

    setupvalue <- eval(setupcall)
    .package_cache_Set(varname, setupvalue)

    .package_cache_Get(varname)
  }

  return (.package_cache_Get(varname))
}


.package_cache_has<-function(varname)
{
  if(exists(varname, envir=.private_pk_cache)){

    return (!is.null(.package_cache_get(varname)))
  }

  return (FALSE)
}

.package_cache_get<-function(varname)
{
	return (get(varname, envir=.private_pk_cache))
}

.package_cache_set<-function(varname, value)
{
  assign(varname, value, envir = .private_pk_cache)
}

.package_cache_delete<-function(varname)
{
	assign(varname, NULL, envir = .private_pk_cache)
  # TODO: use rm / remove (getting "must contain names or character strings" error)
  # rm(list(varname), envir = .private_pk_cache)
}

.package_cache_empty<-function()
{
  rm(list = ls(envir = .private_pk_cache))
}