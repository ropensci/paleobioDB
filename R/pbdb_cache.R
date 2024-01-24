# Cache mecanism for internal use of the package
# Built on the need for a way to store dynamic runtime private package variables
# TODO: look for R native ways

#' .private_pk_cache
#' 
#' Environment for the runtime variables of the package

.private_pk_cache <- new.env()


#' .package_cache_return_or_setup
#'
#' Returns a cached variable value. If it's intializated already, it will be initialized by calling the function provided
#' 
#' @usage .package_cache_return_or_setup(varname, setupfun)
#'
#' @param varname Name of the variable to retrieve
#' @param setupfun Function to compute the value of the variable in case it is not yet initialized
#' @examples \dontrun{
#' .package_cache_return_or_setup("specieslist", function(){
#'     list_from_remote_server <- list("dog", "cat", "chupacabra")
#'     return (list_from_remote_server)
#' })
#' }
#' @return value of the cached variable
#'

.package_cache_return_or_setup<-function(varname, setupfun)
{
  if(!.package_cache_has(varname))
  {
  	if(is.function(setupfun)){
  		setupcall <- as.call(list(setupfun))
    } else {
      setupcall <- call(setupfun)
    }

    setupvalue <- eval(setupcall)
    .package_cache_set(varname, setupvalue)

    .package_cache_get(varname)
  }

  return (.package_cache_get(varname))
}


#' .package_cache_has
#' 
#' Check weather a variable exists in the package cache environment
#' 
#' @usage .package_cache_has(varname)
#' 
#' @param varname Name of the variable
#' @return boolean 
#'

.package_cache_has<-function(varname)
{
  if(exists(varname, envir=.private_pk_cache)){

    return (!is.null(.package_cache_get(varname)))
  }

  return (FALSE)
}


#' .package_cache_get
#' 
#' Retrieves the value of a cache variable already initialized
#' 
#' @usage .package_cache_get(varname)
#' 
#' @param varname Name of the variable
#' @return value of the variable
#'

.package_cache_get<-function(varname)
{
  return (get(varname, envir=.private_pk_cache))
}

#' .package_cache_set
#'
#' Sets a variable value in cache
#'
#' @usage .package_cache_set(varname, value)
#'
#' @param varname Name of the variable
#' @param value the value to be assigned to the variable
#' @return value of the variable
#' @examples \dontrun{
#' .package_cache_set("specieslist", list("dog", "cat", "chupacabra"))
#' }
#' @noRd

.package_cache_set<-function(varname, value)
{
  assign(varname, value, envir = .private_pk_cache)
}

#' .package_cache_delete
#'
#' Delete a variable from cache environment
#' 
#' @usage .package_cache_delete(varname)
#'
#' @param varname the variable to delete
#' @noRd
.package_cache_delete<-function(varname)
{
	assign(varname, NULL, envir = .private_pk_cache)
  # TODO: use rm / remove (getting "must contain names or character strings" error)
  # rm(list(varname), envir = .private_pk_cache)
}

#' .package_cache_empty
#' 
#' Empty the cache
#'

.package_cache_empty<-function()
{
  rm(list = ls(envir = .private_pk_cache))
}
