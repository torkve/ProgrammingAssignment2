## Educational module, providing some cacheable matrix operations.
##
## Due to its educational idea, the only available operation is matrix
## inversion.


## Create a matrix wrapper with cacheable inverse function
## @arg `x` matrix value
## @return matrix wrapper
makeCacheMatrix <- function(x = matrix()) {
  val <- x
  slvArgs <- NULL
  slv <- NULL
  
  set <- function(x) {
    val <<- x
    slvArgs <<- NULL
    slv <<- NULL
  }
  setsolve <- function(args, s) {
    slvArgs <<- args
    slv <<- s
  }
  
  get <- function() val
  getsolve <- function() list(args=slvArgs, solve=slv)
  list(set=set, setsolve=setsolve, get=get, getsolve=getsolve)
}


## Inverse a matrix, returning cached result if available
## @arg `x` matrix wrapper created with `makeCacheMatrix`
## @arg `...` optional args to solve function
## @note new set of arguments invalidates cache
## @return inverted matrix
cacheSolve <- function(x, ...) {
  args <- c(...)
  result <- x$getsolve()
  if (is.null(result$solve) | !identical(args, result$args)) {
    res <- solve(x$get(), ...)
    x$setsolve(args, res)
    return(res)
  }
  return(result$solve)
}
