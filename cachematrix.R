## makeCacheMatrix is a list of functions:
##  set - to set a matrix
##  get - to get a matrix
##  setsolve - to calculate the inverse of the matrix & attempt to cache
##  getsolve - to return the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve references functions constructed via makeCacheMatrix
##  and attempts to first retrieve a cached inverse
##  before calling the solve() routine
##  every subsequent call will retrieve from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
