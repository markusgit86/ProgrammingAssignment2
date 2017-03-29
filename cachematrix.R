
## the function makeCacheMatrix takes a matrix as an input and creates a vector/list of functions
## to set / get the value of the vector and get / set the value of the solve function of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## This function computes the inverse of the list returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
