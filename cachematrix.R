## CacheMatrix takes in a square matrix and returns the inverse of that martix 
## as well as storing the result matrix in cache.


## makeCacheMatrix takes in a square matrix and returns a list of function which are
## now associated with matrix passed in.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverted <- function(solve) m <<- solve
  
  getInverted <- function() m
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
  
}


## the cacheSolve function takes a square matrix as input and uses the 
## functions defined with the makeCacheMatrix to get the inverse of the matrix if cached.
## If the function has not been previously inverted by the function, it then inverts 
## the matrix and caches the result using the functions defined in makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverted()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverted(m)
  m
  
}

