## Speed up matrix inversion computations by caching the inverse
## rather than repeatedly calculating the inverse in a loop.

## Creates a list containing a function which sets/gets the value of 
## the matrix, and sets/gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## Check to see if the inverse matrix has been calculated. If so, 
## get the inverse from the cache, otherwise compute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
