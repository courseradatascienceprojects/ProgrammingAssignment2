## Solving the inverse of a matrix calculation is a costly computation, the below functions can be used to cache the calculation and retrieve if previously solved. Assumes that the matrix supplied is always invertible.

##  makeCacheMatrix creates a special matrix, which is a list containing a functions to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix using makeCacheMatrix; if the inverse has previously been 
## solved it gets the inverse from the cache and skips the computation. Otherwise, it 
## solves the inverse  of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
