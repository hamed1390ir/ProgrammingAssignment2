## This R script has two parts. makeCacheMatrix creates and manipulates 
# a new object and its getter and setter methods.

## makeCacheMatrix function makes the object and its environment containing x and inv 
# and and a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function looks for any inverse value of the object. if it could find it, returns that cached value. If the inverse value cannot be found it inverses the matrix and puts the value in the inv object in the matrix environment and returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}
