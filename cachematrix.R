## makeCachematrix
## set the value of the matrix, get the value of the matrix
## set the value of the solved matrix, get the value of the solved matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(invertmatrix) inv <<- invertmatrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## function calculate the inverted matrix, however, in a first step
## check if the inverted matrix has already been calculated and is available in cache
## in such a scenario, share the result available in cache, alongside a message
## if the result isn't available yet, calculate the inverted matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
