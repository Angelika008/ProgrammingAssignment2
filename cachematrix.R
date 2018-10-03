# The first function, makeVector, creates a special "matrix", which is a list 
# containing a function to:

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <- inverse
  getinverse <- function() invs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The second function, cacheSolve, returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  mat <- x$get()
  invs <- solve(mat, ...)
  x$setinverse(invs)
  invs
}
