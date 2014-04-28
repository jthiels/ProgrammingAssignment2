## These functions permit the solving and caching of the inverse of a numeric matrix
## which is particularly useful if the matrix is large.

## makeCacheMatrix() takes a square numeric matrix and creates functions whereby it can be cached
## and the inverse of the matrix solved and cached.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv <- setinv,
       getinv <- getinv)

}


## cacheSolve() first checks to see if the inverse of a matrix has already been calculated
## and cached, and if so, returns the earlier result.  If not, it solves for the inverse, returning
## and caching that result.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
    
}
