## This code was written for the Coursera "R Programming" course,
## assignment #2. This assignment demonstrates use of lexical scoping.
## The functions below take advantage of lexical scoping to avoid the
## numerically intensive step of inverting a matrix if the inverted
## matrix has previously been solved.


## The following function creates a list of 4 objects.
## The lists' objects are themselves functions which
## allow us to set or get an matrix object. It also
## allows us to set or get the corresponding inverse
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The following function tests whether the matrix object
## passed to it already has an inverse stored in a cache
## associated with the matrix. If it already exists, that
## existing value is returned from this function. If there
## is no existing inverse matrix already stored, the inverse
## is calculated and then stored in the cach of the
## original matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
