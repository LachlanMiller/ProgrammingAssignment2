## A set of R functions to cache potentially time-consuming computations
## in this case, matrix inversion
## This is beneficial if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a matrix are not changing, it may make sense to cache 
## the value of the inversion so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 
## This will take advantage of the scoping rules of the R language 
## and how they can be manipulated to preserve state inside of an R object.


## makeCache Matris creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       getinv = getinv,
       setinv = setinv)
}


## cacheSolve calculates the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## This function assumes that the matrix is always invertible.

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
