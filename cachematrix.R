# Because matrix inversion is costly, these functions allow us
# to avoid repeated computations by making the result of the
# calculation cacheable.

# Wraps given matrix in an object that can hold a cached value of
# the matrix's inverse.
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL

  set <- function(new_matrix) {
    m <<- new_matrix
    inv <<- NULL
  }

  get <- function() m
  setinv <- function(new_inv) inv <<- new_inv
  getinv <- function() inv

  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Returns inverse of matrix contained in object created by
# makeCacheMatrix(); value is returned if available and is
# otherwise calculated, cached and returned.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()

  if (!is.null(inv)) {
    message("Found cached data.")
  } else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
  }

  inv
}
