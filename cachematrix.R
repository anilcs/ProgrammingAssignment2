## put comments here that give an oveall description of what your
## functions do

## Pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y) {
    x <<- y;
    mat_inverse <<- NULL;
  }
  get <- function() return(x);
  setinv <- function(inv) mat_inverse <<- inv;
  getinv <- function() return(mat_inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mat_inverse <- x$getinv()
  if(!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get()
## Computing the inverse of a square matrix
  mat_inverse <- solve(data, ...) 
  x$setinv(mat_inverse)
  return(mat_inverse)
## Return a matrix that is the inverse of 'x'
}
