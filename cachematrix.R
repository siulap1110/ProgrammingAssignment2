## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## m=inverse default inverse is null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x ## function to get matrix x 
  setInverse <- function(inverse) m <<- inverse ## function to get inverse of the matrix x
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) { ## gets cache data
  m <- x$getInverse()
  if (!is.null(m)) {   ## checking inverse is NULL or not 
    message("getting cached data")
    return(m) ## Return a matrix that is the inverse of 'x'
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}

