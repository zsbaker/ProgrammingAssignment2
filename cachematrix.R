## Put comments here that give an overall description of what your


## Handling the inverse of a matrix (especially repeatedly) is a costly computation and so the following functions
## will provide a means to cache the inverse of the matrix for use later.
## makeCacheMatrix and cacheSolve utilize and demonstrate the caching and retrieval of the special object

## makeCacheMatrix creates a special matrix and calculates and stores the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns the inverse of the matrix created using the above function
## If the cached inverse is available, cacheSolve gets it and if isn't available it will then solve it, cache it and retrieve it on spot

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}