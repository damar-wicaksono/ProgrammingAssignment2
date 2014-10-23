# Assignment 2
# Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some 
# benefits to caching the inverse of a matrix.
# Write the following functions:
#   1. makeCacheMatrix  : This function creates a spefial matrix object that can 
#                         cache its inverse
#   2. cacheSolve       : This function computes the inverse of the special 
#                         matrix returned by makeCacheMatrix above. If the 
#                         inverse has already been calculated (and the matrix 
#                         has not changed), then the cachesolve should retrieve
#                         the inverse from the cache

## create a list of functions to store a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  # 'set' function is used to assign new matrix values after it has been set
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 'get' function is used to obtain the matrix value
  get <- function() x
  
  # 'setinv' function is used to calculate the inverse function
  setinv <- function(y) inv <<- y
  
  # 'getinv' function is used to obtain the inverse matrix
  getinv <- function() inv
  
  # return a list of all the functions defined in the makeCacheMatrix
  list( set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv 
  )
}

## Calculate the inverse of matrix stored in object created by 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # If the matrix already has the inverse, compute it
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  
  # If the inverse is not there yet
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
