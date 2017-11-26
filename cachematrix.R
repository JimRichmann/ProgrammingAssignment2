## This is Programming Assignment 2

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  myInverse <- NULL
  set <- function(y) {
    x <<- y
    myInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) myInverse <<- solve
  getInverse <- function() myInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInverse <- x$getInverse()
    if(!is.null(mInverse)) {
      message("getting cached data")
      return(mInverse)
    }
    data <- x$get()
    mInverse <- solve(data)
    x$setInverse(mInverse)
  mInverse
}

## test makeCacheMatrix
##  

myTest <- function() {
  test <- makeCacheMatrix()
  class(test)
  test$set(matrix(data = c(1,0,5,2,1,6,3,4,0),nrow = 3, ncol = 3))
  test$get()
  cacheSolve(test)
  cacheSolve(test)  #again
}
# myTest()
