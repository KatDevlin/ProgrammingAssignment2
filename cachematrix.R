## This assignment consists of two functions that cache matrix inversions.

# K. Devlin
# Coursera R Programming
# Week 3, Programming Assignment 2
# Peer Reviewed Assignment

## The first function creates an object, a matrix, that can cache its inverse.
## For this assignment we work under the assumption that the supplied 
## matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This, the second function, takes the object created in the first function
## above and computes the inverse. If the inverse has already been calculated,
## this function will retrieve it from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("Retrieving cached data ...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}


## Test of above code for both functions:
test <- matrix(c(1,2,3,4),2,2)
test1 <- makeCacheMatrix(test)
cacheSolve(test1) # computed
cacheSolve(test1) # returned from cache