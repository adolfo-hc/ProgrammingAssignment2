## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list that contains 4 functions that are used to
## cache the inverse of a matrix with a further function. The first function
## stores the matrix to be inversed in a variable, the second function gets
## the matrix stored in the variable, the third function stores in a variable
## the function solve (this function does the inverse of a matrix) and the
## fourth function gets the inverse of the matrix stored in the variable. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function gets all the "information" from the list of functions created
## by our previous function. It has two jobs, first checks whether the inverse
## of a matrix has been solved previously or not, if it has been solved
## returns the message "getting cached data" and the value of the inverse of
## the matrix; if it doesn't have been solved, it just returns the inverse of
## the matrix.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
