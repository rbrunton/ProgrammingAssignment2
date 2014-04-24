## The two functions in this file support caching the results of calculating the inverse of a matrix.
#  The "cacheSolve" method takes a single argument, an object created by the "makeCacheMatrix"
#  function, and returns the inverse of the R matrix passed to the "makeCacheMatrix" function
#  to create the object.

## This function creates an object which wraps an R matrix and provides accessor and setter
#  functions that utilize the R scoping rules to return a cached copy of the inverse of the
#  wrapped matrix if that inverse was previously calculated. The "x" argument is a square matrix.
makeCacheMatrix <- function(x = matrix()) {
      #start by setting cached inverse to NULL
      inverse <- NULL
      #allow the wrapped matrix to be modified; if it is modified, reset inverse to NULL
      set <- function(y) {
        x <<- y
        inverse <<- NULL
      }
      #accessor method for the wrapped matrix
      get <- function() x
      #function to set the inverse of the wrapped matrix
      setinverse <- function(inverse) inverse <<- inverse
      #accessor function to return the inverse
      getinverse <- function() inverse
      #return the list of functions, indexed by function name
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function solves for the inverse of a matrix or retrieves the previously calculated
#  inverse of the matrix. The "x" argument variable is an object created using by calling the 
#  "makeCacheMatrix" function on a square R matrix.
cacheSolve <- function(x, ...) {
      #get the inverse variable from the scope of "x"
      inverse <- x$getinverse()
      #if retrieved value is not NULL, then the inverse was previously calculated, so return
      #the cached value
      if(!is.null(inverse)) {
        return(inverse)
      }
      #...otherwise get the wrapped square matrix
      data <- x$get()
      #...and solve for the inverse
      inverse <- solve(data)
      #now set the inverse value in the scope of "x" for future cache retrieval
      x$setinverse(inverse)
      #return the inverse of the matrix wrapped by "x"
      inverse
}