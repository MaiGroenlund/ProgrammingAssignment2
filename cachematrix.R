## The following pair of functions makeCacheMatrix and cacheSolve is
## designed to let you cache the inverse of a matrix. 
## Matrix inversion can be a costly computation so these functions
## allows you to cache and already calculated inverse of a matrix rather 
## than compute it repeatedly.

## The function makeCacheMatrix below returns a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      # Set-function
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      # Get-function
      get <- function() x
      
      # Setinverse-function
      setinverse <- function(inverse) i <<- inverse

      # Getinverse-function
      getinverse <- function() i
      
      # Returns the list of newly defines functions 
      list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
}


## The function cacheSolve below computes the inverse of an invertible matrix.
## If the inverse has already been calculated the function returns the cached 
## inverse.

cacheSolve <- function(x, ...) {
      
      i <- x$getinverse()
      
      # If the inverse is already calculated the function prints a message and
      # returns the ached inverse.
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      # If the inverse is not yet calculated it gets calculated here
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      
      # Returns the newly calculated inverse
      i
      
}
