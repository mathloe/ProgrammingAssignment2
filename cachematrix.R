## This R-script contains a set of functions to create a matrix type which
## caches its inverse, and to calculate the inverse.

## makeCacheMatrix() creates a new matrix and initializes functions to get and
## set the matrix' values and inverse
## the set and setinverse functions are designed to store data in the
## environment of the created matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL #inv is set to NULL, as it is not yet calculated
      set <- function(y){ #ensures inv is set to NULL when setting new values
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve() returns a matrix' inverse. If the inverse is already calculated,
## it uses the cached inverse, else it calculates the inverse and stores it in
## the cache
## it is assumed that the used matrix is square and inversible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
      }
      matrixValues <- x$get()
      inv <- solve(matrixValues)
      x$setinverse(inv)
      inv
}


