## Create a list of getter-setter pair functions to retrieve and return the 
## matrix value and its inverse matrix value to be called by other functions

## Initializing 
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      # Set matrix value x in parenting environment
      setMatrix <- function(y){
            x <<- y
            # Clean previously cached inverse matrix
            inv <<- NULL
      }
      # Get matrix value x from parenting environment makeCacheMatrix
      getMatrix <- function() x
      # Access inverse matrix in parenting environment
      setInverse <- function(inverse) inv <<- inverse
      # Getter for inverse
      getInverse <- function() inv
      # Return all function in a named list
      list(setMatrix = setMatrix, getMatrix = getMatrix, 
           setInverse = setInverse, getInverse = getInverse)
}

## Function to call getter&setter pair functions in makeCacheMatrix, return 
## newly calculated inverse matrix or cached inverse

cacheSolve <- function(x, ...) {
      # retrive cached inverse matrix
      inv <- x$getInverse()
      # check if new matrix, thus NULL inverse
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      # retrive new matrix and calculate new inverse
      matrix <- x$getMatrix()
      inv <- solve(matrix, ...)
      # set inverse matrix in parenting, and return 
      x$setInverse(inv)
      inv
}
