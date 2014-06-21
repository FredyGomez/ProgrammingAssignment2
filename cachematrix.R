#Programming Assignment 2: This R script contains two functions:
#
#Write the following functions:
#  
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.

# Usage:      
# > mat <- makeCacheMatrix(matrix(rnorm(64), nrow = 8))                  
# > mat$get()                                  
# > cacheSolve(mat)                            
# > cacheSolve(mat)                            


# makeCacheMatrix: Function that Creates and special matrix 
# that can store its inverse in cache (with setinv)

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  # Getter for the inverse
  getinv <- function() inv
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated, returns the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  #Check if the inverse is already calculated, if so, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If not, then calculate it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
