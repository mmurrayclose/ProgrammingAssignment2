
# The function makeCacheMatrix creates the cache environment and stores the         
# matrix entered by the user in the cache. For example, if the user enters
# makeCacheMatrix(A), where A is a matrix, the function stores the matrix A in 
# the chache. The function makeCacheMatrix also returns a list of functions
# that are called by the function cacheSolve to access and update the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  # Initiate inverse as null.
  m <- NULL
  
  # Function to store matrix user entered in cache.
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Function to retrieve matrix stored in cache.
  getmatrix <- function() x
  
  # Function to store inverse in cache.
  setinverse <- function(inverse) m <<- inverse
  
  # Function to retrieve inverse stored in cache.
  getinverse <- function() m
  
  # Return list of functions called by cacheSolve to access and update cache.
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# The function cacheSolve retrieves the matrix the user entered from the 
# cache. If the inverse has already been computed, it returns the inverse from
# the cache. Otherwise, it compuetes the inverse, stores the newly computed 
# inverse in the cache, and returns the newly computed inverse.

cacheSolve <- function(x, ...) {
  
  # Retrieve inverse from cache.
  m <- x$getinverse()
  
  # If inverse in cache is not null, return inverse from cache.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If inverse in cacge is null, retrieve matrix from cache.
  data <- x$getmatrix()
  
  # Compute inverse of matrix.
  m <- solve(data, ...)
  
  # Store newly computed inverse in cache.
  x$setinverse(m)
  
  # Return newly computed inverse.
  m
  
}
