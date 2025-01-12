# Function to create a special "matrix" that can cache its inverse
makeCacheMatrix <- function(a = matrix()) {
  # Initializing the inverse 
  i <- NULL  
  
  setMatrix <- function(b) {
    a <<- b
    # whenever a new matrix is set , we will reset the inverse
    i <<- NULL  
  }
  # getting the matrix
  getMatrix <- function() a 
  # Cache the inverse
  setInverse <- function(inverse) i <<- inverse  
  # Retrieve the cached inverse
  getInverse <- function() i  
  
  list(set = setMatrix, get = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the "matrix" or retrieve the cached value
cacheSolve <- function(a, ...) {
  # Checking if the inverse is cached
  i<- a$getInverse() 
  
  if (!is.null(i)) {
    message("getting cached data") 
    # If the inverse is cached, return the value
    return(i)
  }
  # Get the matrix
  m <- a$get()
  # Calculating  the inverse
  i<- solve(m, ...)
  # Cache the inverse
  a$set(i)
  # Return the inverse
  i 
}