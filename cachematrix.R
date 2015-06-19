## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # Initially set to NULL
  set <- function(y) {  #set function
    x <<- y
    inv <<- NULL  #Sets the matrix itself but not the inverse
  }
  get <- function() x  #get function and the matrix itself but not the inverse
  setinverse <- function(inverse) inv <<- inverse  # Manually set the inverse
  getinverse <- function() inv   # Get the inverse
  list(set = set, get = get,      # Encapsulate into a list
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # Get the current state of the inverse and see if it
  if(!is.null(inv)) {   # If it has
    message("Getting cached matrix")  # Simply return the computed inverse  
    return(inv)
  }
  data <- x$get()   # If it hasn't then get the matrix itself
  inv <- solve(data, ...)   # Find the inverse
  x$setinverse(inv)  # Cache this result in the object
  inv # Return this new result
}
