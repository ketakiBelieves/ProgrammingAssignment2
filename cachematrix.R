## The first function, makeCacheMatrix creates a special matrix, which is a list containing a function to:
## set the value of the matrix, get the value of the matrix
## set the value of the inverse, get the value of the inverse

## Second function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix will set & get value of matrix as per input, and set and get inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) { 
    i <<- inverse 
  }
  
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve will calculate the inverse of thematrix returned by makeCacheMatrix().
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  #if the inverse has already been calculated
  if (!is.null(inv)) {
    message("Alert : Getting cached data")
    return(inv)
  }
  
  #otherwise, calculates the inverse 
  data <- x$get()
  inv <- solve(data, ...)
  
  #sets the value of the inverse in the cache via the setinv function.
  x$setInverse(inv)
  return(inv)
}

