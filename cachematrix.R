## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
##initialize
y <- NULL
##To set the matrix
  set <- function(matrix) {
    m <<- matrix
    y <<- NULL
  }
##To get the matrix
  get <- function() m
  
  ##to set the inverse matrix
  setinverse <- function(inverse) y <<- inverse
  ##to get the inverse of the matrix
  getinverse <- function() y
  ##list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
##return to the matrix 
m <- x$getinverse()
##return the inverse_already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##to get the matrix
  data <- x$get()
  ##calculate the inverse of the matrix
  m <- solve(data, ...)
  ##to set the inverse of the matrix
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
       
}
