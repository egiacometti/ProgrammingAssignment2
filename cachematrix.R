

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


cacheSolve <- function(x, ...) {
m <- x$getinverse()
##return to the matrix 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##return the inverse_already set
  data <- x$get()  
  ##to get the matrix
  m <- solve(data, ...)  
  ##calculate the inverse of the matrix
  x$setinverse(m)
  ##to set the inverse of the matrix
    m 
	## Return a matrix that is the inverse of 'x'
       
}

test <- function() 
{
  my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
  my_matrix$get()
  my_matrix$getinverse()
  cacheSolve(my_matrix)
  my_matrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
  cacheSolve(my_matrix)   # Computes, caches, and returns new matrix inverse
  my_matrix$get()         # Returns matrix
  my_matrix$getinverse()  # Returns matrix inverse    
  my_matrix$get() %*% my_matrix$getinverse() # returns the identity matrix
}