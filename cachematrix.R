## Programming Assignment 2: Lexical Scoping
## The functions create and cache a matrix object and thencreate and 
## cache the inverse of the matrix object if it doesn't yet exist. 

## makeCacheMatrix creates a list contining the functions to set a 
## matrix, get a matrix, set the inverse of the matrix, and get the 
## inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix created by 
## makeCacheMatrix if the inverse has already been created 
## and creates and returns the inverse if it hasn't yet been
## created. 

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}

# check

# mat <- matrix(rnorm(9), nrow = 3, ncol = 3)
# mat
# mat1 <- makeCacheMatrix(mat)
# mat1
# cacheSolve(mat1)
