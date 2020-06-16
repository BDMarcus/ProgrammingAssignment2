## Overall Description of Functions: 
## By applying the <<- operator which can 
## be used to assign a value to an object in an environment that is different
## from the current environment. Below are two functions that are used to 
## create a special object that stores a matrix and cache the inverse of a matrix.

## Short Comment on Function:
## A two part function articulated below which contains:
## Firstly, 'makeCacheMatrix' function to create a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {         ## define the argument with default mode "matrix"
  k <- NULL                                         ## iniialize 'k' as NULL which will hold value of matrix inverse
  set <- function(y){                               ## deine the set function to assign new
    x <<- y                                         ## value of matrix in parent environment
    k <<- NULL                                      ## incase of a new matrix,reset 'k' to NULL
  }
## Above function creates a special "matrix" object that catche its inverse
  
  get <- function(){x}                               ## define the get function -returns value of matrix
  set_inverse <- function(inverse) {k <<- inverse}   ## assigns value of 'k' in parent environment
  get_inverse <- function() {k}                      ## gets the value of 'k' where called
  list(set = set, get = get,                         ## Referral function 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}

## finaly,'cacheSolve' function to compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {                      ## Return a matrix that is the inverse of 'x'
  k <- x$get_inverse()
  if(!is.null(k)){
    message("getting cached data")
    return(k)                                         ## returns the inverse of the data
  }
  m <- x$get()                                        ## calls the function of makeCacheMatrix
  k <- solve(m, ...)
  x$set_inverse(k)
  k                                                   ## prints the data by calling the function
}
