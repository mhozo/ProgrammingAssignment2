## These functions compute the inverse of a square matrix and cache
## the result, or call upon the cache if it already exists.

##This first function creates a special "matrix" object that can cache 
##its inverse through the below four steps.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){ #1. set the value of m
    x <<- y
    m <<- NULL
  }
  get <- function() x #2. get the value of m
  setinverse <- function(inverse) m <<- inverse #3. set the value of inverse
  getinverse <- function() m #4. get the value of the matrix inverse
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


  #This function computes the inverse of the special "matrix" returned
  #by makeCacheMatrix above. If the inverse has already been calculated
  #(and the matrix has not changed), then the cachesolve should 
  #retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
  if(!is.null(m)){ #if already computed, then return that value
    message("getting cached data")
    return(m)
  } #otherwise, compute the inverse
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}


# Test cases:
# m0 = matrix(34:37, 2, 2) ## sample matrix
# m1 = makeCacheMatrix(m0)
# m1$get()
#cacheSolve(m1) ##finds the inverse
#cacheSolve(m1) ##run again to see if retrieved from the cache
#cacheSolve(m1) %*% m1$get() ##should result in the identity matrix (with rounding errors)