## makeCacheMatrix creates a special "matrix" object which caches its inverse
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache

## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve computes the inverse of the special "matrix" 
## it first checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets it in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}


