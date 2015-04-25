# The makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
# The cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cachesolve retrieves the inverse from the cache.
# These functions assume that the matrix is always invertible.

# The first function, makeCacheMatrix creates a list containing a function to:
# 1: set the value of the matrix
# 2: get the value of the matrix
# 3: set the value of the inverse
# 4: get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# calculates the inverse of the special "vector" created with the makeCacheMatrix function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it gets the inverse of the data and sets the inverse in the cache 
# via the setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

