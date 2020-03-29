## pair of functions that cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
  
  #This function creates a special "matrix" object
  #that can cache its inverse.
  
  solved <- NULL
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) solved <<- solve
  getinverse <- function() solved
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  #If the inverse has already been calculated (and the matrix has not changed),
  #then the cachesolve should retrieve the inverse from the cache.
  
  
    solved <- x$getinverse()
  if(!is.null(solved)) {
    message("getting cached data")
    return(solved)
  }
  data <- x$get()
  solved <- solve(data, ...)
  x$setinverse(solved)
  solved
  ## Return a matrix that is the inverse of 'x'
  
}
