## Cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  
  ## inverse
  mI <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    mI <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() {
    m
  }
  
  ## Method to set the inverse
  setInverse <- function(inverse) {
    mI <<- inverse
  }
  
  ## Method to get the inverse
  getInverse <- function() {
    mI
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Calculate the inverse of a matrix or return from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()
    
    ## Return the inverse if is already exists
    if( !is.null(m) ) {
      message("getting cached data")
      return(m)
    }
    
    ## Calculate inverse
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    
    ## Return inverse
    m
}
