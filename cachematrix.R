## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix object that can cache its inverse
## This object can get and set the value of x
## Additionally, the object can also set and get the 
## value of the inverse of x
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<-y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## Returns the cached value of the inverse of 
## the special matrix x.
## If no inverse was cached, the function
## computes the inverse and caches it so that
## it is easily looked up next time.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
