## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes in an matrix as argument and returns a list that has getter, setters for values as is and the inverse matrix
## Here is a sample of how to use the function
#y<-matrix(c(4,2,7,6),nrow=2,ncol=2)
#k<-makeCacheMatrix(y)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrixreverse <- function(b) m <<- b
    getmatrixreverse <- function() m
    list(set = set, get = get,
         setmatrixreverse = setmatrixreverse,
         getmatrixreverse = getmatrixreverse)
  }


## Write a short comment describing this function
# This functions checks if there is a reverse matrix that is already exists in the cache. 
# If not computes and caches the reverse matrix. 
# Both the cached and non-cached cases prints out the reverse matrix. This function does not handle the error 
# and depends on the solve function to handle the error for matrix that cannot be reversed
#Example of how to call this function
#y<-matrix(c(4,2,7,6),nrow=2,ncol=2)
#k<-makeCacheMatrix(y)
#cacheSolve(k)... Note: first time the data will not be cached
#cacheSolve(k)... Note: second time the data will be cached


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixreverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mtx <- x$get()
  m <- solve(mtx)
  x$setmatrixreverse(m)
  m
}
