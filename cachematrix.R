## Put comments here that give an overall description of what your
## functions do

##Two functions makeCacheMatrux, makeCacheMatrix
##makeCacheMatrix consists of get, set, getinv, setinv
##library(MASS) used to calculate inverse 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {
    inver<-ginv(x)
    inver%*%x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
##Used to get cache data

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

