## Put comments here that give an overall description of what your
## functions do

## Function that can to cache the matrix 'x'and and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(value) {
    x <<- value
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(pInverse) inverse <<- pInverse
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function that evaluate if the matrix 'x' contais its inverve cached. 
## If so, return the cached value. Otherwise, compute the 'x' inverse 
## through the 'solve' R function, cache the result and return the value.
cacheSolve <- function(x, ...) {
  
  matInverse <- x$getInverse()
  
  if(!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }
  
  data <- x$get()
  
  value <- solve(data, ...)
  
  x$setInverse(value)
  
  value  
}
