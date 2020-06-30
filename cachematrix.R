## The makeCacheMatrix function creates a special matrix that can be cached
## The cacheSolve function inverses a matrix and caches it, if it is not already found in
## the cache

## Takes in a matrix to inverse and creates a list to cache said matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}
# 
# 
# ## Finds the inverse of a matrix and caches it if necessary
# 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null((inv))){
    message("Getting the cached Inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  
}

