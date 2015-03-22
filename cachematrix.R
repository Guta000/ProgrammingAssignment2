## There are two functions which create and work with cached matrix

## Function creates matrix with ability to work with cache during inverse calculaton

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setSolve <- function(inverse) cache <<- inverse
  getSolve <- function() cache
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Function calculates inverse for special matrix if it was not calculated before
## If it was calculated already it gets inverse from cache

cacheSolve <- function(x, ...) {
  cache <- x$getSolve()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setSolve(cache)
  cache
}