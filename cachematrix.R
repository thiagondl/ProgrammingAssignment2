## Put comments here that give an overall description of what your
## functions do

## This function creates an empty matrix with functions that can set/get its values and cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invi <- NULL
  set <- function(y) {
    x <<- y
    invi <<- NULL
  }
  get <- function() x
  setinvi <- function(i) invi <<- i
  getinvi <- function() invi
  list(set = set, get = get,
       setinvi = setinvi,
       getinvi = getinvi)
}


## This function takes a matrix and return its inverse matrix

cacheSolve <- function(x, ...) {
  invi <- x$getinvi()
  if(!is.null(invi)) {
    return(invi)
  }
  data <- x$get()
  invi <- solve(data)
  x$setinvi(invi)
  invi
}
