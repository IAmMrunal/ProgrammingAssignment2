##Caching the Inverse of a Matrix

## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                               # initialize object    
  set <- function(y) {                    
    x <<- y
    i <<- NULL
  }
  get <- function() x                     # function to return matrix
  setinv <- function(inv) i <<- inv       # function to set inverse of matrix
  getinv <- function() i                  # function to return inverse of matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)                   # return list of functions
}


## 'cacheSolve' solves and returns the inverse of a matrix unless the inverse is already
## in cache in which case it will return the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()                         # get the inverse from cache
  if(!is.null(i)) {                       # check if the inverse exists in cache
    message("getting cached data")
    return(i)                             # return inverse from cache
  }
  data <- x$get()                         # get the matrix from cache
  i <- solve(data, ...)                   # calculate the inverse of the matrix
  x$setinv(i)                             # store the inverse in cache
  i                                       # return calculated inverse
}
