## Put comments here that give an overall description of what your
## functions do
## These functions will be used to understand "caching" of values and the <<- operator
## 

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse
## it will create a list of functions to be used to set and get the cached value


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache
## (assuming that the matrix supplied is always invertible). 

cacheSolve <- function(x, ...) {
  m <- x$getsolve() ## try to get cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() # assign parameter to data
  m <- solve(data, ...)
  x$setsolve(m) # set inverse to cache
  
  m         ## Return a matrix that is the inverse of 'x'
}
