## Function to perform matrix cache inverse
## There are two functions makeCacheMatrix() which creates the cache matrix and
## cacheSolve() function which calculate matrix invers using the solve() command.


## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvrse <- function(inverse) m <<- inverse
  getinvrse <- function() m
  list(set = set, get = get,
       setinvrse = setinvrse,
       getinvrse = getinvrse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinvrse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvrse(m)
  m
}