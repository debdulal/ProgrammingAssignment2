## "makeCacheMatrix" function creates special matrix 
## of functions that can be used in "cacheSolve"
## to calculate inverse and cache it for future use 



## This function creates a "matrix",
## a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) invM <<- solve
  getsolve <- function() invM
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This following function calculates the inverse 
## of the special "matrix"created with "makeCacheMatrix".
## It inverse has already been calculated,it gets 
## it from the cache else it calculates ans stores.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getsolve()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve (data)
  x$setsolve(invM)
  invM
}
