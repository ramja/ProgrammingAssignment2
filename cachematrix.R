## Tis set of functions enable the use of special object (cached matrix)
## that allows the user to cache the inverse of a matrix.
## The procedure is to prepare a matrix for caching with funcion "makeCacheMatrix"
## this funcion return a special object that can be used to get the cached inverse 
## of the matrix (Only the first time invoked will be calculated).

## This fuction receives an R matrix and returns a special object with
## methods for getting and setting values of the matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function receives a cache enabled matrix and returns its inverse.
## If the inverse is already calculated returns the cached value of the inverse
## otherwise calculates the inverse and stores it into cache for future calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
