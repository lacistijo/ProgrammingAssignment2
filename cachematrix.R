## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a special matrix object,
## which has a built in variable to store it's inverse and setter/getter 
## methods to enable modifying/querying the value of the matrix and it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL            ##We set the initial value of the calculated inverse to Null
  set <- function(y) { ##We define the set method to modify the matrix
    x <<- y
    s <<- NULL
  }
  get <- function() x  ##We define the get method to query the matrix
  setInverse <- function(solve) s <<- solve ##The setInverse method calculates the inverse
  getInverse <- function() s ##The getInverse method returns the inverse
  list(set = set, get = get, ##We make sure that the methods are available as attributes of the matrix object
     setInverse = setInverse,
      getInverse = getInverse)
}


## Write a short comment describing this function
## The cacheSolve function calculates the inverse of a matrix
## and caches the result for later availability.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse() 
  if(!is.null(s)) { ##If the result is not null
    message("getting cached data") ##We print out a message
    return(s) ## and we return the cached inverse
  }
  data <- x$get() ##If the result was null we query the matrix
  s <- solve(data, ...)  ##Calculate the inverse
  x$setInverse(s) ##Store the inverse in the cache
  s ##And return the calculated inverse
}
