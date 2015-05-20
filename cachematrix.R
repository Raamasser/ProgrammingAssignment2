## In this example we use the <<- operator which can be used to assign a value to an objet
## in an environment that is different from the current environment. 
## Below are two functions that are used to create a special object that stores a matrix
## and caches its inverse 

## The first function, makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the
## above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inversee from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in
## the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
