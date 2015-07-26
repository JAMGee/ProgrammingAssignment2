## Two functions: 
## 1) cacheSolve will either use a pre-existing cached inverse 
## of a matrix if it is available or will calculate the inverse itself. If 
## it calculates the inverse, it will make available the inverse for caching.  
## 2) makeCacheMatrix will use calculated matrix inverse if available and 
## cache it.

## makeCacheMatrix is a function that creates a matrix. 
## The function sets a value for the matrix, gets the value
## for the matrix, sets the value of the matrix's inverse
## and then gets the inverse.


makeCacheMatrix <- function(x = matrix()) {

  
  ## Creates a cache "m". 
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  
  get <- function() x ## gets the matrix
  setInverse <- function(inverse) m <<- inverse ## makes m the inverse of the matrix
  getInverse <- function() m ## gets the inverse of the matrix
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse) 

  
  
}


## The function cacheSolve uses the function "solve" to return the
## inverse of a matrix. If there is an available solution already
## cached, it will return that and note that it is doing so. Otherwise,
## it will calculate the inverse and make it available for caching.

cacheSolve <- function(x, ...) {
     

  
  
  
  
  m <- x$getInverse()
  if(!is.null(m)) { ## if the cache "m" is not null
    message("getting cached data") ## note that it is getting cached data
    return(m) ## return that cached data
  }
  data <- x$get() ## otherwise take the matrix "x"
  m <- solve(data, ...) ## use the solve function to calculate the inverse of matrix "x"
  x$setInverse(m) ## make the inverse of "x" available as m for caching.
  m
  
  
  
  
  }
