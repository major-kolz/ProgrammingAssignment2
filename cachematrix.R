## Two function that speed up yor matrix-using-programm 
#! Do not use <cachedMatrix>$setInvers() without check for correctness
# tab size = 3

## Get matrix and form on it new "cacheable matrix"
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   
   get         <- function() x
   getInvers   <- function() inv
   setInvers   <- function(inverse) inv <<- inverse
   set         <- function(y) {
      x   <<- y
      inv <<- NULL
   }
   
   list(set = set, get = get, setInvers = setInvers, getInvers = getInvers)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
   inverse <- x$getInvers()
   
   if( !is.null(inverse) ) {
      message("> Get cached data")
      return(inverse)
   }
   
   data <- x$get()
   inverse <- solve(data)
   x$setInvers(inverse)
   inverse
}
