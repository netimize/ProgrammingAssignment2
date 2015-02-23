## Creates a cache that contains the source and
## inverse matrix
makeCacheMatrix <- function(x = matrix()) {
   
    ## created a null matrix
    data <- NULL
    
    ## sets the matrix
    set <- function(y) {
        x <<- y
        data <<- NULL
    }
    
    ## gets the matrix
    get <- function() x
    
    ## calculatates an inverse matrix
    setInverse <- function(solve) data <<- solve
    
    ## returns the inverse matrix
    getInverse <- function() data
    
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
       
}


## Checks the cache for the inverse matrix
## and returns the cached value if it exists,
## else is calculates teh inverse matrix
## and stores it in the cache.
cacheSolve <- function(x, ...) {

   ## retrieves the cached matrix
   ## returns null if not cached 
   invMatrix <- x$getInverse()

   ## returns the cached matrix
   ## if it i snull
   if(!is.null(invMatrix)) {
      message("getting cached data")
      return(invMatrix)
   }

   ## gets the matrix
   data <- x$get()

   ## calculates an inverse matrix
   invMatrix <- solve(data)
   
   ## sets the inverse matrix
   x$setInverse(invMatrix)

   ## Return a matrix that is the inverse of 'x'
   return(invMatrix)
}
