## This function firstly sets the value of the matrix and 
##then gets the value of the matrix and then sets the value 
##of the inverse followed by setting the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a<<- NULL
}
get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## here it checks for the inverse of the matrix as calculated in above and if
## the inverse has been calculated, the inverse should be retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   a <- x$getinverse()
        if (!is.null(a)) {
                message("getting cached data")
                        return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
