## This functions allow firstly for the creation of a list that is able to contain a matrix 
## and its inverse in cache. Secondly it calculates and stores the inverse in the list
## so in case we need it again we can take it out of the cache memory instead of having to
## calculate it again.

## This function creates the list that will be able to contain both the matrix and its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function returns the value of the inverse of the input matrix. If it has already
## been calculated and stored in the cache memory it just obtains its value from there
## and otherwise it calculates it and also stores it in cache for later use.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
