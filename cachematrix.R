## This function is for Lexical Scoping peer graded assignment 2
##

## create a Lexical scoping function

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
            x <<- y
            inv <<- NULL
          }
          
          get <- function() x
          
          setinverse <- function(inverse) inv <<- inverse
          getinverse <- function() inv
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the makeCacheMatrix above.
## If the inverse has already been calculated and has not been changed,
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
