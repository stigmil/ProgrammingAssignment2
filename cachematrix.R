## This pair of functions cache the inverse of a matrix so that if the same
## matrix is passed to the function again, the functions will return the 
## cached inverse, so avoiding the need to make the calculation again.

## Function makeCacheMatrix caches a matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL  ## on first call, set flag i to NULL
     get <- function() x  ## pass original matrix
     setinverse <- function(inverse) i <<- inverse  ## cache the inverse
     getinverse <- function() i ## pass i (could be null or inverse)
     list (get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function cacheSolve returns the inverse of the matrix x by checking whether
## the inverse has already been calculated. If yes, it returns the cached
## inverse. If no, it calculates the inverse, caches it and returns the result.

cacheSolve <- function(x) {
        ## Retrieve i from makeCacheMatrix 
     i <- x$getinverse()
     ## if flag i is not null, inverse is already cached: return result from cache
     if (!is.null(i)) {
          message("Getting cached solution")
          return (i)
     }
     ## otherwise, retrieve the matrix to be solved and solve
     inv <- x$get()
     result <- solve(inv)
     ## cache the result and return it
     x$setinverse(result)
     result
}
