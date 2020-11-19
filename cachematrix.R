## CACHING THE INVERSE OF A MATRIX
## Functions which allow a user to cache the inverse of a matrix
## to avoid repeated calculations

## Function 1 - Create a special 'matix' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}

## Function 2 - Compute the inverse of the special 'matrix' returned by
## the function above. If the inverse has been calculated and the matrix
## is unchanged, it will retrieve the cached inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("Getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
