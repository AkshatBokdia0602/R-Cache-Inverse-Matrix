#This R program helps to cache the inverse of a matrix 
# rather than computing it repeatedly.


# The following function (makeCacheMatrix) creates a special "matrix" object 
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


# The following function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) 
{
    inv <- x$get_inverse()
    cond <- is.null(inv)
    if(!cond) 
    {
        message("Getting cached data - Inverse of Matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)   #solve() helps compute the inverse of a square matrix.
    x$set_inverse(inv)
    inv
}
