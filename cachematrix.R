## makeCacheMatrix creates a matrix that cache its inverse
## cachesolve either computes the inverse of a matrix or retrieve its value
## Uses function solve therefore X is assumed square invertible
## this function creates a special type of matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(m) inv <<- m
        getinv <- function() inv
        list(set=set,get=get,
             setinv=setinv,
             getinv=getinv)
}


## this function takes a square invertible matrix and compute its inverse 
## if it has not been computed before. If inv exists then the function retrieves
## its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
