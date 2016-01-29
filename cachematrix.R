## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<- function(x = matrix()) {
## This function create a special "matrix" that is a list,
## you could set, get the special "matrix" using the
## x$setmatrix(matrix) and x$getmatrix(), also you could setinverse and getinverse
## with x$setinverse(inverse) and x$getinverse().    
        
    m <- NULL
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) { 
## This function computes de inverse of a special "matrix" returned by
## the function makeCacheMatrix above. If the inverse already been calculated
## then the cacheSolve should retrive de inverse from the cache.

     
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
