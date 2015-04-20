## Put comments here that give an overall description of what your
## functions do

## The functions are intended to cache the inverse of a matrix to make 
## computation quicker

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
            x <<- y
            inver <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inver <<- inverse
        getinv <- function() inver
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## Write a short comment describing this function

## The cacheSolve function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinv()
    if(!is.null(inver)){
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinv(inver)
    inver        
}
