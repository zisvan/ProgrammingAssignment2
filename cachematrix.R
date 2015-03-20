## Put comments here that give an overall description of what your
## functions do

## A pair of functions compute the inverse of a matrix and store it 
## in cache for fast access.

## Write a short comment describing this function

## makeCacheMatrix() takes a matrix and returns a list of four 
## functions to set the matrix, get the matrix, set its inverse, 
## and get its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## set inverse to null by default
    inv <- NULL               
    ## first function of the returned list
    ## the 'set' function sets matrix to y, inverse to NULL
    set <- function(y){     
        x <<- y
        inv <<- NULL
    }
    ## second function of the returned list of functions
    get <- function() x ## get the matrix
  
    ## third function of the list, sets the inverse of the matrix
    setinv <- function(inverse) inv <<- inverse 
  
    ## fourth function; gets the inverse if it is set
    ## does not compute the inverse
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
  
}


## Write a short comment describing this function

## cacheSolve() takes the output of makeCacheMatrix() as an argument
## and returns the inverse, from cached data if it exists, or by computing
## from scratch. It computing from scratch, the function caches the invserse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## attempt to get inverse
    xinv <- x$getinv()
    ## if inverse exists in cache, return it
    if(!is.null(xinv)){
        message("getting cached data")
        return(xinv) 
    }
    
    message("no cached data, computing inverse")
    ## if inverse doesn't exist, get matrix
    data <- x$get()
    
    ## calculate inverse here
    xinv <- solve(data, ...)
    
    ## set inverse to xinv
    x$setinv(xinv)
    xinv ## return inverse
    
}
