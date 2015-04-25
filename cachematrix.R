## Coursera R Programming
## Programming Assignment 2: Proficiency with scoping rules
## The code consists of two functions: one to set/get (from cache) the value of
## a matrix and its inverse; and one to return the inverse of a matrix, either 
## pulling from cache or calculating new.

## A. Mulliken 4/2015


## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse. 
## The output is a list of functions.
makeCacheMatrix <- function(x = matrix()) {
    Ix <- NULL
    # first list item/function sets value of matrix as x and makes inverse null
    set <- function(y) {
        x <<- y
        Ix <<- NULL
    }
    # second list item/function returns the matrix value
    get <- function() x
    # third list item/function sets the value of inverse
    setinverse <- function(inverse) Ix <<- inverse
    # four list item/function gets the value of inverse
    getinverse <- function() Ix
    # return the list
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. The output is the inverse of x.
cacheSolve <- function(x, ...) {
    Ix <- x$getinverse()  # attempt to pull a value from cache
    if(!is.null(Ix)) {
        message("found a value in cache")
        return(Ix)
    }
    
    # Continue on if there wasn't a value in the cache:
    matrix <- x$get()        # get your matrix
    Ix <- solve(matrix, ...) # calculate inverse
    x$setinverse(Ix)         # store in cache
    Ix                       # return inverse
}
