## The following code calculates the inverse of a square nxn matrix using caching.
## In order for this to work nicely we request the user to create a cachematrix by using
## the makeCacheMatrix function. Once this object is created we can calculate it's inverse
## and the result is stored in the parent environment. It keeps track 
## of changes in the input data, such that when the input data is changed we need to calculate
## the inverse again.


## makeCacheMatrix, creates an object with the original matrix and a cache for the inverse of the data.
##
## It creates an object containing the original data and it's inverse once
## the program decides it needs to be evaluated (lazy evaluation: the
## inverse is only calculated when needed, not when the data is stored). 
## It works by storing introducing getters and setters for the matrix to keep track of any changes in the
## data. If new data is set the cache with the inverse is cleared. If the inverse is calculated
## We store the result in the cached_inverse variable. And if the inverse is calculated more often
## we can obtain the result from this variable.

makeCacheMatrix <- function(x = matrix()) {
        ## The variable to store the inverse
        cached_inverse <- NULL
        
        ## The setter for data: Set the data and empty the cached_inverse variable
        set <- function(y) {
                x <<- y
                cached_inverse <<- NULL
        }
        ## The getter for data: return the original data.
        get <- function() x
        ## The function to store the inverse in the cache. Doesn't calculate anything, just stores the result.
        setinverse <- function(solve) cached_inverse <<- solve
        ## The getter for the cached_inverse.
        getinverse <- function() cached_inverse
        ## Register the getters and setters.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Cache solve calculates the inverse of a matrix that was created by the makeCacheMatrix function.
##
## It checks if an inverse was already calculated and in that case it returns the previous result.
## Otherwise it calculates the inverse by using the default algorithm in r.
## In case of inverting very large matrices we may choose to look for another algorithm.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        ## Check if the inverse was already calculated and if yes return that value.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## No previous result was found, calculate it using the default algorithm.
        data <- x$get()
        ## Calculate the inverse.
        inverse <- solve(data, ...)
        ## Store the result in the cache.
        x$setinverse(inverse)
        ## Return the answer.
        inverse
}
