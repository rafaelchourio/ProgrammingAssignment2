## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function has a matrix as its unique input parameter. This function, creates a 
## list with 4 functions to be applied in the function cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes the list created in the makeCachematrix function and 
## evaluates if the Inverse of Matrix was prevously computed, trough the value
## of inv. If inv is null, implies that inverse was already computed and get
## the computed value from cache. Else, computes the new value of Inverse of the
## matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                print(inv)
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        print(inv)
        
        ## Return a matrix that is the inverse of 'x'
}
