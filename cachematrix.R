## Computing the inverse of a square matrix can be done with the solve function in R.
## This two functions allow to create a matrix object which is capable of caching the calculated inverse
## of the matrix - instead of computing it repeatedly
## Example usage:
## set.seed(42)
## m <- matrix(sample.int(100, size=16, replace=TRUE), nrow=4)
## my_matrix <- makeCacheMatrix(m)
## cacheSolve(my_matrix)                        ## calling it a second time uses cached data

## This function creates a special matrix object, containing functions to
##  * set the value of the matrix
##  * get the value of the matrix
##  * set the value of the inverse
##  * get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix object created with the above function
## It checks if the inverse has already been computed. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse and sets the corresponding
## value via the setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}