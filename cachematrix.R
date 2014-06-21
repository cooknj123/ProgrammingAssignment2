makeCacheMatrix <- function(x = matrix()) {
    m <- NULL           ##initialize
    set <- function(y) {            ##will set new matrix object
        x <<- y                     ##use arguments of $set to populate x
        m <<- NULL                  ##reset m for later calls 
    }
    get <- function() x             ##will return matrix object
    setinv <- function(inv) m <<- inv ##will set new inverse
    getinv <- function() m          ##will return cached inverse
    list(set = set, get = get,      ##define list of functions
         setinv = setinv,
         getinv = getinv)
    
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##Quoting From the Assignment:
        ##For this assignment, assume that the matrix supplied is always invertible.
        ##So
        ##There is no code to verify that the matrix is invertible 
        ##and none is required.
        m <- x$getinv()     ##check for cached data
        if(!is.null(m)) {
            message("getting cached data")
        return(m)           ##return cached data if it exists
        }
        data <- x$get()     ##otherwise, calculate and set new inverse
        m <- solve(data, ...)
        x$setinv(m)
        m
}
