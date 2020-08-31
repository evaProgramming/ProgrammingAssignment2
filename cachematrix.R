## makeCacheMatrix and cacheSolve find the inverse of an invertible square (2x2) matrix. 

## makeCacheMatrix sets x, gets x and caches the matrix and the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        setM <- function(b){
                x <<- b
                a <<- NULL
        }
        getM <- function() x
        setmatrix <- function(solve) a<<- solve
        getmatrix <- function() a
        list(setM = setM, getM = getM,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
## cacheSolve finds out if the inverse of the matrix is cached, and if so, prints the inverse.
## If not, it finds the inverse of the matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getmatrix()
        if(!is.null(a)){
                message("getting cached data")
                return(a)
        }
        dataM <- x$getM()
        a <- solve(dataM, ...)
        x$setmatrix(a)
        a
}
