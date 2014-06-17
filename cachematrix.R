## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Set the matrix, get the matrix, set the inverse of the matrix, get the inverse of the matrix

makeCacheMatrix <- function(M = matrix()) {
        invM <- NULL
        set <- function(N) {
                M <<- N
                invM <<- NULL
        }
        get <- function() M
        setInv <- function(solve) invM <<- solve
        getInv <- function() invM
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(M, ...) {
        invM <- M$getInv()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- M$get()
        invM <- solve(data, ...)
        M$setInv(invM)
        invM
}