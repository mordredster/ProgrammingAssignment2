## Put comments here that give an overall description of what your
## functions do

## Caculating matrix inversion is time-cosuming operation. So we 
## use these two functions to cathe the inversion of a matrix. So
## if the context is not chaged, we can read the cached matrix
## inversion to avoid re-caculation.


## This function creates a "matrix", which is really functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the value of the matrix inversion
## 4. set the value of the matrix inversion
##

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinversion <- function(inversion) i <<- inversion
        getinversion <- function() i
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)

}


## The following function calculates the inversion of the special "matrix"
## created with the above function. However, it first checks to see if the
## inversion has already been calculated. If so, it `get`s the inversion from the
## cache and skips the computation. Otherwise, it calculates the inversion of
## the data and sets the value of the inversion in the cache via the `setinversion`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinversion()
        if(!is.null(i)){
                message("get catched data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinversion(i)
        i
        
}
