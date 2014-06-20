# Dahiana Jimenez
# R-Programming
# Assignment 2: Caching the Inverse of a Matrix

# The following functions, makeCacheMatrix and cacheSolve, cache the inverse of 
# a given matrix. Instead of recalculating the inverse of a matrix you had 
# already gotten the inverse for, you can just access the result that has been 
# cached, thus making the script for efficient. Note, we are assuming that all 
# matrices used are invertible.


# The makeCacheMatrix function takes in an invertible matrix and creates a 
# matrix that can be cached. Hence, it creates a list of functions (set, get, 
# setinverse, and getinverse) that can be later used to access the inverse of 
# the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The cacheSolve function takes in the makeCacheMatrix function, inverts the 
# matrix it holds, caches that inverse and returns it. If the makeCacheMatrix 
# function is held within a variable, the first time cacheSolve is run with that 
# variable as its parameter, the inverse of the matrix will be calculated. The 
# second time cacheSolve is ran with that same variable, instead of recalculating 
# the inverse of the matrix, it will access the cached results and return it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


# EXAMPLE
# k <- makeCacheMatrix(matrix(1:4, 2,2))
# cacheSolve(k) #caches and returns the inverse of the matrix
# cacheSolve(k) #since cacheSolve() was already run once on variable k, it will 
# now return the message "getting cached data" along with the inverse of the 
# matrix, instead of recalculating the inverse