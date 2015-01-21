## The two functions below, makeCacheMatrix() and CacheSolve(), 
## illustrate lexical scoping in R and how it can be used to give
## functions mutable state. Computing the inverse of a matrix can be a costly 
## exercise.The makeCacheMatrix() function is used to cache the 
## value of an invertible matrix  and its inverse in two global variables.  
## The CacheSolve() function checks whether the inverse of the matrix passed to 
## it already exists. If it does (and the matrix 
## has not changed), it simply returns the inverse. It computes the inverse 
## only if it doesn't already exists.  


## The makeCacheMatrix() function below caches the matrix and its inverse passed 
## to it in two global variables. Please note that makeCacheMatrix() does not 
## compute the inverse of the matrix, it simply stores and returns matrices 
## to and from global variabes.There are two sets of set and get 
## functions in this function. They are used to set and get values 
## of the matrix and its inverse from global variables. 
## We are assuming that the matrix x passed to the function is always
## invertible.

makeCacheMatrix <- function(x = matrix()) {
        inv.mat <- matrix()
        set <- function(y) {
                x <<- y
                inv.mat <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(invertedmatrix) inv.mat <<- invertedmatrix
        getinvmatrix <- function() inv.mat
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix
        )
        
}
## The cacheSolve() function below checks whether the inverse of the matrix x
## passed to it exists in a cached global variable. If it does (and the matrix 
## has not changed), it simply returns the inverse. If the inverse of matrix x 
## does not exist, it computes the inverse and returns the computed result.
## We are assuming that the matrix x passed to the function is always 
## invertible.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.mat <- x$getinvmatrix()
        if (!is.na(inv.mat[1,1])){
                message("getting cached matrix")
                return(inv.mat)
        }
        data <- x$get()
        inv.mat <- solve(data)
        x$setinvmatrix(inv.mat)
        inv.mat
}