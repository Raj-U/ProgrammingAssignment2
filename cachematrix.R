## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a special matrix that can cache its inverse, the following 
## functionality is incorporated in this function
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<-y
        inv<<-NULL
        get <- function() x
        setinverse <- function(inverse) inv<<-inverse
        getinverse<-function() inv
        
        list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
    }
}


## Write a short comment describing this function
## this function checks returns the inverse of matrix and check if the matrix is 
## already created by makeCacheMatrix and in case if the matrix is already created
## and has not been changed then the value of the matrix should be retrived 
## from the cache.
                
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    mat<-x$get()
    inv<-solve(mat,...)
    x$setinverse(inv)
    inv
}
