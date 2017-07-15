## The functions can be used to cache the inverse of matrix

## This function creats a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y){
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs << inverse
        getinverse <- function() invrs
        list (set=set,get=get,
              setinverse=setinverse,
              getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs<-x$getinverse()
        if(!is.null(invrs)){
                message("getting cached data")
                return(invrs)
        }
        mat<-x$get()
        invrs<-solve(mat,...) 
        x$setinverse(invrs)
        invrs
}
