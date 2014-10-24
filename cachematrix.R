## Caching the Inverse of a Matrix 
## two functions (first and second) are used to:
## create a special object that stores a matrix and cache's it's inverse 
## this is done to improve upon possible time conusming issues that could occur
## with non caching. No reason to compute it again, if it already was.  

## first function, makeCacheMatrix
## matrix which is really a list containing  a function:
## to get and set the value of the matrix; get and set the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { #set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x #get the value of the matrix
        setinverse <- function(inverse) inv <<- inverse #set value of the inverse
        getinverse <- function() inv #get the value of the inverse
        list(set=set, get=get, 
        setinverse=setinverse, getinverse=getinverse)

}


## second function, cacheSolve 
## returns a matrix that is the inverse of 'x' and also checks to see if 
## the inverse has already been computed.  if so, it gets the inverse from the 
## cache and skips the computation.  otherwise, it computes the inverse of 'x' 
## and sets the value of the inverse in the cache via the setinverse function. 


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
