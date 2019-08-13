## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This create a special matrix!

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <-function(matrix){
        m <<- matrix
        i <<- NULL
    }
    get <-function() m
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set=set, get=get, 
         setInverse=setInverse,
         getInverse=getInverse)
}


## Write a short comment describing this function
## This create special inverse of the matrix!

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached matrix")
        return(m)
    }
    data <-x$get()
    m <-solve(data) %*% data
    x$setInverse(m)
    m
## Return a matrix that is the inverse of 'x'
}
