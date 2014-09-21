## MakeCacheMatrix creates a matrix with provided dimensions and data as input and returns a list of functions 
## that will
## set the matrix in Cache, get the matrix and also set the inverse and get the inverse of matrix.
## CacheSolve will get the value of inverse from Cache if available otherwise provide inverse and also set the value 
## in cache. These functions will provide fast turaround as values are being stored in Cache.
## Solve() - Inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinverse <- function(solve) inv<<-solve
        getinverse <- function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
