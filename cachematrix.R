##makecacheMatrix function  creates an R object that stores a matrix and its inverse 


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function()x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve takes an argument that is returned by makecahematrix in order to retrieve the inverse
##from the cached value that is stored in the makecachematrix object's environment.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <-x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
