## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
    #Same struct as cache vector
    #Create empty Cache
    inv <- NULL
    #Invalidate Cache on mutation
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #Create getter
    get <- function() x
    #setter for cache
    setinverse <- function(inverse) inv <<- inverse
    #getter for cache
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#more arities wouldn't make sense
cacheSolve <- function(x = makeCacheMatrix()) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    # Invert the matrix
    inv <- solve(data)
    #Cache the result
    x$setinverse(inv)
    inv
}

#USAGE 
#cacheSolve(makeCacheMatrix(rbind(c(1, -1), c(1, 1))))
#     [,1] [,2]
#[1,]  0.5  0.5
#[2,] -0.5  0.5


