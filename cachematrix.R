# Cached Matrix inversion aimed to reduce computation costs
# makeCacheMatrix creates a list of functions to
# 1. set value of the matrix
# 2. get value of the matrix
# 3. set value of inverse of the matrix
# 4. get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL
    set <- function(y) 
    {
        x <<- y
        inversa <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversa <<- inverse
    getinverse <- function() inversa
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve returns the inverse of the matrix. 
# if it was already solved, returns the cached value
# otherwise calculate the inverse, keep the value and returns it

cacheSolve <- function(x, ...) {
    inversa <- x$getinverse()
    if(!is.null(inversa)) 
    {
        print("cached value.")
        return(inversa)
    }
    matriz <- x$get()
    inversa <- solve(matriz)
    x$setinverse(inversa)
    inversa
}
