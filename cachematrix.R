## Cache the time-consuming matrix inversion process rather than compute 
## it repeatedly 

## makeCacheMatrix creates a special "Matrix", which is a list including
## 1. set the elements of the matrix
## 2. get the elements of the matrix
## 3. set the elements of the inverse matrix
## 4. get the elements of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
    }
    get <- function() x
    setinverse <- function(solve) m<<- solve
    getinverse <- function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## cachesolve will return the cache if the matrix inversion is already
## been calculated; if not, it will get the matrix data to calculate

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}