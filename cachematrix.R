## Check first to see if the inverse of a matrix has already been calculated or not
## If so, get the inversed matrix from cache. If not create the inverse of the matrix 
## and cache it via the setmean function to save time next time.

## The first function, makeCacheMatrix creates a special "vector"
##, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This is the function that checks whether the inverse of matrix has already been created
## before or not

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}
