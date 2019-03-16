## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }  #set the value of the matrix
        get <- function() x #get the value of the matrix
        setinverse <- function(inverse) i <<- inverse  #set the value of the inverse
        getinverse <- function() i #get the value of the inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse) # stores in list
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i ## Return a matrix that is the inverse of 'x'
}