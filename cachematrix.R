## The makeCacheMatrix function caches a list of functions that:
##   1. set the value of a matrix (set)
##   2. get the value of a matrix (get)
##   3. set the value of the inverse of the matrix (setinverse)
##   4. get the value of the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function returns the inverse of the matrix, checking first 
## whether the result was already cached using the makeCacheMatrix function.
## If so, get the inverse from the cache and skip the computation
## If not, calculate the inverse and set the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m        ## Return a matrix that is the inverse of 'x'
        
}