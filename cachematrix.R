# makeCacheMatrix is a function that returns a list of functions
# It stores a martix and a cached value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
        cache <- NULL
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                cache <<- NULL
        }
        # returns the stored matrix
        getMatrix <- function() {
                x
        }
        # cache the given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        # get the cached value
        getInverse <- function() {
                cache
        }
        # return a list
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# cacheSolve calculates the inverse of a matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()
        # if a cached value exists return cached data
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # if not, get the matrix, caclulate the inverse and store it in the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}        

