
## This function accepts a matrix as argument and provides functions to get and 
## set the matrix and its inverse with caching

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function grabs a makeCacheMatrix type variable and either retrieves the
## the cached matrix inverse or calculates it and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


#########
# Example
#########

# Define a 2 by 2 matrix and use the makeCacheMatrix function
mat = matrix(1:4,2,2)
x <- makeCacheMatrix(mat)
# No inverse calculated yet
x$getinv()
# Calculate inverse and cache
cacheSolve(x)
# Now cacheSolve retrieves the inverse from cache
cacheSolve(x)
# Also available through x$getinv
x$getinv()





