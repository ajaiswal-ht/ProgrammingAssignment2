

## It will create a special matrix object that chaches its inverse in cache
## makeCacheMatrix returns the list of functions on a matrix 'x' that will cache the inverse

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(y){
    # If set is called then invalidate the cache and reset data 'x'
    x <<- y
    inverse <<- NULL
}
get <- function() x
set_inverse <- function(inv) inverse <<- inv
get_inverse <- function() inverse
list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This function will return cached copy of inverse of matrix x if x is not changed and cached copy is available. 
#Else it will create cache, save it in cache and return the invese 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#Try to get inverse from cacheMatrix
inverse <- x$get_inverse()
#If it is not null then return it
if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
#Else get the data and cacluate inverse of matrix using solve
        data <- x$get()
        inverse <- solve(data)
# Now set this value as inverse in special matrix
        x$set_inverse(inverse)
        inverse
}
