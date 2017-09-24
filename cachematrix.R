# These functions create a matrix, calculate the inverse matrix and cache it. So, if the 
# same matrix is created, its inverse matrix can be retrieved from cache instead of getting
# recalculated

# This function creates a vector - a list containing functions to
# Set and Get value of the matrix
# Set and Get Inv Matrix

makeCacheMatrix <- function(x = matrix()){
    im <- matrix()
    set <- function(y) {
        x <<- y
        im <<- matrix()
    }
    get <- function() x
    setInvMatrix <- function(InvMatrix) im <<- InvMatrix
    getInvMatrix <- function() im
    list(get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}

# This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x,...){
    im <- x$getInvMatrix()
    if (!is.na(im)) {
        message("getting cache data")
        return(im)
    }
    data <- x$get()
    im <- solve(data,...)
    x$setInvMatrix(im)
    im
}