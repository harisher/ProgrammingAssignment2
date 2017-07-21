## The functions below calculate the inverse of a matrix and cache the result 
## for faster retrieval 

## The "makeCacheMatrix" function returns a list of functions to 
## 1. set the value of matrix 
## 2. get the value of matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invmtrx <- NULL
        set <- function(y) {
                x <<- y
                invmtrx <<- NULL
        }
        get <- function() x
        setCachedMatrix <- function(inverseMatrix) invmtrx <<- inverseMatrix
        getCachedMatrix <- function() invmtrx
        list(set = set, get = get,
             setCachedMatrix = setCachedMatrix,
             getCachedMatrix = getCachedMatrix)
}


## The "cacheSolve" function returns the inverse of a matrix. 
## It returns a cached result, if present. 
## Else, it calculates the inverse of the matrix and returns the value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmtrx <- x$getCachedMatrix()
        if(!is.null(invmtrx)) {
                message("getting cached data")
                return(invmtrx)
        }
        data <- x$get()
        invmtrx <- solve(data, ...)
        x$setCachedMatrix(invmtrx)
        invmtrx
}
