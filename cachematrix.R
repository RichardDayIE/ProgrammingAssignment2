## Put comments here that give an overall description of what your
## functions do
## Function for assignment in week 3 of R Programming
## First function created a matrix while the second inverts this matrix or retrieves
## the inverse as cached

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() s
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached inverse")
                return(s)
        }
        useMatrix <- x$get()
        s <- solve(useMatrix)
        x$setInverse(s)
        s
}
