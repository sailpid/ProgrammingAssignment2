## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        x <- NULL
        inv <- NULL
        # define a function called set which takes an 
        # argument/input (y) and sets the value of x to y
        set <- function(y) {     
                x <<- y
                inv <<- NULL
        }
        # get returns the value of the  original matrix (x)
        get <- function() {
                return(x)
        }
        # set the value of the object "makeCacheMatrix" to inverse matrix
        setInverseMatrix <- function(InverseMatrix) {
                inv <<- InverseMatrix  
        } 
        # get the value of the inverse matrix
        getInverseMatrix <- function() {
                return(inv)     
        }
        # list of functions for MakeCacheMatrix object        
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverseMatrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverseMatrix(inv)
        return(inv)
}
