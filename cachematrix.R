## Coursera: Assignment2: Caching the Inverse of a Matrix
# The first function, makeVector creates a special "vector", 
# which is really a list containing a function to

# function to set the value of the matrix (set)
# function to get the value of the matrix (get)
# function to set the value of the inverse matrix (setInverseMatrix)
# function to get the value of the inverse matrix (getInverseMatrix)

# makeCacheMatrix: This function creates a special "matrix" 
#object, which has two attributes, the matrix (x) and its inverse (inv).

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

# Coursera: Assignment2: Caching the Inverse of a Matrix

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve 
# retrieves the inverse from the cache. Else, it calculates and returns 
# the inverse  

# cacheSolve is a function that calls the 'getInverseMatrix' function.
# It uses special Matrix object as an input argument (x, comprising of 
# matrix and its inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverseMatrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # The following line gets the matrix from special matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverseMatrix(inv)
        return(inv)
}
