## We have 2 functions
## The first will create a list containing 4 functions.
## These functions will get or set a matrix, or get or set the inverse of the matrix
## The second function allows you to calculate the inverse of a matrix created in the previous funtcion
## If the inverse was calculated before, it will get the inverse from the a 'cached' variable

## This function creates a list with 4 different functions:
## more info inside the function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        
        ##if set is called, this new matrix will be entered     
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##if get is called, the matrix will be printed
        get <- function() x
        
        ##the function to calculate the inverse
        setinverse <- function(solve) m <<- solve
        
        ##if getinverse is called, the inverse will be printed
        getinverse <- function() m
        
        ##store the 4 functions in a list 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function will create the inverse of the matrix set with the previous function
## more info inside the function

cacheSolve <- function(x, ...) {

        ##get the inverse of the matrix
        m <- x$getinverse()
        
        ##if it has been calculated before, don't redo the calculations, but just get the inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##if it has not been calculated, get the matrix, solve it and set the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        ##return the inverse
        m
}
