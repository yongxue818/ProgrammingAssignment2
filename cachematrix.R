## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #set the inverse equal to NULL
        inverse <- NULL        
        set <- function(y) {
                # assign y to x
                x <<- y 
                #set the inverse equal to NULL
                inverse <- NULL  
        }
        # define the get function to return the matirx x 
        get <- function() x   
        # define the setInverse function to assign solve to inverse
        setInverse <- function (solve) inverse <<- solve
        # define the getInverse function to return the inverse
        getInverse <- function() inverse 
        # creates a list of the functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # assign the most recent value to the inverse
        inverse <- x$getInverse()    
        #if the most recent value assigned to inverse is not NULL, return the inverse and stop the function
        if(!is.null(inverse)) {
                message("getting cached data")
                return (inverse) 
        }
        # if the most recent value assigned to inverse is NULL, then retrive the matrix x
        data <- x$get()
        # use solve function to calculate the inverse of matrix x
        inverse <- solve(data, ...) 
        # Sets inverse to the newly value   
        x$setInverse(inverse)
        # return the new value
        inverse
}
