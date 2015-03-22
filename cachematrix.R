## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize a local object mt and set it to NULL,
    # this object will be used for storing the cached matrix
    mt <- NULL
    
    set <- function(y) {
        # assign x with the value of y in the enviroment where the function is
        # defined
        x <<- y  
        # reset the value of mt (matrix) in the enviroment it was defind
        mt <<- NULL        
    }
    
    get <- function() x
    setinvmt <- function(solve) mt <<- solve
    getinvmt <- function() mt
    list(set = set, get = get,
         setinvmt = setinvmt,
         getinvmt = getinvmt)   
}


# cacheSolve calculates the inverse matrix of x if its already cached, it will 
# return the cached copy of the inverse matrix of x, else it will call solve
# function to solve inverse matrix of x and stores it as a cached copy into 
# makeCacheMatrix function internally. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mt <- x$getinvmt()
    ## The following statement check if a NULL is returned which means there is no
    # cached copy of Inverse matrix of x.
    if(!is.null(mt)) {        
        message("getting cached inv matrix")
        ## If mt is not NULL, the return value is the Cached copy of Inverse Matrix x
        ## Thus return the cached copy of Inverse x
        return(mt)
    }
    ## If there is no cached copy of Inverse Matrix X, get matrix x and stores in data
    data <- x$get()
    ## using solve function, solved the value of Inverse x and assigned the Inverse Matrix
    ## to variable mt
    mt <- solve(data,...)
    ## stores the calculated inverse x matrix into cache using 
    x$setinvmt(mt)
    mt
}
