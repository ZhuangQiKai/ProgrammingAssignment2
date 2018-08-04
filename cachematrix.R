## The functions aim to cache a square matrix and its inverse, so that the 
## inverse only needs to be calculated for one time. 

## makeCacheMatrix returns a list of four functions. 
## When calling the list$set or list$get, the original matrix will be set or 
## got. When calling the list$setinverse or list$getinverse, the inverse matrix
## will be set or got. 
## makeCacheMatrix will NOT check whether the matrices are the inverse of each 
## other. The user needs to ensure that the xinv inputted into setinverse is 
## correct. 
makeCacheMatrix <- function(x = matrix()) {
    ## x is for caching the original matrix. m is for the inverse matrix. 
    m <- NULL
    
    ## The function "sets" the original matrix and cleared the previously cached
    ## inverse matrix. 
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    ## The get function returned the cached original matrix
    get <- function () x
    
    ## The setinverse function "sets" the inverse matrix. It will NOT calculate 
    ## xinv or check whether xinv is the inverse of x. xinv is calculated in  
    ## the calling environment. 
    setinverse <- function (xinv) m <<- xinv
    
    ## The getinverse function returned the cached inverse matrix
    getinverse <- function () m
    
    list (set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix cached in x. The cached  
## matrixed will have the priority to be returned. The inverse is calculated 
## through the solve funtion. 
## x should be the object returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
}
