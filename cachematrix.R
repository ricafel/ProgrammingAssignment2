## These functions are due to the programming assignment 2: Lexical Scoping

## The 1st function creates a special matrix, setting and getting the value of the matrix.
## And setting and getting the inverse of that special matrix, as well.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { 
                x <<- y    
                i <<- NULL
        }
        get <- function() x  
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set = set,get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function calculates the inverse of the special "matrix" created above.
## If the inverse has already been calculated, a message appears: "getting cached data"
## And the result is retrieved from cache. When the input is new, the process of 
## getting and inverting starts again, caching the results!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Example of testing:       
## > makeCacheMatrix(matrix(1:4,2,2))$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > makeCacheMatrix(matrix(1:4,2,2))$getinverse()
## NULL
## > cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Or making it easier to test:
## Ricardo<-makeCacheMatrix(matrix(1:4,2,2))
## Ricardo$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## Ricardo$getinverse()
## NULL
## > cacheSolve(Ricardo)
##      [,1] [,2]
## [1,]   -2  1
## [2,]    1 -0.5

## Try cacheSolve again and check it out!
## > cacheSolve(Ricardo)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1
## [2,]    1 -0.
