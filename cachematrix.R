##Create two functions that can input a matrix, invert it, then cache the inverted matrix so one can call it later

##makeCacheMatrix stores the inputted matrix and the inverterd matrix in the global environment 

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        
}



##cacheSolve first checks to see if an inverted matrix is stored in makeCacheMatrix. If the inverted matrix is stored,
##the function returns it. If there is not an inverted matrix stored, it inverts the matrix and stores it.

cacheSolve <- function(x, ...){
        m <-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}