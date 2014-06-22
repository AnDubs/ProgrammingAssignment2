##function cashes entered matrix inversion allowing to retrieve the inversion from enclosing environment
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set=set, 
         get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix
        )
}
##function computes matrix inversion or
##retrieves the matrix inversion from cache if it has been previously computed
cacheSolve <- function(x = matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get() 
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
