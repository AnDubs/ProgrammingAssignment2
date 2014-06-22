##function cashes entered matrix inversion allowing to retrieve the inversion from enclosing environment
makeCacheMatrix <- function(x = matrix()) {
    ##initialize variable that will hold matrix value
    m <- NULL
    ##function caches variables x amd m allowing to retrieve them from enclosing environment
    set <- function(y){
        ##set values of variables used in enclosing environment
        x <<- y
        m <<- NULL
    }
    ##function returns cached x
    get <- function() x
    ##function cashes matrix inversion
    setmatrix <- function(solve) m <<- solve
    ##function retrieves previously cached matrix
    getmatrix <- function() m
    ##define makeCacheMatrix function as a list of functions
    list(set=set, 
         get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix
        )
}
##function computes matrix inversion or
##retrieves the matrix inversion from cache if it has been previously computed
cacheSolve <- function(x = matrix(), ...) {
    ##look up if newly entered matrix matched the one stored in cache
    m <- x$getmatrix()
    ##if matrix retrieved (its inversion has been computed)
    if(!is.null(m)){
        ##print message
        message("getting cached data")
        ##do not compute and return retrieved matrix inversion from cache
        return(m)
    }
    ##retrieve newly entered matrix
    matrix <- x$get() 
    ##inverse newly entered matrix
    m <- solve(matrix, ...)
    ##pass inversed matrix to cache
    x$setmatrix(m)
    ##output the results of inversion
    m
}
