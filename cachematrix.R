
##creates an cacheable matrix and its sets and gets
makeCacheMatrix <- function( x = matrix()){
	m = NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

##solves the matrix, either by calculating it 
##or by getting its cache should it have a value
cacheSolve <- function(x,...){
 	m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
