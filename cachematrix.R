## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## 
## The cacheSolve function computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix` above. If the inverse has already been calculated
## (and the matrix has not changed), then the 'cachesolve` should retrieve the
## inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        
        inv = NULL
        set = function(y) {
                ## `<<-` assigns the object value to the 'cache' 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {

        inv = x$getinv()
        
        ## check to see if the inverse has already been calculated
        if (!is.null(inv)){
                ## get inverse from the cache, avoifing the computation. 
                return(inv)
        }
        
        ## if not already calculated, calculate the inverse
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        ## 'setinv()' sets the inverse value in the cache.
        x$setinv(inv)
        
        return(inv)
}
