
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function makeCacheMatrix defines a list containing 4 'methods' :
##
##      a$set(), a$get(), a$setmatrix(), a$getmatrix()
##
##      set : store matrix, set indicator to 'execute function'
##      get : returns the stored matrix 
##      setmatrix : set cache inverted matrix (filled -> fetch result from cache)
##      getmatrix : get cache inverted matrix (NULL -> function needs to be executed, filled -> fetch result from cache)
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m<<- solve
        getmatrix <- function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## function cacheSolve, following steps :
##
##      get 'cached inverted matrix' :
##
##              filled -> fetched from cache, return inverted matrix
##
##              NULL -> execute invert matrix function (solve)
##              inverted matrix -> cache
##              return inverted matrix
##
##      calculation of execution time to show difference in calculating, fetching from cache
##      executed with a 2000*2000 matrix (difference 6.27 sec <-> 0 sec)
##      put in comment as only useful for testing purposes
##

cacheSolve <- function(x=matrix(), ...) {
        
        ##        ptm <- proc.time()
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                ##                print (proc.time() - ptm)
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        ##        print (proc.time() - ptm)
        m
        
}