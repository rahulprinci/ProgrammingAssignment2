## Put comments here that give an overall description of what your
## functions do
##There are two functions created
##1)first one,to get and set the value of matrix and its inverse
##2)second one,to calculte inverse of matrix if it doesnt exist in cache.Otherwise,directly fetch it from cache.

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "matrix", containing a function to

##1)set the value of the matrix
##2)get the value of the matrix
##3)set the value of the inverse
##4)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
                x <<- y
                i <<- NULL
        }
	get <- function() x
	setinv <- function(solve) i<<- solve
	getinv <- function() i
	list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

##The following function creates inverse of the 'matrix' from the above function.
##However,it first checks if the inverse has already been calculated.If its already there,
##then it fetches inverse from the cache and skips computation.Otherwise it calculates inverse
##and set the cache's value by "setinv" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i<- x$getinv()
	if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	data <- x$get()
	i <- solve(data,...)
	x$setinv(i)
	i
}
