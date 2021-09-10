## Put comments here that give an overall description of what your
## functions do


## create special "matrix", which contains a list of functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv_matrix <<- inverse
        get_inverse <- function() inv_matrix
        list(set = set, 
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## calculate the inverse of matrix, by checking if inverse of matrix 
## already exists in cache. If so, return value from cache. Otherwise 
## calculate inverse and save it in cache with the function set_inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inverse()
        ##check if inverse in cache
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        ##get matrix
        data <- x$get()
        ## compute the inverse of a matrix alternative: ginv() but solve is faster
        inv_matrix <- solve(data, ...)
        #save inverse in cache
        x$set_inverse(inv_matrix)
        #return inverse matrix
        inv_matrix
}
