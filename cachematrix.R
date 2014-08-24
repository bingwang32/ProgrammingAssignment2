# Bing W
# R Programming Assignment 2
# github: bingwang32

makeCacheMatrix <- function(x = matrix()) {

	  # makeCacheMatrix: function makes an object with two fields:
	  # 	x = an invertible matrix 
	  # 	i = the inverse of the matrix 
	  # 
	  # NOTE: function assumes that input x is an invertible matrix


        i <- NULL		# make field i for inverse, initialize to null

	  # setMat: sets the value of matrix x to y,
	  #         sets inverse field to NULL 
        setMat <- function(y) {
                x <<- y
                i <<- NULL
        }

	  # getMat: retrieves value of x (the matrix)
        getMat <- function() {
		    x
	  }

	  # setInv: sets the value of the inverse field i 
	  # NOTE: the inverse itself is calculated externally. 
	  #       this only sets the value.
        setInv <- function(inverse) {
		    i <<- inverse
	  }

	  # getInv: retrieve value of i (inverse)
        getInv <- function() {
		    i
	  }
	
	  # list of functions within cacheMatrix object, 
        # can be called for these values (ex. x$getMat outputs value of x)
        list(setMat = setMat, getMat = getMat,
             setInv = setInv,
             getInv = getInv)

}



cacheSolve <- function(x, ...) {

        # cacheSolve: function takes a matrix object x and outputs its inverse
        # NOTE: x is created by running makeCacheMatrix.R, which makes an object
        #       with field x for storing an invertible matrix, and field i for
        #       storing its inverse
	  # The purpose of the function is to retrieve a cached value of a 
        # matrix's inverse, or calculate, store, then output the matrix inverse.
	

	  # retrieve the inverse of matrix x
        i <- x$getInv()

	  # if the inverse field in x is not null,
        # print message that it is retrieving cached data,
	  # then return the cached inverse 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        # if the inverse field in x is null, get the value of matrix x,
	  # solve for the inverse, and set it as the value of x's inverse, i,
	  # then return i
        mat <- x$getMat()
        i <- solve(mat, ...)
        x$setInv(i)
        i

}
