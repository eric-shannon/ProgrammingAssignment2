## *****************************************************************************
## Function:
##   makeCacheMatrix(x)
##
## Parameters:
##   x - An inversible square matrix.
##
## Return value:
##   A list of functions that will get and set the matrices x and inv.
##
## Description:
##   This function sets up a data object tha allows for the storage and
##   retrieval of two matrices; x and inv.  The matrix x holds a square matrix
##   that is to be applied to the solve() function to obtain its inverse.  The
##   matrix inv holds the inverse of the matrix x.  This function also defines
##   four functions that allow for the setting and getting of the values of
##   these matrices.  Once these functions are defined, a list containg
##
##   The four functions are as follows:
##        set()     - This function sets the value of the matrix x and
##                       reinitialzes the value of the matrix inv.
##        get()     - This function returns the matrix x.
##        setinv()  - This function sets the value of the matrix inv.
##        getinv()  - This function returns the matrix inv.
## *****************************************************************************

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULLcacheSolve

     set <- function(y) {
          x <<- y
          inv <<- NULL
     }

     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv

     list(set = set,
          get = get,
          setinv = setinv,
          getinv = getinv)
}


## *****************************************************************************
## Function:
##   cacheSolve(x, ...)
##
## Parameters:
##   x    - A list of functions that will get and set the matrices x and inv
##             held in the environment of the makeCacheMatrix function.
##   ...  - Parameters to be passed on to the solve() function.
##
## Return value:
##   A matrix containing the inverse of the matrix held in the environment
##   of the makeCacheMatrix function.
##
## Description:
##   This function calculates the inverse of the matrix held in the environment
##   of the makeCacheMatrix function, stores it in that environment and returns
##   the inversed matrix.  If the inverse has already been calculated, the
##   inverse will be pulled from that environment and returned without
##   recalculating it.
## *****************************************************************************

cacheSolve <- function(x, ...) {
     ## Get the the matrix inverse stored in the environment x
     inv <- x$getinv()

     ## If the inv matrix is not NULL, the inverse has already been calculated.
     ## Return the inverse.
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }

     ## If the inv matrix is not NULL, get the matrix to be inversed from x,
     ## calaulate the inverse, set the inverse in the x environment and return
     ## the inversed matrix.
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)

     inv
}
