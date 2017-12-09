# How to cache an inverted matrix

# First function is : makeCacheMatrix to create a list
# 1. set  value of the matrix 2. get  value of the matrix
#3. set  value of inverse of the matrix 4. get  value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Now to provide the inverse of the matrix. Check if computed. If yes, provide the
# results. If not, run the calculations.

# This assigment assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


# creating a 2 by 2 matrix to run the test.

x = matrix(c(600,700,800,900),2,2)
m = makeCacheMatrix(x)
m$get()

# Test one, not cached yet
cacheSolve(m)

# Test or second run
cacheSolve(m)

