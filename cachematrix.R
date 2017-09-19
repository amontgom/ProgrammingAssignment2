#Usage commands:
mInv <- makeCacheMatrix(matrixTest)
cacheSolve(mInv)


#Makes a random 10x10 matrix; included for convenience
matrixTest <- replicate(10, rnorm(10))


#This function, like the example, creates a list with the functions to:
#A) sets the matrix
#B) gets the matrix
#C) sets the inverse
#D) gets the inverse
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL #Clears memory for the inverse matrix
    set <- function(y) { #Function to set up the matrix in the global environment
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x #Function to get the matrix
    setInverse <- function(inverse) matrixInverse <<- inverse #Function to set up the inverse matrix in the global environment
    getInverse <- function() matrixInverse #Function to get the inverse matrix
    list(set = set, get = get,    #List of functions
         setInverse = setInverse,
         getInverse = getInverse)
}


#Solves the matrix to get its inverse, unless it's already been done
#in which case it just pulls it rom the cache
cacheSolve <- function(x, ...) {
    #Return a matrix that is the inverse of the input
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) { #Retrieve inverse from cache, if available
        message("getting cached data")
        return(matrixInverse)
    }
    data <- x$get() #If not, retrieves matrix . . .
    matrixInverse <- solve(data, ...) #. . . inverts it . . .
    x$setInverse(matrixInverse) #. . . sets it . . .
    matrixInverse #. . . and displays it.
}