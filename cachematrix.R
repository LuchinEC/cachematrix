####################################################################
#############   PROGRAMMING ASSINGMENT  - WEEK 3    ################
####################################################################


#The first function is aable to create an object (Matrix) that saves the cach?? of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {  
                x <<- y
                inv <- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse<- function()inv
        list(set=set, 
             get = get,
        setinverse = setinverse,
        getinverse = getinverse) 
}



## This function allows to find the Inverse of the matrix created with make cache matrix. Once the inverse
#has been calculated and as long as we keep working with the same matrix then we should get the inverse of that matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matriz<- x$get()
        inv <- solve(matriz, ...)
        x$setinverse(inv)
        inv
}



## Testing the functions I've created.

m1 <- makeCacheMatrix(matrix(9:12,2,2))
m1$get()
m1$getinverse()
cacheSolve(m1)

getwd()
