## 4 functions have been defined in makeCacheMatrix
## set - this function is for setting the value of the matrix
## getMat - this function is for getting the value of the matrix
## setInv - this is for setting the inverse of the matrix
## getInv - this is for getting the inverse of the matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    getMat <- function() x
    setInv <- function(inv) inv <<- inv
    getInv <- function() inv
    list(set = set, getMat = getMat,
         setInv = setInv,
         getInv = getInv)
}


## this function is used for getting the inverse of the matrix. It first checks whether the ## inverse has been determined, if yes, then it takes from cache, else it determines the ## inverse of the matrix

cacheSolve <- function(x, ...) {
    inv<-x$getInv()
## checking whether the inverse already exists, if yes, then return the inverse
    if(!is.null(inv)){
        message("getting cached data.")
        return(inv)
    }

## If inverse has not yet been determined, then determine the inverse using solve funtion

    data <- x$getMat()
    inv <- solve(data)
    x$setInv(inv)
    inv
}

## example
## define the matrix
> xxx<-matrix(1:4,2,2)
> xxx
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> xy<-makeCacheMatrix(xxx)

> xy$getMat()
     [,1] [,2]
[1,]    1    3
[2,]    2    4

## since the inverse has not yet been determined, NULL is returned
> xy$getInv()
NULL
> cacheSolve(xy)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> xy$getInv()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5