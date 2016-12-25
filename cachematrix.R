## These two functions are written to cache the inverse of an invertible matrix

## Input an invertible matrix and establish the necessary functions used by 
## makecachematrix() 

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<- function(y){
        x<<- y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Solve the matrix and cache the inverse matrix in the function makecachematrix()

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv ## Return a matrix that is the inverse of 'x'
}
