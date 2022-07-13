## Set of functions that creates a class for a matrix that can cache its inverse
##

## Constructor function to create a matrix object capeable of caching its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix<-NULL
        set<-function(y){
                x<<-y
                inv_matrix<<-NULL
        }
        get<- function() x
        setInverse <- function(Temp_Inv_Matrix) inv_matrix <<-Temp_Inv_Matrix
        getInverse <- function() inv_matrix
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function to calculate the inverse and store it in the inv_matrix value in the
## makeCacheMatrix class

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getInverse()
        if(!is.null(inv)){
                message("data already cached")
                return(inv)
        }
        matrix<-x$get()
        inv_matrix<-solve(matrix)
        x$setInverse(inv_matrix)
        inv_matrix
}
