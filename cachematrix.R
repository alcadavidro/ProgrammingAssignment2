## This pair of functions store and calculates the inverse of a square matrix, taking advantage of
## the lexical scoping in R language
## The first function uses 4 internal functions and 2 objects. Basically it stores a matrix, that will
## be used later in the second function, and stores it's results. So in the future, if you have to
## recalculate the inverse of the squared matrix, it has been already stored

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  #Creation of the object inv for forward calculation
    set<- function(y){ #setter function for the matrix introduced as an argument and the object inv for the parent enviroment
        x <<- y
        inv <<- NULL
    }
    get <- function(){  #getter function for the matrix
        x
    }
    setinv <- function(inverse){  #setter function for the inv object that will be the inverse of the matrix
        inv <<- inverse
    }
    getinv <- function(){  #getter function for the inverse of the matrix
        inv
    }
    list(set = set, setinv = setinv, get = get, getinv = getinv) #list of all the function within the CacheMatrix function
}

## This functions calls the functions stored in the first one, and calculates the inverse of a 
## square matrix, if it hasn't been calculated and stores the results. If it has done it, it simply 
## returns it from the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinv()  #getting the inv value stored in the cachematrix
    if(!is.null(inv)){ #evaluating if it already has been calculated
        message('the inverse has already been calculated, retriving from the cache')
        return(inv)
    }
    matrix <- x$get() #If not, get the matrix given in the CacheMatrix
    inv <- solve(matrix, ...)  #Solve the inverse of the cuadratic matrix
    x$setinv(inv) #assigning the inverse in the cache
    inv  #printing the inverse
}
