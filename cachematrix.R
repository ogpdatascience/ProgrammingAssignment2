#######
##This function creates an object(list) that can cache the inverse of a matrix.
##Inside this list are four elements which are called: 
##1. set_value_m
##2. get_value_m
##3. set_inverse_m
##4. get_inverse_m
##In the function there are small description of what eachh of them do.
#######
 #Should be notice that these functions
 #do not check if supplied matrix is invertible
 #as mentioned in the assigment descriptions
#######
makeCacheMatrix <- function(x = matrix()) {
        ######
        #Inverse I
        I <- NULL
        #set matrix
        set_value_m <- function(Y){
                   x <<- Y
                   I <<- NULL
        }
        #get_value_m, get the value of the matrix
        get_value_m <- function() x
        #set_inverse_m, set the inverse of matrix
        set_inverse_m <- function(solve) I <<- solve
        #get_inverse_m, get the inverse of the matrix
        get_inverse_m <- function() I
        list(set_value_m = set_value_m, 
             get_value_m = get_value_m,
             set_inverse_m = set_inverse_m,
             get_inverse_m = get_inverse_m)
}

#####
##This function computes the inverse of the special matrix returned 
##by `makeCacheMatrix` above. In the case where the inverse have been calculated
##(and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache
#####
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #get the inverse from the cache
        I <- x$get_inverse_m()
        if(!is.null(I)){
               message("getting cached data")
               return(I)
        }
        #if inverse is not in cache calculate it
        data <- x$get_value_m()
        I <- solve(data, ...)
        x$set_inverse_m(I)
        I
}

