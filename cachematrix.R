## The first function contains all the information for building our matrix (using the parameters data, side1 and side2)
## In case a matrix is being passed as a parameter, then there is no need for using the parameters data, side1, and side2
## data is a vector of vectors in which each of the elements is a column of our desired matrix, side1 and side2 are simply parameters
##indicating the length and width of the matrix.
##The second function checks if the inverse of that amtrix has been calculated before, being that the case 
## displays a message indicating that the inverse has already been computed and will be obtained from the cache
##If the inverse has not been computed it builds the matrix using the parameters obtained from the function get inside
## the function makeCacheMatrix and solves the inverse of the matrix, after finishing it stores that inverse inside
## the information described at the begining of this comment.


## This function checks if in the parameters a matrix is already included, that way it decides which functions to list
##in order to store the information of our matrix

makeCacheMatrix <- function(side1,side2,data,matrix = NULL) {
  if (is.null(matrix)){
    inverse<-NULL
    set <- function(y,m){
      data<<-y
      side<<-m
      inverse<<- NULL
    }
    get <- function() {
      c(side1,side2,data)
    }
    setinverse<-function(solve)inverse<<-solve
    getinverse<-function()inverse
    list(set= set, get= get, setinverse=setinverse,getinverse=getinverse)
  }
  else {
    inverse<-NULL
    set <- function(y){
      matrix<<-y
      inverse<<- NULL
    }
    get <- function() {
      matrix
    }
    setinverse<-function(solve)inverse<<-solve
    getinverse<-function()inverse
    list(set= set, get= get, setinverse=setinverse,getinverse=getinverse)
    
  }
}  

##Checks if in the lists of our "matrix" the inverse has already been cached, if so it returns the cached matrix
##if that is not the case the function either builds the matrix and calculates its inverse or it only calculates
##its inverse depending on wheter the matrix is already built (the x parameter was used in the first function).

cacheSolve <- function(matrix, ...) {
  inverse<-matrix$getinverse()
  if(!is.null(inverse)) {
    message("That inverse has been already computed, getting cached data")
    return(inverse)
  }
  data2<-matrix$get()
  if (is.matrix(data2)){
    inverse<-solve(data2)
    matrix$setinverse(inverse)
    return(inverse)
  }
  else {  
  side1<-data2[1]
  side2<-data2[2]
  data2<-data2[3:length(data2)]
  data1<-matrix(data2,side1,side2)
  inverse<-solve(data1)
  matrix$setinverse(inverse)
  return(inverse)
  }
}
