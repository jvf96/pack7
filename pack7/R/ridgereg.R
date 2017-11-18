#' @title Ridge Regression
#' @description The  function it should take a formula object as well as a dataset and return a ridgereg object.
#' @param formula formula
#' @param data data.frame
#' @param lambda numeric
#' @param x_norm matrix
#' @param beta_ridge numeric
#' @param y_hat numeric
#' @param data_name character
#' @return a ridgereg object.
#' 
#' @importFrom methods new
#' 
#' 
#' @export 
#' @exportClass ridgereg
#' 

#THIS LAB IS BASED TO THE LAB 4 --> linreg

#Create the function:
ridgereg <- setRefClass(Class = "ridgereg",
                      fields = list(formula = "formula",data = "data.frame", 
                                    lambda = "numeric", x_norm ="matrix",
                                    beta_ridge = "numeric", y_hat = "numeric",
                                    data_name = "character"),
                      methods = list(initialize = function(formula, data, lambda){
                        formula <<- formula
                        data <<- data
                        data_name <<- deparse(substitute(data))
                        lambda <<- lambda
                        
                        
                        #Normalize:
                           
                        x <- model.matrix(object = formula, data = data)
                        for (i in 2:ncol(x)){
                          x[,i] <- ((x[,i]-mean(x[,i]))/ (sqrt(var((x[,i])))))
                        }
                        x_norm <<- x

                            
                        #Using QR
                        QR <- qr(x)
                        R <- qr.R(QR)
                        
                        #Regressions coeﬃcients: (beta_ridge)
                        ident <- diag(lambda, nrow = ncol(x))
                        aux <- all.vars(expr = formula)[1]
                        y <- as.matrix(data[,aux])
                        beta_ridge_aux <- solve(t(R) %*% R + ident) %*% (t(x) %*% y)
                        beta_ridge <<- beta_ridge_aux[,1]
                        
                        #The ﬁtted values: (y_hat)
                        y_hat_aux <- x %*% beta_ridge
                        y_hat <<- y_hat_aux[,1]
                        
                      },
                          
                      # PRINT out the coefficients and coefficients names
                      print = function(){
                        cat("\n","Call:","\n",
                            paste("ridgereg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],", ", "data = ", data_name, ", lambda = ", lambda, ")",sep = "", collapse = "\n" ),
                            "\n","Coefficients:","\n")
                        beta_ridge

                      },
                      
                      #PRED should return the predicted values of y_hat   
                      predict = function(d=NULL){
                        
                        result <- y_hat
                        if(!(is.null(d))){
                          
                          d <- data.frame(Intercept = 1, d)
                          a1 <- as.matrix(d)
                          a2 <- matrix(beta_ridge, nrow = length(beta_ridge))
                          res <- (a1 %*% a2)
                          result <- res[,1]
                        }
                          return(result)
                        
                      },
                      
                      #COEF should return the coefficients as a named vector    
                      coef = function(){
                        
                        return(beta_ridge)
                            
                      }
                      ))

