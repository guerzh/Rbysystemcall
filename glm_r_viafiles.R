
my.glm.binomial.coefs <- function(X, Y){
  # X is a data frame of size n x m, with n cases and m 
  # dimensions per case
  # Y is a vector of 1's and 0's
  # The function returns a vector of coefficients a0, a1, ..., am
  
  
  # Set up the dataframe so that it's convenient to read in in R
  
  X.dims <- dim(X)[2]
  my.X <- X
  my.X.dims.names <- paste("X", 1:X.dims, sep = "")
  colnames(my.X) <- my.X.dims.names
  my.X[, "Y"] <- Y
  my.X.plussed.strings <- paste(my.X.dims.names, collapse = " + ")
  
  # Set up the formula
  
  formula <- paste0("Y ~ ", my.X.plussed.strings)
  
  # We are now ready to to dump the dataframe and write the R file
  
  file.prefix <- "a345763285764329"
  write.csv(my.X, sprintf("%s.csv", file.prefix), row.names = F)
  
  script <- 
    c(sprintf("X <- read.csv('%s.csv')", file.prefix),
              "fit <- glm(Y ~ ., family = binomial, data = X)",
              "coefs <- as.vector(coef(fit))",
       sprintf("write.csv(coefs, '%s_coefs.csv', row.names = F)", file.prefix))
  
  fileConn <- file(sprintf("%s.R", file.prefix))
  writeLines(script, fileConn)
  close(fileConn)
  
  system(sprintf("Rscript %s.R", file.prefix),  
         intern = F, ignore.stdout = T, ignore.stderr = T, wait = T)
  
  coefs <- read.csv(sprintf("%s_coefs.csv", file.prefix))$x
  names(coefs) <- c("(Intercept)", colnames(X))
  coefs
}

my.glm.binomial.pred <- function(newdata, my.coef){
  # Obtain the predictions for the dataframe newdata, with logistic
  # regression coefficients coef
  # The coefficients (except the intercept) and columns must be in the same order
  # newdata should contain just the X, not the outputs
  
  stopifnot(all(names(my.coef)[2:3] == colnames(X)))

  ones <- matrix(nrow = (dim(newdata)[1]), ncol = 1, data = 1)
  newdata.mat <- as.matrix(newdata)
  newdata.design.mat <- cbind(ones, newdata.mat)
  
  activation <- newdata.design.mat %*% my.coef
  as.vector(plogis(activation))
}


X <- cars[, 1:2]
Y <- rbinom(n = dim(X)[1], size = 1, prob = 0.8)

# get coefs using external call
my.coef <- my.glm.binomial.coefs(X, Y)
my.coef

# get coefs using GLM
my.df <- data.frame(speed = X$speed, dist = X$dist, Y = Y)
fit <- glm(Y ~ speed + dist, data = my.df, family = binomial)
my.coef <- coef(fit)
my.coef

my.glm.binomial.pred(X, my.coef)

predict(fit, newdata = X, type = "response")
