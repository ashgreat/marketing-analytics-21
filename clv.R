## Function 1 to get the customer distribution

clv_cust_dist <- function(tmat,cust0,numperiod=5, round=TRUE){
  cust <- matrix(NA,numperiod+1,nrow(as.matrix(tmat)))
  cust[1,] <- cust0
  for (i in 1:numperiod) cust[i+1,] <- cust[i,] %*% as.matrix(tmat)
  tcust <- t(cust)
  colnames(tcust) <- paste0("Period_",0:numperiod)
  if (!is.null(colnames(tmat))) rownames(tcust) <- colnames(tmat)
  if (round){return(round(tcust,0))} else {return(tcust)}
}

# Function 2 to get the distribution of discounted gross margin
# For every customer segment in each period

clv_dismargin <- function(tmat,cust0,margin,drate,numperiod=5){
  grmargin.cust <- diag(margin) %*% clv_cust_dist(tmat,cust0,numperiod,round = F)
  drate.vec <- matrix(NA,1,numperiod+1)
  drate <- 1+drate
  for (i in 0:numperiod) drate.vec[,i+1] <- drate^i
  mat1 <- grmargin.cust %*% diag(as.vector(1/drate.vec))
  colnames(mat1) <- paste0("Period_",0:numperiod)
  if (!is.null(colnames(tmat))) rownames(mat1) <- colnames(tmat)
  return(mat1)
}

# Function 3 to get CLV per customer in each segment and customer equity

clv <- function(tmat,cust0,margin,drate,numperiod=5){
  drate.vec <- matrix(NA,1,numperiod+1)
  drate <- 1+drate
  for (i in 0:numperiod) drate.vec[,i+1] <- drate^i
  clvmat <- matrix(NA,nrow(tmat),1)
  colnames(clvmat) <- "CLV/Customer"
  if (!is.null(colnames(tmat))) rownames(clvmat) <- colnames(tmat)
  for (i in 1:nrow(tmat)){
    cmat <- matrix(NA,nrow = 1+numperiod, ncol = nrow(tmat))
    cmat[1,] <- 0
    cmat[1,i] <- cust0[i]
    for (j in 1:numperiod) cmat[j+1,] <- cmat[j,] %*% as.matrix(tmat)
    clvmat[i] <- sum(diag(margin)%*%t(cmat)%*%diag(as.vector(1/drate.vec)))/cust0[i]
  }
  #cust.equity <- sum(clv_dismargin(tmat,cust0,margin,drate,numperiod), na.rm = T)
  cust.equity = sum((clvmat - margin) * cust0, na.rm = TRUE)
  return(list(CLV = clvmat - margin, Cust.Equity = cust.equity))
}
