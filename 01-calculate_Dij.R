library(RcppArmadillo)
library(Rcpp)
library(Matrix)
library(data.table)

# provide edgelist with supplier index, customer index, and weight (here 1/kin).
edges <- fread("./edgelist_toy.csv")

# provide nodelist with weights (here degree k), industry, and country information
nodes <- fread("./nodelist_toy.csv")

Nfirms <- nodes[,.N]

# Create impact matrix
Wij <- Matrix::sparseMatrix(
  # id of sender firm
  i = edges[,SuppIdx],
  
  # id of receiver firm
  j = edges[,CustIdx],
  
  # weight of goods and services sent
  x = edges[,weight],
  
  # number of firms
  dims = rep(Nfirms, 2)
)

# source cascade code
sourceCpp('DebtRank_cascade.cpp')

# helper function for running
runFromTo <- function(i0, i1){
  run_name <- paste0("Dij_",i0,"_to_",i1)
  print(paste(Sys.time(), run_name))
  indexlist <- seq(i0, i1)-1
  myres <- lapply(indexlist, function(x){Dij(x,Wij, nodes[,k])})
  myres <- Matrix(do.call(cbind, myres))
  saveRDS(myres, paste0(run_name, ".RDS"))
}

# start calculation
runFromTo(1, Nfirms)
