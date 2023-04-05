library(data.table)
library(Matrix)

# prep data
edges <- fread("./edgelist_toy.csv")
nodes <- fread("./nodelist_toy.csv")

# Prepare node x country matrix with degrees as weights for aggregation
kmat <- Matrix::sparseMatrix(i = nodes[,idx], j=nodes[,country], x=nodes[,k])
kcountry <- nodes[,.(ktot = sum(k)), by=country][,ktot]

############################
### Aggregate Dij to Eid ###
############################

Dij_to_Eid <- function(hd_mat, kmat, kcountry, nodes){
  x <- t(hd_mat)%*%kmat
  Eid_n <- t(t(x)/kcountry)
  return(Eid_n)
}

run_name <- 'Dij_1_to_9.RDS'
results <- readRDS(run_name)

Eid <- Dij_to_Eid(results, kmat, kcountry)

# save Eid here
saveRDS(Eid, "Eid_toyExample.RDS")


##############################
### Calculate Ecd from Eid ###
##############################

# transform Eid to data.table
Eid<-readRDS('Eid_toyExample.RDS')

# define helper matrix & vector
country_lookup <- nodes[,.(country.k=sum(k), Nnodes = .N), by=country]
country_mat <- (kmat>0)*1

Ecd <- t(country_mat) %*% Eid / country_lookup[,Nnodes]

Ecd <- data.frame(as.matrix(Ecd))
setDT(Ecd)
colnames(Ecd) <- as.character(c('A','B','C'))
Ecd[, idx := c('A','B','C')]

fwrite(Ecd, "Ecd_toyExample.csv")
