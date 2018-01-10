bc_prcomp <- prcomp(select(bc_data,3:32),
                    center=TRUE,
                    scale.=TRUE)

bc_Rtsne <- Rtsne(select(bc_data,3:32), 
                  dims = 2, 
                  perplexity=40, 
                  max_iter = 2000,
                  pca=T,
                  initial_dims=30)

bc_dim_reduce <- diagnosis %>% mutate(pc1=bc_prcomp$x[,1],
                                      pc2=bc_prcomp$x[,3],
                                      ts1=bc_Rtsne$Y[,1],
                                      ts2=bc_Rtsne$Y[,2])

