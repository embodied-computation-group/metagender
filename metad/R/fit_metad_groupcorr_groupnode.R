#####################################

# Version of wrapper for estimating 4-task covariance at group level, together with top-level node for group Mratio
#
# Steve Fleming May 2023

#####################################

## Packages
library(rjags)
library(coda)
library(magrittr)

fit_metad_groupcorr <- function (nR_S1, nR_S2) {
  
  # Type 1 parameters
  nTot <- sum(nR_S1[[1]]$V1, nR_S2[[1]]$V1)
  nratings <- nrow(nR_S1[[1]])/2
  nsubj <- ncol(nR_S1[[1]])
  nTask <- length(nR_S1)
  
    # Adjust to ensure non-zero counts for type 1 d' point estimate
    d1 <- data.frame()
    c1 <- data.frame()
    
    for (task in 1:4) {
      
      for (n in 1:(nsubj)) {
        
        adj_f <- 1/((nratings)*2)
        nR_S1_adj = nR_S1[[task]][,n] + adj_f
        nR_S2_adj = nR_S2[[task]][,n] + adj_f
        
        ratingHR <- matrix()
        ratingFAR <- matrix()
        
        for (c in 2:(nratings*2)) {
          ratingHR[c-1] <- sum(nR_S2_adj[c:length(nR_S2_adj)]) / sum(nR_S2_adj)
          ratingFAR[c-1] <- sum(nR_S1_adj[c:length(nR_S1_adj)]) / sum(nR_S1_adj)
          
        }
        
        t1_index <- nratings
        a <- qnorm(ratingHR[(t1_index)]) - qnorm(ratingFAR[(t1_index)])
        d1 %<>%
          rbind(a)
        a <- -0.5 * (qnorm(ratingHR[(t1_index)]) + qnorm(ratingFAR[(t1_index)]))
        c1 %<>%
          rbind(a)
      }
    }
    
    dt1 <- c(d1[1:nsubj,1])
    dt2 <- c(d1[(nsubj+1):(nsubj*2),1])
    dt3 <- c(d1[(nsubj*2+1):(nsubj*3),1])
    dt4 <- c(d1[(nsubj*3+1):(nsubj*4),1])
    
    ct1 <- c(c1[1:nsubj,1])
    ct2 <- c(c1[(nsubj+1):(nsubj*2),1])
    ct3 <- c(c1[(nsubj*2+1):(nsubj*3),1])
    ct4 <- c(c1[(nsubj*3+1):(nsubj*4),1])
    
    d1 <- data.frame(T1 = dt1,
                     T2 = dt2,
                     T3 = dt3,
                     T4 = dt4)
    
    c1 <- data.frame(T1 = ct1,
                     T2 = ct2,
                     T3 = ct3,
                     T4 = ct4)
    
    # Data preparation for model
    counts1 <- t(nR_S1[[1]]) %>% 
      cbind(t(nR_S2[[1]]))
    counts2 <- t(nR_S1[[2]]) %>% 
      cbind(t(nR_S2[[2]]))
    counts3 <- t(nR_S1[[3]]) %>% 
      cbind(t(nR_S2[[3]]))
    counts4 <- t(nR_S1[[4]]) %>% 
      cbind(t(nR_S2[[4]]))
    
    Tol <- 1e-05
    
    d1 <<- as.matrix(d1)
    c1 <<- as.matrix(c1)
    
    data <- list(
      d1 = d1,
      c1 = c1,
      nsubj = nsubj,
      counts1 = counts1,
      counts2 = counts2,
      counts3 = counts3,
      counts4 = counts4,
      nratings = nratings,
      Tol = Tol
    )
    
    ## Model using JAGS
    # Create and update model
    cor_model <- jags.model(file = 'Bayes_metad_group_corr4_groupNode_R.txt', data = data,
                            n.chains = 3, quiet=FALSE)
    update(cor_model, n.iter=1000)
    
    # Sampling
    output <- coda.samples( 
      model          = cor_model,
      variable.names = c("mu_logMratio_group", "mu_logMratio_task", "sigma_logMratio", "rho", "Mratio", "mu_c2"),
      n.iter         = 10000,
      thin           = 1 )
  
  return(output)
}

 