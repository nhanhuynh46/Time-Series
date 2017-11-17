########################################################################## 
### R code to reproduce overheads II-101 and II-102, Stat 519, Winter 2011
##########################################################################

innovations.algorithm <- function(n.max,acvs)
  {
    thetas <- matrix(rep(0,n.max^2),nrow=n.max)
    vs <- rep(0,n.max+1)
    vs[1] <- acvs[1]
    for(n in 1:n.max)
      {
        thetas[n,n] <- acvs[n+1]/vs[1]
        if(n>1)
          {
            for(k in 1:(n-1))
              {
                inner.sum <- acvs[n-k+1]
                for(j in 0:(k-1))
                  {
                    inner.sum <- inner.sum - thetas[k,k-j]*thetas[n,n-j]*vs[j+1]
                  }
                thetas[n,n-k] <- inner.sum/vs[k+1]
              }
          }
        vs[n+1] <- acvs[1]
        for(j in 0:(n-1))
          {
            vs[n+1] <- vs[n+1] - thetas[n,n-j]^2*vs[j+1]
          }
      }
    structure(list(thetas=thetas,vs=vs))
  }

#######################################################
# Variance...							#
#######################################################

get.sigma.2.n.hs <- function(n,h.max,ts.var,ia.stuff)
  {
    thetas <- ia.stuff$thetas
    vs <- ia.stuff$vs
    sigma.2.n.hs <- rep(ts.var,h.max)
    for(h in 1:h.max)
      {
        sigma.2.n.hs[h] <- sigma.2.n.hs[h] - sum((thetas[n+h-1,(n+h-1):h])^2*vs[1:n])
      }
    sigma.2.n.hs
  }
