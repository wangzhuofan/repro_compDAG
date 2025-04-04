model0 = stan_model('./code/02-5-0compDAG.stan')
model1 = stan_model('./code/02-5-1compDAG.stan')
model2 = stan_model('./code/02-5-2compDAG.stan')
model3 = stan_model('./code/02-5-3compDAG.stan')
model4 = stan_model('./code/02-5-4compDAG.stan')


admissible = function(i, j, gam_short_old) {
  if (gam_short_old[i, j]) {
    #delete an edge is always admissible
    return(TRUE)
  } else{
    gam_short_old[i, j] = 1
    return(igraph::is_dag(igraph::graph_from_adjacency_matrix(gam_short_old)))
  }
}

admissible_rev = function(i, j, gam_short_old) {
  if (gam_short_old[i, j] == gam_short_old[j, i]) {
    return(FALSE)
  } else{
    tmp = gam_short_old[i, j]
    gam_short_old[j, i] = tmp
    gam_short_old[i, j] = !tmp
    return(igraph::is_dag(igraph::graph_from_adjacency_matrix(gam_short_old)))
  }
}

compReg = function(y,x,iters) {
  number = length(x)
  if(number==0){
    tryCatch({
      model = model0
      data = list(n = ncol(y), 
                  p1 = nrow(y), 
                  y1=y)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {
      
    }, finally = {
      
    })
  }
  if(number==1){
    tryCatch({
      model = model1
      data = list(n = ncol(y), 
                  p1 = nrow(x[[1]]),
                  p2 = nrow(y),
                  y1=x[[1]],
                  y2=y,
                  mu2=0.09)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {
      
    }, finally = {
      
    })
  }
  if(number==2){
    tryCatch({
      model = model2
      data = list(n = ncol(y), 
                  p1 = nrow(x[[1]]),
                  p2 = nrow(x[[2]]), 
                  p3 = nrow(y), 
                  y1=x[[1]], 
                  y2=x[[2]], 
                  y3 = y,
                  mu3=0.09)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {
      
    }, finally = {
      
    })
  }
  if(number==3){
    tryCatch({
      model = model3
      data = list(n = ncol(y), 
                  p1 = nrow(x[[1]]),
                  p2 = nrow(x[[2]]), 
                  p3 = nrow(x[[3]]), 
                  p4 = nrow(y), 
                  y1 = x[[1]], 
                  y2 = x[[2]], 
                  y3 = x[[3]],
                  y4 = y,
                  mu4=0.09)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {
      
    }, finally = {
      
    })
  }
  if(number==4){
    tryCatch({
      model = model4
      data = list(n = ncol(y), 
                  p1 = nrow(x[[1]]),
                  p2 = nrow(x[[2]]), 
                  p3 = nrow(x[[3]]), 
                  p4 = nrow(x[[4]]),
                  p5 = nrow(y),
                  y1 = x[[1]], 
                  y2 = x[[2]], 
                  y3 = x[[3]],
                  y4 = x[[4]],
                  y5 = x[[5]],
                  mu5=0.09)
      Reg = sampling(model,data,iter=iters,chains=1,seed=1,refresh = 0)
      pars = extract(Reg)
      logl = pars$log_lik
      # looRes <- loo(Reg,pars="log_lik")
      # looic = looRes$looic
    }, error = function(e) {
      
    }, finally = {
      
    })
  }
  return(logl)
}

calLOOIC = function(logll){
  sumlogll = apply(logll, c(2,3), sum)
  tryCatch({looic = loo(sumlogll)$looic
  }, error = function(e) {
    
  }, finally = {
    
  })
  return(looic)
}

greedyCompDAG = function(y,
                          gam = NULL,
                          ic = "looic",
                          iters = 2000,
                          verbose = 1) {
  #hill-climbing
  
  
  q = length(y)
  n = ncol(y[[1]])
  
  if (is.null(gam)) {
    gam = matrix(FALSE, q, q)
  } else{
    gam = (gam != 0)
  }
  
  
  ind_q = matrix(0, q, q - 1)
  for (i in 1:q) {
    if (i == 1) {
      ind_noi = 2:q
    } else if (i == q) {
      ind_noi = 1:(q - 1)
    } else{
      ind_noi = c(1:(i - 1), (i + 1):q)
    }
    ind_q[i, ] = ind_noi
  }
  
  iter = 0
  ic_improv = 1
  act_ind = c(NA, NA)
  state = "add" # or "del"
  if (ic == "looic") {
    # ic_best = rep(0, q)
    logll = array(0,dim = c(q,0.5*iters,n))
    for (i in 1:q) {
        logll[i,,] = compReg(y[[i]], y[which(gam[i,]!=0)],iters = iters)
    }
    
    ic = calLOOIC(logll)
    while (ic_improv > 0) {
      iter = iter + 1
      ic_improv = 0
      # ic_improv_rev = rep(-Inf, 2)
      gam_new = gam
      ic_improv_new = -Inf
      # ic_improv_rev_new = rep(-Inf, 2)
      # ic_best_new = -Inf
      # ic_rev_best_new = rep(-Inf, 2)
      for (i in 1:q) {
        for (j in 1:(q - 1)) {
          # print(paste0("now operate",i,ind_q[i, j]))
          if (admissible(i, ind_q[i, j], gam)) {
            if (gam[i, ind_q[i, j]]) {
              #delete
              gam_new[i, ind_q[i, j]] = FALSE
              # logllinew = compReg(y[[i]], y[[which(gam_new[i,]!=0)]])
              logllnew = logll
              logllnew[i,,] = compReg(y[[i]], y[which(gam_new[i,]!=0)],iters = iters)
              icnew  = calLOOIC(logllnew)
              ic_improv_new = ic - icnew
              if (ic_improv_new > ic_improv) {
                ic_improv = ic_improv_new
                newlogll = logllnew[i,,]
                act_ind = c(i, ind_q[i, j])
                state = "del"
                # print(paste0("del",act_ind))
              }
              gam_new[i, ind_q[i, j]] = TRUE
            } else{
              #add
              gam_new[i, ind_q[i, j]] = TRUE
              
              logllnew = logll
              logllnew[i,,] = compReg(y[[i]], y[which(gam_new[i,]!=0)],iters = iters)
              icnew  = calLOOIC(logllnew)
              ic_improv_new = ic - icnew
              
              if (ic_improv_new > ic_improv) {
                ic_improv = ic_improv_new
                newlogll = logllnew[i,,]
                act_ind = c(i, ind_q[i, j])
                state = "add"
                # print(paste0("add",act_ind))
              }
              gam_new[i, ind_q[i, j]] = FALSE
            }
          }
        }
      }
      #reverse edge
      for (i in 1:q) {
        for (j in 1:(q - 1)) {
          # print(paste0("now operate",i,ind_q[i, j]))
          if (admissible_rev(i, ind_q[i, j], gam)) {
            tmp = gam_new[i, ind_q[i, j]]
            gam_new[ind_q[i, j], i] = tmp
            gam_new[i, ind_q[i, j]] = !tmp
            logllnew = logll
            logllnew[i,,] = compReg(y[[i]], y[which(gam_new[i,]!=0)],iters = iters)
            # ic_rev_best_new[1]  = calLOOIC(logllnew)
            
            # logllnew = logll
            logllnew[ind_q[i, j],,] = compReg(y[[ind_q[i,j]]], y[which(gam_new[ind_q[i,j],]!=0)],iters = iters)
            # ic_rev_best_new[2]  = calLOOIC(logllnew)
            
            # ic_improv_rev_new[1] = ic - ic_rev_best_new[1]
            # ic_improv_rev_new[2] = ic - ic_rev_best_new[2]
            ic_new = calLOOIC(logllnew)
            ic_improv_new = ic - ic_new
            if (ic_improv_new > ic_improv) {
              ic_improv = ic_improv_new
              newlogll = logllnew[i,,]
              newlogll_inv = logllnew[ind_q[i, j],,]
              # ic_improv_rev = ic_improv_rev_new
              act_ind = c(i, ind_q[i, j])
              state = "rev"
              # print(paste0("rev",act_ind))
            }
            gam_new[i, ind_q[i, j]] = tmp
            gam_new[ind_q[i, j], i] = !tmp
          }
        }
      }
      
      if (ic_improv > 0) {
        if (state == "add") {
          gam[act_ind[1], act_ind[2]] = TRUE
          logll[act_ind[1],,] = newlogll
          # ic_best[act_ind[1]] = ic_best[act_ind[1]] - ic_improv
        } else if (state == "del") {
          gam[act_ind[1], act_ind[2]] = FALSE
          logll[act_ind[1],,] = newlogll
          # ic_best[act_ind[1]] = ic_best[act_ind[1]] - ic_improv
        } else if (state == "rev") {
          tmp = gam[act_ind[1], act_ind[2]]
          gam[act_ind[2], act_ind[1]] = tmp
          gam[act_ind[1], act_ind[2]] = !tmp
          logll[act_ind[1],,] = newlogll
          logll[act_ind[2],,] = newlogll_inv
          # ic_best[act_ind[1]] = ic_best[act_ind[1]] - ic_improv_rev[1]
          # ic_best[act_ind[2]] = ic_best[act_ind[2]] - ic_improv_rev[2]
        }
        ic = ic -ic_improv
      }
      if (verbose&&iter%%1==0){
        print(paste(iter," iterations have completed",sep=""))
        print("The current DAG adjacency matrix is")
        print(gam)
        print(paste("with ic  = ",ic,sep=""))
      }
    }
  }
  return(list(gam = gam, ic= ic))
}

#allow multiple runs of the greedy search with different initializations
# oBN_greedy_wrap = function(y,
#                            ic = "bic",
#                            method = "probit",
#                            nstart = 1,
#                            verbose=verbose) {
#   q = ncol(y)
#   gam_list  = vector("list", nstart)
#   ic_best_list = rep(NA, nstart)
#   if (nstart == 1) {
#     fit = oBN_greedy(y,
#                      gam = NULL,
#                      ic = ic,
#                      method = method,
#                      verbose=verbose)
#     gam_list[[1]] = fit$gam
#     ic_best_list[1] = fit$ic_best
#   } else{
#     netlist = bnlearn::random.graph(
#       nodes = as.character(1:q),
#       method = "ordered",
#       num = nstart - 1,
#       prob = 1 / q
#     )
#     if (nstart == 2) {
#       netlist = list(netlist)
#     }
#     for (i in 1:nstart) {
#       gam = matrix(FALSE, q, q)
#       if (i != 1) {
#         gam[apply(netlist[[i - 1]]$arcs, 2, as.numeric)] = TRUE
#       }
#       fit = oBN_greedy(y,
#                        gam = gam,
#                        ic = ic,
#                        method = method,
#                        verbose=verbose)
#       gam_list[[i]] = fit$gam
#       ic_best_list[i] = fit$ic_best
#     }
#   }
#   i = which.min(ic_best_list)
#   gam = gam_list[[i]]
#   ic_best = ic_best_list[i]
#   return(list(gam = gam, ic_best = ic_best))
#   # }
# }

# oBN_exhaust = function(y,
#                        gam_list = NULL,
#                        ic = "bic",
#                        method = "probit") {
#   n = nrow(y)
#   q = ncol(y)
#   if (is.null(gam_list)) {
#     if (q == 2) {
#       gam_list = array(0, c(q, q, 3))
#       gam_list[1, 2, 2] = 1
#       gam_list[2, 1, 3] = 1
#     } else if (q == 3) {
#       gam_list = array(0, c(q, q, 25))
#       gam_list[1, 2, 2] = 1
#       gam_list[1, 3, 3] = 1
#       gam_list[2, 3, 4] = 1
#       gam_list[2, 1, 5] = 1
#       gam_list[3, 1, 6] = 1
#       gam_list[3, 2, 7] = 1
#       gam_list[1, 2, 8] = gam_list[1, 3, 8] = 1
#       gam_list[2, 3, 9] = gam_list[2, 1, 9] = 1
#       gam_list[3, 1, 10] = gam_list[3, 2, 10] = 1
#       
#       gam_list[2, 1, 11] = gam_list[3, 1, 11] = 1
#       gam_list[3, 2, 12] = gam_list[1, 2, 12] = 1
#       gam_list[1, 3, 13] = gam_list[2, 3, 13] = 1
#       
#       gam_list[1, 2, 14] = gam_list[2, 3, 14] = 1
#       gam_list[1, 3, 15] = gam_list[3, 2, 15] = 1
#       gam_list[2, 3, 16] = gam_list[3, 1, 16] = 1
#       gam_list[2, 1, 17] = gam_list[1, 3, 17] = 1
#       gam_list[3, 2, 18] = gam_list[2, 1, 18] = 1
#       gam_list[3, 1, 19] = gam_list[1, 2, 19] = 1
#       
#       gam_list[1, 2, 20] = gam_list[1, 3, 20] = gam_list[2, 3, 20] = 1
#       gam_list[1, 2, 21] = gam_list[1, 3, 21] = gam_list[3, 2, 21] = 1
#       gam_list[2, 1, 22] = gam_list[2, 3, 22] = gam_list[1, 3, 22] = 1
#       gam_list[2, 1, 23] = gam_list[2, 3, 23] = gam_list[3, 1, 23] = 1
#       gam_list[3, 1, 24] = gam_list[3, 2, 24] = gam_list[1, 2, 24] = 1
#       gam_list[3, 1, 25] = gam_list[3, 2, 25] = gam_list[2, 1, 25] = 1
#     } else{
#       stop("The number of nodes must be 2 or 3")
#     }
#   }
#   IC = rep(0, dim(gam_list)[3])
#   
#   nl = rep(0, q)
#   for (i in 1:q) {
#     nl[i] = nlevels(y[, i])
#   }
#   
#   if (ic == "bic") {
#     for (m in 1:length(IC)) {
#       gam = gam_list[, , m]
#       for (i in 1:q) {
#         gam_tmp = gam[i,]
#         if (sum(gam_tmp) == 0) {
#           if (nl[i] == 2) {
#             IC[m] = IC[m] + stats::BIC(stats::glm(y[, i] ~ 1, family = stats::binomial(link = method)))
#           } else{
#             IC[m] = IC[m] + stats::BIC(MASS::polr(y[, i] ~ 1, method = method))
#           }
#         } else{
#           if (nl[i] == 2) {
#             IC[m] = IC[m] + stats::BIC(stats::glm(
#               y[, i] ~ .,
#               data = y[, gam_tmp == 1],
#               family = stats::binomial(link = method)
#             ))
#           } else{
#             boolFalse = FALSE
#             tryCatch({
#               #sometimes MASS::polr fails to initialize
#               IC[m] = IC[m] + stats::BIC(MASS::polr(y[, i] ~ ., data = y[, gam_tmp ==
#                                                                            1], method = method))
#               boolFalse <- TRUE
#             }, error = function(e) {
#               
#             }, finally = {
#               
#             })
#             
#             if (!boolFalse) {
#               IC[m] = IC[m] + stats::BIC(MASS::polr(
#                 y[, i] ~ .,
#                 data = y[, gam_tmp == 1],
#                 start = stats::rnorm(nl[i] - 1 + sum(nl[gam_tmp == 1] - 1)),
#                 method = method
#               ))
#             }
#           }
#         }
#       }
#     }
#   } else if (ic == "aic") {
#     for (m in 1:length(IC)) {
#       gam = gam_list[, , m]
#       for (i in 1:q) {
#         gam_tmp = gam[i,]
#         if (sum(gam_tmp) == 0) {
#           if (nl[i] == 2) {
#             IC[m] = IC[m] + stats::AIC(stats::glm(y[, i] ~ 1, family = stats::binomial(link = method)))
#           } else{
#             IC[m] = IC[m] + stats::AIC(MASS::polr(y[, i] ~ 1, method = method))
#           }
#         } else{
#           if (nl[i] == 2) {
#             IC[m] = IC[m] + stats::AIC(stats::glm(
#               y[, i] ~ .,
#               data = y[, gam_tmp == 1],
#               family = stats::binomial(link = method)
#             ))
#           } else{
#             boolFalse = FALSE
#             tryCatch({
#               #sometimes MASS::polr fails to initialize
#               IC[m] = IC[m] + stats::AIC(MASS::polr(y[, i] ~ ., data = y[, gam_tmp ==
#                                                                            1], method = method))
#               boolFalse <- TRUE
#             }, error = function(e) {
#               
#             }, finally = {
#               
#             })
#             
#             if (!boolFalse) {
#               IC[m] = IC[m] + stats::AIC(MASS::polr(
#                 y[, i] ~ .,
#                 data = y[, gam_tmp == 1],
#                 start = stats::rnorm(nl[i] - 1 + sum(nl[gam_tmp == 1] - 1)),
#                 method = method
#               ))
#             }
#           }
#         }
#       }
#     }
#   }
#   
#   mi = which.min(IC)
#   ic_best = ic[mi]
#   gam = gam_list[, , mi]
#   return(list(gam = gam, ic_best = ic_best))
# }


#' @title Causal Discovery for Ordinal Categorical Data
#' 
#' @description Estimate a causal directed acyclic graph (DAG) for ordinal categorical data with greedy or exhaustive search.
#' 
#' @param y a data frame with each column being an ordinal categorical variable, which must be a factor.
#' @param search the search method used to find the best-scored DAG. The default search method is "greedy". When the number of nodes is less than 4, "exhaust" search is available.
#' @param ic the information criterion (AIC or BIC) used to score DAGs. The default is "bic".
#' @param link the link function for ordinal regression. The default is "probit". Other choices are "logistic", "loglog", "cloglog", and "cauchit".
#' @param G a list of DAG adjacency matrices that users want to restrict their search on for the "exhaust" search. The default is "NULL" meaning no restriction imposed on the search.
#' @param nstart number of random graph initializations for the "greedy" search.
#' @param verbose if TRUE, messages are printed during the run of the greedy search algorithm.
#' @return A list with two elements, gam and ic_best. gam is an estimated DAG adjacency matrix whose (i,j)th entry is 1 if j->i is present in the graph and 0 otherwise. ic_best is the correponding information criterion value.
#' 
#' @export
#' 
#' @examples
#' set.seed(2020)
#' n=1000 #sample size
#' q=3 #number of nodes
#' y = u = matrix(0,n,q)
#' u[,1] = 4*rnorm(n)
#' y[,1] = (u[,1]>1) + (u[,1]>2)
#' for (j in 2:q){
#'   u[,j] = 2*y[,j-1] + rnorm(n)
#'   y[,j]=(u[,j]>1) + (u[,j]>2)
#' }
#' A=matrix(0,q,q) #true DAG adjacency matrix
#' A[2,1]=A[3,2]=1
#' y=as.data.frame(y)
#' for (j in 1:q){
#'   y[,j]=as.factor(y[,j])
#' }
#'
#' G=OCD(y) #estimated DAG adjacency matrix
#' print(A)
#' print(G)

# OCD = function(y,
#                search = "greedy",
#                ic = "bic",
#                link = "probit",
#                G = NULL,
#                nstart = 1,
#                verbose = FALSE) {
#   if (search == "exhaust") {
#     G = oBN_exhaust(y, G, ic, link)
#   } else{
#     G = oBN_greedy_wrap(y, ic, link, nstart, verbose)
#   }
#   return(G)
# }
