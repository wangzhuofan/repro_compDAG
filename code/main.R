rm(list = ls())

source("./code/PARAMS.R")
cat(noise.type,signal,simu_n,method,rparam,param_verbose,hyper_verbose,prop_verbose,"\n")
source("./code/00load_all.R")
savepath = paste0("./output/simulation_results/simulation-noisetype-",noise.type,
                  "-signal-",signal,
                  "-n-", simu_n,
                  "-method-",method,
                  # "-param-",rparam,
                  # "-pgammaa-",pgammaa,
                  # "-pgammab-",pgammab,
                  # "-pdir-",pdir,
                  # "-prop-",prop,
                  ".RData")
if(param_verbose==1){
  savepath = paste0("./output/simulation_results/simulation-noisetype-",noise.type,
                    "-signal-",signal,
                    "-n-", simu_n,
                    "-method-",method,
                    "-param-",rparam,
                    # "-pgammaa-",pgammaa,
                    # "-pgammab-",pgammab,
                    # "-pdir-",pdir,
                    # "-prop-",prop,
                    ".RData")
}
if(hyper_verbose==1){
  savepath = paste0("./output/simulation_results/simulation-noisetype-",noise.type,
                    "-signal-",signal,
                    "-n-", simu_n,
                    "-method-",method,
                    "-param-",rparam,
                    "-pgammaa-",pgammaa,
                    "-pgammab-",pgammab,
                    "-pdir-",pdir,
                    # "-prop-",prop,
                    ".RData")
}
if(prop_verbose==1){
  savepath = paste0("./output/simulation_results/simulation-noisetype-",noise.type,
                    "-signal-",signal,
                    "-n-", simu_n,
                    "-method-",method,
                    # "-param-",rparam,
                    # "-pgammaa-",pgammaa,
                    # "-pgammab-",pgammab,
                    # "-pdir-",pdir,
                    "-prop-",prop,
                    ".RData")
}
cl<- makeCluster(nCluster)
registerDoParallel(cl)
result = foreach(i=1:nRepli,.packages = c("rstan","gtools","loo","quantregForest","rvinecopulib","statmod","qrnn","COLP","cluster","ExtDist","robCompositions")) %dopar%
  {
    if(noise.type == "Dirichlet"){
      data = generate_data(simu_n,simu_px,simu_py,signal,i)
      x = data$x
      y = data$y
    }
    if(noise.type == "Additive"){
      data = generate_misspecified_data(simu_n,simu_px,simu_py,signal,i)
      x = data$x
      y = data$y
    }
    if(noise.type == "Cyclic"){
      data = generate_cyclic_data(simu_n,simu_px,simu_py,1,signal,i)
      x = data$x
      y = data$y
    }
    if(noise.type == "LiNGAM"){
      data = generate_lingam_comp_data(simu_n,simu_px,simu_py,signal,i)
      x = data$x
      y = data$y
    }
    if(noise.type == "Sparse"){
      data = generate_sparse_data(simu_n,simu_px,simu_py,signal,i)
      x = data$x
      y = data$y
    }
    if(noise.type == "zero-inflated"){
      data = generate_latent_comp_data(simu_n,simu_px,simu_py,signal,"truncate",prop,i)
      x = data$x
      y = data$y
    }
    if(noise.type == "Multi3"){
      data = generate_multi_data(simu_n,c(simu_px,simu_px,simu_px),c(signal,signal,signal),i)
      y = data$y
    }
    if(noise.type == "CF"){
      data = generate_confounder_data(simu_n,c(simu_px,simu_px,simu_px),c(signal,signal,signal),i)
      #x = data$x
      y = data$y
    }
    if(noise.type == "Multi5"){
      data = generate_multi_data(simu_n,c(simu_px,simu_px,simu_px,simu_px,simu_px),c(signal,signal,signal,signal,signal),i)
      y = data$y
    }
    if(method == "compDAG"&noise.type == "Multi3"){
      res = multiCompDAG3(3,c(simu_px,simu_px,simu_px),y,mu1=0.09,mu2=0.09,mu3=0.09)
      # print(sessionInfo())
    }
    if(method == "compDAG"&noise.type == "Multi5"){
      res = greedyCompDAG(y)
    }
    if(method == "microbe"){
      res = microbe_fit(y,i)
    }
    if(method == "compDAG"&noisetype !="Multi3"&noisetype !="Multi5"){
      #res = compDAG(x,y,rparam,pgammaa,pgammab,pdir)
      res = compDAG(x,y,rparam,rparam)
    }
    if(method == "compDAG_param"){
      #res = compDAG(x,y,rparam,pgammaa,pgammab,pdir)
      res = compDAG_param(x,y,rparam,rparam)
    }
    if(method == "compDAG_hyper"){
      res = compDAG_hyper(x,y,rparam,pgammaa,pgammab,pdir)
      # res = compDAG(x,y,rparam,rparam)
    }
    if(method == "compDAGPW"){
      #res = compDAG(x,y,rparam,pgammaa,pgammab,pdir)
      res = compDAG(y[[2]],y[[3]],rparam,rparam)
    }
    if(method == "COLP"){
      asw <- numeric(5)
      for (k in 2:5)
        asw[[k]] <- pam(t(x), k) $ silinfo $ avg.width
      kx.best <- which.max(asw)
      asw <- numeric(5)
      for (k in 2:5)
        asw[[k]] <- pam(t(y), k) $ silinfo $ avg.width
      ky.best <- which.max(asw)

      kx = as.factor(kmeans(t(x),kx.best)$cluster)
      ky = as.factor(kmeans(t(y),ky.best)$cluster)

      fit = COLP(kx,ky,algo="E")
      res = fit$cd
    }

    if(method == "PC"&noisetype !="Multi3"&noisetype !="Multi5"){
      data = rbind(x,y)
      p = simu_px+simu_py
      test1 = gnlearn::pc(as.data.frame(t(data)),to = "adjacency")
      adj = t(matrix(as.numeric(test1!=0),p,p))
      xtoy = sum(adj[(simu_px+1):p,1:simu_px])
      ytox = sum(adj[1:simu_px,(simu_px+1):p])
      if(xtoy>ytox) respc = 1 else respc = 0
      res = list(cd = respc,mEst = adj[(simu_px+1):p,1:simu_px])
    }
    if(method == "PCPW"){
      data = rbind(x,y)
      p = simu_px+simu_py
      test1 = gnlearn::pc(as.data.frame(t(data)),to = "adjacency")
      adj = t(matrix(as.numeric(test1!=0),p,p))
      xtoy = sum(adj[(simu_px+1):p,1:simu_px])
      ytox = sum(adj[1:simu_px,(simu_px+1):p])
      if(xtoy>ytox) respc = 1 else respc = 0
      res = list(cd = respc,mEst = adj[(simu_px+1):p,1:simu_px])
    }
    if(method == "PC"&(noise.type == "Multi3"|noise.type =="CF")){
      data = rbind(y[[1]],y[[2]],y[[3]])
      p = 3*simu_px
      test1 = gnlearn::pc(as.data.frame(t(data)),to = "adjacency")
      adj = t(matrix(as.numeric(test1!=0),p,p))
      edge = matrix(0,3,3)
      for (i in 1:3) {
        for (j in 1:3) {
          edge[i,j] = sum(adj[(simu_px*(i-1)+1):(simu_px*i),(simu_px*(j-1)+1):(simu_px*j)])
        }
      }
      res = list(adj=adj,edge=edge)
    }

    if(method == "PC"&noise.type == "Multi5"){
      data = rbind(y[[1]],y[[2]],y[[3]],y[[4]],y[[5]])
      p = 5*simu_px
      test1 = gnlearn::pc(as.data.frame(t(data)),to = "adjacency")
      adj = t(matrix(as.numeric(test1!=0),p,p))
      edge = matrix(0,5,5)
      for (i in 1:5) {
        for (j in 1:5) {
          edge[i,j] = sum(adj[(simu_px*(i-1)+1):(simu_px*i),(simu_px*(j-1)+1):(simu_px*j)])
        }
      }
      res = list(adj=adj,edge=edge)
    }
    if(method == "bQCD"){
      pcax = prcomp((x))
      pcax = pcax$rotation[,1]
      pcay = prcomp((y))
      pcay = pcay$rotation[,1]
      resbqcd = QCCD(cbind(pcax,pcay),7)
      res = resbqcd$cd
    }
    res = res
  }
save(result, file = savepath)
