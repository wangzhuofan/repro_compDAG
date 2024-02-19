rm(list = ls())

source("./code/PARAMS.R")
cat(noise.type,signal,simu_n,method, "\n")
source("./code/00load_all.R")
savepath = paste0("./output/simulation_results/simulation-noisetype-",noise.type,
                  "-signal-",signal,
                  "-n-", simu_n,
                  "-method-",method,
                  ".RData")

cl<- makeCluster(nCluster)
registerDoParallel(cl)
result = foreach(i=1:nRepli,.packages = c("rstan","gtools","loo","quantregForest","rvinecopulib","statmod","qrnn","COLP","cluster")) %dopar%
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
    if(method == "compDAG"){
      res = compDAG(x,y,0.1,0.1)
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
    
    if(method == "PC"){
      data = rbind(x,y)
      p = simu_px+simu_py
      test1 = gnlearn::pc(as.data.frame(t(data)),to = "adjacency")
      adj = t(matrix(as.numeric(test1!=0),p,p))
      xtoy = sum(adj[(simu_px+1):p,1:simu_px])
      ytox = sum(adj[1:simu_px,(simu_px+1):p])
      if(xtoy>ytox) respc = 1 else respc = 0
      res = list(cd = respc,mEst = adj[(simu_px+1):p,1:simu_px])
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
