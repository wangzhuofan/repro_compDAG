load("AML_results.RData")
sam_mest <- m_est
rownames(sam_mest) <- unlist(lapply(strsplit(on,split = ";"), function(x){x[6]}))
colnames(sam_mest) <- unlist(lapply(strsplit(sn,split = ";"), function(x){x[6]}))
t = tail(sort(sam_mest),10)
cn = matrix(0,nrow = 10,ncol = 2)
for (i in 1:10) {
  cn[i,1] = rownames(sam_mest)[which(sam_mest==t[11-i],arr.ind = T)[1]]
  cn[i,2] = colnames(sam_mest)[which(sam_mest==t[11-i],arr.ind = T)[2]]
}
colnames(cn) <- c("oral","stool")
print("The top 10 values of the estimated matrix are")
print(cn)
