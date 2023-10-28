load("MOMS_PI_results.RData")
sam_mest3 <- m_est
colnames(sam_mest3) <- rownames(moms_cervix_of_uterus)
rownames(sam_mest3) <- rownames(moms_vagina)
sam_mest2 = sam_mest3
t = tail(sort(sam_mest2),20)
cn = matrix(0,nrow = 20,ncol = 2)
for (i in 1:20) {
  cn[i,1] = rownames(sam_mest2)[which(sam_mest2==t[21-i],arr.ind = T)[1]]
  cn[i,2] = colnames(sam_mest2)[which(sam_mest2==t[21-i],arr.ind = T)][2]
}
colnames(cn) <- c("vagina","uterus")
cnn=cn
cnn[,1] = cn[,2]
cnn[,2]=cn[,1]
colnames(cnn) <- c("uterus","vagina")
print("The top 20 values of the estimated matrix are:")
print(cnn)