
alpha=0.05
nazvy=c("A1","A2","A3","A4")

anova_res=aov(data_bezOP$`automaticke mereni` ~ data_bezOP$`typ abraziva`)
post_hoc_res=TukeyHSD(anova_res)
p_vals_post_hoc=post_hoc_res[[1]][,4]
it=1
mat=matrix(0, 4,4)
for(i in 1:3){
  for(j in (i+1):4){
    mat[i,j]=p_vals_post_hoc[it]
    mat[j,i]=p_vals_post_hoc[it]
    it=it+1
  }
  
}
means_all=tapply(data_bezOP$`automaticke mereni` , data_bezOP$`typ abraziva`,mean)
poradi=sort.list(means_all,decreasing = TRUE)
mat=mat[poradi,poradi]

normality_exact_empiric <- function(i,j,tag,text){
  if(tag){
    rect(i-1, j-1, i, j, col = "green")
  }else{
    rect(i-1, j-1, i, j, col = "orange")
  }
  if (text >= 0.0005) {
    if(text >= 0.999){
      t_p = "0.999"
    }else{
      t_p = paste0(round(text, digits = 3))
    }
  } else {
    t_p = "<<0.001"
  }
  text(i-0.5,j-0.5,t_p)
}



plot(c(0,4),c(0,4),type="l", xaxt = 'n', yaxt = 'n', xlim=c(0.15,3.85),ylim = c(3.85,0.15),xlab="",ylab="")
lines(c(0,1),c(1,0))
lines(1+c(0,1),1+c(1,0))
lines(2+c(0,1),2+c(1,0))
lines(3+c(0,1),3+c(1,0))
axis(3,at = c(0.5,1.5,2.5,3.5), labels = nazvy[poradi], las = 1)
axis(2, at = c(0.5,1.5,2.5,3.5), labels = nazvy[poradi], las = 1)
grid(nx = NULL, ny = NULL, col = "black", lty = "solid")


for(i in 1:4){
  for(j in 1:4){
    if (i==j){}else{
      normality_exact_empiric(i,j,mat[i,j]>alpha,mat[i,j])
    }
  }
}
