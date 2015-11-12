CI <- function(data, R=10000){
  samples <- boot::boot(data,function(x,id){return(mean(x[id]))},R)
  return(boot::boot.ci(samples,type="basic")$basic[c(4,5)])
}
LowerCI <- function(data, R=10000){
  samples <- boot::boot(data,function(x,id){return(mean(x[id]))},R)
  return(boot::boot.ci(samples,type="basic")$basic[c(4)])
}
TopCI <- function(data, R=10000){
  samples <- boot::boot(data,function(x,id){return(mean(x[id]))},R)
  return(boot::boot.ci(samples,type="basic")$basic[c(5)])
}

CI_Corr <- function(d1,d2, R=10000){
  data = rbind(mutate(d1,Type="d1"),mutate(d2,Type="d2"))
  samples <- boot::boot(data,function(x,id){
    x$d1=sample(x$d1)
    samples<-x[id,]
    return(cor(samples$d1,samples$d2))},R)
  return(boot::boot.ci(samples,type="basic")$basic[c(4,5)])
}
LowerCI_Corr <- function(d1,d2, R=10000){
  data = rbind(mutate(d1,Type="d1"),mutate(d2,Type="d2"))
  samples <- boot::boot(data,function(x,id){
    x$d1=sample(x$d1)
    samples<-x[id,]
    return(cor(samples$d1,samples$d2))},R)
  return(boot::boot.ci(samples,type="basic")$basic[c(4)])
}
TopCI_Corr <- function(d1,d2, R=10000){
  data = rbind(mutate(d1,Type="d1"),mutate(d2,Type="d2"))
  samples <- boot::boot(data,function(x,id){
    x$d1=sample(x$d1)
    samples<-x[id,]
    return(cor(samples$d1,samples$d2))},R)
  return(boot::boot.ci(samples,type="basic")$basic[c(5)])
}