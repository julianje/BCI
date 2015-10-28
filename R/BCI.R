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
