 rm(list=ls())
 library(Rcpp)
 library(lattice)
 library(latticeExtra)


 #include <Rcpp.h>

 
 using namespace Rcpp;

 
 
 // [[Rcpp::export]]

 
 int PARTITION(NumericVector x, int p, int r){

   float temp, pivot;

   int i, j;

   pivot = x[r];

   i = p-1;

   for(j = p; j < r; j++){

     if(x[j] <= pivot){

       i = i+1;

       temp = x[j];

       x[j] = x[i];

       x[i] = temp;

     }

   }

   temp = x[i+1];

   x[i+1] = x[r];

   x[r] = temp;

   return i+1;

 }

 
 // [[Rcpp::export]]

 
 float FIND_ORDER_STAT( NumericVector x, int w, int p, int r){

   int q;

   q = PARTITION(x, p, r);

   if (q < w){

     return FIND_ORDER_STAT(x, w, q+1, r);

   }

   else if (q > w) {

      return FIND_ORDER_STAT(x, w, p, q-1);

   }

   else if (q == w){

    return x[q];

   }

 }

 
 // [[Rcpp::export]]

 
 float order_statistic(NumericVector x, int w){

   int n;

   n = x.size();

   if (w <= n){

     w = w-1;

     return FIND_ORDER_STAT(x, w, 0, n-1);

   }

 }

 
 
 

 ---------------------------------------------------------------------------------------------------
 x=sample(1:1000, 5000, replace = TRUE )
 indices=sample(1:5000,10,replace=FALSE)
 ord_stats_quicksort=rep(0,10)
 for(i in 1:10)
 {
   ord_stats_quicksort[i]=order_statistic(x, indices[i])
 }
 ord_stats_quicksort
 sort(x)[indices]
 identical(sort(x)[indices],as.integer(ord_stats_quicksort))
 

 timefun=function(size, nrep , order.fun )
 {
     x=replicate(nrep, runif(size), simplify = FALSE)
     system.time(lapply(x, order.fun))["elapsed"] / nrep
 }
 n=seq(10000, 10000000, by = 10000)
 
 call_order_statistic_min=function(x){
   w=1
   order_statistic(x, w)
 }
 time_order_statistic_min=sapply(n, timefun, nrep = 5,
                                        order.fun = call_order_statistic_min)
 call_order_statistic_med=function(x){
   w=ceiling(length(x)/2)
   order_statistic(x, w)
 }
 time_order_statistic_med=sapply(n, timefun, nrep = 5,
                                        order.fun = call_order_statistic_med)
 call_order_statistic_max=function(x){
   w=length(x)
   order_statistic(x, w)
 }
 time_order_statistic_max=sapply(n, timefun, nrep = 5,
                                        order.fun = call_order_statistic_max)


 write.csv(data.frame(n,time_order_statistic_min,time_order_statistic_med,time_order_statistic_max),file="Time Data.csv")


 data=read.csv("Time Data.csv")
 n=data[,2]
 time_order_statistic_min=data[,3]
 time_order_statistic_med=data[,4]
 time_order_statistic_max=data[,5]
 library(lattice)
 xyplot( time_order_statistic_min + time_order_statistic_med + time_order_statistic_max ~ n,
         type = "p" ,auto.key = list(space = "top"),grid = TRUE,
         ylab = "time (seconds)", main = "Time taken to find order statistic", pch = 20)


 summary(time_order_statistic_min)
 summary(time_order_statistic_med)
 summary(time_order_statistic_max)


 adjrsq=function(data){
   summary(lm(data[,2]~data[,1]))$adj.r.squared
 }
 nlogn=n*log(n)
 Rsq_n=adjrsq(data.frame(n=n,time=time_order_statistic_med))
 Rsq_nlogn=adjrsq(data.frame(n=n*log(n),time=time_order_statistic_med))
 
 simulate=function(data,size){
   data[sample(nrow(data),size,replace=T),]
 }
 
 bootadjrsq <- function(data , bootsize, nboot)
 {
     x=replicate(nboot, simulate(data,bootsize), simplify = FALSE)
     sapply(x,adjrsq)
 }
 bootRsq_n=bootadjrsq(data.frame(n=n,time=time_order_statistic_med),200,100000)
 bootRsq_nlogn=bootadjrsq(data.frame(n=n*log(n),time=time_order_statistic_med),200,100000)
 write.csv(data.frame(Rsq_n=bootRsq_n,Rsq_nlogn=bootRsq_nlogn),file="Bootstrap distribution of adjusted Rsquare.csv")
 
 diff=bootRsq_n-bootRsq_nlogn
 hist(diff,main="Histogram of Difference between adjusted R square values")
 summary(diff)
 pvalue=sum(as.numeric((diff<=0)))/length(diff)
 pvalue
 


 timevarfun=function(size, nrep , order.fun) {
   t=vector(mode = "numeric")
   for ( i  in 1 : nrep) {
     x <- runif(size)
     t <- c(t , system.time(order.fun(x))["elapsed"])
   }
   var(t)
 }
 m=seq(100000, 10000000, by = 50000)
 var_time_order_statistic_med <- sapply(m, timevarfun, nrep = 10,
                                        order.fun = call_order_statistic_med)
 write.csv(data.frame(m,var_time_order_statistic_med),file="Variance of Time Data.csv")


 data=read.csv("variance of Time Data.csv")
 m=data[,2]
 time_order_statistic_med=data[,3]
 library(lattice)
 xyplot(var_time_order_statistic_med ~ m, grid = TRUE, jitter = TRUE,
        main = "Variance of average case runtime", pch = 19)


 vtime_m=var_time_order_statistic_med/m
 vtime_m2=var_time_order_statistic_med/(m^2)
 vtime_logm=var_time_order_statistic_med/log(m)
 vtime_mlogm=var_time_order_statistic_med/(m*log(m))

 xyplot(vtime_m + vtime_m2 +vtime_logm + vtime_mlogm ~ m,
        outer = TRUE, scale = list(x = "free", y = "free"), auto.key = list(space = "right")
        , main = "Plot of variance time ratio 1", grid = TRUE, pch = 20)


 xyplot( vtime_m + vtime_m2 + vtime_mlogm  ~ m,
         scale = list(x = "free", y = "free"), auto.key = list(space = "top"),
         main ="Plot of var time ratio 2", grid = TRUE, pch = 16)

