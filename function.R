KS_dist<-function(vec,Prob,Empi){
  weight<-0
  pp<-1:length(Empi)/length(Empi)
  vec_use<-Prob[which(vec==1)]
  if(length(vec_use)>1){
    rr<-sort(vec_use)
    
    for(i in 1:(length(rr)-1))
    {
      rs<-i/length(rr)
      ii<-sum(rs>=pp)
      tt<-mean(Empi[ii],Empi[ii+1])-rr[i]
      tt<-tt/(1-mean(Empi[ii],Empi[ii+1]))
      if(tt<0)
      {
        weight<-weight-tt
      }
    }
  }
  return(weight)
}





MAT_Generate<-function(Size=100,
                       Prob=0.9,
                       Background=0.6,
                       Pattern_size=15){
  pr<-runif(Size,0.1,Background)
  pc<-runif(Size,0.1,Background)
  rrr<-c()
  ccc<-c()
  for(i in 1:Size)
  {
    rr<-runif(Size,0,1)
    rc<-runif(Size,0,1)
    rrr<-rbind(rrr,(rr<pr[i])*1)
    ccc<-cbind(ccc,(rc<pc[i])*1)
  }
  MAT<-rrr*ccc
  VEC_r<-sample(1:Size,Pattern_size,replace = F)
  VEC_c<-sample(1:Size,Pattern_size,replace = F)
  MAT[VEC_r,VEC_c]<-1
  res<-list()
  res[[1]]<-MAT
  res[[2]]<-VEC_r
  res[[3]]<-VEC_c
  return(res)
}

Empirical_generate<-function(Prob,kk=100){
  vec<-c()
  for(i in 1:length(Prob))
  {
    vec<-c(vec,rep(Prob[i],floor(Prob[i]*kk)))
  }
  vec<-sort(vec)
  return(vec)
}

