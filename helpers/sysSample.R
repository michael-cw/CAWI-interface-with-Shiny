# FUNCTION for systematic
sys.sample = function(N,n){
  ##  First Group
  interV = N/n
  k = ceiling(interV)
  r = sample(1:k, 1)
  ##  Align the rest
  sys.samp = round(seq(r, N, interV), digits = 0)
  ##  In case that sample is 1 less, sample 1 additional unit
  if (length(sys.samp)<n) sys.samp<-c(sys.samp, sample((1:N)[-sys.samp],1))
  return(sys.samp)
}
