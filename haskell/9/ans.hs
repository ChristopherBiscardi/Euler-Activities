main = print a
  where
    a = pythag
pRange = [1..1000]
pythag = [a*b*c|a<-pRange,b<-pRange,c<-pRange,(a*a+b*b)==(c*c),a<b,b<c,a+b+c==1000]
