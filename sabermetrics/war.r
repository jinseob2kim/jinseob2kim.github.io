a=read.table("/home/secondmath/Dropbox/consult/dutyarticle/wOBA.csv",sep="\t",header=T)

head(a)
install.packages("xtable")
library(xtable)
xtable(a)

a=read.csv("/home/secondmath/Dropbox/consult/dutyarticle/choo.csv")
xtable(a)

a=read.csv("/home/secondmath/Dropbox/consult/dutyarticle/Park_factor.csv")
xtable(a)