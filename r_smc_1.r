## vector  
x=c(1,2,3,4,5,6)            ## vector of variable
y=c(7,8,9,10,11,12)
x+y              
x*y
sqrt(x)
sum(x)
sum(x>=4)   ## No sum, number
diff(x) 
mean(x)
sd(x)
max(x)
max(x,y)
length(x)

x[2]
x[-2]  
x[1:3]
x[c(1,3,4,5,6)]
x[c(1,2,3)]


## Sequence
v1=seq(-5,5,by=.2); v1 
## Repeat 
v2=rep(1,3); v2
v3=rep(c(1,2,3),2); v3              ## Repeat for vector
v4=rep(c(1,2,3),each = 2); v4       ## Repeat for vector : each


## for loop
for (i in 1:3){
  print(i)
}

i=0
for (j in c(1,2,4,5,6)){
  i=i+j
}
i

## if 
x=5
if (x >=3 ){
  x=x+3
}
x

x=5
if (x >=10){
  print("High")
} else if (x >=5){
  print("Medium")
} else {
  print("Low")
}                              


## ifelse
x=5
y=ifelse(x==5,"OK","Suck")       
y


## Function
x=c(1:10,12,13,NA,NA,15,17)               
mean(x)
mean0=function(x){
  mean(x,na.rm=T)
}                                         

mean0=function(x){mean(x,na.rm=T)}        
mean0(x)


twomean=function(x1,x2){
  a=(x1+x2)/2
  a
}
twomean(4,6)


## Data save & Load
getwd()                                                                  
setwd("C:/Users/sec/Desktop/Language/r/edu")        
getwd()

a=read.csv("smc_example.csv")                           
a=read.table("smc_example.csv", sep=",", header=T)     
head(a)  

write.csv(a,"smc_example1.csv")
write.csv(a,"smc_example1.csv",row.names=F)   


## See data
head(a)  

tail(a)               
names(a)              
a$BMI[1:10]                           
dim(a)               
nrow(a)               
ncol(a)              
class(a)              
str(a)               
summary(a)           


a$BMI
a[,"BMI"]
a[,11]

#a[1:7]                       
a[,1:7]                      
a[,c(1,2,3,4,5,6,7)]
a[,seq(1,7)]
a[,c("Patient_ID","Sex", "Age" , "Height" ,"Weight","BMI","DM")]  ## 변수명으로도 가능 
a[,names(a)[1:7]]     

length(a$BMI)
a[2,11]  
a[2,]   

mean(a$BMI)                            
mean(a$BMI,na.rm=T)                    
round(mean(a$BMI,na.rm=T),2)         
mean(a[,6],na.rm=T)
sd(a$BMI,na.rm=T)                   
var(a$BMI,na.rm=T)                   
median(a$BMI,na.rm=T)               
IQR(a$BMI,na.rm=T)                   ## 25%-75% range
quantile(a$BMI, na.rm=T)             ## quantile
max(a$BMI,na.rm=T)                   ## Max
min(a$BMI,na.rm=T)                   ## Min

tb=table(a$Sex,a$STRESS_EXIST)
tb                              ## Table 
prop.table(tb)                  ## Proportion
prop.table(tb,1)                ## By Row
prop.table(tb,2)                ## By Col


## Etc: 
mean(a$BMI)
cut= a$BMI >=25                                   ## TRUE of FALSE
table(cut)                         
rows=which(a$BMI >= 25)                           ## row numbers
head(rows)                               
values=a$BMI[a$BMI>=25]                         ## Values
head(values)
length(values)
BMI_old_and=(a$BMI>=25 & a$Age >= 50)          ## and
BMI_old_or=(a$BMI>=25 | a$Age >= 50)           ## or
table(BMI_old_and)
table(BMI_old_or)
stent123 = (a$Number_stent %in% c(1,2,3))      ## %in%
table(stent123,a$Number_stent)


## Statistics
### 2 group 
## t.test
t.test(a$Age ~ a$STRESS_EXIST)
t.test(a[,"Age"] ~ a[,"STRESS_EXIST"])
t.test(a[,3]~a[,14])
tt=t.test(Age~STRESS_EXIST,data=a)
tt
tt$p.value

## wilcoxon rank sum test: Non parametric t.test
wilcox.test(a$Age ~ a$STRESS_EXIST)
wilcox.test(a[,"Age"] ~ a[,"STRESS_EXIST"])
wilcox.test(a[,3]~a[,14])
wt=wilcox.test(Age~STRESS_EXIST,data=a)
wt$p.value

## paired t.test
t.test(a$MACCE_date,a$Death_date,paired=T)
ptt=t.test(a[,"MACCE_date"],a[,"Death_date"],paired=T)
ptt
ptt$p.value

## wilcoxon signed rank test: Non parametric paired t.test
wilcox.test(a$MACCE_date,a$Death_date,paired=T)
wptt=wilcox.test(a[,"MACCE_date"],a[,"Death_date"],paired=T)
wptt
wptt$p.value



## Normality test
hist(a$Age)
shapiro.test(a$Age)
ks.test(a$Age,pnorm)                ## pnorm: probability of normal distribution

hist(a$Number_stent)          
shapiro.test(a$Number_stent)


## Multiple variables at once
varname=c("Age","Height","Weight","BMI","MACCE_date","Death_date")
varname
varname_num=c(3:6,12,13)

## For loop: print 
for(i in varname){
  res=t.test(a[,i]~a$STRESS_EXIST)
  print(res)
}

## For loop: Only p-value
for(i in varname){
  res=t.test(a[,i]~a$STRESS_EXIST)
  print(res$p.value)
}

## Apply
tfunc=function(x){
  t.test(a[,x]~a$STRESS_EXIST)$p.value
}

sapply(varname,tfunc)                                                        ## vector
tps_vector=sapply(varname,function(x){t.test(a[,x]~a$STRESS_EXIST)$p.value})
tps_vector[1]

tps_list=lapply(varname,tfunc)                                               ## List
tps_list[[1]]


## Summary: for table

# Mean, Sd per group
## Method 1: tapply
tapply(a$Age,a$STRESS_EXIST,mean)
tapply(a$Age,a$STRESS_EXIST,function(x){mean(x,na.rm=T)})
tapply(a$Age,a$STRESS_EXIST,sd)

## Method2: aggegate
aggregate(a[,varname],list(a$STRESS_EXIST),mean)
aggregate(a[,varname],list(a$STRESS_EXIST,a$Sex),sd)


## Method3: mean, sd, pvalue
ttsum=function(x){
  mn=tapply(a[,x],a$STRESS_EXIST,mean)
  s= tapply(a[,x],a$STRESS_EXIST,sd)
  pv=t.test(a[,x]~a$STRESS_EXIST)$p.value
  return(c(mn,s,pv))
}
t(sapply(varname,ttsum))


## Method4: Nonparametric
wcsum=function(x){
  md=tapply(a[,x],a$STRESS_EXIST,median)                         ## Median
  q25=tapply(a[,x],a$STRESS_EXIST,function(x){quantile(x)[2]})   ## 25%
  q75=tapply(a[,x],a$STRESS_EXIST,function(x){quantile(x)[4]})   ## 75%
  pv=wilcox.test(a[,x]~a$STRESS_EXIST)$p.value
  return(c(md,q25,q75,pv))
}

t(sapply("Number_stent",wcsum))


## New grammar: dplyr package: %>%-  ctrl+ shift + M
#install.packages("dplyr")
library(dplyr)
a %>% group_by(STRESS_EXIST) %>% select(varname_num) %>% summarise_each(funs(mean))

dpres2 = a %>% group_by(STRESS_EXIST) %>% select(varname_num) %>% summarise_each(funs(mean,sd))
dpres2
t(dpres2)



## 3 group: ANOVA
a$BMI_group=ifelse(a$BMI>25,"High",ifelse(a$BMI>20,"Normal","Low"))  # High/Normal/Low
a$BMI_group2= (a$BMI > 25) + (a$BMI >20)                             # 2/1/0

## ANOVA
aov(a$Age~a$BMI_group)               
aov(a[,"Age"]~a[,"BMI_group"])            
a_res=aov(Age~BMI_group, data=a)
summary(a_res)                                                      ## ANOVA table
summary(a_res)[[1]][["Pr(>F)"]][1]                                  ## Only p-value


## Non-parametric ANOVA 
kruskal.test(a$Number_stent~a$BMI_group2)                                        
kruskal.test(a[,"Number_stent"]~a[,"BMI_group2"])            
a_res=kruskal.test(Number_stent~BMI_group2, data=a)
a_res
a_res$p.value         


## Chisq test
## Create table
tb=table(a$Sex,a$STRESS_EXIST)

## Chi-square test
chisq.test(tb)
chisq.test(table(a$Sex,a$STRESS_EXIST))
chisq.test(tb)$p.value

table(a$BMI_group,a$STRESS_EXIST)
chisq.test(table(a$BMI_group,a$STRESS_EXIST))

## Non-parametric: fisher-test
tb2=table(a[,"Number_stent"],a$STRESS_EXIST)
chisq.test(tb2)

fisher.test(tb2)
fisher.test(tb2)$p.value


## Multiple var at once
varname2=c("Sex","DM","HTN","Smoking","MACCE","Death","Number_stent","BMI_group")

## For loop 
for (i in varname2){
  tb=table(a[,i],a$STRESS_EXIST)
  print(tb)
  print(chisq.test(tb))
}

for (i in varname2){
  print(chisq.test(table(a[,i],a$STRESS_EXIST))$p.value)
}

for (i in varname2){
  print(fisher.test(table(a[,i],a$STRESS_EXIST))$p.value)
}

## Apply 
### P-value
p_chisq=function(x){
  chisq.test(table(a[,x],a$STRESS_EXIST))$p.value
}

sapply(varname2,p_chisq)
sapply(varname2,function(x){chisq.test(table(a[,x],a$STRESS_EXIST))$p.value})
sapply(varname2,function(x){fisher.test(table(a[,x],a$STRESS_EXIST))$p.value})  ## Fisher

### Table
sapply(varname2,function(x){table(a[,x],a$STRESS_EXIST)})
lapply(varname2,function(x){table(a[,x],a$STRESS_EXIST)})


## Table 1
library(moonBook);library(ztable)

## Make table 1: digits 반올림, max.ylev: 범주형 변수 한계, method 2: 비모수통계 for 연속변수 
tb1=mytable(STRESS_EXIST ~Sex + Age + Height + Weight + BMI + DM + HTN + Smoking + MACCE + Death + MACCE_date + Death_date + Number_stent, data=a)  
tb1 

mytable(STRESS_EXIST ~Sex + Age + Height + Weight + BMI + DM + HTN + Smoking + MACCE + Death + MACCE_date + Death_date + Number_stent, data=a, digits=2, max.ylev=6 )

mytable(STRESS_EXIST ~Sex + Age + Height + Weight + BMI + DM + HTN + Smoking + MACCE + Death + MACCE_date + Death_date + Number_stent, data=a, digits=2, max.ylev=6,method=2 ) ## Non-parametric: Median, 25%;75% 

ztable(tb1)


## Else
### Categorical variab
conti_var=c(3:6,12,13,15)                                       ## Age, Height, Weight, BMI, date, number_stent
cat_var= names(a)[-conti_var]                                   ## Not conti_var

cat_var

for (vn in conti_var){
  a[,vn]=as.numeric(a[,vn])
}

for (vn in cat_var){
  a[,vn]=as.factor(a[,vn])
}

summary(a)


### Sort
ord=order(a$BMI)         
head(a$BMI[ord])        
ord2=order(-a$BMI)      
tail(a$BMI[ord2])
b=a[ord,]       


### Subset
b=a[a$Age %in% 50:60,]                    
b=subset(a, Age %in% 50:60)                
b=subset(a, (Age >= 50 & Age <= 60))      ## Same  


### Merge
b=a[,1:6]                
c=a[,c(1,7:15)]           ##
head(b)
head(c)
d=merge(b,c,by=c("Patient_ID"), all=F)  