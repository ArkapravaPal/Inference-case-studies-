#case 1(var is known)
DU<-c(5000, 2000, 3000, 3456, 3623, 5200, 3400, 1200, 4500, 3500)
smean<-mean(DU)

install.packages("BSDA")
library(BSDA)
z.test(DU,mu=3500,sigma.x=sqrt(3),alternative="two.sided")
z.test(DU,mu=3500,sigma.x=sqrt(3),alternative="greater")

pnorm(smean,mean=3500,sd=sqrt(3),lower.tail = F)<0.05
pnorm(smean,mean=3500,sd=sqrt(3),lower.tail = T)<(0.05/2)

qnorm(0.05,mean=3500,sd=sqrt(3),lower.tail = F)

#var is unknown
pt(smean,df=9,lower.tail=T)<(0.05/2)
t.test(DU,mu=3500,alternative="two.sided")

pt(smean,df=9,lower.tail=F)<0.05
t.test(DU,mu=3500,alternative="greater")
#case3
teststat<-(sqrt(22)*(153.7-146.3))/17.2
pnorm(teststat,lower.tail = F)

#case4
mea<-c(2.5, 2.3, 2.4, 2.3, 2.5, 2.6, 2.5, 2.6, 2.6,2.7, 2.5)
variance<-1/0.16
teststatistics=(11-1)*var(mea)/variance
pchisq(teststatistics,df=10,lower.tail=T)<0.01

#case5

tstat<-(67.9-68.9)/(sqrt((1/1000)+(1/2000))*2.5)
pnorm(tstat,lower.tail = T)<(0.05/2)



#var not given(case 6)
sailors<-c(63, 65, 68, 69, 71, 72 )
soldiers<-c(61, 62, 65, 66, 69, 69, 70, 71, 72,73)
t.test(sailors,soldiers,alternative = "two.sided")


#related data(case 7)
v<-sleep
t.test(v$extra[1:10],v$extra[11:20],alternative = "two.sided",paired = T)
#case8
var.test(v$extra[1:10],v$extra[11:20])

#case9

View(ChickWeight)
fit <- aov(weight ~ Diet, data=ChickWeight)
summary(fit)
tapply(ChickWeight$weight,ChickWeight$Diet,"mean")



#non-parametric
#case10

height<-c(58,59,60,61,62,63,64,65,66,67,68,69,70,71,72)
weight<-c(115,117,120,123,126,129,132,135,139,142,146,150,154,159,164)
med<-median(height)
#height
sum<-sum(height>med)
pbinom(sum,size=length(height),p=0.5,lower.tail = F)<(0.05/2)
#weight
med1<-median(weight)
sum1<-sum(weight>med1)
pbinom(sum1,size=length(weight),p=0.5,lower.tail = F)<(0.05/2)

#case11
f<-read_excel("C:/Users/Arka/Documents/nonpara2.xlsx")
View(f)

games<-c(1:50)
outcomes<-c(1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,0,1,1,1,0,0,1,1,
            1,1,0,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,0,0,0,1,1,0,1,1,1)
data<-cbind(games,outcomes)
positives<-sum(outcomes)
negatives<-50-positives
library(BSDA)
SIGN.test(positives,negatives,alternative = "two.sided")


#case12
first_group<-c(227,176,252,149,16,55,234,194,247,92,184,147,88,161,171)
second_group<-c(202,14,165,171,292,271,151,235,147,99,63,284,53,228,271)

wilcox.test(first_group,second_group,alternative = "two.sided")


#case13

sample1<-c(0.075204597,0.282203071,0.473605304,0.171775727,0.084642496,
           0.601160542,0.212552515,0.294969478,0.026919861,0.054462148,
           0.076084169,0.021943532,0.486042232,0.083376869,0.62800881,
           1.317637268,0.431532897,0.151809043,0.645182388,0.018898663)
sample2<-c(1.319177696,0.255423126,0.250284353,0.941835437,3.078396099,
           0.270368067,0.413272132,0.05425652,1.340734424,0.127618122,
           0.060699583,0.208278913,0.104869289,1.126610877,1.179774988,
           2.015836491,0.43267859,0.686019322,1.210587738,0.230682213)
ks.test(sample1,sample2,alternative = "two.sided")

#case14

prof1<-c(79,87,24,41,59,12,91,78,63,30,09,64,50,92,64,39,49,86,23,45,12,88)
prof2<-c(83,91,18,39,67,34,78,89,38,45,10,45,56,89,67,35,40,82,32,38,23,92)
SIGN.test(prof1,prof2,alternative = "two.sided")

#case15
View(ChickWeight)
kruskal.test(weight~Diet,data=ChickWeight)

#case16
sample1<-c(1.089781309,1.962787672,1.724451834,1.63955842,0.144050286,0.232942589,
           1.68271611,3.633887711,1.81341443,1.683039558,1.659612162,0.8396626,
           3.427254188,1.127955432,1.552543896,0.214796062,0.475882672,3.013061127,
           2.73502768,2.583921184)
sample2<-c(0.046136443,0.296905535,0.013852846,0.149763684,0.216846562,0.549152735,
           0.075868307,0.147932045,0.29035859,0.027180583,0.163903305,0.8104371,
           0.078686029,0.153359897,0.141724322,0.066255849,0.085298693,0.507875983,
           0.104899753,0.020127363)
sample3<-c(4,2,1,1,2,3,4,3,3,4,3,1,8,4,5,2,4,5,0,5)
sample4<-c(10.25068427,10.12379195,17.81733259,18.87337658,15.32347378,11.97729205,
           16.79090476,14.40535435,14.10547096,14.99055234,12.68408943,10.62609998,
           15.59840961,17.59452935,10.60249139,17.17324608,11.59441059,14.11860911,
           19.68738385,17.20303417)
set.seed(100)
ks.test(sample1,rnorm(length(sample1)))
ks.test(sample2,rexp(length(sample2)))
ks.test(sample3,rpois(length(sample3),lambda=mean(sample3)))
ks.test(sample4,runif(length(sample4)))


#case17
employfema<-matrix(c(800,7200,120,1480),nrow=2)
chisq.test(employfema)