####################################################################
### Sensitivity analysis for indivdual parameters to the surface area
### ratio (RSA) as well as to where RSA = 1
###
### Author: John Gardner
### Contact: johngardner87@gmail.com
### Associated publication:
###
###
####################################################################

### load packages needed, may need to install.packages()
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(scales)

### Define parameters as constants

## parameters that predict river depth (m)
a=0.1
b=0.5
## parameters that predict river width (m)
c=1
d=.4
## parameters for suspended sediment concentration (Kg/m3)
e=0.01
f=1.5

## Discharge (m3/s)
Q=100

## benthic sediment diameter (m)
rb=0.05
## suspended sediment diameter (m)
rs=0.0005

## Sediment porosity
o=0.4764
## Hyporheic depth (m)
H=0.1
#sediment density (Kg/m3)
p=2650


###########################################
### Sensitivity to Discharge Q, holding all others constant

## set up range of discharge
QO<-c(0.01, 0.1, 1, 10, 100, 1000, 10000, 85000)
## calculate RSA
RsaQ<- (((a*c*e)*QO^(b+d+f))*rb ) / (p*H*((a*QO^b) + 2*(c*QO^d)) * rs * (1-o))
rsaQ<-cbind.data.frame(QO, RsaQ)

## Make plot
p1<-ggplot(data=rsaQ, aes(x=QO, y=RsaQ)) +
  geom_line(size=2) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits=c(10^-6, 10^6)) +
  theme_few()+
  xlab(expression(Q~(m^3~s^-1)))+
  ylab(expression(R[SA])) 
 
###########################################
### Sensitivity to hyporheic depth H, holding all others constant

## set up range of hyporheic depths
HO<-c(0.005, 0.01, 0.05, 0.1, 0.5, 1, 3.5)
## calculate RSA
RsaH<- (((a*c*e)*Q^(b+d+f))*rb ) / (p*HO*((a*Q^b) + 2*(c*Q^d)) * rs * (1-o))
rsaH<-cbind.data.frame(HO, RsaH)

## make plot
p2<-ggplot(data=rsaH, aes(x=HO, y=RsaH)) +
  geom_line(size=2) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=3),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits=c(10^-6, 10^6)) +
  theme_few()+
  xlab("H (m)")+
  ylab(expression(R[SA])) 

###########################################
### Sensitivity to benthic sediment size rb, holding all others constant


## vary rb
rbO<-c(0.0005, 0.001, 0.005, 0.01, 0.05, 0.15)
## calculate RSA
Rsarb<- (((a*c*e)*Q^(b+d+f))*rbO ) / (p*H*((a*Q^b) + 2*(c*Q^d)) * rs * (1-o))
rsarb<-cbind.data.frame(rbO, Rsarb)

## plot
p3<-ggplot(data=rsarb, aes(x=rbO, y=Rsarb)) +
  geom_line(size=2) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=3),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits=c(10^-6, 10^6)) +
  theme_few()+
  xlab(expression(D["50b"]~(m))) +
  ylab(expression(R[SA])) 

###########################################
### Sensitivity to suspended sediment size rs, holding all others constant

## vary rs
rsO<-c(2.5*10^-7, 2.5*10^-6, 2.5*10^-5, 2.5*10^-4)

## calculate RSA
Rsars<- (((a*c*e)*Q^(b+d+f))*rb ) / (p*H*((a*Q^b) + 2*(c*Q^d)) * rsO * (1-o))
rsars<-cbind.data.frame(rsO, Rsars)

## plot
p4<-ggplot(data=rsars, aes(x=rsO, y=Rsars)) +
  geom_line(size=2) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=3),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits=c(10^-6, 10^6)) +
  theme_few()+
  xlab(expression(D["50s"]~(m))) +
  ylab(expression(R[SA])) 

###########################################
### Sensitivity to exponents for changind river width and depth
## (b, d), holding all others constant

## vary b width and d depth
bO<-seq(0.05, 0.8, by=0.01)
dO<-1-0.2-bO

## Calculate RSA
Rsab<- (((a*c*e)*Q^(bO+dO+f))*rb ) / (p*H*((a*Q^bO) + 2*(c*Q^dO)) * rs * (1-o))
rsab<-cbind.data.frame(bO,dO, Rsab)

## plot
p5<-ggplot(data=rsab, aes(x=bO/dO, y=Rsab)) +
  geom_line(size=2) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=3),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits=c(10^-6, 10^6)) +
  theme_few()+
  xlab("b/d") +
  ylab(expression(R[SA])) 

###########################################
### Sensitivity to suspended sediment rating curve f, holding all others constant

## vary f
fO<-seq(0.1, 3.5, by=0.1)

## calculate RSA
Rsaf<- (((a*c*e)*Q^(b+d+fO))*rb ) / (p*H*((a*Q^b) + 2*(c*Q^d)) * rs * (1-o))
rsaf<-cbind.data.frame(fO, Rsaf)

## plot
p6<-ggplot(data=rsaf, aes(x=fO, y=Rsaf)) +
  geom_line(size=2) +
  scale_x_continuous(breaks = pretty_breaks(n=3)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits=c(10^-6, 10^6)) +
  theme_few()+
  xlab("f") +
  ylab(expression(R[SA])) 
#plot(log10(fO), log10(Rsaf), type="l")

###########################################
### Sensitivity to sediment porosity, holding all others constant

## vary porosity o
oO<-seq(0.2, 0.8, by=0.01)

## calculate RSA
Rsao<- (((a*c*e)*Q^(b+d+f))*rb ) / (p*H*((a*Q^b) + 2*(c*Q^d)) * rs * (1-oO))
rsao<-cbind.data.frame(oO, Rsao)

## plot
p7<-ggplot(data=rsao, aes(x=oO, y=Rsao)) +
  geom_line(size=2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits=c(10^-6, 10^6)) +
  theme_few()+
  xlab(expression(Phi)) +
  ylab(expression(R[SA])) 

###########################################
### Sensitivity to sediment density, holding all others constant

## vary p 1.2-4 (buffington and mont 1997)
pO<-seq(1200, 4000, by=0.1)
## calculate RSA
Rsap<- (((a*c*e)*Q^(b+d+f))*rb ) / (pO*H*((a*Q^b) + 2*(c*Q^d)) * rs * (1-o))
rsap<-cbind.data.frame(pO, Rsap)

## plot
p8<-ggplot(data=rsap, aes(x=pO, y=Rsap)) +
  geom_line(size=2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits=c(10^-6, 10^6)) +
  theme_few()+
  xlab(expression(rho~(Kg~m^-3))) +
  ylab(expression(R[SA])) 

## plot all 
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol=3, nrow=3, labels="AUTO")


##############################################################
### Sensitivity to where RSA = 1

## set up site specific parameters as constants
a=0.1
b=0.5

c=1
d=0.4

e=0.01

o=0.4764

p=2650

## set up vector of distance downstream (km)
x<-c(1,10,50,100,500,1000,2000,3000)
rev.x<-rev(x)

##### set up river specific and global params as constants
## most upstream benthic sediment size (m)
 Do<-0.01

## benthic sediment fining exponent (m per km)
 alpa<-0.001

## suspended sedimetn fining rate (m per km)
 Bss<-10^-9

## scaling param for hyporheic depth with slope
 BS<-15 #15
 
## scaling param for hyporheic depth with benthic sed size
 BD<-20  #20

## set up dataframe of all combination of parameters
params<-expand.grid(alpha=c(0.0001, 0.001, 0.015), Do=c(0.01, 0.15), 
                    Bss=c(10^-8.5, 10^-9.5, 10^-10.5), BS=c(5,15,25), 
                    BD=c(10,20,30), f=c(0.5, 1.25, 2.5))

## set up dataframe with all params combinations along a river
## ignore the warnings generated by this loop.
data.ls<-list()
for(i in 1:nrow(params)){
  
  data.ls[[i]]<-cbind.data.frame(x, rev.x, params[i,])
}
data<-do.call("rbind.data.frame", data.ls)

## calculate benthic D50 along a river and constrain the min size
data$D50<-(data$Do*exp(-data$alpha*x))
data$D50[data$D50 < 0.0001] <- 0.0001

## calculate discharge (Q) and slope (S) along the river as function of 
##distance dowstream (Leopold and Maddock 1957)
data$Q<-((4.85*10^-8) * data$x^2.16) *(10^9 / (24*60*60))
data$S<- 0.0005*data$Q^-0.4

## calculate rsa
data$rsa<- with(data, (((a*c*e)*Q^(b+d+f)) * (D50)) / (p*((Do/1000)- (Bss*x)) * ((BD*D50) + (BS*S)) 
                                                    * ((a*Q^b) + 2*(c*Q^d)) * (1-o)))
data<-data[data$rsa >0,]

## create unqiue ID for each scenario
data$run<-with(data, as.character(paste(alpha,Do,Bss,BS,BD,f,sep=".")))
## fix labels
data$f<-as.factor(data$f)
levels(data$f) <- c("f=0.5", "f=1.25", "f=2.5")

## Plot all
qplot(x, (rsa), group=as.factor(run), geom="line",color=as.factor(BD), linetype=as.factor(Do),
      facets=~f, data=data) +
      facet_wrap(~f) +
      theme_few() +
      geom_hline(yintercept = 1, linetype=2) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      xlab("Distance downstream (km)") +
      ylab(expression(R[SA])) +
      guides(color=guide_legend(title=expression(beta[D])), linetype=guide_legend(title=expression(D[o]~(m))))


### END
###############################################################################################



