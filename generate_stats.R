require(SimInf)
require(dplyr)
require(glue)
require(rapportools)

args = commandArgs(trailingOnly=TRUE)

dens = args[1] # density
j = as.numeric(args[2]) # app use
C0 = args[3] # contact rate
v = as.numeric(args[4]) # velocity

outfile <- paste0('www/j_',j,'_c0_',C0,'_v_',v,'_dens_',dens,'.png')
stats_file <- paste0('stats/j_',j,'_c0_',C0,'_v_',v,'_dens_',dens,'.txt')

# data preparation
Prov_pop=read.csv("data/Popolazione.csv", sep=";")
Prov_pop$Provincia=sapply(Prov_pop$Provincia, trim.space, what="both")
Prov_pop$Ettari=Prov_pop$Superficie..kmq./0.01
Prov_pop$Density=Prov_pop$Residenti/(Prov_pop$Superficie..kmq.*1000)

Data <-Prov_pop
Data$Density=Data$Residenti/Data$Ettari

Tot_Dens=(sum(Prov_pop$Residenti)/sum(Prov_pop$Ettari))

# CONSTANTS:

transitions <- 
  c("S -> (PI*(1-pa)*      c0*Density*S*(I)*(1-J*j*j)+
            PI*(1-pa)*f*   c0*Density*S*(A)+
            PI*(1-pa)*f*   c0*Density*S*(P)*(1-J*j*j)+
              PI*(1-pa)*             c0*Density*S*(QI)*q_0+
                PI*(1-pa)*f*  c0*Density*S*(QP+QA)*q_)
                /(S+I+R+A+QA+QP+QS+QI+P) 
                  -> P",
    
    "S -> (PI*pa*             c0*Density*S*(I)*(1-J*j*j)+
            PI*pa*f*   c0*Density*S*(A)+
            PI*pa*f*   c0*Density*S*(P)*(1-J*j*j)+
              PI*pa*             c0*Density*S*(QI)*q_0+
                PI*pa*f*  c0*Density*S*(QP+QA)*q_)
                /(S+I+R+A+QA+QP+QS+QI+P) 
                  -> A+Acum", 
      
    "S -> eff*test*(1-(PI*(1-pa))-PI*pa)*  c0*Density*S*(I)*J*j*j/
              (td*(S+I+R+A+QA+QP+QS+QI+P)) -> QS+Qcum+T",
    "S -> eff*PI*(1-pa)*           c0*Density*S*(I)*J*j*j/
              (td*(S+I+R+A+QA+QP+QS+QI+P)) -> QP+QIcum+Qcum",
    "S -> eff*PI*    pa*           c0*Density*S*(I)*J*j*j/
              (td*(S+I+R+A+QA+QP+QS+QI+P)) -> QA+Qcum",

    
    "P -> P/k -> I+Icum", 
    "P -> eff*c0*Density*P*(I)*J*j*j/
              (td*(S+I+R+A+QA+QP+QS+QI+P)) -> QP+QIcum+Qcum",
 
    "I -> J*I/td -> QI+Qcum",
    "I -> I/th -> R", 
    "I -> m*I -> D",
    
    "A -> A/th -> R", 
    "A -> eff*c0*Density*A*(I)*J*j*j/
              (td*(S+I+R+A+QA+QP+QS+QI+P)) -> QA+Qcum",
    
    "QI -> QI/th -> R", 
    "QI -> m*QI -> D",
    
    "QP -> QP/k -> QI+Icum", 
    
    "QA -> QA/th -> R", 
    
    "QS -> QS/tq -> S")


compartments <- c("S", "I" ,"Icum", "R", "A" ,"Acum","QA", "QP","QS", 
                  "QI","Qcum", "P", "D", "T","QIcum")




###

nsim = 1

node_data=NULL
if (dens=='NA') {
  node_data=data.frame(rep(Prov_pop$Density, nsim), rep(Prov_pop$Ettari, nsim))
  names(node_data)=c("Density","Ettari")
} else {
  node_data=data.frame(rep(rep(as.numeric(dens), nrow(Prov_pop)), nsim), rep(Prov_pop$Ettari, nsim))
  names(node_data)=c("Density","Ettari")
}


nodes=rep(1:nrow(Prov_pop), nsim)
sim=rep(1:nsim, each=nrow(Prov_pop))
n <- nsim*nrow(Prov_pop)

startP = 100
u0 <- data.frame(S = rep(Prov_pop$Residenti, nsim), I = rep(startP*0.2, n),Icum = rep(0, n), R = rep(0, n),
                 P = rep(startP*0.8,n), QA=(rep(0,n)),QP=(rep(0,n)),QS=(rep(0,n)),
                 QI=(rep(0,n)),Qcum=(rep(0,n)),QIcum=(rep(0,n)), A=(rep(0,n)), Acum=(rep(0,n)),
                 D=(rep(0,n)),T=(rep(0,n)))

span = c(seq(1,50, by=2), seq(51,100, by=5), seq(101,600, by=20))

R = 1
if (C0!='NA') {
	C0=as.numeric(C0)
}else{
	C0=8*R*v/pi
}

model <- mparse(transitions = transitions, compartments = compartments,
                gdata = c(c0 = C0,
                          PI = 0.1, pa = 0.4, 
                          j = j, th = 12, tq = 15, test = 1, 
                          k = 5.1-2, td = 2, J = 0.75, eff = 1, q_ = 0.1, q_0 = 0,
                          m = 0.01/8, f = 0.1),
                ldata = node_data,
                u0 = u0, tspan = span)

set.seed(123)
result <- run(model = model)
data_out=trajectory(model = result)

data_out=cbind(nodes, sim, data_out)

data_out_G_app_=aggregate(cbind( S ,    I ,    R,     A  ,  
                            	Acum,  QA  ,  QP   , QS  ,  QI ,   Qcum,  P ,    D,    
                            	Icum,  T, Qcum)~time+sim, data=data_out, FUN=sum)

data_out_G_app=aggregate(cbind( S ,    I ,    R,     A  ,  
                           		Acum,  QA  ,  QP   , QS  ,  QI ,   Qcum,  P ,    D,    
                           		Icum,  T, Qcum)~time, data=data_out_G_app_, FUN=mean)

data_std=aggregate(cbind( S ,    I ,    R,     A  ,  
                     		Acum,  QA  ,  QP   , QS  ,  QI ,   Qcum,  P ,    D,    
                     		Icum,  T, Qcum)~time, data=data_out_G_app_, FUN=sd)


lst <- list(data_out_G_app$time,data_out_G_app$I+data_out_G_app$QI,data_out_G_app$D)

#options(scipen=999)
#png(outfile)
#plot(lst[[1]], lst[[2]], type="l", col="red", xlab="Time (days)", ylab="Subjects", lwd=4, ylim=c(0,100000))
#lines(lst[[1]], lst[[3]], col="black", lwd=4)
#abline(v=lst[[1]][which(lst[[2]]==max(lst[[2]]))],col="blue",lty="dashed")
#abline(h=max(lst[[3]]),col="blue",lty="dashed")
#dev.off()

write.table(cbind('time'=lst[[1]],'subjects'=lst[[2]],'deaths'=lst[[3]]),stats_file,quote=F,row.names=F,sep='\t')