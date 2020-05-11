library(data.table)

args = commandArgs(trailingOnly=TRUE)

dens = args[1] # density
j = as.numeric(args[2]) # app use
c0 = args[3] # contact rate
v = as.numeric(args[4]) # velocity

pattern <- paste0('j_.*_c0_',c0,'_v_',v,'_dens_',dens,'.txt')
j_range <- c(0,0.25,0.5,0.75)

filenames <- list.files("stats",pattern=pattern,full.names=TRUE)
list.DFs <- lapply(filenames,fread)

png(paste0('www/j_',j,'_c0_',c0,'_v_',v,'_dens_',dens,'.png'), width = 480*2, height = 480)
op <- par(ps=18, mfrow=c(1,2), mar=c(5, 5, 9, 3))
options(scipen=999)

#Subjects
y_max <- max(unlist(lapply(list.DFs, function(x) max(x$subjects))))
x_max <- 300
plot(0,0,type='n',ylim=c(0,y_max),xlim=c(0,x_max), ylab='Subjects', xlab='Time (days)')
lapply(1:length(list.DFs), function(x) lines(list.DFs[[x]]$time,list.DFs[[x]]$subjects, lty=x, col=rgb(t(col2rgb('red')),alpha=50,maxColorValue=255), lwd=3))
lines(list.DFs[[which(j_range==j)]]$time, list.DFs[[which(j_range==j)]]$subjects, lty=which(j_range==j), col='red', lwd=3)
title("Population in the I and QI compartments",cex=0.8)
legend("top", inset = c(0, -0.15), title="Population using the app", legend=c('0%','25%','50%','75%'),
       col='red', lty=1:4, box.lty=0, horiz=TRUE, cex=0.8, xpd=TRUE)
#Deaths
y_max <- max(unlist(lapply(list.DFs, function(x) max(x$deaths))))
x_max <- 300
plot(0,0,type='n',ylim=c(0,y_max),xlim=c(0,x_max), ylab='Subjects', xlab='Time (days)')
lapply(1:length(list.DFs), function(x) lines(list.DFs[[x]]$time,list.DFs[[x]]$deaths, lty=x, col=rgb(t(col2rgb('black')),alpha=50,maxColorValue=255), lwd=3))
lines(list.DFs[[which(j_range==j)]]$time, list.DFs[[which(j_range==j)]]$deaths, lty=which(j_range==j), col='black', lwd=3)
title("Simulated mortality",cex=0.8)
legend("top", inset = c(0, -0.15), title="Population using the app", legend=c('0%','25%','50%','75%'),
       col='black', lty=1:4, box.lty=0, horiz=TRUE, cex=0.8, xpd=TRUE)

par(op)
dev.off()