rm(list = ls())
source('prepare data.R')
require(ggplot2)
require(lm.beta)


vars <- list()
for (dep in deps){
  vars <- c(vars,sapply(seq(1,13,2), function(x) sprintf('%s.%s',dep,x)))
}
unlist(vars)

plot_means_histograms_wide <- function(wdata,time_range,var_list, year){
  start <- time_range[1]
  stop <- time_range[2]
  
  for (var in var_list){
    new_var <- sprintf('%s_all',var)
    wdata[new_var]<- rowSums(wdata[sapply(start:stop, function(x) sprintf('%s.%s',var,x))], na.rm = T)
    histogram <- qplot(wdata[new_var])  + scale_x_continuous(breaks = c(seq(0,20,1))) + xlab(var) + theme_bw() + geom_histogram(fill = '#870052')
    ggsave(sprintf('out/%s_%s_histogram.png', as.character(year),var), dpi = 300, plot = histogram)
    ftab <- table(wdata[new_var])
    write.table(ftab, sprintf('out/%s_freq%s.csv',as.character(year),var),sep = ',', row.names = F, col.names = c(var,'freq'))
  }
  
  all_m <- data.frame(start:stop)
  colnames(all_m) <- 'week'
  for (var in var_list){
    w_m <- c()
    for (w in start:stop){
      new_var <- sprintf('%s.%s',var,w)
      vec <- wdata[[new_var]][!is.na(wdata[new_var])]
      m <- mean(vec)
      sem <-sd(vec)/sqrt(length(vec))
      w_m <- rbind(w_m,c(w,m,sem))
    }
    colnames(w_m) <- c('week',var,sprintf('se_%s',var))
    w_m <- data.frame(w_m)
    all_m <- merge(all_m,w_m,by = 'week')
    write.table(all_m,file = sprintf("out/%s_means.csv",year),sep = ',', row.names = F)
  }
  
  
  for (var in var_list){
    mplot <- ggplot(data = all_m, aes(x=week, y=get(var)),ylim(c(0,1))) + geom_point(colour = 'red') + theme_bw() + geom_line(colour = 'red') + ylab(var) +
      geom_errorbar(colour = 'red',width=.1,aes(ymin=get(var)-1.96*get(sprintf("se_%s",var)), ymax=get(var)+1.96*get(sprintf("se_%s",var)))) + 
      scale_y_continuous(breaks=c(seq(0,1,.2)), limits = c(0,1)) + scale_x_continuous(breaks=c(0,2,4,6,8,10,12), limits = c(0,13))
    ggsave(sprintf('out/%s_%s_mean_plot.png', year, var), dpi = 300, plot = mplot)
  }
}


year <- 'joined'
time_range <- c(0,13)

rdata <- cdata[which(cdata['from'] == year),]
rdata <- cdata


wdata <- l.to.w(rdata,'kod_id','week')

wdata['from']<- rowMeans(wdata[sapply(0:13, function(x) sprintf('%s.%s','from',x))], na.rm = T)




#plot_means_histograms_wide(wdata,time_range,indeps,year)


deps <- c('nsfs_c_mean','nsfs_r_mean','roleclarity_mean')
for (dep in deps){
  vars <- sapply(c(7,9,11), function(x) sprintf('%s.%s',dep,x))
  wdata[dep] <- rowMeans(wdata[vars], na.rm = T)
}
file.create('temp.csv')

fdata <- wdata
wdata['from2016'] <- NA
wdata['from2016'][which(wdata['from'] == 2015),] <- 0
wdata['from2016'][which(wdata['from'] == 2016),] <- 1

indeps <- c('ex_jobsat_mean.13','ex_org_com_mean.13','nse_mean.13','ex_itl_mitlw_mean.13',
            'ex_itl_mitlp_mean.13','olbi_mean.13','fitperc_mean.13','emot_anx_mean.12',
            'emot_dep_mean.12','seq_stress_mean.12','seq_energy_mean.12')
out <- c()
for (var in indeps){
  s <- lm(get(var) ~ nsfs_c_mean + nsfs_r_mean + roleclarity_mean + from2016, data = wdata)
  ss <- summary(lm.beta(s))
  n <- nobs(s)
  fstat <- ss$fstatistic
  f <- round(fstat[['value']],2)
  p <- pf(fstat[1],fstat[2],fstat[3],lower.tail=F)
  r2 <- round(ss$r.squared,2)
  cs <- coef(ss)[2:5,2:5]
  c <- t(sapply(1:4, function(x) as.character(round(c(cs[x,1],cs[x,2],cs[x,3]),2))))
  for (x in 1:4 ){
    px <- ''
    if (cs[x,4] < .05){
      px <- '*'
    }
    if (cs[x,4] < .01){
      px <- '**'
    }
    if (cs[x,4] < .001){
      px <- '***'
    }
    c[x,1] <- sprintf('%s%s',c[x,1],px)
  }
  preds <- c(c[1,],c[2,],c[3,],c[4,])
  out_temp <- c(var,n,f,p,r2,preds)
  #cbind(n,f,p,r2,c[1,],c[2,],c[3,],c[4,])
  #out_temp <- cbind(n,f,round(p,4),r2,preds)
  out <- rbind(out,out_temp)
}


colnames(out) <- c('Dependent','N','F','P','R2','nsfs_c_mean_beta','SE','t','nsfs_r_mean_beta','SE','t','roleclarity_mean_beta','SE','t','from2016_beta','SE','t')
write.csv(out,'reg.csv',row.names = F)
length(c('N','F','P','R2','nsfs_c_mean_beta','SE','t','nsfs_r_mean_beta','SE','t','roleclarity_mean_beta','SE','t','from2016_beta','SE','t'))
ss
s


all_outs <- c(var_list,deps,indeps)
pvals <- c()
for (var in all_outs){
  if (var %in% var_list){
    new_var <- sprintf('%s_all',var)
    wdata[new_var]<- rowSums(wdata[sapply(0:13, function(x) sprintf('%s.%s',var,x))], na.rm = T)
  } else {new_var <- var}
  t <- t.test(x=wdata[new_var][which(wdata['from'] == 2015),],y=wdata[new_var][which(wdata['from'] == 2016),])
  m <- t$estimate
  sd <- c(sd(wdata[new_var][which(wdata['from'] == 2015),],na.rm = T),sd(wdata[new_var][which(wdata['from'] == 2016),],na.rm = T))
  pvals <- rbind(pvals,c(var,round(t$p.value,99),m[1],sd[1],m[2],sd[2]))
}
colnames(pvals) <- c('variabel','p-val','m-2015','sd-2015','m-2016','sd-2016')
pvals
write.csv(pvals,file = 't.test.csv')
all_outs
sdata<- wdata[all_outs[4:length(all_outs)]]
cormat <- round(corr.test(sdata)[[1]],3)
write.csv(cormat,file = 'cormat.csv')
#write.csv(out,'temp.csv')
m <- mean(wdata[[new_var]], na.rm = T)
sd <- sd(wdata[[new_var]], na.rm = T)
new_var<-'nsfs_c_mean'
for (new_var in deps){
  m <- round(mean(wdata[[new_var]], na.rm = T),2)
  sd <- round(sd(wdata[[new_var]], na.rm = T),2)
  histogram <- qplot(wdata[new_var])  + scale_x_continuous(breaks = c(seq(0,20,1))) + xlab(new_var) + 
    theme_bw() + geom_histogram(fill = '#870052', colour = 'black', binwidth = .5) + annotate("text", label = sprintf("m=%s\nsd=%s",m,sd), colour = 'Dark blue', x=1,y=Inf, vjust=3, )
    
  histogram
  ggsave(sprintf('out/%s_%s_histogram.png', as.character(year),new_var), dpi = 300, plot = histogram)
}

