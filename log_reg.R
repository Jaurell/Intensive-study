source('prepare data.R', encoding = 'utf-8')
source('functions.R')

wdata <- l.to.w(cdata,id,time, invariants = 'from')
dist2 <- sapply(distal_outcomes, paste0, paste0('.',seq(12,13)))
prox2 <- sapply(proximal_outcomes, paste0, paste0('.',seq(7,11)))
dist2 <- dist2[which(dist2 %in% colnames(wdata))]
prox2 <- prox2[which(prox2 %in% colnames(wdata))]

for (var in proximal_outcomes){
  vars <- sapply(c(7,9,11), function(x) sprintf('%s.%s',var,x))
  wdata[var] <- rowMeans(wdata[vars], na.rm = T)
}
prox
for (var in c(dist2,proximal_outcomes)){
  new_var <- sprintf('%s_median',var)
  median <- median(wdata[[var]], na.rm = T)
  wdata[new_var] <- NA
  wdata[new_var][which(wdata[var] > median),] <- 1
  wdata[new_var][which(wdata[var] <= median),] <- 0
}

wdata['from2016'] <- NA
wdata['from2016'][which(wdata['from'] == 2015),] <- 0
wdata['from2016'][which(wdata['from'] == 2016),] <- 1

out <- c()
for (var in dist2){
  var <- sprintf('%s_median',var)
  s <- glm(get(var) ~ nsfs_c_mean_median + nsfs_r_mean_median + roleclarity_mean_median + from2016 -1 , data = wdata, family = 'binomial') # both split
  #s <- glm(get(var) ~ nsfs_c_mean + nsfs_r_mean + roleclarity_mean + from2016 -1 , data = wdata, family = 'binomial') # only dependet split
  #ss <- summary(lm.beta(s))
  ss <- summary(s)
  n <- nobs(s)
  #fstat <- ss$fstatistic
  #f <- round(fstat[['value']],2)
  #p <- pf(fstat[1],fstat[2],fstat[3],lower.tail=F)
  #r2 <- round(ss$r.squared,2)
  cs <- coef(ss)
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

  OR <- round(exp(cbind(OR = coef(s), confint(s))),2)
  preds <- c(c[1,],OR[1,],c[2,],OR[2,],c[3,],OR[3,],c[4,],OR[4,])
  out_temp <- c(var,n,preds)
  #cbind(n,f,p,r2,c[1,],c[2,],c[3,],c[4,])
  #out_temp <- cbind(n,f,round(p,4),r2,preds)
  out <- rbind(out,out_temp)
}
colnames(out) <- c('Dependent','N','nsfs_c_median_beta','SE','z','OR','lower','upper','nsfs_r_median_beta','SE','z','OR','lower','upper','roleclarity_median_beta','SE','z','OR','lower','upper','from2016_beta','SE','z','OR','lower','upper')
write.csv(out,'reg.csv',row.names = F)
