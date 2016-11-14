##turns long file into wide
l.to.w <- function(ldata, id, time, dropvar = NULL, invariants = NULL){
  ldata.sorted <- ldata[with(ldata, order(get(time), get(id))), ]
  w <- reshape(ldata.sorted,
               timevar = time,
               idvar = c(id,invariants),
               drop = dropvar,
               direction = "wide")
  w <- w[!sapply(w, function(x) all(is.na(x)))]
  return(w)
}

## Plot mean_plots and frequency histograms. write means and frequencies to csv
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

