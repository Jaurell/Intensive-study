source('prepare data.R')

scales <- read.csv('scales.csv')

year <- c(2016)
vars <- paste(as.vector(scales[['scale']]),'_mean', sep = '')
ydata <- cdata[which(cdata[['from']] %in% year),]
#samp <- ydata[[id]]
samp <- sample(ydata[[id]],49)
for (var in vars){
  require(ggplot2)
  y <- paste(as.character(c(year)),collapse = '_')
  cdata2 <-ydata[c(id,time,var,'from')][which(ydata[[id]] %in% samp),]
  cdata2 <- cdata2[which(cdata2[['from']] %in% year),]
  if (!all(is.na(cdata2[[var]]))){
    breaks <- ceiling(c(max(cdata2[var],na.rm = T)/2,max(cdata2[var],na.rm = T)))
    sp <- ggplot(cdata2, aes(x=get(time),y=get(var))) + geom_point(size = .5) + 
      geom_line(data = cdata2[!is.na(cdata2[[var]]),]) + facet_wrap(~ kod_id) + 
      theme(strip.background = element_blank(), strip.text.x = element_blank()) +
      ylab(var) + xlab(time) + scale_y_continuous(breaks = breaks)
  sp
    ggsave(sprintf('out/%s_linegrid_%s.png',y,var), dpi = 300, plot = sp)
  }
}
