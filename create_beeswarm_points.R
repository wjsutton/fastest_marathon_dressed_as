
library(ggplot2)
library(ggbeeswarm)
library(dplyr)

dressed_as <- read.csv("data/fastest_marathon_dressed_as.csv",stringsAsFactors = F)

full <- filter(dressed_as,type=='full')
half <- filter(dressed_as,type=='half')

# Test chart outputs
#ggplot(full,aes(1,total_time_in_secs)) + geom_beeswarm(dodge.width=.8,cex=2)
#ggplot(half,aes(1,total_time_in_secs)) + geom_beeswarm(dodge.width=.8,cex=2.5)



# Build plots for full & half
# then extract the x co-ordinate data
h <- ggplot(half,aes(1,total_time_in_secs)) + geom_beeswarm(dodge.width=.8,cex=2)
hg <- ggplot_build(h)
h_bee <- data.frame(half,x=hg$data[[1]]$x, stringsAsFactors = F)

f <- ggplot(full,aes(1,total_time_in_secs)) + geom_beeswarm(dodge.width=.8,cex=2.5)
fg <- ggplot_build(f)
f_bee <- data.frame(full,x=fg$data[[1]]$x, stringsAsFactors = F)

data_bee <- rbind(f_bee,h_bee)

write.csv(data_bee,"data/fastest_marathon_dressed_as_beeswarm.csv", row.names = F)

