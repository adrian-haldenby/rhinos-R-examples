library(ggplot2)
library(directlabels)


#csv file -> pdf output
 
get_md <- function (path)
{
  data <- read.csv(path, header=TRUE,sep=",",row.names="X")
  data_matrix <- as.matrix(data)
  #scale the data across actions for store data
  data_multi <- data_matrix %*% t(data_matrix)
  #find the Euclidean distance between each of the properties
  data_distance <- dist(data_multi)
  #apply multi-dimentional scaling where:
  #   d is the distance matrix
  #   eig indicates if the eigen values should be returned - we choose false as only concerned with points
  #   k is the number of parameters we want to rescale to - we choose 2 to get a sense of closeness
  data.fit <-cmdscale(data_distance,k=2)
  #place the scaled data into a dataframe for graphing, retieve property names for labeling the graph  
  data.df <- as.data.frame(data.fit)
  names(data.df) <- c("x","y")
  #return the datafram with the MDS co-ordinates
  return(data.df)
}


#enter the path to your data file here
csv_path <- "~/TransBannerData.csv"

data.df <- get_md(csv_path)
#use ggplot to make pretty MDS diagrams - we get rid of the axis labels because they don't provide us with
# any extra interpritive value
data_plot <- ggplot(data.df, aes(x=x, y=y)) + geom_text(aes(x=x, y=y,label=rownames(data.df),col=rownames(data.df)),show_guide=FALSE)
data_plot <- data_plot + xlab("") + ylab("")
data_plot <- data_plot + opts(axis.ticks = theme_blank(), axis.text.x = theme_blank())
data_plot <- data_plot + opts(axis.ticks = theme_blank(), axis.text.y = theme_blank())

#save the plot to a png
png("mds-plot.png", width=645, height=640)
data_plot
dev.off()
