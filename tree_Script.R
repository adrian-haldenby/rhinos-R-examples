library(rpart)

#enter the path to your data file here
csv_path <- "~/FBPostDataPos.csv"

#read the data
data.raw <- read.csv(csv_path,header=TRUE,colClasses=c("character","numeric","factor","numeric","numeric","numeric"))


#Accepts a dataframe and produces several plots
get_tree <- function(df){
  #fit the tree to the data with anova parameter for regression tree
  fit_tree<- rpart(clicks~type+chars+time,method="anova",data=df)
  #Print the tree to a png
  png("dec-tree.png", width=645, height=640)
  plot(fit_tree)
  text(fit_tree,use.n=T)
  dev.off()
  #View the cross validation results and the r-squared statistic
  png("dec-tree-crossvalidation.png", width=645, height=640)
  #add two plots to a page
  par(mfrow=c(1,2))
  rsq.rpart(fit_tree)
  dev.off()
  #Print a prettier version of the tree in a post script file
  post(fit_tree, file = "dec-tree-pretty.ps", title = "Decision Tree Results")
}

get_tree(data.raw)