library(car)

#enter the path to your data file here
csv_path <- "~/TransBannerData.csv"

#import the data
FB<-read.csv(csv_path,stringsAsFactors=FALSE)
#fill the missing values that apear as N/A with 0s
FB[is.na(FB)]<-0

#save the hi-res scatterplot
png('social_correlation_matrix.png',width=1600,height=1400,res=120)
scatterplotMatrix(FB[,-1])
dev.off()

#assign the most interesting variables
TotalRefs <- FB[,9]
FriendViews <- FB[,10]
Likes <- FB[,11]

#run ols regression
res <- lm(Likes~TotalRefs+FriendViews)
summary(res)
