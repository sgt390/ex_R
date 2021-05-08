college <- read.csv("College.csv")

rownames(college) <- college[,1]
fix(college)
college = college[,-1]
fix(college)

summary(college)
college$Private <- as.factor(college$Private)
is.factor(college$Private)
pairs(college[,1:10])


#plot
plot(college$Outstate, college$Private)

