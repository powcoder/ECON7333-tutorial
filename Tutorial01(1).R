# Exercise 8

# (a)
college = read.csv(file="College.csv", header=TRUE, sep=",")

# (b)
rownames (college )=college [,1]
fix(college)

college = college [,-1]
fix(college)

# (c) i.
summary(college)  

# (c) ii.
pairs(college[,1:10])  

# (c) iii.
plot(college$Private, college$Outstate)


# (c) iv.
Elite = rep("No",nrow(college ))
Elite[college$Top10perc >50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college ,Elite)

summary(Elite)

plot(Elite, college$Outstate)

# (c) v.
par(mfrow=c(2,2))
hist(college$Outstate, nclass=4)
hist(college$Outstate, nclass=10)
hist(college$Outstate, nclass=20)
hist(college$Outstate, nclass=100)

# (c) vi.


