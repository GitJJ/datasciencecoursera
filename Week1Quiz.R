rd <- read.csv("hw1_data.csv")
x <- is.na(rd$Ozone)
y <- rd$Ozone[x]
mean(rd$Ozone, na.rm=TRUE)

extract1 <- rd[which((rd$Ozone>31) & (rd$Temp>90)),]
mean(extract1$Solar.R)

x <- rd[rd$Ozone>31,]
y <- x[x$Temp > 90,]
mean(y$Solar.R, na.rm=TRUE)

month6 <- rd[which(rd$Month==6),]
mean(month6$Temp)

month5 <- rd[which(rd$Month==5),]

max(month5$Ozone, na.rm=TRUE)

#Don't forget that you can, temporarily, leave the lesson by typing play() and then return by typing nxt().

#dir.create(file.path('testdir2', 'testdir3'), recursive = TRUE) will do the trick. If you forgot the recursive argument, the
#| command may have appeared to work, but it didn't create the nested directory.

> dir.create(file.path("testdir2", "testdir3"), recursive = TRUE)
