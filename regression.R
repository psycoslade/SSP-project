#Multiple liniar regression based on lifestyles-income no correlation
model1 <- lm(mean_income ~ adventure + pleasure + harmony + connection + rest + insight + style, data=DataFinal)
summary(model1)

plot1.1 <- ggplot(DataFinal, aes(x=adventure, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
plot1.2 <- ggplot(DataFinal, aes(x=pleasure, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
plot1.3 <- ggplot(DataFinal, aes(x=harmony, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
plot1.4 <- ggplot(DataFinal, aes(x=connection, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
plot1.5 <- ggplot(DataFinal, aes(x=rest, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
plot1.6 <- ggplot(DataFinal, aes(x=insight, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)
plot1.7 <- ggplot(DataFinal, aes(x=style, y=mean_income)) + 
  geom_point() +
  geom_smooth(method = lm)

######thisone is not fixed/updated
#plot1 <- ggplot(DataFinal, aes(mean_income)) + 
  geom_point(aes(y = adventure , colour = "Adventure")) + 
  geom_point(aes(y = pleasure , colour = "Pleasure")) +
  geom_point(aes(y = harmony , colour = "Harmony")) +
  geom_point(aes(y = connection , colour = "Connection")) +
  geom_point(aes(y = rest , colour = "Yellow")) +
  geom_point(aes(y = insight , colour = "Insight")) +
  geom_point(aes(y = style , colour = "Style"))
###


#Multiple liniar regression based on lifestyles-DurationOfStay. Corelation for certain lifestyles
model2 <- lm(Duration.of.Stay ~ adventure + pleasure + harmony + connection + rest + insight + style, data=DataFinal)
summary(model2)

plot2.1 <- ggplot(DataFinal, aes(x=adventure, y=Duration.of.Stay)) + 
  geom_point() +
  geom_smooth(method = lm)
plot2.2 <- ggplot(DataFinal, aes(x=pleasure, y=Duration.of.Stay)) + 
  geom_point() +
  geom_smooth(method = lm)
plot2.3 <- ggplot(DataFinal, aes(x=harmony, y=Duration.of.Stay)) + 
  geom_point() +
  geom_smooth(method = lm)
plot2.4 <- ggplot(DataFinal, aes(x=connection, y=Duration.of.Stay)) + 
  geom_point() +
  geom_smooth(method = lm)
plot2.5 <- ggplot(DataFinal, aes(x=rest, y=Duration.of.Stay)) + 
  geom_point() +
  geom_smooth(method = lm)
plot2.6 <- ggplot(DataFinal, aes(x=insight, y=Duration.of.Stay)) + 
  geom_point() +
  geom_smooth(method = lm)
plot2.7 <- ggplot(DataFinal, aes(x=style, y=Duration.of.Stay)) + 
  geom_point() +
  geom_smooth(method = lm)


######thisone is not fixed/updated
#plot2 <- ggplot(DataFinal, aes(Duration.of.Stay)) + 
  geom_point(aes(y = adventure , colour = "Adventure")) + 
  geom_point(aes(y = pleasure , colour = "Pleasure")) +
  geom_point(aes(y = harmony , colour = "Harmony")) +
  geom_point(aes(y = connection , colour = "Connection")) +
  geom_point(aes(y = rest , colour = "Yellow")) +
  geom_point(aes(y = insight , colour = "Insight")) +
  geom_point(aes(y = style , colour = "Style"))
###
  
  
#Multiple liniar regression based on duration.of.stay-Income. Negative corelation
model3 <- lm(Duration.of.Stay ~ mean_income, data=DataFinal)
summary(model3)

plot3 <- ggplot(DataFinal, aes(x=mean_income, y=Duration.of.Stay)) + 
  geom_point() +
  geom_smooth(method = lm)

plot1
plot1.1
plot1.2
plot1.3
plot1.4
plot1.5
plot1.6
plot1.7
plot2
plot2.1
plot2.2
plot2.3
plot2.4
plot2.5
plot2.6
plot2.7
plot3
