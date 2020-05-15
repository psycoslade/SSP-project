#Multiple liniar regression based on lifestyles-income no correlation
model1 <- lm(mean_income ~ adventure + pleasure + harmony + connection + rest + insight + style, data=data)
summary(model1)


#Multiple liniar regression based on lifestyles-DurationOfStay. Corelation for certain lifestyles
model2 <- lm(Duration.of.Stay ~ adventure + pleasure + harmony + connection + rest + insight + style, data=data)
summary(model2)


#Multiple liniar regression based on duration of stay-Income. Negative corelation
model3 <- lm(Duration.of.Stay ~ mean_income, data=data)
summary(model3)


