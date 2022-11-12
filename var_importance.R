# Get dataframe of points within augsburg
betlocspaugdf <- as.data.frame(betlocspaug)
betlocspaugdf <- betlocspaugdf[, -c(1:2, 6, 41:42)]

# normalize response variable
betlocspaugdf$Bet_v_1 <- sqrt(sqrt(sqrt(sqrt(betlocspaugdf$Bet_v_1))))
shapiro.test(betlocspaugdf$Bet_v_1)

# Set seed for reproducibility
set.seed(123)

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

betlocspaugdf[is.na(betlocspaugdf)] <- 0

# Train the model
step.model <- train(Bet_v_1 ~., data = betlocspaugdf,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control, 
                    na.action = na.pass
)
step.model$results

# Best model 
step.model$bestTune
summary(step.model$finalModel)

# Get coefficient of the best model
coef(step.model$finalModel, 5)

# Linear regression model for all variables 
modelall <- lm(Bet_v_1 ~ ., data = betlocspaugdf)
summary(modelall)

# Sort dataframe 
modelimp <- data.frame(varImp(modelall), "names"=rownames(varImp(modelall)), row.names = 1:35)
modelimp[order(modelimp$Overall, decreasing = T),]
