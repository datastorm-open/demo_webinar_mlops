source("MLOpsMonitoring/R/dataset.R")
source("MLOpsMonitoring/R/drift_score.R")

library(caret)

agg[, .N/nrow(agg), VAR_REP]

# Gestion des Inf
agg <- apply(agg, 2,  function(x) replace(x, is.infinite(x), NA))
agg[is.na(agg)] <- 0
agg <- as.data.table(agg)

# col_isna <- apply(agg_na, 2, anyNA)
# col_isna <- col_isna[col_isna == TRUE]
# aa <- as.data.table(agg_na)[is.na(FREQ_ACHAT_3M)]
# aa[, colnames(aa)[grep("_3M", colnames(aa))], with=FALSE]

# Random Forest
length(unique(agg$Customer.ID))
set.seed(28)
customer_validation <- sample(agg$Customer.ID, round(length(unique(agg$Customer.ID))*0.25))

agg_val <- agg[Customer.ID %in% customer_validation]
agg_train <- agg[!(Customer.ID %in% customer_validation)]

X_train <- agg_train[, -c("Customer.ID", "VAR_REP", "MONTH")]
y_train <- as.factor(agg_train$VAR_REP)
X_val <- agg_val[, -c("Customer.ID", "VAR_REP", "MONTH")]
y_val <- as.factor(agg_val$VAR_REP)

dim(X_train)
dim(X_val)

colnames(X_train)

train_control <- caret::trainControl(method="cv", number=4)
rf <- caret::train(x=X_train, 
                   y=y_train, 
                   trControl=train_control, method="rf", ntree=250)
print(rf)
pred_validation <- predict(rf, X_val, type="prob")[, 2]
auc(actual = agg_val$VAR_REP, pred = pred_validation)

# Importance
varImp(rf)
plot(varImp(rf), top=30)



# Modèle target sur 3 mois
train_rf <- function(agg){
  agg <- apply(agg, 2,  function(x) replace(x, is.infinite(x), NA))
  agg[is.na(agg)] <- 0
  agg <- as.data.table(agg)
  
  set.seed(28)
  customer_validation <- sample(agg$Customer.ID, round(length(unique(agg$Customer.ID))*0.25))
  agg_val <- agg[Customer.ID %in% customer_validation]
  agg_train <- agg[!(Customer.ID %in% customer_validation)]
  
  X_train <- agg_train[, -c("Customer.ID", "VAR_REP")]
  y_train <- as.factor(agg_train$VAR_REP)
  X_val <- agg_val[, -c("Customer.ID", "VAR_REP")]
  y_val <- as.factor(agg_val$VAR_REP)
  
  train_control <- caret::trainControl(method="cv", number=4)
  rf <- caret::train(x=X_train, 
                     y=y_train, 
                     trControl=train_control, method="rf", ntree=250)
  print(rf)
  pred_validation <- predict(rf, X_val, type="prob")[, 2]
  metric_auc <- auc(actual = agg_val$VAR_REP, pred = pred_validation)
  return(list(rf=rf, auc=metric_auc, pred=pred_validation))
}

res3mois <- train_rf(agg_target3)
res3mois$auc


res_mai <- train_rf(agg_target1_windows3)
res_mai$auc
plot(varImp(res_mai$rf), top=10)

res_mai12 <- train_rf(agg_target1_windows12)
res_mai12$auc
plot(varImp(res_mai$rf), top=10)



# Faire evoluer un modele dans le temps : prev à M+1, M+2 etc


