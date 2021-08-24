# Load data

setwd("~/Projects/bike-index")

df <- read.csv('data/crashes.csv')

n <- ncol(df)
factor_cols <- c(5:n-1)

for(i in factor_cols){
  df[,i] <- as.factor(df[,i])
}

df$severity <- as.ordered(df$severity)

str(df)

# Split train/test

train_size = 0.8 * nrow(df)
set.seed(1234)
ix = sample(seq_len(nrow(df)), size=train_size)
train = df[ix,]
test = df[-ix,]

# Develop model- full model

library(rms)
full_m <- orm(severity ~ ., data = train)

# Drop variables based on confidence level


order_anv <- function(m){
  # return ordered anova table without total
  anv <- as.data.frame(anova(m), test = 'Chisq')
  anv <- anv[!row.names(anv)=='TOTAL',] # keep just the variables
  return(anv[order(anv$P, decreasing = TRUE),])
}

ci <- 0.9
max_p <- max(anv_df$P)
all_vars <- colnames(train)[colnames(train)!="severity"]
drop_vars <- NULL
temp_vars <- NULL


refit_model <- function(ci, m){
  anv <- order_anv(m)
  max_p <- max(anv$P)
  drop_vars <- NULL
  while(max_p > ci){
    drop_var <- row.names(anv[1,]) # largest p value
    drop_vars <- append(drop_vars, drop_var)
    temp_vars <- all_vars[!all_vars %in% drop_vars]
    form_x <- paste(temp_vars, collapse = "+")
    form <- as.formula(paste("severity~", form_x))
    m_temp <- orm(form, data = train)
    #print(drop_vars)
    anv <- order_anv(m_temp)
    max_p <- max(anv$P)
  }
  return(m_temp)
}


refit_model(0.1, full_m)
