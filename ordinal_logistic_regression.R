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

anv_df <- as.data.frame(anova(full_m, test = 'Chisq'))
anv_df <- anv_df[!row.names(anv_df)=='TOTAL',] # Variables only
anv_df[order(anv_df$P, decreasing = TRUE),]

ci <- 0.9
max_p <- max(anv_df$P)
all_vars <- colnames(train)[colnames(train)!="severity"]
drop_vars <- NULL
temp_vars <- NULL

refit_m <- function(ci){
  m_temp <- NULL
  while(max_p < ci | (length(drop_vars) < length(all_vars))){
    drop_var <- row.names(anv_df[1,]) # remove largest p value
    drop_vars <- append(drop_vars, drop_var)
    temp_vars <- all_vars[all_vars!=drop_vars] # exclude dropped variable
    form_x <- paste(temp_vars, collapse = "+") # create x syntax
    form <- as.formula(paste("severity~", form_x)) # formula syntax
    m_temp <- orm(form, data = train) # temp model refitted without dropped variable
    anv_df <- as.data.frame(anova(m_temp, test = 'Chisq')) # recalculate p values
    anv_df <- anv_df[!row.names(anv_df)=='TOTAL',] # exclude total
    anv_df <- anv_df[order(anv_df$P, decreasing = TRUE),] # order descending to get highest p value
    max_p <- max(anv_df$P) # update max value
  }
  return(m_temp)
}


order_anv <- function(m){
  # return ordered anova table without total
  anv <- as.data.frame(anova(m), test = 'Chisq')
  anv <- anv[!row.names(anv)=='TOTAL',] # keep just the variables
  return(anv[order(anv$P, decreasing = TRUE),])
}

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



m_0.9 <- refit_m(0.10)
anova(m_0.9, test = 'Chisq')


m_0.95 <- refit_m(0.95)
anova(m_0.95, test = 'Chisq')

full_m
anova(full_m, test = 'Chisq')





i <- 1
max_iter <- 5