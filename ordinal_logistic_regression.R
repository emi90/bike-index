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

refit_m <- function(ci){
  while(max_p > ci){
    drop_var <- row.names(anv_df[1,]) # remove largest p value
    temp_vars <- all_vars[all_vars!=drop_var] # exclude dropped variable
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

refit_m(0.95)

while(max_p > ci){
  drop_var <- row.names(anv_df[1,]) # remove largest p value
  temp_vars <- all_vars[all_vars!=drop_var] # exclude dropped variable
  form_x <- paste(temp_vars, collapse = "+") # create x syntax
  form <- as.formula(paste("severity~", form_x)) # formula syntax
  m_temp <- orm(form, data = train) # temp model refitted without dropped variable
  anv_df <- as.data.frame(anova(m_temp)) # recalculate p values
  anv_df <- anv_df[!row.names(anv_df)=='TOTAL',] # exclude total
  anv_df <- anv_df[order(anv_df$P, decreasing = TRUE),] # order descending to get highest p value
  max_p <- max(anv_df$P) # update max value
}

m_0.9 <- m_temp



