## Tugas Day 8 - dibimbing Data Science Bootcamp Batch 8
# Nama: Dhania Putri Sarahtika

library(caret)
df_houseprices <- read.csv('train.csv')
head(df_houseprices)

# 1. Missing Value Checking and Handling
is.na(df_houseprices)
View(colSums(is.na(df_houseprices)))

# The missing value in the chosen column is deleted because it is only 1 and 
# the NA here does not stand for "none" (e.g. no garage, no basement), unlike in other variables.
is.na(df_houseprices$Electrical)
df_houseprices$Electrical
df_houseprices <- df_houseprices[!is.na(df_houseprices$Electrical),]
View(colSums(is.na(df_houseprices)))

# 2. Normalization and Standardization
View(df_houseprices)
boxplot(df_houseprices['BsmtFinSF1'])
hist(df_houseprices$BsmtFinSF1)

mean(df_houseprices$BsmtFinSF1)
sd(df_houseprices$BsmtFinSF1)

norm_scaler_bsmt <- preProcess(df_houseprices['BsmtFinSF1'], method = c('range'))
norm_scaler_bsmt
df_houseprices['BsmtFinSF1_norm'] <- predict(norm_scaler_bsmt, df_houseprices['BsmtFinSF1'])
mean(df_houseprices$BsmtFinSF1_norm)
sd(df_houseprices$BsmtFinSF1_norm)

# 3. Data Transformation
# Viewing the attributes and distribution of the data
SalePrice <- df_houseprices$SalePrice
SalePrice
summary(SalePrice)
hist(SalePrice)
hist(SalePrice[SalePrice < 275000])

# The following is the log transformation of the sale prices.
# Such transformation enables the sale price data to approach a normal distribution,
# as seen in the histogram of log_sale.

log_sale <- log(SalePrice)
hist(log_sale)

#I also tried using the boxcox transformation.

lambda <- BoxCoxTrans(SalePrice)$lambda
lambda
boxcox_sp <- (SalePrice**-0.1 - 1)/lambda
hist(boxcox_sp[boxcox_sp < 50])

# 4. Outlier Handling
#For this part, I used SalePrice instead of the transformed variable.
SalePrice <- df_houseprices$SalePrice
boxplot(SalePrice)
q1_sale_price <- quantile(SalePrice,0.25)
q1_sale_price
q3_sale_price <- quantile(SalePrice,0.75)
q3_sale_price

iqr_sale_price <- q3_sale_price - q1_sale_price
iqr_sale_price

upper_bound_sp <- q3_sale_price + 1.5 * iqr_sale_price
upper_bound_sp
lower_bound_sp <- q1_sale_price - 1.5 * iqr_sale_price
lower_bound_sp

df_houseprices[SalePrice > upper_bound_sp, 'SalePrice'] <- upper_bound_sp
df_houseprices[SalePrice < lower_bound_sp, 'SalePrice'] <- lower_bound_sp
boxplot(df_houseprices$SalePrice)


# 5. Categorical Data Handling
View(df_houseprices$HeatingQC)
df_houseprices$HeatingQC
table(df_houseprices['HeatingQC'])
df_houseprices['label_HeatingQC'] <- ifelse(df_houseprices['HeatingQC']=='Po',0,
                                                 ifelse(df_houseprices['HeatingQC']=='Fa',1,
                                                        ifelse(df_houseprices['HeatingQC']=='TA',2, 
                                                               ifelse(df_houseprices['HeatingQC']=='Gd', 3, 4))))

df_houseprices$label_HeatingQC

# 6. Final Output Combination
df_houseprices_exercise <- data.frame(df_houseprices$Electrical, df_houseprices$BsmtFinSF1_norm, boxcox_sp, df_houseprices$SalePrice, df_houseprices$label_HeatingQC)
View(df_houseprices_exercise)






