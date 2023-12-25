
## testing Jakob's code

dat_raw <- read.csv(file = 'flows2020.csv', fileEncoding="UTF-8-BOM") 

# Abel 2018 uses da_min_closed 
dat_raw <- dat_raw[,c("year0", "orig", "dest", "da_min_closed")]
names(dat_raw)[4] <- 'flow'

# add lags
## add helper
dat_raw$helper <- dat_raw$year0+5

## merge data with lag
dat_raw <- merge(x = dat_raw, y = dat_raw, by.x = c('orig', 'dest', 'year0'), by.y = c('orig', 'dest', 'helper'), all.x = T, all.y = F)

## omit columns 
dat <- subset(dat_raw, select = -c(helper, year0.y))

## rename
names(dat)[4:5] <- c('flow', 'flow_lag')

## remove NAs
dat <- na.omit(dat)

## training data
dat_train <- subset(dat, year0!=2015)

## test data
dat_test <- subset(dat, year0==2015)

# lag as estimate ---------------------------------------------------------

pred_1 <- subset(dat, subset =  year0==2010, select = flow)


# mean flows | orig, dest -------------------------------------------------

# calculate mean flows for given orig and dest
data_mean <- aggregate(dat_train, by=list(dat_train$orig, dat_train$dest), function(x){ifelse(is.numeric(x),return(mean(x, na.rm = T)), return(NaN))})                

# select relevant columns
data_mean <- data_mean[,c('Group.1','Group.2', 'flow', 'flow_lag')]

# give correct names
names(data_mean)[1:2] <- c('orig','dest')

# select predictions
pred_2 <- data_mean$flow

gravity_prediction_v5


# AR(1) with interacted country fixed effects -----------------------------


# within transform data, i.e. demean data

## merge data with means
dat_ar_tmp <- merge(x = dat_train, y = data_mean, by = c('orig', 'dest'), all.x = T, all.y = T)

## rename
names(dat_ar_tmp)[4:7] <- c('flow', 'flow_lag', 'flow_mean', 'flow_lag_mean')

## demean data
dat_ar <- data.frame(dat_ar_tmp[,c('orig', 'dest', 'year0')],
                     flow_demeaned = dat_ar_tmp$flow - dat_ar_tmp$flow_mean,
                     flow_lag_demeaned = dat_ar_tmp$flow_lag - dat_ar_tmp$flow_lag_mean)

# estimated ar(1)
ar_1_demeaned <- lm(flow_demeaned ~ flow_lag_demeaned, data = dat_ar)

# predict

## prediction data ?should this data be removed for estimation?
dat_ar_pred <- subset(dat_ar, year0==2010)

## predict
pred_3_demeaned <- predict(ar_1_demeaned, newdata = dat_ar_pred)

# add means to prediction

## add countries to pred_3_demeaned
pred_3_demeaned_cntry <- data.frame(dat_ar_pred[,c('orig', 'dest')],
                                    pred_demeaned = pred_3_demeaned)

## merge with means
pred_3_tmp <- merge(x = pred_3_demeaned_cntry, y = data_mean, by = c('orig', 'dest'), all.x = T, all.y = F)

## add means
pred_3 <- data.frame(pred_3_tmp[,c('orig', 'dest')],
                     pred_3 = pred_3_tmp$pred_demeaned + pred_3_tmp$flow)

