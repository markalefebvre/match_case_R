rm(list = ls())
library(readr)
library(MatchIt)

Nissan <- read_csv("nissan_match_test.csv",
                             col_types = cols(ch_member_id = col_character(),  
                                              gender = col_factor(levels = c("F", "M")), 
                                              grp = col_factor(levels = c("1",  "0")),
                                              post_adv_img = col_factor(levels = c("1", "0")), 
                                              post_colon = col_factor(levels = c("1", "0")), 
                                              post_dynam_act = col_factor(levels = c("1", "0")), 
                                              post_eye = col_factor(levels = c("1", "0")), 
                                              post_hip = col_factor(levels = c("1",  "0")), 
                                              post_knee = col_factor(levels = c("1","0")), 
                                              post_lab_test = col_factor(levels = c("1",  "0")), 
                                              post_lamin = col_factor(levels = c("1","0")), 
                                              post_mammogram = col_factor(levels = c("1", "0")), 
                                              post_ov = col_factor(levels = c("1","0")), 
                                              post_pap = col_factor(levels = c("1", "0")), 
                                              post_sleep = col_factor(levels = c("1","0")),
                                              post_vag_dev = col_factor(levels = c("1","0")),
                                              pre_adv_img = col_factor(levels = c("1","0")), 
                                              pre_colon = col_factor(levels = c("1","0")), 
                                              pre_dynam_act = col_factor(levels = c("1","0")),
                                              pre_ekg = col_factor(levels = c("1","0")),
                                              pre_eye = col_factor(levels = c("1","0")), 
                                              pre_hip = col_factor(levels = c("1","0")), 
                                              pre_knee = col_factor(levels = c("1","0")), 
                                              pre_lab_test = col_factor(levels = c("1","0")),
                                              pre_lamin = col_factor(levels = c("1","0")), 
                                              pre_mammogram = col_factor(levels = c("1","0")),
                                              pre_ov = col_factor(levels = c("1","0")), 
                                              pre_pap = col_factor(levels = c("1","0")), 
                                              pre_sleep = col_factor(levels = c("1","0")), 
                                              pre_vag_dev = col_factor(levels = c("1","0"))))

# outliers capping approach

#x <- Nissan$pre_avg_amt
#qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#caps <- quantile(x, probs=c(.05, .95), na.rm = T)
#H <- 1.5 * IQR(x, na.rm = T)
#x[x < (qnt[1] - H)] <- caps[1]
#x[x > (qnt[2] + H)] <- caps[2]


#qn = quantile(Nissan$pre_avg_amt, c(0.05, 0.95), na.rm = TRUE)
#df = within(Nissan, { pre_avg_amt = ifelse(pre_avg_amt < qn[1], qn[1], pre_avg_amt)
#pre_avg_amt = ifelse(pre_avg_amt > qn[2], qn[2], pre_avg_amt)})


pre = quantile(Nissan$pre_avg_amt, c(0.05, 0.95), na.rm = TRUE)
post = quantile(Nissan$post_avg_amt, c(0.05, 0.95), na.rm = TRUE)

Nissan = within(Nissan, { pre_avg_amt = ifelse(pre_avg_amt < qn[1], qn[1], pre_avg_amt)
pre_avg_amt = ifelse(pre_avg_amt > qn[2], qn[2], pre_avg_amt)})

Nissan = within(Nissan, { post_avg_amt = ifelse(post_avg_amt < qn[1], qn[1], post_avg_amt)
post_avg_amt = ifelse(post_avg_amt > qn[2], qn[2], post_avg_amt)})



# Study
study <- subset(Nissan, grp == 1)
summary(study$age)
table(study$age)
hist(study$age)

# Control
control <- subset(Nissan, grp == 0)
summary(control$age)
table(control$age)
hist(control$age)

## unpaired testing
t.test(Nissan$pre_avg_amt[Nissan$grp==1],Nissan$post_avg_amt[Nissan$grp==1],paired = FALSE)
t.test(Nissan$pre_avg_amt[Nissan$grp==0],Nissan$post_avg_amt[Nissan$grp==0],paired = FALSE)
t.test(Nissan$pre_avg_amt[Nissan$grp==0],Nissan$post_avg_amt[Nissan$grp==1],paired = FALSE)


## matching

colnames(Nissan)

#m.out1 <- matchit(grp ~ age + gender + latitude + longitude + pre_adv_img + pre_ov + pre_lab_test + pre_mammogram + pre_colon + pre_vag_dev + pre_hip + pre_knee + pre_sleep + pre_eye + pre_lamin + pre_dynam_act + pre_ekg + pre_pap + 
#                    pre_avg_amt + pre_ut, data = Nissan, method = "nearest", distance = "logit", caliper=.01,
#                  Replacement = TRUE)

m.out1 <- matchit(grp ~ age + gender + latitude + longitude + pre_adv_img + pre_ov + pre_lab_test + pre_mammogram + pre_colon + pre_vag_dev + pre_hip + pre_knee + pre_sleep + pre_eye + pre_lamin + pre_dynam_act + pre_ekg + pre_pap + 
                    pre_avg_amt + pre_ut, data = Nissan, method = "exact", distance = "logit")

# replace = false good change?  try mahalanobis distance next
m.out1 <- matchit(grp ~ age + gender + latitude + longitude + pre_adv_img + pre_ov + pre_lab_test + pre_mammogram + pre_colon + pre_vag_dev + pre_hip + pre_knee + pre_sleep + pre_eye + pre_lamin + pre_dynam_act + pre_ekg + pre_pap + 
                    pre_avg_amt + pre_ut, data = Nissan, method = "nearest", distance = "logit", replace = FALSE,
                  caliper = .01)

m.out1 <- matchit(grp ~ age + gender + latitude + longitude + pre_adv_img + pre_ov + pre_lab_test + pre_mammogram + pre_colon + pre_vag_dev + pre_hip + pre_knee + pre_sleep + pre_eye + pre_lamin + pre_dynam_act + pre_ekg + pre_pap + 
                    pre_avg_amt + pre_ut, data = Nissan, method = "nearest", distance = "mahalanobis", replace = FALSE)



m.out1

summary(m.out1)

m.out1$match.matrix

#First, I generate a large dataset of all person-level observations that went into the matching process, with the essential output: id_numbers, treatment identifiers, and weights.
#  Nissan
a <- data.frame(Nissan$ch_member_id, m.out1$treat, m.out1$weights)
colnames(a)<-c("id_num","trt","weights")
head(a)

### figure out who has been matched
b <- as.data.frame(m.out1$match.matrix)
colnames(b) <- c("matched_unit")
b$matched_unit <- as.numeric(as.character(b$matched_unit))
b$treated_unit <- as.numeric(rownames(b))
# delete na matches
c <- b[!is.na(b$matched_unit),]
c$match_num <- 1:dim(c)[1]
head(c)


# attach match number to large datafile
a[c$matched_unit,4]<-c$match_num
a[c$treated_unit,4]<-c$match_num
colnames(a)[4]<-"match_num"
head(a)


# check match numbers
a[!is.na(a$match_num) & a$match_num==1,]
#       ch_id     trt   weights  match_num
#1   52,829,750   1       1         1
#659 52,805,126   0       1         1

Nissan[1,]
Nissan[1232,]

# combine non sequential row numbers
#Y <- X[c(1:5, 10:14, 20), ]
Y = Nissan[c(1,2013),]

a[!is.na(a$match_num) & a$match_num==2,]
#        ch_id     trt    weights  match_num
#2    52,823,445   1       1         2
#4101 52,816,158   0       1         2

Y = Nissan[c(2,3325),]


a[!is.na(a$match_num) & a$match_num==3,]
#        ch_id      trt weights match_num
#3    52,827,869   1       1         3
#2801 52,803,060   0       1         3
a.tab <- as.data.frame(table(a$match_num))


######
m.data1 <- match.data(m.out1)

m.data1
hist(m.data1$age)

# Estimation of treatment effects can be obtained via paired or matched comparisons (t -test)
# change to false if you want to view mean of individual groups
t.test(m.data1$pre_avg_amt[m.data1$grp==1],m.data1$post_avg_amt[m.data1$grp==1],paired = TRUE)
t.test(m.data1$pre_avg_amt[m.data1$grp==0],m.data1$post_avg_amt[m.data1$grp==0],paired= TRUE)

t.test(m.data1$pre_avg_amt[m.data1$grp==0],m.data1$pre_avg_amt[m.data1$grp==1],paired = TRUE)
t.test(m.data1$post_avg_amt[m.data1$grp==0],m.data1$post_avg_amt[m.data1$grp==1],paired=TRUE)

#  differences of differences

m.data1$diff <- m.data1$pre_avg_amt - m.data1$post_avg_amt

t.test(m.data1$diff[m.data1$grp==0],m.data1$diff[m.data1$grp==1],paired=TRUE)

summary(lm(diff ~ grp, data = m.data1))

plot(lm(diff ~ grp, data = m.data1))

