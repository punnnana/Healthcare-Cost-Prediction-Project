library("car")
library("lmtest")
library("nortest")
library("lm.beta")
library("lmtest")
library("Hmisc")
library("ppcor")

insurance_clean <- read.csv("insurance_clean.csv")

model = lm(charges ~ age + bmi + smoker_yes +
             region_southeast + region_southwest + region_northwest, data=insurance_clean)


#ก่อนสร้างโมเดล

#Linearity
names(insurance_clean)

with(insurance_clean, cor(charges, age, method = "pearson", use = "complete.obs"))
with(insurance_clean, cor(charges, bmi, method = "pearson", use = "complete.obs"))
with(insurance_clean, cor(charges, smoker_yes, method = "pearson", use = "complete.obs"))

with(insurance_clean, cor.test(charges, age, method = "pearson"))
with(insurance_clean, cor.test(charges, bmi, method = "pearson"))
with(insurance_clean, cor.test(charges, smoker_yes, method = "pearson"))

pcorY <- function(x_name) {
  all_x <- c("age", "bmi", "smoker_yes",
             "region_southeast", "region_southwest", "region_northwest")
  control_vars <- setdiff(all_x, x_name)
  ppcor::pcor.test(
    insurance_clean$charges,
    insurance_clean[[x_name]],
    insurance_clean[, control_vars, drop = FALSE]
  )
}
pcorY("age")
pcorY("bmi")
pcorY("smoker_yes")

#Y ~ Normal
 #แปลงข้อมูล charges ให้เป็น log
insurance_clean$log_charges <- log(insurance_clean$charges)

shapiro.test(insurance_clean$log_charges)
qqnorm(insurance_clean$log_charges)
qqline(insurance_clean$log_charges, col = "red")

hist(insurance_clean$log_charges, 
     main = "Histogram of log(charges)", 
     xlab = "log(charges)", 
     col = "lightgray", 
     border = "white")

#VIF
vif(model)

#หลังสร้างโมเดล

#Independent
lmtest::dwtest(model)

#Equality
lmtest::bptest(model)
car::ncvTest(model)

plot(model)

#E ~ Normal
nortest::ad.test(residuals(model))
qqnorm(residuals(model))
qqline(residuals(model), col = "red", lwd = 2)

#เหมาะสม
summary(model)

lm.beta(model)

anova(model)

car::Anova(model)

# การเลือกโมเดลแบบ Stepwise เพื่อหาความเหมาะสมที่สุด
model.forward <-step(model,direction = "forward")
summary(model.forward)
model.backward <-step(model,direction = "backward")
summary(model.backward)
model.both <- step(model,direction = "both")
summary(model.both)

#ตัวแปลหุ่น(สร้างdummyจากไฟล์raw)
df <- read.csv("insurance.csv")
df$smoker_yes <- ifelse(df$smoker == "yes", 1, 0)
region_dummy <- model.matrix(~ region, data = df)[, -1]
df <- cbind(df, region_dummy)
head(df)
write.csv(df, "insurance_clean.csv", row.names = FALSE)

#แปลงข้อมูล
anova(model) ->an2; an2
model2 <- lm(log(charges) ~ age + bmi + smoker_yes +
               region_southeast + region_southwest + region_northwest, data=insurance_clean)
summary(model2)
model3 <- lm(log(charges) ~ log(age) + log(bmi) + smoker_yes +
               region_southeast + region_southwest + region_northwest,
             data = insurance_clean)

summary(model3)

#ข้อมูลผิดปกติ/มีอิทธิพล (diagnostice/influence)
mydata.cor = rcorr(as.matrix(insurance_clean))
round(mydata.cor$r,4)
round(mydata.cor$P,4)
my_matrix <- cbind(hatvalues=round(hatvalues(model),4), residuals=round(residuals(model),4), rstandard=round(rstandard(model),4), rstudent=round(rstudent(model),4))
influence.measures(model)

#เพิ่มเติม
boxcox(model)

boxplot(insurance_clean$charge, main="After Box-Cox")