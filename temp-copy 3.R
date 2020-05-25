##Load data laptop.csv
library(tidyverse)
laptop <- read_csv("laptop.csv")
head(laptop)
sum(is.na(laptop))
laptop

## Change the size of screen (inches) to integer (remove decimals)
laptop_inch <- laptop %>% mutate(Inches = as.integer(Inches))
laptop_inch

## Remove model details (ex: MacBook Pro -> MacBook), because the other variables will express the same info
laptop_product <- laptop_inch %>% separate(Product, into = c("Product", "Model"), sep = " ") %>% dplyr::select(-Model) 
laptop_product

## Change Weight(char) variable into Weight_kg(dbl)
laptop_weight <- laptop_product %>% mutate(Weight_kg = as.double(str_remove(Weight, "kg"))) %>% dplyr::select(-Weight)
laptop_weight

## Change ram to numeric value with only size of the ram with unit GB.
laptop_ram <- laptop_weight %>% mutate(Ram_GB = as.double(str_remove(Ram, "GB"))) %>% dplyr::select(-Ram)
laptop_ram

## Change ScreenResolution with its resolution and information about whether containing touchscreen
laptop_screen0 <- laptop_ram %>% mutate(Screen_resolution = unlist(lapply((str_split(ScreenResolution, " ")), tail, n = 1L)), Touchscreen = ifelse(str_detect(ScreenResolution, "Touchscreen"), 1, 0)) %>%
  dplyr::select(-ScreenResolution)
laptop_screen <- laptop_screen0 %>% mutate(Screen_resolution_num = as.double(unlist(lapply((str_split(Screen_resolution, "x")), head, n = 1L))) * as.double(unlist(lapply((str_split(Screen_resolution, "x")), tail, n = 1L))) )
laptop_screen



## Change CPU column into columns count and frequency for further use

laptop_Cpu <- laptop_screen %>% mutate(Cpu_Frequency_GHz = lapply((str_split(Cpu, " ")), tail, n = 1L)) %>% mutate(Cpu_Frequency_GHz = as.double(str_remove(Cpu_Frequency_GHz, "GHz"))) %>% filter(Cpu != "Samsung Cortex A72&A53 2.0GHz") %>%
  mutate(Cpu = str_replace(Cpu, "-", " ")) %>% mutate(Cpu = str_replace(Cpu, "-", " ")) %>% mutate(Cpu = str_sub(Cpu, 1, -8)) %>% mutate(Cpu = str_remove(Cpu,"Series ")) %>%
  mutate(Cpu = str_replace_all(Cpu, " A10","")) %>% mutate(Cpu = str_replace(Cpu, " A12 ", ' ')) %>% mutate(Cpu = str_replace(Cpu, " A4 ", ' ')) %>% mutate(Cpu = str_replace(Cpu, " A6 ", ' ')) %>%
  mutate(Cpu = str_replace(Cpu, "[ ]A.[ ]", ' ')) %>% mutate(Cpu = str_replace(Cpu, " E ", ' ')) %>% mutate(Cpu = str_replace(Cpu, " E2 ", ' '))  %>% mutate(Cpu = str_replace(Cpu, " FX ", ' ')) %>%
  mutate(Cpu = str_replace(Cpu, " Ryzen ", ' ')) %>% mutate(Cpu = str_replace(Cpu, " A9 ", ' '))%>%mutate(Cpu = str_replace(Cpu," M "," "))%>%mutate(Cpu = str_replace(Cpu," Dual Core "," "))%>%
  mutate(Cpu = str_replace(Cpu," Quad Core "," "))%>%mutate(Cpu = str_replace(Cpu," M "," "))%>% mutate(Cpu = ifelse(str_sub(Cpu,start=-1)==" ",str_sub(Cpu,1,-2),Cpu))%>%
  mutate(Cpu = str_replace(Cpu," M7","")) %>% mutate(Cpu = str_replace(Cpu,"[ ]x5[ ]"," "))%>% mutate(Cpu = str_replace(Cpu,"[ ]X5[ ]"," "))%>%mutate(Cpu = str_replace(Cpu,"M3[ ]",""))%>%
  mutate(Cpu = str_remove(Cpu," V6"))%>%mutate(Cpu = str_remove(Cpu," V7"))%>%mutate(Cpu = str_replace(Cpu,"[ ]M3",""))%>%mutate(Cpu = str_replace(Cpu,"[ ]m3",""))%>%
  mutate(Cpu = str_replace(Cpu," m7 "," "))%>%mutate(Cpu = str_replace(Cpu,"Intel Core i5 7500U", "INTEL CORE I7 7500U"))%>%mutate(Cpu = str_replace(Cpu,"Intel Core i5 7500U", "INTEL CORE I7 7500U"))%>%
  mutate(Cpu = str_replace(Cpu," FX "," "))


laptop_Cpu$Cpu <- toupper(laptop_Cpu$Cpu)

laptop_Cpu <- laptop_Cpu %>%mutate(Cpu = str_replace(Cpu,"INTEL CELERON N33", "INTEL CELERON N3350"))%>%
  mutate(Cpu = str_replace(Cpu,"INTEL CELERON N3710", "INTEL PENTIUM N3710"))%>%
  mutate(Cpu = str_replace(Cpu,"INTEL CELERON N3710", "INTEL PENTIUM N3710"))%>%
  mutate(Cpu = str_remove(Cpu," V6"))%>%
  mutate(Cpu = str_replace(Cpu,"INTEL CELERON N335050", "INTEL CELERON N3350"))%>%
  mutate(Cpu = str_replace(Cpu,"AMD 1600", "AMD 5 1600"))%>%
  mutate(Cpu = str_replace(Cpu,"AMD 94", "AMD 9400"))%>%
  mutate(Cpu = str_replace(Cpu,"AMD 17", "AMD 7 1700"))%>%
  mutate(Cpu = str_replace(Cpu,"AMD 73", "AMD 7300"))%>%
  mutate(Cpu = str_replace(Cpu,"AMD 983", "AMD 9830P"))%>%
  mutate(Cpu = str_replace(Cpu,"AMD 940010", "AMD 9410"))%>%
  mutate(Cpu = str_replace(Cpu,"AMD 940020", "AMD 9420"))%>%
  mutate(Cpu = str_replace(Cpu,"INTEL CORE I3 600", "INTEL CORE I3 6006U"))%>%
  mutate(Cpu = str_replace(Cpu,"INTEL CORE I3 6006U6U", "INTEL CORE I3 6006U"))

laptop_Cpu

## Get Cpu score from internet and evaluate the Cpu we have in our dataframe

cpu_score <- read_csv("c.csv")
cpu_score$names <- toupper(cpu_score$names)

cpu_score <- cpu_score %>% mutate(names = ifelse(str_sub(cpu_score$names,start=-1)==" ",str_sub(names,1,-2),names))%>%
  mutate(names = str_remove(names," APU"))%>%mutate(names = str_replace(names," E "," "))%>%
  mutate(names = str_replace(names," E1 "," "))%>%mutate(names = str_replace(names," E2 "," ")) %>%
  mutate(names = str_replace(names,"[ ]A.[ ]"," "))%>%mutate(names = str_replace(names," A10 "," "))%>%
  mutate(names = str_replace(names," A12 "," "))%>%mutate(names = str_replace(names," RYZEN "," "))%>%
  mutate(names = str_replace(names," DUAL CORE "," "))%>%mutate(names = str_replace(names," M5 "," "))%>%
  mutate(names = str_replace(names," M7 "," "))%>%mutate(names = str_replace(names," X5 "," "))%>%
  mutate(names = str_replace(names," M3 "," "))%>%mutate(names = str_remove(names," V6"))%>%
  mutate(names = str_remove(names," V7"))%>%mutate(names = str_replace(names," FX "," "))


#sum(is.na(laptop_cpu$count))
mean_count <- mean((cpu_score %>% filter(str_detect(names, "INTEL CORE I5")))$count)
laptop_cpu <- left_join(laptop_Cpu, cpu_score, by=c("Cpu"="names")) %>% dplyr::select(-Cpu) %>% dplyr::select(-Frequency)
colnames(laptop_cpu)[which(names(laptop_cpu) == "count")] <- "Cpu_score"
laptop_cpu$Cpu_score <- replace_na(laptop_cpu$Cpu_score, mean_count)
laptop_cpu

## Clean the data of Memory column

## library(stringr)
select <- dplyr::select
laptop_test <- laptop_cpu %>% select(Memory) #keep only the Memory variable for data cleaning convenience

tmp <- laptop_test %>% separate(Memory, c("a", "b"), sep = "[+]") %>%  #separate parts by + (a + b)
  separate(a, c("c", "d"), "[ ]") %>%  #separate part a by whitespace
  mutate(b = str_remove(str_remove(b, "[ ]"), "[ ]")) %>%  #remove the two whitespaces in b from the previous step
  separate(b, c("e", "f"), "[ ]") %>%  #separate part b by whitespace
  mutate(c = ifelse(c %in% c("1", "1.0TB"), "1TB", c)) %>%  #fix typo
  mutate(c = as.numeric(str_remove_all(c, "[A-Za-z]"))) %>%  #remove non numeric for c (ex: GB..)
  mutate(c = ifelse(c %in% c(1, 2), c*1024, c)) %>%  #TB into GB
  mutate(e = as.numeric(str_remove_all(e, "[A-Za-z]"))) %>% #remove non numeric for e (ex: GB..)
  mutate(e = ifelse(e %in% c(1, 2), e*1024, e)) %>% #TB into GB
  mutate(index = 1:nrow(laptop_test)) %>% #add an index column
  spread(d, c) %>% 
  arrange(index) %>% 
  replace(., is.na(.), 0) 

#add the part after + to tmp 
for (i in 1:nrow(tmp)) {
  if(tmp$f[i] == "HDD") {
    tmp$HDD[i] = tmp$e[i] + tmp$HDD[i]
  }
  if(tmp$f[i] == "Hybrid") {
    tmp$Hybrid[i] = tmp$e[i] + tmp$Hybrid[i]
  }
  if(tmp$f[i] == "SSD") {
    tmp$SSD[i] = tmp$e[i] + tmp$SSD[i]
  }
}

tmp <- tmp %>% select(-c(e, f))
laptop_Memory <- inner_join(laptop_cpu, tmp, by = c("X1" = "index")) %>% select(-Memory)
laptop_Memory

laptop_Memory[9 :19] %>% tail(10)
## Change Gpu column to performance and frequency for future use

laptop_Memory$Gpu <- toupper(laptop_Memory$Gpu) 
graphics_score <- read_csv("gpu_per.csv")
graphics_score$Gpu <- toupper(graphics_score$Gpu)
laptop_Gpu <- left_join(laptop_Memory, graphics_score, by="Gpu") %>% select(-Rating) %>% select(-Gpu)
laptop_Gpu <- laptop_Gpu %>% rename(Gpu_score = Performance)

laptop_data <- laptop_Gpu

laptop_data <- laptop_data %>% filter(Product != "Yoga Book")

laptop_data <- laptop_data %>% select(-c(Product, TypeName, Screen_resolution,X1))


#library(olsrr)


# change company and OpSys to categorical variable

laptop_data <- laptop_data %>% mutate(Company =   case_when(
  Company == "Chuwi"  ~ "Other",
  Company == "Fujitsu " ~ "Other",
  Company == "Google"  ~ "Other",
  Company == "Chuwi"  ~ "Other",
  Company == "LG" ~ "Other",
  Company == "Mediacom"  ~ "Other",
  Company == "Microsoft" ~ "Other",
  Company == "Razer"  ~ "Other",
  Company == "Samsung" ~ "Other",
  Company == "Vero"  ~ "Other",
  Company == "Xiaomi" ~ "Other",
  TRUE ~ as.character(Company)
))
laptop_data <- unique(laptop_data)
laptop_data <- laptop_data %>% mutate(temp = 1) %>% spread(key = Company, value = temp, fill=0)  %>%
  select(-Other)
laptop_data <- laptop_data %>% mutate(temp = 1) %>% spread(key = OpSys, value = temp, fill=0)  %>%
  select(-Apple)
laptop_data
View(laptop_data)

str(laptop_data)

#################################################################################################################
## cross validation 80%, 20%
#laptop_data <- laptop_data %>% mutate(Price_euros = log(Price_euros))
set.seed(100)
n <- nrow(laptop_data)
ind <- sample.int(n, 1029)
laptop_train <- laptop_data[ind,]
laptop_test <- laptop_data[-ind,]

##lowess method
##library(olsrr)
# minmodel <- lm(Price_euros~1, laptop_train)
# full_model <- lm(Price_euros~., laptop_train)
# 
# s <- step(minmodel,direction = "both",scope = list(lower = ~1, 
#                                  upper = ~price_euros ~ Inches+ Weight_kg+Ram_GB+Touchscreen+
#                                   Screen_resolution_num +Cpu_Frequency_GHz +Cpu_score+Flash+
#                                   HDD+ Hybrid+SSD+Gpu_score+Acer+Asus+ Dell+ Fujitsu+ HP+
#                                   Huawei+Lenovo+ MSI+Toshiba+Android+`Chrome OS`+ 
#                                   Linux+ `Mac OS X` +macOS+ `No OS` +`Windows 10`+`Windows 10 S`+`Windows 7`))
# 
# s$call
# final <- lm(formula = Price_euros ~ Ram_GB + Screen_resolution_num + Cpu_score + 
#               `Windows 7` + Cpu_Frequency_GHz + Inches + `No OS` + Acer + 
#               Dell + Asus + Gpu_score + Touchscreen + Lenovo + MSI + HP + 
#               Linux, data = laptop_train)
# final %>% summary()
# y <- laptop_train$Price_euros
# laptop_train <- laptop_train %>% select(-Price_euros)
# y_pred <- predict(final, laptop_train)
# res <- y - y_pred
# SSE <- sum((y - y_pred)^2)
# SSE
# SSTO <- sum((y - mean(y))^2)
# SSTO
# ratio <- 1 - SSE/SSTO
# ratio

#################################################################################################################
##Model Diagnostic
  
# residual_plot <- tibble(y = y, y_pred = y_pred, res = res) %>% ggplot(aes(x = y_pred, y = res))+
#   geom_point()+
#   geom_abline(intercept = 0, slope = 0, color = "red")+
#   labs(title = "Residual plot", x = "Fitted Values", y = "Residual")+
#   theme_light()
# 
# normal_probability_plot <- tibble(y = y, y_pred = y_pred, res = res) %>% ggplot(aes(sample = res))+
#   geom_qq()+
#   geom_qq_line(color = "red")+
#   labs(title = "Normal Probability plot", x = "Standard_res", y = "Exp_res")+
#   theme_light()
# 
# library(car)
# attach(laptop_data)
# summary(powerTransform(cbind(Price_euros, Ram_GB + Screen_resolution_num + Cpu_score + 
#                                `Windows 7` + Cpu_Frequency_GHz + Inches + `No OS` + Acer + 
#                                Dell + Asus + Gpu_score + Touchscreen + Lenovo + MSI + HP + 
#                                Linux)~1))
# inverseResponsePlot(final, key = T)

##full model
full_model <- lm(formula = Price_euros ~ ., data = laptop_train)
summary(full_model)
y <- laptop_train$Price_euros
y_test <- laptop_test$Price_euros
laptop_test <- laptop_test %>% select(-Price_euros)

y_pred <- predict(full_model, laptop_test)
SSE<- sum((y_test- y_pred)^2)
SSE
SSTO<- sum((y_test- mean(y_test))^2)
SSTO
ratio<- 1 - SSE/SSTO
ratio

residual_plot_full <- tibble(y = y, y_pred = y_pred, res = res) %>% ggplot(aes(x = y_pred, y = res))+
  geom_point()+
  geom_abline(intercept = 0, slope = 0, color = "red")+
  labs(title = "Residual plot of full model", x = "Fitted Values", y = "Residual")+
  theme_light()

normal_probability_plot_full <- tibble(y = y, y_pred = y_pred, res = res) %>% ggplot(aes(sample = res))+
  geom_qq()+
  geom_qq_line(color = "red")+
  labs(title = "Normal Probability plot of full model", x = "Standard_residuals", y = "Expected_residuals")+
  theme_light()

## Y transformation for non-normality, transform y to log(y)
laptop_new <- laptop_data %>% mutate(Price_euros = log(Price_euros))
set.seed(100)
n <- nrow(laptop_new)
ind <- sample.int(n, 1029)
laptop_train_new <- laptop_new[ind,]
laptop_test_new <- laptop_new[-ind,]

trans_model <- lm(formula = Price_euros ~ ., data = laptop_train_new)
y_trans <- laptop_train_new$Price_euros
y_new_test <- laptop_test_new$Price_euros
laptop_train_new1 <- laptop_train_new %>% select(-Price_euros)
laptop_test_new %>% select(-Price_euros)
y_pred_trans <- predict(trans_model, laptop_test_new)
SSE_trans<- sum((y_new_test- y_pred_trans)^2)
SSE_trans
SSTO_trans<- sum((y_new_test- mean(y_new_test))^2)
SSTO_trans
ratio_trans<- 1 - SSE_trans/SSTO_trans
ratio_trans

summary(trans_model)

residual_plot_trans <- tibble(y = y_trans, y_pred_trans = y_pred_trans, res_trans = res_trans) %>% ggplot(aes(x = y_pred_trans, y = res_trans))+
  geom_point()+
  geom_abline(intercept = 0, slope = 0, color = "red")+
  labs(title = "Residual plot after transform y to log(y)", x = "Fitted Values", y = "Residual")+
  theme_light()

normal_probability_plot_trans <- tibble(y = y_trans, y_pred_trans = y_pred_trans, res_trans = res_trans) %>% ggplot(aes(sample = res_trans))+
  geom_qq()+
  geom_qq_line(color = "red")+
  labs(title = "Normal Probability plot after transform y to log(y)", x = "Standard residuals", y = "Expected residuals")+
  theme_light()


############################################
##Model Seletion
minmodel <- lm(Price_euros~1, data =laptop_train_new)
s_new <- step(minmodel,scope = list(lower = ~1, 
                                upper = ~price_euros ~ Inches+ Weight_kg+Ram_GB+Touchscreen+
                                  Screen_resolution_num +Cpu_Frequency_GHz +Cpu_score+
                                  Flash+ HDD+ Hybrid+SSD +Gpu_score+ 
                                  Acer+Asus+ Dell+ Fujitsu+ HP+ Huawei+Lenovo+ MSI+Toshiba+Android+
                                  `Chrome OS`+ Linux+ `Mac OS X` +macOS+ `No OS` +`Windows 10`+`Windows 10 S`+`Windows 7`), direction = "both")

s_new$call
final_new <- lm(formula = Price_euros ~ Cpu_score + Ram_GB + Weight_kg + Cpu_Frequency_GHz + 
                  `Windows 7` + Screen_resolution_num + `No OS` + Acer + Toshiba + 
                  Touchscreen + Linux + Asus + macOS + `Mac OS X` + HP + Inches + 
                  SSD + Android, data = laptop_train_new)
final_new %>% summary()
inverseResponsePlot(final_new, key = T)
y_new <- laptop_train_new$Price_euros
laptop_train_new1 <- laptop_train_new %>% select(-Price_euros)
y_pred_new <- predict(final_new, laptop_test_new)
SSE_new <- sum((y_new_test - y_pred_new)^2)
SSE_new
SSTO_new <- sum((y_new_test - mean(y_new_test))^2)
SSTO_new
ratio_new <- 1 - SSE_new/SSTO_new
ratio_new
MSPR_new <- (SSTO_new - SSE_new)/18



residual_plot_new <- tibble(y = y_new, y_pred_new = y_pred_new, res_new = res_new) %>% ggplot(aes(x = y_pred_new, y = res_new))+
  geom_point()+
  geom_abline(intercept = 0, slope = 0, color = "red")+
  labs(title = "Residual plot after model selection", x = "Fitted Values", y = "Residual")+
  theme_light()
normal_probability_plot_new <- tibble(y = y_new, y_pred_new = y_pred_new, res_new = res_new) %>% ggplot(aes(sample = res_new))+
  geom_qq()+
  geom_qq_line(color = "red")+
  labs(title = "Normal Probability plot after model selection", x = "Standard_res", y = "Expected residuals")+
  theme_light()

ols_plot_cooksd_bar(final_new)
## Diagnostic for multicolinearity is checked
vif(final_new)




#################################################################################################################
##Remedial measures for robust regression for outliers
require(foreign)
require(MASS)
#laptop_train_new2 <- unique(laptop_train_new[-c(12, 887, 42), ])
sum(is.na(laptop_train_new2))
laptop_train_new <- laptop_train_new %>% rename(Windows_7 = `Windows 7`, No_OS = `No OS`,Mac_OS_X=`Mac OS X`)
laptop_test_new <- laptop_test_new %>% rename(Windows_7 = `Windows 7`, No_OS = `No OS`,Mac_OS_X=`Mac OS X`)
# final_new2 <- lm(formula = Price_euros ~ Cpu_score + Ram_GB + Weight_kg + Cpu_Frequency_GHz + 
#                    Windows_7 + Screen_resolution_num + No_OS + Acer + Toshiba + 
#                   Touchscreen + Linux + Asus + macOS + Mac_OS_X + HP + Inches + 
#                   SSD + Android, data = laptop_train_new2)
wts <- 1/fitted(lm(abs(residuals(final_new)) ~ Cpu_score + Ram_GB + Weight_kg + Cpu_Frequency_GHz + 
                     Windows_7 + Screen_resolution_num + No_OS + Acer + Toshiba + 
                     Touchscreen + Linux + Asus + macOS + Mac_OS_X + HP + Inches + 
                     SSD + Android, data = laptop_train_new))^2
robust_model <- rlm(formula = Price_euros ~ Cpu_score + Ram_GB + Weight_kg + Cpu_Frequency_GHz + 
                      Windows_7 + Screen_resolution_num + No_OS + Acer + Toshiba + 
                      Touchscreen + Linux + Asus + macOS + Mac_OS_X + HP + Inches + 
                      SSD + Android, data = laptop_train_new, weights=wts)
# fit_robust <- lqs(formula = Price_euros ~ Cpu_score + Ram_GB + Weight_kg + Cpu_Frequency_GHz + 
#                    Windows_7 + Screen_resolution_num + No_OS + Acer + Toshiba + 
#                    Touchscreen + Linux + Asus + macOS + Mac_OS_X + HP + Inches + 
#                    SSD + Android, data = laptop_train_new, method = "lts")
summary(robust_model)
robust_model$coefficients
Price_euros = exp{5.502134e+00 + 1.112194e-04 * Cpu_score + 4.522034e-02 * Ram_GB - 1.549282e-01 * Weight_kg + 2.534259e-01 * Cpu_Frequency_GHz + 
  4.611448e-01 * Windows_7 + 6.354001e-08 * Screen_resolution_num - 2.611588e-01 * No_OS - 1.817890e-01 * Acer + 2.950733e-01 * Toshiba + 
  1.183354e-01 * Touchscreen - 1.564195e-01 * Linux - 7.146286e-02 * Asus + 2.646900e-01 * macOS + 2.658332e-01 * Mac_OS_X + 
  3.309027e-02 * HP - 2.686983e-03 * Inches - 4.055090e-05 * SSD - 3.304614e-01 * Android}
test_final <- exp(5.502134e+00 + 1.112194e-04 * 5815.869 + 4.522034e-02 * 8 - 1.549282e-01 * 1.37 + 2.534259e-01 * 3.1 +
  6.354001e-08 * 4096000 + 2.646900e-01 *1 - 2.686983e-03 * 13 - 4.055090e-05 * 256)

y_pred_robust<- predict(robust_model, laptop_test_new)
SSE_robust <- sum((y_new_test - y_pred_robust)^2)
SSE_robust
SSTO_robust <- sum((y_new_test - mean(y_new_test))^2)
SSTO_robust
ratio_robust <- 1 - SSE_robust/SSTO_robust
ratio_robust
MSPR_robust <- (SSTO_robust - SSE_robust)/18

library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(robust_model)




residual_plot_robust <- tibble(y = y_new, y_pred_robust = y_pred_robust, res_robust = res_robust) %>% ggplot(aes(x = y_pred_robust, y = res_robust))+
  geom_point()+
  geom_abline(intercept = 0, slope = 0, color = "red")+
  labs(title = "Residual plot of Robust Model)", x = "Fitted Values", y = "Residual")+
  theme_light()
normal_probability_plot_robust <- tibble(y = y_new, y_pred_robust = y_pred_robust, res = res_robust) %>% ggplot(aes(sample = res_robust))+
  geom_qq()+
  geom_qq_line(color = "red")+
  labs(title = "Normal Probability plot of Robust Model", x = "Standard_residuals", y = "Expected residuals")+
  theme_light()

plot(robust_model)

##Remedial measure for heteroskedasticity


weighted_model <- lm(formula = Price_euros ~ Cpu_score + Ram_GB + Weight_kg + Cpu_Frequency_GHz + 
                       Windows_7 + Screen_resolution_num + No_OS + Acer + Toshiba + 
                       Touchscreen + Linux + Asus + macOS + Mac_OS_X + HP + Inches + 
                       SSD + Android, data = laptop_train_new, weights=wts)

y_pred_weighted<- predict(weighted_model, laptop_test_new)
SSE_weighted <- sum((y_new_test - y_pred_weighted)^2)
SSE_weighted
SSTO_weighted <- sum((y_new_test - mean(y_new_test))^2)
SSTO_weighted
ratio_weighted <- 1 - SSE_weighted/SSTO_weighted
ratio_weighted
MSPR_weighted <- (SSTO_weighted - SSE_weighted)/18

residual_plot_weighted <- tibble(y = y_new, y_pred_robust = y_pred_robust, res_robust = res_robust) %>% ggplot(aes(x = y_pred_robust, y = res_robust))+
  geom_point()+
  geom_abline(intercept = 0, slope = 0, color = "red")+
  labs(title = "Residual plot of Weighted Model)", x = "Fitted Values", y = "Residual")+
  theme_light()
normal_probability_plot_weighted <- tibble(y = y_new, y_pred_robust = y_pred_robust, res = res_robust) %>% ggplot(aes(sample = res_robust))+
  geom_qq()+
  geom_qq_line(color = "red")+
  labs(title = "Normal Probability plot of Weighted Model", x = "Standard_residuals", y = "Expected residuals")+
  theme_light()

library(olsrr)
ols_plot_cooksd_bar(weighted_model)

