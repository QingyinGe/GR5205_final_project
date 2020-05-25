library(tidyverse)
laptop <- read_csv("laptop.csv")
head(laptop)
sum(is.na(laptop))
#str(laptop)
table(laptop$Product)
table(laptop$Memory)
laptop_new <- laptop %>% mutate(Inches = as.integer(Inches)) %>% 
  separate(Product, into = c("Product", "Model"), sep = " ") %>% select(-Model) %>% mutate(Weight_kg = as.double(str_remove(Weight, "kg"))) %>%
  separate(Memory, into = c("Memory", "Memory_type"), sep = " ") %>% mutate(Memory = ifelse(str_detect(Memory, "TB"), "1024GB", Memory)) %>% mutate(Memory_GB = as.double(str_remove(Memory, "GB"))) %>%
  mutate(Ram_GB = as.double(str_remove(Ram, "GB"))) %>% select(-c(Memory, Weight, Ram)) 
laptop_new  
sum(is.na(laptop_new))  
unique(laptop_new$Inches)
unique(laptop_new$OpSys)
unique(laptop_new$Memory_type)
table(laptop_new$Cpu)


View(laptop_new1)
laptop_new1 <- laptop_new %>% mutate(Screen_resolution = lapply((str_split(ScreenResolution, " ")), tail, n = 1L), Touchscreen = ifelse(str_detect(ScreenResolution, "Touchscreen"), TRUE, FALSE)) %>%
  select(-ScreenResolution) %>% mutate(Cpu_Frequency = lapply((str_split(Cpu, " ")), tail, n = 1L)) %>% filter(Cpu != "Samsung Cortex A72&A53 2.0GHz") %>% 
  mutate(Cpu_brand = lapply((str_split(Cpu, " ")), head, n = 1L))
table(laptop_new1$Touchscreen)

View(laptop_new1_Cpu_intel)
laptop_new1_Cpu_intel <- laptop_new1 %>% filter(Cpu_brand == "Intel") %>% mutate(Cpu_model1 = lapply(str_split(Cpu, " "), '[[', 2), Cpu_model2 = lapply(str_split(Cpu, " "), '[[', 3))
table((laptop_new1 %>% filter(Cpu_brand == "Intel"))$Cpu)

View(laptop_new1_Cpu_AMD)
laptop_new1_Cpu_AMD <- laptop_new1 %>% filter(Cpu_brand == "AMD") %>% mutate(Cpu_model1 = lapply(str_split(Cpu, " "), '[[', 2), Cpu_model2 = lapply(str_split(Cpu, " "), '[[', 3))
table((laptop_new1 %>% filter(Cpu_brand == "AMD"))$Cpu)

View(laptop_new2)
laptop_new2 <- rbind(laptop_new1_Cpu_intel, laptop_new1_Cpu_AMD)



