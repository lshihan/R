# Lim Shi Han
# TP054993

# Data Import
assign_data = read.csv("C:\\Users\\ShiHan\\Desktop\\APU\\Degree\\Level 2 Sem 1\\PFDA\\Assignment\\Placement_Data_Full_Class.csv",header=TRUE)
library(ggplot2)
library(dplyr)
assign_data

View(assign_data)


#summary of dataset
summary(assign_data)

#attributes of dataset
attributes(assign_data)

#column name of dataset
colnames(assign_data)

##============================##
##    DATA PREPROCESSING      ##
##============================##


# Check data
apply(assign_data, 2, function(x)
  any(is.na(x)))

# Replace missing value to 0
assign_data[is.na(assign_data)] = 0
apply(assign_data, 2, function(x)
  any(is.na(x)))

# Rename columns
names(assign_data)[names(assign_data)=="paid"] <- "extra_class"
names(assign_data)[names(assign_data)=="activities"] <- "extra_act"
assign_data

# Add new column
assign_data$etest_group <- as.factor(ifelse(assign_data$etest_p <=72,'A','B'))
assign_data$mba_group <- as.factor(ifelse(assign_data$mba_p <=72,'A','B'))

##============================##
##    DATA TRANSFORMATION     ##
##============================##

# Check datatype
str(assign_data)

# Filter out data rows which contains salary = 0 for more accurate analysis
acc_salary <- filter(assign_data, salary!=0)

# Change address datatype to factor and rename the levels
assign_data <- assign_data %>%
  mutate(address=as.factor(address))

levels(assign_data$address) <- c("Rural","Urban")

# Change education level of Medu and Fedu
assign_data = assign_data %>%
  mutate(
    Medu = factor(Medu, labels = c("None", "Primary", "5th to 9th grade", "Secondary", "Higher")),
    Fedu = factor(Fedu, labels = c("None", "Primary", "5th to 9th grade", "Secondary", "Higher"))
  )


###========================================================================###
###========================================================================###
###                                                                        ###
###                        QUESTION AND ANALYSIS                           ###
###                                                                        ###
###========================================================================###
###========================================================================###

# Question 1: What factors affect salary? 
# Analysis (1-1) : Find the relationship between working experience and salary 
wx_salary <- acc_salary%>%
  group_by(workex)%>%
  summarise(salary=as.numeric(format(round(mean(salary)),0)))

ggplot(wx_salary,aes(x=workex,y=salary,fill=workex)) +
  geom_bar(stat="identity",width=0.5) +
  scale_fill_manual(values = c("pink","steelblue")) +
  geom_text(aes(label=salary),position=position_dodge(0.9),vjust=-0.25) +
  labs(title = "Analysis between Working Experience and Salary",
       x = "Working Experience",
       y = "Salary",
       fill = "Working Experience") +
  theme_bw()

# Analysis (1-2) : Find the relationship between gender and salary
gen_salary <- acc_salary%>%
  group_by(gender)%>%
  summarise(salary=as.numeric(format(round(mean(salary)),0)))

ggplot(gen_salary,aes(x=gender,y=salary,fill=gender)) +
  geom_bar(stat="identity",width=0.5) +
  ggtitle("Analysis between gender and salary") +
  scale_fill_manual("Legend",values = c("pink","steelblue")) +
  geom_text(aes(label=salary),position=position_dodge(0.9),vjust=-0.25) +
  labs(title = "Analysis between Gender and Salary",
       x = "Gender",
       y = "Salary",
       fill = "Gender") +
  theme_bw()


# Analysis (1-3) : Find the relationship between age and salary
age_salary <- acc_salary%>%
  group_by(age)%>%
  summarise(salary=as.numeric(format(round(mean(salary)),0)))

ggplot(age_salary,aes(x=age,y=salary,fill=age)) +
  geom_bar(stat="identity",width=0.5) +
  geom_text(aes(label=salary),position=position_dodge(0.9),vjust=-0.25) +
  labs(title = "Analysis between Age and Salary",
       x = "Age",
       y = "Salary",
       fill = "Age") +
  theme_bw()

# Analysis (1-4) : Find the relationship between employability test and salary
etest_salary <- acc_salary%>%
  group_by(etest_group)%>%
  summarise(salary=as.numeric(format(round(mean(salary)),0)))

ggplot(etest_salary,aes(x=etest_group,y=salary,fill=etest_group)) +
  geom_bar(stat="identity",width=0.5) +
  geom_text(aes(label=salary),position=position_dodge(0.9),vjust=-0.25) +
  labs(title = "Analysis between Employability test and Salary",
       x = "Employability Test Group",
       y = "Salary",
       fill = "Employability Test Group") +
  theme_bw()

# Analysis (1-5) : Find the relationship between MBA Percentage and salary
mba_salary <- acc_salary%>%
  group_by(mba_group)%>%
  summarise(salary=as.numeric(format(round(mean(salary)),0)))

ggplot(mba_salary,aes(x=mba_group,y=salary,fill=mba_group)) +
  geom_bar(stat="identity",width=0.5) +
  geom_text(aes(label=salary),position=position_dodge(0.9),vjust=-0.25) +
  labs(title = "Analysis between MBA Percentage and Salary",
       x = "MBA Group",
       y = "Salary",
       fill = "MBA Group") +
  theme_bw()

# Question 2: Does living place affect education level?
# Analysis (2-1) : Analysis of student staying in Urban or Rural

add <- assign_data%>%
  group_by(address)%>%
  summarise(count=n())

ggplot(add, aes(x="", y=count, fill=address)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(title = "Analysis on Candidate's living place") +
  theme_void() 

# Analysis (2-2) : Analysis of candidate's type of school
school_type <- mutate(acc_salary%>%
                        group_by(ssc_b,hsc_b)%>%
                        summarise(count=n()))

ggplot(school_type,aes(x=ssc_b,y=count,fill=hsc_b)) +
  geom_bar(stat="identity",position="dodge") + 
  geom_text(aes(label=count),position=position_dodge(0.9),vjust=-0.25) +
  labs(title = "Where students study for secondary education and higher education?",
       x = "Secondary School",
       y = "Count",
       fill = "Higher Secondary School") +
  theme_bw()

# Analysis 2-3: Find the relationship between school type and degree percentage
school_type = select(assign_data, ssc_b, hsc_b, degree_p)
school_type <- mutate(school_type%>%
                        group_by(ssc_b,hsc_b)%>%
                        summarise(degree_p))

ggplot(school_type,aes(x=ssc_b,y=degree_p,fill=hsc_b)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1,position=position_dodge(0.9)) + 
  labs(title = "Where students study affects their degree course mark",
       x = "Secondary School Type",
       y = "Degree Course Mark",
       fill = "Higher Secondary School Type")+
  theme_bw()


# Analysis (2-4) : Find the relationship between living place and degree percentage
add_degree <- mutate(assign_data%>%
                       group_by(address)%>%
                       summarise(degree_p))

ggplot(add_degree, aes(x=address , y=degree_p , fill=address)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1,fill=c("pink","steelblue")) +
  labs(title = "Degree Course Mark grouped by Living Place",
       x = "Living Place",
       y = "Degree Course Mark",
       fill = "Living Place") +
  theme_bw()

# Analysis 2-5 : Find the relastionship between living place and employability test
add_etest <- assign_data%>%
  group_by(address)%>%
  summarise(etest_p)

ggplot(add_etest,aes(x=address,y=etest_p,fill=address)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1,fill=c("pink","steelblue")) +
  labs(title = "Employability Test Percentage grouped by Living Place",
       x = "Living Place",
       y = "Employability Test Percentage",
       fill = "Living Place") +
  theme_bw()


# Question 3: What factors affect degree_p?
# Analysis (3-1) : Find the relationship between degree type and degree percentage
tc_degree <- assign_data%>%
  group_by(degree_t)%>%
  summarise(count=n())

t_degree <- assign_data%>%
  group_by(degree_t)%>%
  summarise(degree_p)

ggplot(t_degree,aes(x=degree_t,y=degree_p,fill=degree_t)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width = 0.1) +
  labs(title = "Degree Course Mark grouped by Degree Type",
       x = "Degree Type",
       y = "Degree Course Mark",
       fill = "Degree Type") + 
  theme_bw()

# Analysis (3-2) : Find the relationship between extra activities and degree percentage
act_degree <- assign_data%>%
  group_by(extra_act)%>%
  summarise(degree_p)

ggplot(act_degree,aes(x=extra_act,y=degree_p,fill=extra_act)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, fill=c("pink","steelblue")) +
  labs(title = "Degree Course Mark grouped by Extra Activities",
       x = "Extra Activities",
       y = "Degree Course Mark",
       fill = "Extra Activities") +
  theme_bw()


# Analysis (3-3) : Find the relationship between extra classes and degree percentage
class_degree <- assign_data%>%
  group_by(extra_class)%>%
  summarise(degree_p)

ggplot(class_degree,aes(x=extra_class,y=degree_p,fill=extra_class)) +
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1,fill=c("pink","steelblue")) +
  labs(title = "Degree Course Mark grouped by Extra Classes",
       x = "Extra Classes",
       y = "Degree Course Mark",
       fill = "Extra Classes") +
  theme_bw()

# Analysis (3-4) : Find the relationship between family education support and degree percentage

famsup_d <- assign_data%>%
  group_by(famsup)%>%
  summarise(degree_p)

ggplot(famsup_d, aes(x = famsup, y = degree_p, fill = famsup)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width = 0.1, fill=c("pink","steelblue")) +
  labs(title = "Degree Course Mark grouped by family education support",
       x = "Family Support",
       y = "Degree Course Mark",
       fill = "Family Support") +
    theme_bw()


# Analysis (3-5) : Find the relationship between Parent Education Level and degree percentage
pedu1 <-assign_data%>%
  group_by(Medu,Fedu)%>%
  summarise(count=n())

pedu_degree <- assign_data%>%
  group_by(Medu, Fedu)%>%
  summarise(degree_p)

ggplot(pedu_degree, aes(x = Medu, y = degree_p, fill = Fedu)) + 
  geom_boxplot(width=0.5,position = position_dodge(0.9)) +
  labs(title = "Degree course mark of students grouped by mother's education level",
       x = "Mother's Education Level",
       y = "Average Grade",
       fill = "Father's Education Level") +
  theme_bw()


# Analysis (3-6) : Find the relationship between internet access and degree percentage
internet_n <- assign_data%>%
  group_by(internet)%>%
  summarise(count=n())

internet_degree <- assign_data%>%
  group_by(internet)%>%
  summarise(degree_p)

ggplot(internet_degree,aes(x=internet,y=degree_p,fill=internet)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill = c("pink","steelblue")) +
  labs(title = "Degree Course Mark grouped by Internet Access and Living Place",
       x = "Internet Access",
       y = "Degree Course Mark",
       fill = "Internet Access") +
  theme_bw()

# Code test(old code)
# workex_status <- assign_data%>%
#   group_by(workex, status)%>%
#   summarise(wcount=n())
# 
# 
# bar3 <- ggplot(medu_degree,aes(x=Medu,y=degree_p,fill=Medu)) +
#   geom_bar(stat="identity",width=0.5) +
#   scale_fill_manual("Legend",values = c("pink","steelblue","red","yellow","black")) 
# bar3
# 
# bar4 <- ggplot(fedu_degree,aes(x=Fedu,y=degree_p,fill=Fedu)) +
#   geom_bar(stat="identity",width=0.5) +
#   scale_fill_manual("Legend",values = c("pink","steelblue","red","yellow","black")) 
# bar4
# 
# gen = aggregate(assign_data$salary, list(assign_data$gender), FUN=mean )
# colnames(gen) <- c("Gender","Salary")
# gen
# # Replace missing value of Salary to the average
# assign_data$salary = ifelse(is.na(assign_data$salary),
#                      ave(assign_data$salary, FUN = function (x)mean(x, na.rm = TRUE)),
#                      assign_data$salary)
# assign_data$salary = as.numeric(format(round(assign_data$salary, 0)))
# assign_data
#
# add1 <- assign_data%>%
#   group_by(address)%>%
#   summarise(count=n())
# 
# miss <- filter(assign_data,salary==0)
# miss1 <- miss%>%
#   group_by(sl_no)%>%
#   summarise(salary)
# 
# x <- assign_data%>%
#   group_by(Medu, Fedu)%>%
#   summarise(count=n())