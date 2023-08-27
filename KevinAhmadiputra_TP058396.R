#KEVIN AHMADIPUTRA
#TP058396

library(ggplot2)    
library(dplyr)      
library(epiDisplay) 
library(hrbrthemes)
library(tidyverse)
library(gganimate)  
#library(plyr)       #for make freq tables/data frame for chart

#3.DATA IMPORT/CLEANING/PRE-PROCESSING/TRANSFORMATION
#3.1 IMPORT DATA
old_ea= read.csv(file = "C:\\Users\\Viz12\\Desktop\\PFDA Assign\\employee_attrition.csv", header=TRUE, sep=",")


#3.2 FILTER DATASET
unique(old_ea$EmployeeID)
#Change the type to date first !!!
old_ea$recorddate_key =  as.Date(old_ea$recorddate_key, format =  "%m/%d/%Y") 

ea = old_ea %>%
  group_by(EmployeeID) %>%
  filter(recorddate_key == max(recorddate_key,na.rm = TRUE)) %>%
  distinct(EmployeeID, .keep_all = TRUE) %>% arrange(EmployeeID)

unique(ea$EmployeeID)


#3.3 CHECK & CHANGE VARIABLES TYPE.
#CHECK ALL VARIABLE TYPE.
class(ea$EmployeeID)
class(ea$recorddate_key)
class(ea$birthdate_key)
class(ea$orighiredate_key)
class(ea$terminationdate_key)
class(ea$age)
class(ea$length_of_service)
class(ea$city_name)
class(ea$department_name)
class(ea$job_title)
class(ea$store_name)
class(ea$gender_short)
class(ea$gender_full)
class(ea$termreason_desc)
class(ea$termtype_desc)
class(ea$STATUS_YEAR)
class(ea$STATUS)
class(ea$BUSINESS_UNIT)

#CHANGE VARIABLES TYPE.
ea$recorddate_key =  as.Date(ea$recorddate_key, format =  "%m/%d/%Y")
ea$birthdate_key =  as.Date(ea$birthdate_key, format =  "%m/%d/%Y")
ea$orighiredate_key =  as.Date(ea$orighiredate_key, format =  "%m/%d/%Y")
ea$terminationdate_key =  as.Date(ea$terminationdate_key, format =  "%m/%d/%Y")
ea$store_name = as.character(ea$store_name)
ea$STATUS_YEAR=as.character(ea$STATUS_YEAR)
ea$age = as.numeric(ea$age)
ea$length_of_service = as.numeric(ea$length_of_service)


#3.4 MUTATE
#ADD NEW VARIABE LENGTH_OF_SERVICE IN MONTHS
ea = ea %>%
  group_by(EmployeeID) %>% 
  mutate(length_months = ifelse(orighiredate_key > terminationdate_key, 
                                round(length_of_service*12, digits = 0), 
                                round((terminationdate_key - orighiredate_key)/30, digits = 0))) %>% 
  arrange(EmployeeID)


#ADD NEW VARIABLES AGE CATEGORY
ea = ea %>%
  group_by(EmployeeID) %>% 
  mutate(age_category = case_when(age >= 65 ~ 'Senior',
                                  age >= 45 ~ 'Old Adult',
                                  age >= 25 ~ 'Adult',
                                  age >= 15 ~ 'Youth',
                                  TRUE ~ 'Child')) %>% arrange(EmployeeID)


#3.5 Correcting Inconsistent Data
ea$city_name[which(ea$city_name == "New Westminister")] = "New Westminster"



#4 ANALYSIS
#4.1 Age (age & age_category)
#SUMMARY STATISTICS
summary(ea$age)    

#FREQUENCY TABLE & BAR CHART
Age_Category = ea$age_category
tab1(Age_Category, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = TRUE, missing = TRUE,
     horiz = FALSE, cex = 1, cex.names = 1, 
     main = "Bar Chart of Age Category", xlab = "auto", 
     ylab = "Frequency", col = "auto")

#BOXPLOT
box1 = ea %>% 
  group_by(age_category, age)%>%
  summarise(freq = n())

ggplot(box1, aes(fill=age_category, x=age_category, y = age)) + 
  geom_boxplot()+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize =0.5)+
  #geom_text(aes(label = age),size = 1, hjust = 0.5 , vjust = -0.5,position = position_dodge(width = 1))+
  labs(fill = "Age Category", x="Age Category", y="Age") +
  scale_fill_manual(values=c("snow1", "brown2", "green3", "steelblue3"))


#4.2 Length of Works (Length_months)
#SUMMARY STATISTICS
summary(ea$length_months) 

#FREQUENCY TABLE
Length_Months = ea$length_months
tab1(Length_Months, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = FALSE, missing = TRUE)

#HISTOGRAM
ggplot(data = ea, aes(x=ea$length_months)) + 
  geom_histogram(aes(y=..density..),
                 col="cadetblue2", fill="cadetblue2") +
  geom_density(col="firebrick2")+
  labs(title="Histogram of Length of Works (Months) ", x="Month", y="Frequency")


#4.3 City Name
#FREQUENCY TABLE
City_Name = ea$city_name
tab1(City_Name, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = FALSE, missing = TRUE)

#CIRCULAR BAR CHART
unloadNamespace("epiDisplay")
bar1 = ea %>% group_by(city_name) %>%
  summarise(freq = n())
bar1 = bar1 %>% mutate(id = 1:nrow(bar1))

nrows = nrow(bar1)
angle =  90 - 360*(bar1$id - 0.5)/nrows
label = bar1
label$hjust = ifelse(angle < -90, 1, 0)
label$angle = ifelse(angle < -90, angle + 180, angle)
ggplot(bar1, aes(x = as.factor(id), y = freq)) +
  geom_bar(stat = "identity", fill = alpha("tan", 0.7)) +
  ylim(-200, 1500) +
  theme_void()+
  coord_polar(start = 0) +
  geom_text(data = label, aes(x = id, y = freq + 10, label = city_name, hjust = hjust),
            color = "black", fontface = "bold", alpha = 0.6, size = 3,
            angle = label$angle, inherit.aes = TRUE)
library(epiDisplay)

#4.4 Department Name
#FREQUENCY TABLE
Department_Name = ea$department_name
tab1(Department_Name, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = FALSE, missing = TRUE)

#BARCHART
ggplot(data = ea, aes(y=ea$department_name)) + 
  geom_bar(col="grey", fill="deepskyblue") +
  labs(title="Bar Chart of Department Name", x="Frequency", y="")


#4.5 Job Title
#FREQUENCY TABLE
Job_Title = ea$job_title
tab1(Job_Title, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = FALSE, missing = TRUE)

#BARCHART
ggplot(data = ea, aes(y=ea$job_title)) + 
  geom_bar(col="darkslategray", fill="darkseagreen") +
  labs(title="Bar Chart of Job Title", x="Frequency", y="")


#4.6 Store Name
#FREQUENCY TABLE
Store_Name= ea$store_name
tab1(Store_Name, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = FALSE, missing = TRUE)

#BARCHART
ggplot(data = ea, aes(y=ea$store_name)) + 
  geom_bar(col="slategrey", fill="slategray3") +
  labs(title="Bar Chart of Store Name", x="Frequency", y="")


#4.7 Gender
#FREQUENCY TABLES
Gender = ea$gender_full
tab1(Gender, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = FALSE, missing = TRUE)

#PIE CHART
pie1 =  ea %>% 
  group_by(gender_full)%>%
  summarise(freq = n()) %>% mutate(per = freq/sum(freq))

ggplot(pie1, aes(x="", y=freq, fill=gender_full)) + 
  geom_bar(stat="identity", color = "lavenderblush3")+
  geom_text(aes(label = scales::percent(per)),
            position = position_stack(vjust=0.5))+
  coord_polar("y", start=0)+
  labs(x="", y="Frequency", fill ="Gender")+
  scale_fill_manual(values=c("lavenderblush", "lavender"))


#4.8 Termination Reason
#FREQUENCY TABLES & BAR CHART
Termination_Reason = ea$termreason_desc
tab1(Termination_Reason, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = TRUE, missing = TRUE,
     horiz = FALSE, cex = 1, cex.names = 1, 
     main = "Bar Chart of Termination Reason", xlab = "auto", 
     ylab = "Frequency", col = c("ivory","ivory2","ivory3","ivory4"))


#4.9 Termination Type
#FREQUENCY TABLES & BAR CHART
Termination_Type = ea$termtype_desc
tab1(Termination_Type, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = TRUE, missing = TRUE,
     horiz = FALSE, cex = 1, cex.names = 1, 
     main = "Bar Chart of Termination Type", xlab = "auto", 
     ylab = "Frequency", col = c("mistyrose","mistyrose3","mistyrose4"))


#4.10 Status
#FREQUENCY TABLES
Status= ea$STATUS
tab1(Status, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = FALSE, missing = TRUE)

#PIE CHART
pie2 = ea %>% 
  group_by(STATUS)%>%
  summarise(freq = n())%>% mutate(per = freq/sum(freq))

ggplot(pie2, aes(x="", y=freq, fill=STATUS)) + 
  geom_bar(stat="identity", color = "dodgerblue3")+
  geom_text(aes(label = scales::percent(per)),
            position = position_stack(vjust=0.5))+
  coord_polar("y", start=0)+
  labs(title="Bar Chart of Status", x="", y="Frequency", fill ="Status")+
  scale_fill_manual(values=c("lightskyblue", "lightskyblue3"))


#4.11 Business Unit
#FREQUENCY TABLES & BAR CHART
Business_Unit= ea$BUSINESS_UNIT
tab1(Business_Unit, decimal = 1, sort.group = TRUE, 
     cum.percent = TRUE, graph = TRUE, missing = TRUE,
     horiz = FALSE, cex = 1, cex.names = 1, 
     main = "Bar Chart of Business Unit", xlab = "auto", 
     ylab = "Frequency", col = c("cadetblue", "cadetblue1"))



#5.0 Question & Analysis
#5.1 Question 1 : Why is the number of female staff higher than male staff?
#5.1.1 Analysis 1-1: Filter gender.
ea_male = ea %>%
  group_by(EmployeeID) %>% filter(gender_short == "M") %>% arrange(EmployeeID)

ea_female = ea %>%
  group_by(EmployeeID) %>% filter(gender_short == "F") %>% arrange(EmployeeID)


#5.1.2 Analysis 1-2: Find the relations between gender and age.
summary(ea_female$age)
summary(ea_male$age)

plot1b = ea %>% 
  group_by(gender_full, age_category)%>%
  summarise(amount = n()) %>% mutate(frame =rep('b'))

plot1a = plot1b %>% mutate(amount = 0, frame=rep('a'))

plot1 = rbind(plot1a, plot1b)
plot_1 = ggplot(plot1, aes(x=age_category, y=amount, fill=gender_full)) + 
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label = amount),size = 4, hjust = 0.5 , vjust = -0.5,
            position = position_dodge(width = 1))+
  labs(fill ="Gender", x="Age Category", y="Frequency") +
  scale_fill_manual(values=c("peachpuff", "peachpuff3"))+
  transition_states(frame,transition_length = 2,state_length = 1)
animate(plot_1, renderer = gifski_renderer("Analysis 1-1.gif"))


#5.1.3 Analysis 1-3: Locate the cities with a lot of female employees.
plot2 = ea %>%
  group_by(gender_full, city_name)%>%
  summarise(amount = n()) 

ggplot(plot2, aes(fill=plot2$gender_full, y = plot2$city_name, x=plot2$amount)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = plot2$amount),size = 3, hjust = -0.5 , vjust = 0.5,position=position_dodge(1))+
  labs(title= "Grouped Bar Chart for Gender in each City", fill = "Gender", x="Frequency", y="City Name") +
  scale_fill_manual(values=c("peachpuff", "peachpuff3"))


plot3 = ea_female %>%
  group_by(age_category, city_name)%>%
  summarise(amount = n()) 

ggplot(plot3, aes(fill=plot3$age_category, y = plot3$city_name, x=plot3$amount)) + 
  geom_bar(position="stack", stat="identity") +
  #geom_text(aes(label = plot3$amount),size = 3, hjust = 1 , vjust = 0.5, position ="stack")+
  labs(title= "Stacked Bar Chart for Female Age Category", fill = "Age Category", x="Frequency", y="City Name") +
  scale_fill_manual(values=c("paleturquoise1","paleturquoise2","paleturquoise3","paleturquoise4"))


#5.1.4 Analysis 1-4: Discover in which department the female more dominant.
plot4 = ea %>%
  group_by(gender_full, department_name)%>%
  summarise(amount = n()) 

ggplot(plot4, aes(fill=plot4$gender_full, y = plot4$department_name, x=plot4$amount)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = plot4$amount),size = 4, hjust = 0 , vjust = 0.5, position = position_dodge(width = 1))+
  labs(title= "Grouped Bar Chart for Gender in each Department", fill = "Gender", x="Frequency", y="Department Name") +
  scale_fill_manual(values=c("peachpuff", "peachpuff3"))




#5.2 Question 2 : Why do newly joined employees leave the company sooner?
#5.2.1 Analysis 2-1: Find the new employees.
ea_new_emp = ea %>% filter(length_months <= 18)
plot5 = ea_new_emp%>% 
  group_by(STATUS)%>%
  summarise(freq = n()) %>% mutate(per = freq/sum(freq))

ggplot(plot5, aes(x="", y=freq, fill=STATUS)) + 
  geom_bar(stat="identity", color = "dodgerblue3")+
  geom_text(aes(label = scales::percent(per)),
            position = position_stack(vjust=0.5))+
  coord_polar("y", start=0)+
  labs(x="", y="Frequency", fill ="Status")+
  scale_fill_manual(values=c("lightskyblue", "lightskyblue3"))


#5.2.2 Analysis 2-2: Find out how age affects why new employees leave the company.
plot8 = ea_new_emp%>% 
  group_by(STATUS, age_category,age)%>%
  summarise(freq = n()) %>% group_by(STATUS, age_category) %>% mutate(tot = sum(freq))

ggplot(plot8, aes(fill=STATUS, x=age_category, y = tot)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = tot),size = 5, hjust = 0.5 , vjust = -0.5, 
            position = position_dodge(width = 1))+
  labs(fill = "STATUS", x="Age Category", y="Frequency") +
  scale_fill_manual(values=c("lightskyblue", "lightskyblue3"))

ggplot(plot8, aes(fill=STATUS, x=age_category, y = age)) + 
  geom_boxplot()+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize =0.5)+
  geom_text(aes(label = age),size = 3, hjust = 0.5 , vjust = -0.5, 
            position = position_dodge(width = 1))+
  labs(fill = "STATUS", x="Age Category", y="Age") +
  scale_fill_manual(values=c("lightskyblue", "lightskyblue3"))


#5.2.3 Analysis 2-3: Find the job title of most new employees.
plot6 = ea_new_emp%>% 
  group_by(STATUS,job_title)%>%
  summarise(freq = n()) 

ggplot(plot6, aes(fill=STATUS, x=freq, y = job_title)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = freq),size = 4, hjust = 0.5 , vjust = 0.5, 
            position = position_dodge(width = 1))+
  labs( fill = "STATUS", x="Frequency", y="Job Title") +
  scale_fill_manual(values=c("lightskyblue", "lightskyblue3"))


#5.2.4 Analysis 2-4: Discover the most terminated reason of new workers.
plot9 = ea_new_emp%>% 
  group_by(STATUS, termreason_desc)%>%
  summarise(freq = n()) 

ggplot(plot9, aes(fill=STATUS, x=termreason_desc, y = freq)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = freq),size = 5, hjust = 0.5 , vjust = -0.5, 
            position = position_dodge(width = 1))+
  labs(fill = "STATUS", x="Termination Reason", y="Frequency") +
  scale_fill_manual(values=c("lightskyblue", "lightskyblue3"))


#5.2.5 Analysis 2-5: Locate the cities with a high number of new staff.
plot7 = ea_new_emp%>% 
  group_by(STATUS, city_name)%>%
  summarise(freq = n()) 

ggplot(plot7, aes(fill=STATUS, x=freq, y = city_name)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = freq),size = 4, hjust = 0 , vjust = 0.3, 
            position = position_dodge(width = 1))+
  labs(fill = "STATUS", x="Frequency", y="City Name") +
  scale_fill_manual(values=c("lightskyblue", "lightskyblue3"))




#5.3 Question 3 : Why do more staff have regular jobs that usually easy to do & don't require higher education?
#produce clerk, shelf stocker, cashier

#5.3.1 Analysis 3-1: Find the employees with that general job title.
ea_job = ea %>% 
  filter(job_title == "Produce Clerk" | job_title == "Shelf Stocker" | job_title == "Cashier")

plot10 = ea_job %>% 
  group_by(job_title)%>%
  summarise(freq = n()) 

ggplot(data = plot10, aes(x=job_title, y=freq)) + 
  geom_bar(stat ="identity", col="azure3", 
           fill=c("turquoise4","aquamarine3","darkseagreen1")) +
  geom_text(aes(label = freq),size = 5, hjust = 0.5 , vjust = -0.5)+
  labs(x="Job Title", y="Frequency")


#5.3.2 Analysis 3-2: Find relations between age and those job titles
plot11 = ea_job %>% 
  group_by(job_title, age_category, age)%>%
  summarise(freq = n()) %>% group_by(job_title, age_category) %>% mutate(tot = sum(freq))

ggplot(plot11, aes(fill=job_title, x=age_category, y = tot)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = tot),size = 5, hjust = 0.5 , vjust = -0.5, 
            position = position_dodge(width = 1))+
  labs(fill = "Job Title", x="Age Category", y="Frequency") +
  scale_fill_manual(values=c("turquoise4","aquamarine3","darkseagreen1"))

ggplot(plot11, aes(fill=job_title, x=age_category, y = age)) + 
  geom_boxplot()+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize =0.5)+
  #geom_text(aes(label = age),size = 1, hjust = 0.5 , vjust = -0.5,position = position_dodge(width = 1))+
  labs(fill = "job_title", x="Age Category", y="Age") +
  scale_fill_manual(values=c("turquoise4","aquamarine3","darkseagreen1"))



#5.3.3 Analysis 3-3: Find out if gender affects the employees taking those jobs or not.
plot12 = ea_job %>% 
  group_by(job_title, gender_full)%>%
  summarise(freq = n()) 

ggplot(plot12, aes(fill=job_title, x=gender_full, y = freq)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = freq),size = 5, hjust = 0.5 , vjust = -0.5, 
            position = position_dodge(width = 1))+
  labs(fill = "Job Title", x="Gender", y="Frequency") +
  scale_fill_manual(values=c("turquoise4","aquamarine3","darkseagreen1"))


#5.3.4 Analysis 3-4: Locate the cities with a high number of staff with those job titles
plot13 = ea_job %>% 
  group_by(job_title, city_name)%>%
  summarise(freq = n()) 

ggplot(plot13, aes(fill=job_title, x=freq, y = city_name)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = freq),size = 3, hjust = 0 , vjust = 0.2, 
            position = position_dodge(width = 1))+
  labs(fill = "Job Title", x="Frequency", y="City Name") +
  scale_fill_manual(values=c("turquoise4","aquamarine3","darkseagreen1"))



#5.4 Question 4 : Why do most employees voluntarily leave the company?

#5.4.1 Analysis 4-1: Find the employees with voluntary termination type.
ea_vol = ea %>% filter(termtype_desc == "Voluntary")

#5.4.2 Analysis 4-2: Find the relations between that termination type and termination reason.
plot14b = ea_vol %>% 
  group_by(termreason_desc)%>%
  summarise(freq = n()) %>% mutate(frame =rep('b'))

plot14a = plot14b %>% mutate(freq = 0, frame=rep('a'))

plot14 = rbind(plot14a, plot14b)

plot_14 = ggplot(plot14, aes(x=termreason_desc, y=freq, fill=termreason_desc)) + 
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label = freq),size = 5, hjust = 0.5 , vjust = -0.5,
            position = position_dodge(width = 1))+
  labs(fill ="Termination Reason", x="Termination Reason", y="Frequency") +
  scale_fill_manual(values=c("plum2", "plum4"))+
  transition_states(frame,transition_length = 2,state_length = 1)
animate(plot_14, renderer = gifski_renderer("Analysis 4-2.gif"))



#5.4.3 Analysis 4-3: Find out if age affects it or not.
plot15 = ea_vol %>% 
  group_by(termreason_desc, age_category, age)%>%
  summarise(freq = n()) %>% group_by(termreason_desc, age_category) %>% mutate(tot = sum(freq))

ggplot(plot15, aes(fill=termreason_desc, x=age_category, y = tot)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = tot),size = 5, hjust = 0.5 , vjust = -0.5, 
            position = position_dodge(width = 1))+
  labs(fill = "Termination Reason", x="Age Category", y="Frequency") +
  scale_fill_manual(values=c("plum2", "plum4"))


ggplot(plot15, aes(fill=termreason_desc, x=age_category, y = age)) + 
  geom_boxplot()+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize =0.5)+
  #geom_text(aes(label = age),size = 1, hjust = 0.5 , vjust = -0.5,position = position_dodge(width = 1))+
  labs(fill = "Termination Reason", x="Age Category", y="Age") +
  scale_fill_manual(values=c("plum2", "plum4"))


#5.4.4 Analysis 4-4: Find out if gender affects it or not.
plot21 = ea_vol %>% 
  group_by(termreason_desc, gender_full)%>%
  summarise(freq = n()) 

ggplot(plot21, aes(fill=termreason_desc, x=gender_full, y = freq)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = freq),size = 5, hjust = 0.5 , vjust = -0.5, 
            position = position_dodge(width = 1))+
  labs(fill = "Termination Reason", x="Gender", y="Frequency") +
  scale_fill_manual(values=c("plum2", "plum4"))


#5.4.5 Analysis 4-5: How long do the employees work before voluntarily leaving?
plot23 = ea_vol %>% 
  group_by(termreason_desc, length_months)%>%
  summarise(freq = n()) 

ggplot(plot23, aes(fill=termreason_desc, x=termreason_desc, y = length_months)) + 
  geom_boxplot()+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize =0.5)+
  #geom_text(aes(label = freq),size = 4, hjust = 0.5 , vjust = -0.5,position = position_dodge(width = 1))+
  labs(fill = "Termination Reason", x="Age Category", y="Age") +
  scale_fill_manual(values=c("plum2", "plum4"))


#5.5 Question 5 : Why are most of the employee statuses still active?

#5.5.1 Analysis 5-1: Find out employee status based on status year.
plot20 = old_ea %>%
  group_by(STATUS_YEAR,STATUS) %>%
  summarise(freq = n()) %>% mutate(move = as.numeric(STATUS_YEAR))
plot20$STATUS_YEAR = as.character(plot20$STATUS_YEAR)

plot_20= ggplot(plot20, aes(x=STATUS_YEAR, y=freq, group=STATUS, color=STATUS)) +
  geom_line(aes(color = STATUS), size = 1) +
  geom_point() + 
  geom_text(aes(label = freq),color = "black",size = 5, hjust = 0.5 , vjust = -0.5)+
  scale_color_manual (values = c("darkseagreen3", "firebrick3"))+
  labs(x="Status Year", y="Frequency")+
  transition_reveal(move)
animate(plot_20, renderer = gifski_renderer("Analysis 5-1.gif"))



#5.5.2 Analysis 5-2: Fiter the employees with active status.
ea_active = ea %>% filter (STATUS == "ACTIVE")


#5.5.3 Analysis 5-3: Find out the age of the employees with active status.
plot16 = ea_active %>% 
  group_by(age_category,age)%>%
  summarise(freq = n()) %>% group_by(age_category) %>% mutate(tot = sum(freq))

ggplot(plot16, aes(fill=age_category, x=age_category, y = tot)) + 
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label = tot),size = 5, hjust = 0.5 , vjust = -0.5,
            position = position_dodge(width = 1))+
  labs(fill = "Age Category", x="Age Category", y="Frequency") +
  scale_fill_manual(values=c("lightpink","lightpink3", "mediumpurple", "mediumpurple4"))


ggplot(plot16, aes(fill=age_category, x=age_category, y = age)) + 
  geom_boxplot()+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize =0.5)+
  #geom_text(aes(label = age),size = 1, hjust = 0.5 , vjust = -0.5,position = position_dodge(width = 1))+
  labs(fill = "Age Category", x="Age Category", y="Age") +
  scale_fill_manual(values=c("lightpink","lightpink3", "mediumpurple", "mediumpurple4"))


#5.5.4 Analysis 5-4: Find the relations between job titles and status.
plot17 = ea_active %>% 
  group_by(job_title)%>%
  summarise(freq = n()) 

ggplot(data = plot17, aes(x=freq, y=job_title)) + 
  geom_bar(stat ="identity", col="mintcream", 
           fill="snow3") +
  geom_text(aes(label = freq),size = 4, hjust = 0.5 , vjust = 0.25)+
  labs(x="Frequency", y="Job Title")

#5.5.5 Analysis 5-5: Find out the gender of the employees with active status.
plot18 = ea_active %>% 
  group_by(gender_full)%>%
  summarise(freq = n()) %>% mutate(per = freq/sum(freq)) 

ggplot(plot18, aes(x="", y=freq, fill=gender_full)) + 
  geom_bar(stat="identity", color = "plum")+
  geom_text(aes(label = scales::percent(per)),
            position = position_stack(vjust=0.5))+
  coord_polar("y", start=0)+
  labs(x="", y="Frequency", fill ="Gender")+
  scale_fill_manual(values=c("lavenderblush", "lavender"))

plot24 = old_ea %>%
  group_by(gender_full, STATUS_YEAR) %>% filter (STATUS == "ACTIVE") %>%
  summarise(freq = n()) %>% mutate(move = as.numeric(STATUS_YEAR))
plot24$STATUS_YEAR = as.character(plot24$STATUS_YEAR)

plot_24= ggplot(plot24, aes(x=STATUS_YEAR, y=freq, group=gender_full, color=gender_full)) +
  geom_line(aes(color = gender_full), size = 2) +
  geom_point() + 
  geom_text(aes(label = freq),color = "gray90",size = 5, hjust = 0.5 , vjust = -0.5)+
  theme_dark()+
  scale_color_manual (values = c("lavenderblush", "lavender"))+
  labs(x="Status Year", y="Frequency", color = "Gender")+
  transition_reveal(move)
animate(plot_24, renderer = gifski_renderer("Analysis 5-5.gif"))

