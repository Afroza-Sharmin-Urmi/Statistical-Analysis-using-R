library(tidyverse)

setwd("E:/Space/project/archive")
travel <- read.csv('TravelInsu.csv')
head(travel, 3)

plot(travel)


#building model to analyze travel insurance based on people's annual income
model1 = lm(TravelInsurance ~ AnnualIncome, data = travel)
model1

#Getting model1 summary
summary(model1)


#plotting histogram to generalize and visual the the amount of travel insurenace based on annual income!
ggplot(travel, aes(x = AnnualIncome , color = TravelInsurance)) +
  geom_histogram()




#building another model to analyze travel insurance based on number of family members present in a family
model2 = lm(TravelInsurance  ~ FamilyMembers, data = travel)
model2

#getting model 2 summary
summary(model2)





#building collaborate model to use if travel insurance affects int this
model3 = lm(TravelInsurance ~ GraduateOrNot + AnnualIncome, data = travel)
model3

#getting model 3 summary
summary(model3)


#plotting histogram to generalize and visual the the amount of travel insurance based on annual income!
ggplot(travel, aes(x = AnnualIncome,  y = TravelInsurance, color =GraduateOrNot))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
  ggtitle("Insured or not depending on graduation") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_blank())+
  coord_flip()


#Grouping data together to get insight from the specific columns
travel %>%
  group_by(Employment.Type) %>%
  summarise(mean(AnnualIncome))

ggplot(travel, aes(x = AnnualIncome, y = TravelInsurance, color ='Employment.Type' )) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle("Insurance depending on people's annual income") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title = element_blank())


model4 = lm(FrequentFlyer  ~ TravelInsurance, data = travel)
model4

summary(model4)




