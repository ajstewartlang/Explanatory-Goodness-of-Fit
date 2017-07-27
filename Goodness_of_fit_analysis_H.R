require(dplyr)
require(tidyr)
require(ggplot2)
require(afex)
require(lattice)

#use the RPs_plus_ratings data file
RPs_plus_ratings <- read.csv("RPs_plus_ratings.csv")
RPs_plus_ratings$P.s <- factor(RPs_plus_ratings$P.s)
RPs_plus_ratings$Item <- factor(RPs_plus_ratings$Item)

str(RPs_plus_ratings)


drp_r <- drp_r %>%
  mutate(acc_z = (Accepts - mean(Accepts))/sd(Accepts),
         conf_z = (Confident- mean(Confident))/sd(Confident),
         log_acc= log(Accepts),
         log_con = log(Consequent),
         inv_con = 1/Consequent)

RPs_plus_ratings %>% 
  group_by(P.s, Cond) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  summarise(mean(n), sd(n))

RPs_plus_ratings %>% 
  group_by(Item, Fit) %>% 
  summarise(a=length(unique(Accepts)),
            c=length(unique(Confident))) %>% 
  ungroup() %>% 
  summarise(max(a), max(c))

ggplot(data = RPs_plus_ratings, aes(x = Accepts, y = Consequent, group = P.s)) +
  geom_line()

str(drp_r)

hist(drp_r$Consequent)
hist(drp_r$log_con)
hist(drp_r$inv_con)
hist(drp_r$log_acc)
hist(drp_r$Accepts)


drp_r %>% group_by(P.s, Fit) %>% 
  summarise(n = n())
drp_r %>% group_by(Item, Fit) %>% 
  summarise(n = n())


m1 <- mixed(Consequent ~ Fit + (Fit|P.s) + (Fit| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000))) 
m1
summary(m1)

m1_nc <- mixed(Consequent ~ Fit + (Fit||P.s) + (Fit|| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000)), expand_re = TRUE) 
m1_nc
summary(m1_nc)

m2 <- mixed(Consequent ~ Fit + (1|P.s) + (Fit| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000))) 
m2
summary(m2)


m2_nc <- mixed(Consequent ~ Fit + (1|P.s) + (Fit|| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000)), expand_re = TRUE) 
m2_nc
summary(m2_nc)


m1S <- mixed(Consequent ~ Fit + (Fit|P.s) + (Fit| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000)), method = "S") 
m1S


rp_mn1 <- mixed(Consequent ~ acc_z * conf_z + (acc_z * conf_z|P.s) + (Fit| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000))) 
rp_mn1
summary(rp_mn1)
qqmath(rp_mn1$full_model)


rp_mn2 <- mixed(log_con ~ acc_z * conf_z + (acc_z * conf_z|P.s) + (Fit| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000))) 
rp_mn2
summary(rp_mn2)
plot(rp_mn2$full_model)
qqmath(rp_mn2$full_model)

rp_mn2 <- mixed(log_con ~ acc_z * conf_z + (acc_z * conf_z|P.s) + (1| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000))) 
rp_mn2
summary(rp_mn2)
plot(rp_mn2$full_model)
lattice::qqmath(rp_mn2$full_model, id = 0.01)

rp_mn3 <- mixed(log_con ~ acc_z + (acc_z|P.s) + (1| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000))) 
rp_mn3
summary(rp_mn3)
plot(rp_mn3$full_model)
lattice::qqmath(rp_mn3$full_model, id = 0.01)

rp_mn4 <- mixed(log_con ~ log_acc + (log_acc|P.s) + (1| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000)), expand_re = TRUE) 
rp_mn4
summary(rp_mn4)
plot(rp_mn4$full_model)
lattice::qqmath(rp_mn4$full_model, id = 0.01)

rp_mn5 <- mixed(log_con ~ acc_z + conf_z + (acc_z + conf_z|P.s) + (1| Item), data=drp_r, control=lmerControl(optCtrl=list(maxfun=200000000))) 
rp_mn5
summary(rp_mn5)
plot(rp_mn3$full_model)
lattice::qqmath(rp_mn3$full_model, id = 0.01)


anova(rp_mn2, rp_mn3)

#in the following analyses, we first look at how the eye-movement data (regression path times) are
#predicted first by our offline data, and then by our condition as a categorical predictor
#Note, the first model is significant - the t-value of our continuosu predictor (Accepts) in the first model is (t=2.496)
#and there is a significant effect of our Condition (Fit) in the second model (t=2.292)

#this excludes item 9 as the critical sentence erroneously contains "knows" 
#rather than "thinks" - this is in contrast to all the others which involve "thinks"
index <- RPs_plus_ratings$Item != "9" 

#IGNORE THE FOLLOWING - IT DOESN'T MAKE SENSE FOR A CONTINUOUS PRODICTOR TO BE USED IN A RANDOM EFFECT STRUCTURE
     #first let's look at the models with our two offline predictors (Accepts - how acceptable the explanation is - and
     #Confident - how confident people are in their ratings) 
     #note that the following models have the most complex random effects structures that converge
     #model.with.interaction <- lmer (Consequent ~ Accepts * Confident + (1+Accepts+Confident|P.s) + (1+Accepts| Item), data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
     #model.without.interaction <- lmer (Consequent ~ Accepts + Confident + (1+Accepts+Confident|P.s) + (1+Accepts| Item), data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000)))
     #anova (model.with.interaction, model.without.interaction)

     #interaction not significant so look at parameter estimates for model without the interaction term
     #summary (model.without.interaction)
re


#this is the sensible way to do it with continuous predictors - just with random intercepts
model.with.interaction.no.slopes <- lmer (Consequent ~ Accepts * Confident + (1|P.s) + (1| Item), data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000)))
model.without.interaction.no.slopes <- lmer (Consequent ~ Accepts + Confident + (1|P.s) + (1| Item), data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000)))
anova (model.with.interaction.no.slopes, model.without.interaction.no.slopes)

#model with interaction doesn't improve fit (high correlation between predictors) so stick with 
#model with just main effects
summary (model.without.interaction.no.slopes)

#also run model with just Accepts as single predictor
model.without.interaction.no.slopes.accepts <- lmer (Consequent ~ Accepts  + (1|P.s) + (1| Item), data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000)))
summary (model.without.interaction.no.slopes.accepts)

#let's also look at the model where our condition (High vs. Low Goodness of Fit) is one categorical predictor
#full model does not converge
model.with.condition <- lmer (Consequent ~ Fit  + (1+Fit|P.s) + (1|Item), data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
summary (model.with.condition)




#Use the FPs_plus_ratings file

#this excludes item 9 as the critical sentence erroneously contains "knows" 
#rather than "thinks" - this is in contrast to all the others which involve "thinks"
index <- FPs_plus_ratings$Item != "9" 

#first let's look at the models with our two offline predictors (Accepts - how acceptable the explanation is - and
#Confident - how confident people are in their ratings) 
#note that the following models have the most complex random effects structures that converge
model.with.interaction <- lmer (Consequent ~ Accepts * Confident + (1+Accepts+Confident|P.s) + (1+Accepts| Item), data=FPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
model.without.interaction <- lmer (Consequent ~ Accepts + Confident + (1+Accepts+Confident|P.s) + (1+Accepts| Item), data=FPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000)))
anova (model.with.interaction, model.without.interaction)

#interaction not significant so look at parameter estimates for model without the interaction term
summary (model.without.interaction)

#let's also look at the model where our condition (High vs. Low Goodness of Fit) is one categorical predictor
#full model does not converge
model.with.condition <- lmer (Consequent ~ Fit  + (1+Fit|P.s) + (1|Item), data=FPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
summary (model.with.condition)




#Use the TTs_plus_ratings file
#this excludes item 9 as the critical sentence erroneously contains "knows" 
#rather than "thinks" - this is in contrast to all the others which involve "thinks"
index <- TTs_plus_ratings$Item != "9" 

#first let's look at the models with our two offline predictors (Accepts - how acceptable the explanation is - and
#Confident - how confident people are in their ratings) 
#note that the following models have the most complex random effects structures that converge
model.with.interaction <- lmer (Consequent ~ Accepts * Confident + (1+Accepts+Confident|P.s) + (1+Accepts| Item), data=TTs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
model.without.interaction <- lmer (Consequent ~ Accepts + Confident + (1+Accepts+Confident|P.s) + (1+Accepts| Item), data=TTs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000)))
anova (model.with.interaction, model.without.interaction)

#interaction not significant so look at parameter estimates for model without the interaction term
summary (model.without.interaction)

#let's also look at the model where our condition (High vs. Low Goodness of Fit) is one categorical predictor
#full model does not converge
model.with.condition <- lmer (Consequent ~ Fit  + (1+Fit|P.s) + (1|Item), data=TTs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
summary (model.with.condition)




#let's look at the regressions out of the critical region
#use the RO_plus_ratings file

#this excludes item 9 as the critical sentence erroneously contains "knows" 
#rather than "thinks" - this is in contrast to all the others which involve "thinks"
index <- ROs_plus_ratings$Item != "9" 

#model with interaciton does not converge so just build model without interaction effect
model.without.interaction <- glmer(Consequent ~ Accepts+Confident + (1+Accepts|P.s) + (1+Accepts| Item) , data = RO_plus_ratings [index,], family=binomial, control=glmerControl(optCtrl=list(maxfun=200000000))) 
summary (model.without.interaction)

#let's also look at the model where our condition (High vs. Low Goodness of Fit) is one categorical predictor
#full model does not converge
model.with.condition <- glmer (Consequent ~ Fit  + (1+Fit|P.s) + (1|Item), data=RO_plus_ratings[index,], family=binomial, control=glmerControl(optCtrl=list(maxfun=200000000))) 
summary (model.with.condition)
