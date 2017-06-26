#use the RPs_plus_ratings data file

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
