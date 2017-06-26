#use the RPs_plus_ratings data file

#in the following analyses, we first look at how the eye-movement data (regression path times) are
#predicted first by our offline data, and then by our condition as a categorical predictor
#Note, both work - there is a significant effect of our continuosu predictor (Accepts) in the first model (t=2.183)
#and a significant effect of our Condition (Fit) in the second (F1, t=2.598, F2, t=2.534)

#this excludes item 3 as the critical sentence erroneously contains "knows" 
#rather than "thinks" - this is in contrast to all the others which involve "thinks"
index <- RPs_plus_ratings$Item != "3" 

#first let's look at the models with our two offline predictors (Accepts - how acceptable the explanation is - and
#Confident - how confident people are in their ratings) 
#note that the following models have the most complex random effects structures that converge
model.with.interaction <- lmer (Consequent ~ Accepts * Confident + (1+Accepts+Confident|P.s) + (1+Accepts| Item), data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
model.without.interaction <- lmer (Consequent ~ Accepts + Confident + (1+Accepts+Confident|P.s) + (1+Accepts| Item), data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000)))
anova (model.with.interaction, model.without.interaction)

#interaction not significant so look at parameter estimates for model without the interaction term
summary (model.without.interaction)

#let's also look at the model where our condition (High vs. Low Goodness of Fit) is one categorical predictor
#full model does not converge so build by-subjects and by-items models
#this is the by subjects (F1) model
model.with.condition.F1 <- lmer (Consequent ~ Fit  + (1+Fit|P.s) , data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
summary (model.with.condition.F1)

#this is the by items (F2) model
model.with.condition.F2 <- lmer (Consequent ~ Fit  + (1|Item) , data=RPs_plus_ratings[index,], control=lmerControl(optCtrl=list(maxfun=200000000))) 
summary (model.with.condition.F2)