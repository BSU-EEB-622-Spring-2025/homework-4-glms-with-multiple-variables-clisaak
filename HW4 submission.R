#### HW4 GLMMs with multiple vairables: Confounds, interactions, and categorical variables ####

# load packages
library(MASS)
library(performance)
library(marginaleffects)
library(modelr)
library(dplyr)
library(ggplot2)

# load and proof data
mistletoe <- read.csv("mistletoes.csv")
str(mistletoe)

# clean data
mistletoe <- mistletoe %>%
  mutate(Treatment = factor(Treatment, levels = c("unparasitized", "parasitized")))

# 1a Fit a glm for the hypothesis: Seedling density is increased beneath trees experiencing mistletoe infection. Describe the rationale you used in selecting a glm structure and probability distribution. Calculate model fit using MAE.

# run poisson glm to test seedling density is increased underneath trees that have been infected by mistletoe
mod <- glm(Seedlings ~ Treatment, 
           data = mistletoe, family = poisson(link = "log"))
summary(mod)

# check for overdispersion
check_overdispersion(mod) # there is overdispersion p= < 0.05, the mean and the variance are not equal

# fit a negative binomial due to the overdispersion
mod.nbin <-glm.nb(Seedlings ~ Treatment, data=mistletoe)
summary(mod.nbin)
exp(2.5733) # convert Intercept (for unparasitized trees)
exp(3.1575)  #  how many times more seedlings parasitized trees have
exp(3.1575 + 2.5733) # mean seedling count for parasitized trees

# predict Seedlings
mistletoe$Predicted <- predict(mod.nbin, type = "response")

# calculate model fit with MAE
mae_value <- mean(abs(mistletoe$Seedlings - mistletoe$Predicted))
print(paste("Mean Absolute Error (MAE):", round(mae_value, 2)))
# 145.84, the model is not a good fit for the data, there is a wide range of data (0 seedlings to 2472 seedlings) which is why 

boxplot(mistletoe$Seedlings)
range(mistletoe$Seedlings) # look at the range 0 to 2474
mean(mistletoe$Seedlings)

# I used the poisson probabilty distribution because seedlings are count data that are bounded at zero, the predictor data was binary, but after I ran the poisson glm I checked for overdispersion and it was detected (the variance > mean). So I ran a negative binomial instead.

# MAE of 145.84 indicates on average the predicted seedling count differs from observed values by ~145 seedlings. Mean seedling density = 160.66 and range is from 0 to 2472 seedlings. The level of error is high but expected due to high variability in seedling counts. The large spread suggests that some trees support extremely high seedling densities, contributing to increased prediction errors.

## ASW: excellent answer!!

# 1b Make a marginal effects plot) and written (effect sizes on scale of response) approaches to interpret the model.

preds <- predictions(mod.nbin, 
                     newdata = data.frame(Treatment = factor(c("unparasitized", "parasitized"))), conf_level = 0.95)


plot_predictions(mod.nbin, condition=c("Treatment")) # plot we learned

ggplot(data=preds, aes(x=Treatment, y=estimate)) + # fancy ggplot
  geom_point(size=4, color="#438a6e") +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color="#57bd94") +
  ylab("Predicted Seedling Count") +  
  xlab("Tree Treatment") +  
  #scale_x_discrete(labels = c("Unparasitized", "Parasitized")) +
  theme_bw() +
  theme(legend.position = "none")

# Does mistletoe infection alter seedling density? How much does seedling recruitment differ beneath parasitized and unparasitized trees? Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters.

# I analyzed the relationship between tree parasitism status and the number of seedlings beneath each tree using a Generalized Linear Model (GLM) with a Negative Binomial distribution, accounting for overdispersion in seedling counts. Mistletoe infection alters seedling density (maybe). Using the intercept (2.5733) and transforming it back to scale, means that unparasitized trees have an average of ~13 seedlings beneath them. The estimate for parasitized (3.1575) status and transforming it, means parasitized trees have ~23 times more seedlings compared to unparasitized trees. The pvalue (<2e-16) indicates a highly significant effect for parasitized trees. The MAE = 145.84, which means the predicted seedling count deviated by ~146 seedlings, the range of seedlings was 0 - 2472, suggesting suggesting considerable variation in the data that the model does not fully capture. This could be an indicator of other ecological variables influencing seedling dispersion patterns. 

## ASW: excellent!

# 1c) 2012 was an atypically rainy year, compared to 2011, model the effect of mistletoe differs between the two years. Write ~2 new sentences that summarize the results of the new model/biological implications.
  
# fit negative binomial GLM + year
mod_nbin_year <- glm.nb(Seedlings ~ Treatment * as.factor(Year), data = mistletoe)
  
# model summary
summary(mod_nbin_year)
  
# exponentiate coefficients 
exp(coef(mod_nbin_year))

# The model indicates that overall seedling density was significantly higher in 2012 (4.46 times higher than 2011), likely due to increased rainfall. However, the facilitative effect of mistletoe was weaker in 2012 (exp[-0.8956] = 0.41), suggesting that in wetter conditions, the relative advantage of parasitized trees for seedling recruitment is lessened, possibly because water was more readily available everywhere.

## ASW: great work, Cat! 30/30



# Forest thinning removes fuels from forests, 10,000 ponderosa diameters measured, some areas received thinning treatment, a wildfire burned a large sample, returned to resample the survival. Q: does thining decrease the probability of tree mortality in wildfire?


# 2a) Fit a glm to answer the question above. Write 2-4 sentences about biological significance of the effect of thinning, including descriptions of size of the effect on the response, evidence for your answer, and a metric of model fit.

# import and proof data
treemortality <- read.csv("treemortality.csv")
head(treemortality)

# fit a glm
tree_mod <- glm(mortality ~ thinning, data = treemortality, family = binomial)

# model summary and compute odds ration
summary(tree_mod)
exp(coef(tree_mod))

# get predictions
pred_probs <- predict(tree_mod, type = "response")

# calculate MAE
mae <- mean(abs(treemortality$mortality - pred_probs))
print(mae)

## ASW: Something like ROC/AUC may be better fit metric for this binary outcome.

# The model strongly suggests that forest thinning treatments significantly reduce tree mortality (Beta = -1.8559, p <0.001). The odds ratio of 0.156 indicates that trees in thinned areas are ~ 84% less likely to die compared to areas without the thinning treatment (1-0.156). This supports the hypothesis that thinning reduces tree mortality, potentially by decreasing the competition for resources like light, water, and nutrients. The MAE of 0.408 suggests that on average, the predicted probabilities deviate from the actual mortality outcomes by ~41%, indicating a decent level of predictive accuracy for this model.

# 2b) The researchers explicitly considered the potential for confounding relationships related to tree size and randomized their post-fire sampling by tree size. Given this information, do the researchers need to incorporate tree size into their glm to accurately estimate the effect of thinning? Why or why not?

# My answer is that it depends. Because the researchers explicitly randomized their post-fire sampling by tree size, they controlled for the potential confounding effect of tree size in the design. However, incorporating tree size in the model could be advantageous for these reasons: 1. for better precision: randomization controls for confounding, adding tree size as a covariate could reduce variation and improve predictive power; 2. if tree size is a significant predictor of mortality, including it might improve model fit. Short answer no.

## ASW: Love it! This is great. My short answer is: You could include it for other reasons, but it's not needed to accurately estimate the effect of thinning!

# 2c) Refit the model from 2a to include the necessary variables to minimize bias in our estimation of the “thinning” variable, based on the reviewer’s proposed DAG (above). Does the effect of “thinning” change? If so, describe the degree of change and why the two models may differ in their conclusions.

# updated glm
tree_mod_2 <- glm(mortality ~ thinning + roaddist + slope, 
                    data = treemortality, family = binomial)

# mod summary and  odds ratios
summary(tree_mod_2)
exp(coef(tree_mod_2))

# mae
pred_probs_2 <- predict(tree_mod_2, type = "response")
mae_2 <- mean(abs(treemortality$mortality - pred_probs_2))
print(mae_2)

# Thinning still significantly reduces tree mortality, but the effect is lessened than originally estimated. In the original model, thinning was associated with an 84% reduction in mortality (odds ratio 0.156) whereas in the adjusted model, thinning is associated with a 60% reduction in mortality (odds ratio 0.40) after accounting for slope and road distance. This change suggests that some of the initial effect of thinning was due to confounding by landscape features that influence the likelihood of thinning. Additionally, the Mean Absolute Error (MAE) improved from 0.408 to 0.162, indicating that the updated model provides a more accurate prediction of tree mortality.

##ASW: Nice! See my comment about using MAE with binomial glm above. The key thing here is that slope and distance from roads are biasing the effect of thinning in the first model, making it appear more effective than it is because of the fact that thinning treatments are more likely to occur in locations where fire severity is already lower (closer to roads, on shallower slopes). 

## 18/20

## ASW: Great work, Cat! 48/50
