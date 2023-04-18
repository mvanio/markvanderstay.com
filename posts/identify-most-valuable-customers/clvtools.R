library(CLVTools)
data("apparelTrans")

# Create a CLV data object, split data in estimation and holdout sample
clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd",
                          time.unit = "week", estimation.split = 39, name.id = "Id")

plot(clv.apparel)
plot(clv.apparel,which="frequency")
plot(clv.apparel,which="spending")
plot(clv.apparel,which="spending", mean.spending=FALSE)
plot(clv.apparel,which="interpurchasetime")

# summary of data
summary(clv.apparel)

# Fit a PNBD model without covariates on the first 39 periods
pnbd.apparel <- pnbd(clv.apparel,
                   start.params.model = c(r=0.5, alpha=8, s=0.5, beta=10))

# Plot the fitted model to the actual repeat transactions
plot(pnbd.apparel)

# inspect fit
summary(pnbd.apparel)


# Predict 10 periods (weeks) ahead from estimation end
#   and compare to actuals in this period
pred.out <- predict(pnbd.apparel, prediction.end = 10)


# covariates
# gender, channel (time-invarying)
data("apparelStaticCov")
clv.static<- SetStaticCovariates(clv.data = clv.apparel,
                                 data.cov.life = apparelStaticCov,
                                 data.cov.trans = apparelStaticCov,
                                 names.cov.life = c("Gender", "Channel"),
                                 names.cov.trans =c("Gender", "Channel"),
                                 name.id = "Id")

# marketing (time-varying)
data("apparelDynCov")
clv.dyn <- SetDynamicCovariates(clv.data = clv.apparel,
                                data.cov.life = apparelDynCov,
                                data.cov.trans = apparelDynCov,
                                names.cov.life = c("Marketing", "Gender", "Channel"),
                                names.cov.trans = c("Marketing", "Gender", "Channel"),
                                name.id = "Id",
                                name.date = "Cov.Date")

# estimate the model
est.pnbd.static <- pnbd(clv.static,
                        start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                        start.params.life = c(Gender=0.6, Channel=0.4),
                        start.params.trans = c(Gender=0.6, Channel=0.4))

# The start.params.model argument is used to define the initial values for the 
# shape and scale parameters of the Pareto/NBD or BG/NBD model. These parameters 
# describe the purchase behavior (transaction rate) and dropout behavior (lifetime) 
# of the customers in your dataset.
# 
# The parameters are as follows:
#   
# r: Shape parameter for the Gamma distribution of the transaction rate (lambda)
# alpha: Scale parameter for the Gamma distribution of the transaction rate (lambda)
# s: Shape parameter for the Gamma distribution of the lifetime (mu)
# beta: Scale parameter for the Gamma distribution of the lifetime (mu)
# 
# The values you provide for these parameters will serve as the starting point for 
# the estimation process. The optimization algorithm will iterate from these starting 
# values to find the best-fitting parameters for your data.
# 
# If you provide incorrect or suboptimal starting values, it may cause the 
# optimization algorithm to converge slower, get stuck in local optima, or fail to 
# converge at all. However, in many cases, the optimization algorithm is robust 
# enough to find reasonable solutions even with suboptimal starting values.
# 
# To calculate accurate starting values, you generally need some prior knowledge or 
# insight about the data or the underlying distributions. However, in practice, it 
# is often challenging to obtain accurate starting values. If you don't have any 
# prior knowledge, you can use the default values provided by the CLVTools package, 
# which are designed to work reasonably well in many scenarios.
# 
# Additionally, you can also try running the model with different sets of starting 
# values and compare the results. If the algorithm consistently converges to 
# similar solutions, it suggests that the optimization process is not overly 
# sensitive to the choice of starting values.


# Similarly...
# In the CLVTools package for R, when you define start parameters for the covariates
# in a Pareto/NBD or a BG/NBD model, you are specifying the initial values for the 
# model estimation process. 
# The numbers you assign to each covariate are the initial guesses for the 
# regression coefficients. These initial values are important because they can 
# impact the convergence of the model fitting process.
# 
# For example, when you write start.params.life = c(Gender=0.6, Channel=0.4), you
# are providing an initial guess for the regression coefficients of the covariates
# "Gender" and "Channel" in the model. Here, you're guessing that the "Gender" 
# covariate has an initial coefficient of 0.6 and the "Channel" covariate has an 
# initial coefficient of 0.4. The optimization algorithm will use these initial 
# values to start the iterative process of fitting the model to your data.
# 
# It's important to note that these initial values are just a starting point, and 
# the algorithm may converge to different final values depending on the data you 
# provide. If you have no specific reason to choose particular starting values, 
# you can use arbitrary numbers or even use the default values provided by the 
# CLVTools package.
# 
# However, if you have some prior knowledge or intuition about the relationships 
# between your covariates and the target variable, you can use that knowledge to 
# provide better starting values for the estimation process, which might help the 
# algorithm converge faster or reach a better solution.



## Let's get advanced

# When estimating models with covariates using CLVTools, the documentation mentions 
# two additional estimation options: regularization and constraints for the parameters 
# of the covariates. Let me explain these concepts in simple terms for an 
# e-commerce store owner.
# 
# Regularization: Regularization is a technique used in statistical modeling to 
# reduce the complexity of a model and prevent overfitting. Overfitting happens 
# when a model fits the training data too closely and captures noise instead of 
# the underlying patterns. As a result, the model may perform poorly when applied
# to new, unseen data. Regularization adds a penalty term to the model's objective
# function, discouraging it from relying too heavily on any single covariate or 
# allowing the covariate's effect to become too large. In essence, regularization 
# helps the model strike a balance between fitting the data well and maintaining 
# simplicity, resulting in better generalization to new data.
# For an e-commerce store owner, this means that by applying regularization, the 
# model becomes less sensitive to fluctuations or noise in the data, making it more 
# reliable when predicting future customer behavior.
# 
# Constraints: Constraints are restrictions imposed on the parameter values in the
# model. For example, you may want to enforce that the effect of a particular 
# covariate is non-negative or lies within a specific range. Constraints can help
# incorporate domain knowledge or business rules into the model, ensuring that the
# model's predictions are in line with the store owner's expectations.
# For an e-commerce store owner, this means that if you have some prior knowledge 
# about how certain factors should influence customer behavior, you can incorporate 
# that knowledge into the model by imposing constraints on the relevant parameters. 
# This can help improve the interpretability and reliability of the model's predictions.

# In summary, regularization helps to prevent overfitting by discouraging the model 
# from relying too heavily on any single covariate, while constraints allow you to 
# incorporate domain knowledge or business rules into the model by restricting parameter 
# values. Both of these techniques can help improve the performance and 
# interpretability of customer lifetime value models for e-commerce store owners.

#add some regularisation: Regularization only affects the parameters of the covariates.
# The larger the \lambda_{reg} the stronger the effects of the regularization
est.pnbd.reg <- pnbd(clv.static,
                     start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                     reg.lambdas = c(trans=100, life=100))


est.pnbd.constr <- pnbd(clv.static,
                        start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                        start.params.constr = c(Gender=0.6),
                        names.cov.constr=c("Gender"))


est.gg<- gg(clv.data = clv.apparel)
# Note that CLVTools does not use by default the monetary value of the first transaction
# to fit the model since it might be atypical of future purchases

results.spending <- predict(est.gg)
print(results.spending)

# Show the actual mean spending and predicted mean spending for a set of customers 
# (identified by their IDs). The actual mean spending represents the average 
# spending per transaction for each customer in your historical data, while the 
# predicted mean spending is the model's estimate of the average spending per 
# transaction for each customer in the future. Here's how to interpret the results:
#   
# Customer ID 1: This customer has not made any purchases in the past 
# (actual mean spending is 0), but the model predicts that they will spend an 
# average of 40.11523 per transaction in the future.



# Keep in mind that these predictions are based on the customer behavior patterns 
# observed in the historical data and the covariates used in the model. You can use 
# these predicted mean spending values to better understand your customers' future 
# spending behavior, prioritize marketing efforts, or customize promotions and offers 
# for specific customer segments. However, it's essential to remember that these 
# predictions are not guaranteed, and various factors can influence the actual 
# future spending of customers. 
# Regularly updating and validating the model with new data can help improve the 
# accuracy of the predictions over time.


plot(est.gg)

# The term "density" in the context of the ggplot comparing the estimated and actual 
# density of customer spending refers to the probability density function (PDF) of a 
# continuous random variable. In this case, the random variable is customer spending.
# 
# The probability density function represents the likelihood of a particular spending
# value (or a range of values) occurring. In simpler terms, it shows how customer 
# spending is distributed across different spending levels. A higher density at a 
# specific spending level indicates that more customers are likely to spend around 
# that amount, while a lower density suggests that fewer customers will spend at 
# that level.
# 
# To interpret the ggplot comparing the estimated and actual density of customer 
# spending, you should look at how closely the estimated density (predicted by the 
# model) matches the actual density (observed in your data). A good fit between the 
# estimated and actual densities means that the model is accurately capturing the 
# spending patterns of your customers. In other words, the model is doing a good 
# job of predicting the distribution of customer spending.
# 
# When examining the plot, consider the following:
#   
#   Are the peaks (areas of highest density) of the estimated and actual densities 
# similar? This would indicate that the model is correctly identifying the most 
# common spending levels.
# Are the shapes of the estimated and actual densities similar? If so, this suggests 
# that the model is capturing the overall spending patterns well.
# Are there any significant discrepancies between the estimated and actual densities? 
#   If the model is consistently overestimating or underestimating spending at specific 
# levels, it may indicate that the model can be improved by refining the input 
# features or using additional data.
# Keep in mind that no model will be perfect, but comparing the estimated and 
# actual densities can provide valuable insights into the model's performance and 
# help you identify areas for improvement.




