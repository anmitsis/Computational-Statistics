# Computational-Statistics
This project was part of the Computational Statistics and Stochastic Optimisation class in my university. The tools I used was R for coding and LaTeX for the text synthesis. The pdf file is in Greek.

## Excersice 1
The first excersice requires to understand the monte carlo estimation method and estimate the volume of a d-dimentional sphere as well as the Metropolis-Hastings (MCMC) algorithm for the same task.

## Excersice 2
Utilising the Mass library in R, firstly I estimated the density function of the data using the kernel method and maximising the cross-validated likelihood for the optimal h. Then, i fitted a non-parametric regression model (Nadaraya-Watson) and for the optimal h I used the leave-one-out Cross Validation method.

## Excersice 3
In this task, i have some data that appear to be from a Gaussian mixture distribution. I simulated datasets from different distributions to see which one fits better. Lastly i used the Expectation Maximisation(EM) algorithm to estimate the parameters of the Gaussian mixture and resample some data, which i then procceded to plot and compare to the first plot.

## Excersice 4
With the help of the glmnet library in R, I performed Lasso Regression in some data from the faraway::fat library using 4/5 of the data as training sample and 1/5 as test sample. Next, I performed various variable selection methods, 5-fold cross validation and model comparison with BIC. Finally in the last model i created 1000 bootstrap samples to estimate the regression parameters and errors.

## Conclusion
The project was a challenge for me and it helped me better understand Bayesian Statistics as well as basic machine learning concepts. I also mastered the LaTeX text editor which is a great tool for composing a big project with a lot of graphs and code inside.
