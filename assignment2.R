# setup
# clear the environment
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

make_dir <- function(d) {
  if (file.exists(d)) unlink(d, recursive=TRUE, force=TRUE)
  dir.create(d)
}
lapply(c(IMAGES_DIR, OUTPUT_DIR),make_dir)


## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
  result <- paste(x1,x2,sep="")
  return(result)
}

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
  if (x %in% rownames(installed.packages())) { 
    print(concat("package already installed: ", x))
  }
  else { 
    install.packages(x) 
  }
  library(x, character.only=TRUE)
}
lapply(c("car","psych","ggplot2", "grid", "gridExtra"), load_package)

#######################################################
# PROBLEM 1                                           #
#######################################################
# CRIM: per capita crime rate by town 
# ZN: proportion of residential land zoned for lots over 25,000 sq.ft. 
# INDUS: proportion of non-retail business acres per town 
# CHAS: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise) 
# NOX: nitric oxides concentration (parts per 10 million) 
# RM: average number of rooms per dwelling 
# AGE: proportion of owner-occupied units built prior to 1940 
# DIS: weighted distances to five Boston employment centers 
# RAD: index of accessibility to radial highways 
# TAX: full-value property-tax rate per $10,000 
# PTRATIO: pupil-teacher ratio by town 
# B: 1000(Bk - 0.63)^2 where Bk is the proportion of African Americans by town 
# LSTAT: % lower status of the population 
# MEDV: Median value of owner-occupied homes in $1000's (output variable)
# import the housing.data file
data <- read.table(concat(DATA_DIR,'/housing.data'), header=F)
# rename the columns to something more human-readable
colnames(data) <- c('per_capita_crime_rate',
                    'zoned_over_25k_sq_ft',
                    'business_acres',
                    'bounded_by_charles_river',
                    'nox_ppm',
                    'rooms_per_dwelling',
                    'units_built_before_1940',
                    'distance_to_employment_centers',
                    'highway_accessibility',
                    'property_tax_rate_per_10k',
                    'student_teacher_ratio',
                    'african_american_proportion',
                    'percent_lower_status',
                    'median_home_value')
# describe the data
write.table(describe(data), file=concat(OUTPUT_DIR,'/housing - descriptions.csv'), sep=",")
# create variable vectors
per_capita_crime_rate            <- data$per_capita_crime_rate
zoned_over_25k_sq_ft             <- data$zoned_over_25k_sq_ft
business_acres                   <- data$business_acres
bounded_by_charles_river         <- data$bounded_by_charles_river
nox_ppm                          <- data$nox_ppm
rooms_per_dwelling               <- data$rooms_per_dwelling
units_built_before_1940          <- data$units_built_before_1940
distance_to_employment_centers   <- data$distance_to_employment_centers
highway_accessibility            <- data$highway_accessibility
property_tax_rate_per_10k        <- data$property_tax_rate_per_10k
student_teacher_ratio            <- data$student_teacher_ratio
african_american_proportion      <- data$african_american_proportion
percent_lower_status             <- data$percent_lower_status
median_home_value                <- data$median_home_value


# function to plot histograms
plot_histogram <- function (d, v, name) {
  # v is the vector we are plotting
  # name is the name of the vector
  y_max_offset <- function(y, offset) {
    y - (0.1 * y * offset)
  }
  m <- mean(v)
  y_max <- max(v)
  ggplot(d, aes(x=v), environment = environment()) +
    geom_histogram(binwidth=1, col="black", aes(fill=..count..)) +
    geom_vline(data=d, aes(xintercept=m),
               linetype="dashed", size=1, colour="black") +
    scale_fill_gradient("Count", low = "green", high = "red") +
    annotate("text", x = m+0.5*m, y = y_max, label = concat("mean: ", round(m,4)), hjust = 0) +
    annotate("text", x = m+0.5*m, y = y_max_offset(y_max, 1), label = concat("skew: ", round(skew(v),4)), hjust = 0) +
    annotate("text", x = m+0.5*m, y = y_max_offset(y_max, 2), label = concat("kurtosis: ", round(kurtosi(v),4)), hjust = 0) +
    labs(title=concat("Histogram for ", name)) +
    labs(x=name, y="Count")
}
# what does the histogram of median home value look like?
name <- "Median Home Value in $1000's"
png(concat(IMAGES_DIR,concat(concat('/housing - histogram of ', name), '.png')), 
    width = 1024, height = 1024)
plot_histogram(data, median_home_value, name)
dev.off()

# look at relationships between variables
write.table(cor(data), file=concat(OUTPUT_DIR,'/housing - correlations.csv'), sep=",")
png(concat(IMAGES_DIR,'/housing - scattplot matrix.png'), 
    width = 2048, height = 2048)
scatterplotMatrix(data, diagonal="density")
dev.off()

# scatterplot of per capita crime rate versus median_home_value
png(concat(IMAGES_DIR,'/housing - per capita crime rate versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~per_capita_crime_rate, 
            xlab="Per Capita Crime Rate", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Per Capita Crime Rate vs. Median Home Value (in $1000's)")
dev.off()

# per capita crime rate appears to need a transformation
per_capita_crime_rate_log        <- log(per_capita_crime_rate)
# scatterplot of log of per capita crime rate versus median_home_value
png(concat(IMAGES_DIR,'/housing - log of per capita crime rate versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~per_capita_crime_rate_log, 
            xlab="Log of Per Capita Crime Rate", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Log of Per Capita Crime Rate vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of zoned_over_25k_sq_ft versus median_home_value
# ****Need to make this a dummy / reference variable b/c it is not continuous****
png(concat(IMAGES_DIR,'/housing - zoned over 25k sq ft versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~zoned_over_25k_sq_ft, 
            xlab="Zoned Over 25k Square Feet", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Zoned Over 25k Square Feet vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of percent_lower_status versus median_home_value
png(concat(IMAGES_DIR,'/housing - percent lower status versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~percent_lower_status, 
                  xlab="Percent Lower Status", 
                  ylab="Median Home Value (in $1000's)", 
                  main="Enhanced Scatterplot of \n Percent Lower Status vs. Median Home Value (in $1000's)")
dev.off()

# percent lower status appears to need a transformation
percent_lower_status_log10       <- log10(percent_lower_status)
# scatterplot of percent_lower_status_log10 versus median_home_value
png(concat(IMAGES_DIR,'/housing - log10 of percent lower status versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~percent_lower_status_log10, 
                  xlab="Log10 of Percent Lower Status", 
                  ylab="Median Home Value (in $1000's)", 
                  main="Enhanced Scatterplot of \n Log10 of Percent Lower Status vs. Median Home Value (in $1000's)")
dev.off()

# a) regression of median_home_value - full first-order model
a <- lm(median_home_value ~ 
          per_capita_crime_rate + 
          zoned_over_25k_sq_ft + 
          business_acres + 
          bounded_by_charles_river + 
          nox_ppm + 
          rooms_per_dwelling + 
          units_built_before_1940 + 
          distance_to_employment_centers + 
          highway_accessibility + 
          property_tax_rate_per_10k + 
          student_teacher_ratio + 
          african_american_proportion + 
          percent_lower_status)
summary(a)
# Call:
#     lm(formula = median_home_value ~ per_capita_crime_rate + 
#            zoned_over_25k_sq_ft + business_acres + bounded_by_charles_river + 
#            nox_ppm + rooms_per_dwelling + units_built_before_1940 + 
#            distance_to_employment_centers + highway_accessibility + 
#            property_tax_rate_per_10k + student_teacher_ratio + african_american_proportion + 
#            percent_lower_status)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -15.595  -2.730  -0.518   1.777  26.199 
# 
# Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
#     (Intercept)                     3.646e+01  5.103e+00   7.144 3.28e-12 ***
#     per_capita_crime_rate          -1.080e-01  3.286e-02  -3.287 0.001087 ** 
#     zoned_over_25k_sq_ft            4.642e-02  1.373e-02   3.382 0.000778 ***
#     business_acres                  2.056e-02  6.150e-02   0.334 0.738288    
#     bounded_by_charles_river        2.687e+00  8.616e-01   3.118 0.001925 ** 
#     nox_ppm                        -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
#     rooms_per_dwelling              3.810e+00  4.179e-01   9.116  < 2e-16 ***
#     units_built_before_1940         6.922e-04  1.321e-02   0.052 0.958229    
#     distance_to_employment_centers -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
#     highway_accessibility           3.060e-01  6.635e-02   4.613 5.07e-06 ***
#     property_tax_rate_per_10k      -1.233e-02  3.760e-03  -3.280 0.001112 ** 
#     student_teacher_ratio          -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
#     african_american_proportion     9.312e-03  2.686e-03   3.467 0.000573 ***
#     percent_lower_status           -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.745 on 492 degrees of freedom
# Multiple R-squared:  0.7406,    Adjusted R-squared:  0.7338 
# F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16

# Goodness of fit: Adjusted R-squared is 0.7338
# Utility of the model: the F-stat of 108.1 is significant with a p-value < 2.2e-16
# Estimated coefficients:
coefficients(a) # model coefficients

# look at collinearity
write.table(cor(data), file=concat(OUTPUT_DIR,'/problem1_model_correlations.csv'), sep=",")
write.table(summary(a)$coefficients, file=concat(OUTPUT_DIR,'/problem1_model_coefficients.csv'), sep=",")
write.table(anova(a), file=concat(OUTPUT_DIR,'/problem1_model_anova.csv'), sep=",")
load_package('lm.beta')
lm.a.beta <- lm.beta(a)
write.table(coef(lm.a.beta), file=concat(OUTPUT_DIR,'/problem1_model_coefficients_standard.csv'), sep=",")
load_package('DAAG')
vif(a)
write("VIF", file=concat(OUTPUT_DIR,'/problem1_model_vif.csv'))
write.table(vif(a), file=concat(OUTPUT_DIR,'/problem1_model_vif.csv'), sep=",", append=TRUE)
write("", file=concat(OUTPUT_DIR,'/problem1_model_vif.csv'), append=TRUE)
write("", file=concat(OUTPUT_DIR,'/problem1_model_vif.csv'), append=TRUE)
sqrt(vif(a))
write("sqrt(VIF)", file=concat(OUTPUT_DIR,'/problem1_model_vif.csv'), append=TRUE)
write.table(sqrt(vif(a)), file=concat(OUTPUT_DIR,'/problem1_model_vif.csv'), sep=",", append=TRUE)
# create corrgram
load_package('corrgram')
png(concat(IMAGES_DIR,'/problem1_corrgram.png'), height=2048, width=2048)
corrgram(data, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Quantitative Variables in PC2/PC1 Order")
dev.off()

# plot of deleted studentized residuals vs hat values
png(concat(IMAGES_DIR,'/problem1_model_influentials.png'))
plot(hatvalues(a), rstudent(a))
abline(a=0,b=0, col="red")
# add labels to points
text(hatvalues(a), rstudent(a), cex=0.7, pos=2)
dev.off()

# create a plot of residuals versus predicted values
png(concat(IMAGES_DIR,'/problem1_residuals_vs_predicted.png'), width = 512, height = 512)
plot(fitted(a), rstandard(a), main="Predicted vs. Residuals Plot")
abline(a=0, b=0, col="red")
text(fitted(a), rstandard(a), cex=0.7, pos=2)
dev.off()

# Assessing Outliers
png(concat(IMAGES_DIR,'/problem1_model_qqplot.png'), width = 1024, height = 1024)
qqPlot(a, main="QQ Plot") #qq plot for studentized resid 
dev.off()

# print out only observations that may be influential
write.table(summary(influence.measures(a)), 
            file=concat(OUTPUT_DIR,'/problem1_model_influentials.csv'), sep=",")
# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(data)-length(a$coefficients)-2)) 
png(concat(IMAGES_DIR,'/problem1_cook_levels.png'), width = 1024, height = 1024)
plot(a, which=4, cook.levels=cutoff)
dev.off()
# Influence Plot 
png(concat(IMAGES_DIR,'/problem1_influence_plot.png'), width = 1024, height = 1024)
influencePlot(a, id.method="identify", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
dev.off()

# residuals histogram
x = rstudent(a)
png(concat(IMAGES_DIR,'/problem1_model_residuals_hist.png'))
hist(x, breaks=100, col="red", xlab="Median Home Value in $1000's", 
     main="Histogram of Residuals with Normal Curve")
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
lines(xfit, yfit, col="blue", lwd=2)
dev.off()

# b) forward selection
# Stepwise Regression
load_package('MASS')
base <- a
step1 <- stepAIC(base, direction="forward")
step$anova # display results
