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

lapply(c("car","psych","ggplot2", "lm.beta", "MASS","leaps"), load_package)

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
png(concat(IMAGES_DIR,'/housing - zoned over 25k sq ft versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~zoned_over_25k_sq_ft, 
            xlab="Zoned Over 25k Square Feet", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Zoned Over 25k Square Feet vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of business_acres versus median_home_value
png(concat(IMAGES_DIR,'/housing - business acres versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~business_acres, 
            xlab="Proportion of Non-Retail Business Acres per Town", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Proportion of Non-Retail Business Acres per Town vs. Median Home Value (in $1000's)")
dev.off()

# boxplot of bounded_by_charles_river versus median_home_value
png(concat(IMAGES_DIR,'/housing - bounded by charles river versus median home value.png'), 
    width = 1024, height = 1024)
boxplot(median_home_value~bounded_by_charles_river, 
        xlab="Bounded by Charles River (1=TRUE; 0=FALSE)", 
        ylab="Median Home Value (in $1000's)", 
        main="Boxplot of \n Bounded by Charles River vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of nox_ppm versus median_home_value
png(concat(IMAGES_DIR,'/housing - nox ppm versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~nox_ppm, 
            xlab="Nitric Oxides Concentration (parts per 10M)", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Nitric Oxides Concentration (parts per 10M) vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of rooms_per_dwelling versus median_home_value
png(concat(IMAGES_DIR,'/housing - rooms per dwelling versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~rooms_per_dwelling, 
            xlab="Rooms per Dwelling", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Rooms per Dwelling vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of units_built_before_1940 versus median_home_value
png(concat(IMAGES_DIR,'/housing - units built before 1940 versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~units_built_before_1940, 
            xlab="Proportion of Units Built Before 1940", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Proportion of Units Built Before 1940 vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of distance_to_employment_centers versus median_home_value
png(concat(IMAGES_DIR,'/housing - distance to employment centers versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~distance_to_employment_centers, 
            xlab="Weighted distances to five Boston employment centers", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Weighted distances to five Boston employment centers vs. Median Home Value (in $1000's)")
dev.off()

# boxplot of highway_accessibility versus median_home_value
png(concat(IMAGES_DIR,'/housing - highway accessibility versus median home value.png'), 
    width = 1024, height = 1024)
boxplot(median_home_value~highway_accessibility, 
        xlab="index of accessibility to radial highways", 
        ylab="Median Home Value (in $1000's)", 
        main="Boxplot of \n index of accessibility to radial highways vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of property_tax_rate_per_10k versus median_home_value
png(concat(IMAGES_DIR,'/housing - property tax rate per 10k versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~property_tax_rate_per_10k, 
            xlab="full-value property-tax rate per $10,000", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n full-value property-tax rate per $10,000 vs. Median Home Value (in $1000's)")
dev.off()

# boxplot of property_tax_rate_binned versus median_home_value
property_tax_rate_per_10k_binned <- property_tax_rate_per_10k
property_tax_rate_per_10k_binned[property_tax_rate_per_10k < 500] <- "< 500"
property_tax_rate_per_10k_binned[property_tax_rate_per_10k >= 500] <- ">= 500"
png(concat(IMAGES_DIR,'/housing - property tax rate per 10k binned versus median home value.png'), 
    width = 1024, height = 1024)
boxplot(median_home_value~property_tax_rate_per_10k_binned, 
        xlab="full-value property-tax rate per $10,000", 
        ylab="Median Home Value (in $1000's)", 
        main="Boxplot of \n full-value property-tax rate per $10,000 vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of student_teacher_ratio versus median_home_value
png(concat(IMAGES_DIR,'/housing - student teacher ratio versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~student_teacher_ratio, 
            xlab="pupil-teacher ratio by town", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n pupil-teacher ratio by town vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of african_american_proportion versus median_home_value
png(concat(IMAGES_DIR,'/housing - african american proportion versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~african_american_proportion, 
            xlab="1000(Bk - 0.63)^2 where Bk is the proportion of African Americans by town", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n 1000(Bk - 0.63)^2 vs. Median Home Value (in $1000's)")
dev.off()


# scatterplot of african_american_proportion_sqrt versus median_home_value
african_american_proportion_sqrt <- sqrt(african_american_proportion)
png(concat(IMAGES_DIR,'/housing - sq rt of african american proportion versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~african_american_proportion_sqrt, 
            xlab="1000(Bk - 0.63) where Bk is the proportion of African Americans by town", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n 1000(Bk - 0.63) vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of african_american_proportion_sqrt_div1000 versus median_home_value
african_american_proportion_sqrt_div1000 <- african_american_proportion_sqrt/1000
png(concat(IMAGES_DIR,'/housing - sq rt of african american proportion divided by 1000 versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~african_american_proportion_sqrt_div1000, 
            xlab="Bk - 0.63 where Bk is the proportion of African Americans by town", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Bk - 0.63 vs. Median Home Value (in $1000's)")
dev.off()

# scatterplot of african_american_proportion_sqrt_div1000_bk versus median_home_value
african_american_proportion_sqrt_div1000_bk <- african_american_proportion_sqrt_div1000 + 0.63
png(concat(IMAGES_DIR,'/housing - sq rt of african american proportion divided by 1000 plus 0.63 versus median home value.png'), 
    width = 1024, height = 1024)
scatterplot(median_home_value~african_american_proportion_sqrt_div1000_bk, 
            xlab="Bk (where Bk is the proportion of African Americans by town)", 
            ylab="Median Home Value (in $1000's)", 
            main="Enhanced Scatterplot of \n Bk vs. Median Home Value (in $1000's)")
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
write.table(summary(a)$coefficients, file=concat(OUTPUT_DIR,'/housing_model_coefficients.csv'), sep=",")

# Look at standardized coefficients to see which ones have the most impact on 
# median_home_value
lm.a.beta <- lm.beta(a)
write.table(coef(lm.a.beta), file=concat(OUTPUT_DIR,'/housing_model_coefficients_standard.csv'), sep=",")

# b) forward selection based on AIC
# Stepwise Regression
null <- lm(median_home_value~1, data=data)
full <- lm(median_home_value~., data=data)
step(null, scope=list(lower=null, upper=full), direction="forward")
# Start:  AIC=2246.51
# median_home_value ~ 1
# 
# Df Sum of Sq   RSS    AIC
# + percent_lower_status            1   23243.9 19472 1851.0
# + rooms_per_dwelling              1   20654.4 22062 1914.2
# + student_teacher_ratio           1   11014.3 31702 2097.6
# + business_acres                  1    9995.2 32721 2113.6
# + property_tax_rate_per_10k       1    9377.3 33339 2123.1
# + nox_ppm                         1    7800.1 34916 2146.5
# + per_capita_crime_rate           1    6440.8 36276 2165.8
# + highway_accessibility           1    6221.1 36495 2168.9
# + units_built_before_1940         1    6069.8 36647 2171.0
# + zoned_over_25k_sq_ft            1    5549.7 37167 2178.1
# + african_american_proportion     1    4749.9 37966 2188.9
# + distance_to_employment_centers  1    2668.2 40048 2215.9
# + bounded_by_charles_river        1    1312.1 41404 2232.7
# <none>                                        42716 2246.5
# 
# Step:  AIC=1851.01
# median_home_value ~ percent_lower_status
# 
# Df Sum of Sq   RSS    AIC
# + rooms_per_dwelling              1    4033.1 15439 1735.6
# + student_teacher_ratio           1    2670.1 16802 1778.4
# + bounded_by_charles_river        1     786.3 18686 1832.2
# + distance_to_employment_centers  1     772.4 18700 1832.5
# + units_built_before_1940         1     304.3 19168 1845.0
# + property_tax_rate_per_10k       1     274.4 19198 1845.8
# + african_american_proportion     1     198.3 19274 1847.8
# + zoned_over_25k_sq_ft            1     160.3 19312 1848.8
# + per_capita_crime_rate           1     146.9 19325 1849.2
# + business_acres                  1      98.7 19374 1850.4
# <none>                                        19472 1851.0
# + highway_accessibility           1      25.1 19447 1852.4
# + nox_ppm                         1       4.8 19468 1852.9
# 
# Step:  AIC=1735.58
# median_home_value ~ percent_lower_status + rooms_per_dwelling
# 
# Df Sum of Sq   RSS    AIC
# + student_teacher_ratio           1   1711.32 13728 1678.1
# + bounded_by_charles_river        1    548.53 14891 1719.3
# + african_american_proportion     1    512.31 14927 1720.5
# + property_tax_rate_per_10k       1    425.16 15014 1723.5
# + distance_to_employment_centers  1    351.15 15088 1725.9
# + per_capita_crime_rate           1    311.42 15128 1727.3
# + highway_accessibility           1    180.45 15259 1731.6
# + business_acres                  1     61.09 15378 1735.6
# <none>                                        15439 1735.6
# + zoned_over_25k_sq_ft            1     56.56 15383 1735.7
# + units_built_before_1940         1     20.18 15419 1736.9
# + nox_ppm                         1     14.90 15424 1737.1
# 
# Step:  AIC=1678.13
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio
# 
# Df Sum of Sq   RSS    AIC
# + distance_to_employment_centers  1    499.08 13229 1661.4
# + african_american_proportion     1    389.68 13338 1665.6
# + bounded_by_charles_river        1    377.96 13350 1666.0
# + per_capita_crime_rate           1    122.52 13606 1675.6
# + units_built_before_1940         1     66.24 13662 1677.7
# <none>                                        13728 1678.1
# + property_tax_rate_per_10k       1     44.36 13684 1678.5
# + nox_ppm                         1     24.81 13703 1679.2
# + zoned_over_25k_sq_ft            1     14.96 13713 1679.6
# + highway_accessibility           1      6.07 13722 1679.9
# + business_acres                  1      0.83 13727 1680.1
# 
# Step:  AIC=1661.39
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio + distance_to_employment_centers
# 
# Df Sum of Sq   RSS    AIC
# + nox_ppm                      1    759.56 12469 1633.5
# + african_american_proportion  1    502.64 12726 1643.8
# + bounded_by_charles_river     1    267.43 12962 1653.1
# + business_acres               1    242.65 12986 1654.0
# + property_tax_rate_per_10k    1    240.34 12989 1654.1
# + per_capita_crime_rate        1    233.54 12995 1654.4
# + zoned_over_25k_sq_ft         1    144.81 13084 1657.8
# + units_built_before_1940      1     61.36 13168 1661.0
# <none>                                     13229 1661.4
# + highway_accessibility        1     22.40 13206 1662.5
# 
# Step:  AIC=1633.47
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio + distance_to_employment_centers + 
#   nox_ppm
# 
# Df Sum of Sq   RSS    AIC
# + bounded_by_charles_river     1    328.27 12141 1622.0
# + african_american_proportion  1    311.83 12158 1622.7
# + zoned_over_25k_sq_ft         1    151.71 12318 1629.3
# + per_capita_crime_rate        1    141.43 12328 1629.7
# + highway_accessibility        1     53.48 12416 1633.3
# <none>                                     12469 1633.5
# + business_acres               1     17.10 12452 1634.8
# + property_tax_rate_per_10k    1     10.50 12459 1635.0
# + units_built_before_1940      1      0.25 12469 1635.5
# 
# Step:  AIC=1621.97
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio + distance_to_employment_centers + 
#   nox_ppm + bounded_by_charles_river
# 
# Df Sum of Sq   RSS    AIC
# + african_american_proportion  1   272.837 11868 1612.5
# + zoned_over_25k_sq_ft         1   164.406 11977 1617.1
# + per_capita_crime_rate        1   116.330 12025 1619.1
# + highway_accessibility        1    58.556 12082 1621.5
# <none>                                     12141 1622.0
# + business_acres               1    26.274 12115 1622.9
# + property_tax_rate_per_10k    1     4.187 12137 1623.8
# + units_built_before_1940      1     2.331 12139 1623.9
# 
# Step:  AIC=1612.47
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio + distance_to_employment_centers + 
#   nox_ppm + bounded_by_charles_river + african_american_proportion
# 
# Df Sum of Sq   RSS    AIC
# + zoned_over_25k_sq_ft       1   189.936 11678 1606.3
# + highway_accessibility      1   144.320 11724 1608.3
# + per_capita_crime_rate      1    55.633 11813 1612.1
# <none>                                   11868 1612.5
# + business_acres             1    15.584 11853 1613.8
# + units_built_before_1940    1     9.446 11859 1614.1
# + property_tax_rate_per_10k  1     2.703 11866 1614.4
# 
# Step:  AIC=1606.31
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio + distance_to_employment_centers + 
#   nox_ppm + bounded_by_charles_river + african_american_proportion + 
#   zoned_over_25k_sq_ft
# 
# Df Sum of Sq   RSS    AIC
# + per_capita_crime_rate      1    94.712 11584 1604.2
# + highway_accessibility      1    93.614 11585 1604.2
# <none>                                   11678 1606.3
# + business_acres             1    16.048 11662 1607.6
# + property_tax_rate_per_10k  1     3.952 11674 1608.1
# + units_built_before_1940    1     1.491 11677 1608.2
# 
# Step:  AIC=1604.19
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio + distance_to_employment_centers + 
#   nox_ppm + bounded_by_charles_river + african_american_proportion + 
#   zoned_over_25k_sq_ft + per_capita_crime_rate
# 
# Df Sum of Sq   RSS    AIC
# + highway_accessibility      1   228.604 11355 1596.1
# <none>                                   11584 1604.2
# + business_acres             1    15.773 11568 1605.5
# + units_built_before_1940    1     2.470 11581 1606.1
# + property_tax_rate_per_10k  1     1.305 11582 1606.1
# 
# Step:  AIC=1596.1
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio + distance_to_employment_centers + 
#   nox_ppm + bounded_by_charles_river + african_american_proportion + 
#   zoned_over_25k_sq_ft + per_capita_crime_rate + highway_accessibility
# 
# Df Sum of Sq   RSS    AIC
# + property_tax_rate_per_10k  1   273.619 11081 1585.8
# <none>                                   11355 1596.1
# + business_acres             1    33.894 11321 1596.6
# + units_built_before_1940    1     0.096 11355 1598.1
# 
# Step:  AIC=1585.76
# median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#   student_teacher_ratio + distance_to_employment_centers + 
#   nox_ppm + bounded_by_charles_river + african_american_proportion + 
#   zoned_over_25k_sq_ft + per_capita_crime_rate + highway_accessibility + 
#   property_tax_rate_per_10k
# 
# Df Sum of Sq   RSS    AIC
# <none>                                 11081 1585.8
# + business_acres           1   2.51754 11079 1587.7
# + units_built_before_1940  1   0.06271 11081 1587.8
# 
# Call:
#   lm(formula = median_home_value ~ percent_lower_status + rooms_per_dwelling + 
#        student_teacher_ratio + distance_to_employment_centers + 
#        nox_ppm + bounded_by_charles_river + african_american_proportion + 
#        zoned_over_25k_sq_ft + per_capita_crime_rate + highway_accessibility + 
#        property_tax_rate_per_10k, data = data)
# 
# Coefficients:
#   (Intercept)            percent_lower_status              rooms_per_dwelling  
# 36.341145                       -0.522553                        3.801579  
# student_teacher_ratio  distance_to_employment_centers                         nox_ppm  
# -0.946525                       -1.492711                      -17.376023  
# bounded_by_charles_river     african_american_proportion            zoned_over_25k_sq_ft  
# 2.718716                        0.009291                        0.045845  
# per_capita_crime_rate           highway_accessibility       property_tax_rate_per_10k  
# -0.108413                        0.299608                       -0.011778  
a_red <- lm(median_home_value ~ 
              percent_lower_status + 
              rooms_per_dwelling + 
              student_teacher_ratio + 
              distance_to_employment_centers + 
              nox_ppm + 
              bounded_by_charles_river + 
              african_american_proportion + 
              zoned_over_25k_sq_ft + 
              per_capita_crime_rate + 
              highway_accessibility + 
              property_tax_rate_per_10k, 
            data = data)
# Estimated coefficients:
write.table(summary(a_red)$coefficients, file=concat(OUTPUT_DIR,'/housing_model_reduced_coefficients.csv'), sep=",")

# Look at standardized coefficients to see which ones have the most impact on 
# median_home_value
lm.a_red.beta <- lm.beta(a_red)
write.table(coef(lm.a_red.beta), file=concat(OUTPUT_DIR,'/housing_model_reduced_coefficients_standard.csv'), sep=",")


#######################################################
# PROBLEM 2                                           #
#######################################################
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

####################################################
# data_marsh_cleaned_hw2.xls > data_marsh
# STATION   station number
# MEHGSWB   Methyl Mercury in surface water, ng/L 	                                           water
# TURB	    in situ surface water turbidity	                                                   water
# DOCSWD	  Dissolved Organic Carbon in surface water, mg/L	                                   water
# SRPRSWFB	Soluble Reactive Phosphorus in surface water,mg/L or ug/L 	                       water
# THGFSFC	  Total Mercury in mosquitofish (Gambusia affinis), average of 7 individuals, ug/kg	 water
# THGSDFC	  Total Mercury in soil, ng/g 	                                                     soil
# TCSDFB	  Total Carbon in soil, % 	                                                         soil
# TPRSDFB	  Total Phosphorus in soil, ug/g	                                                   soil
lapply(c("gdata","ggplot2","CCA"), load_package)
install.packages("GGally")
library(GGally)
# get the data
data <- read.xls(xls=concat(DATA_DIR,'/data_marsh_cleaned_hw2.xls'),
                 sheet='data_marsh',
                 verbose=FALSE,
                 pattern='12',
                 method=c('csv'),
                 header=FALSE,
                 perl="C:/Perl/bin/perl.exe")
str(data)
head(data)
colnames(data) <- c('STATION','MEHGSWB','TURB','DOCSWD','SRPRSWFB','THGFSFC','THGSDFC','TCSDFB','TPRSDFB')
str(data)
head(data)
# remove the STATION id
drops <- c("STATION")
data <- data[,!(names(data) %in% drops)]
str(data)
head(data)

# summarize the data
summary(data)
water <- data[,c('MEHGSWB','TURB','DOCSWD','SRPRSWFB','THGFSFC')]
soil <- data[,c('THGSDFC','TCSDFB','TPRSDFB')]

# look at the correlations between the water variables
png(concat(IMAGES_DIR,'/water soil - correlations - water.png'), 
    width = 1024, height = 1024)
ggpairs(water)
dev.off()
# look at the correlations between the soil variables
png(concat(IMAGES_DIR,'/water soil - correlations - soil.png'), 
    width = 1024, height = 1024)
ggpairs(soil)
dev.off()

# look at the correlations within and between the water and soil variables
corrs <- matcor(water, soil)
write('Xcor', 
      file=concat(OUTPUT_DIR,'/water soil cross correlations.csv'),
      append=FALSE)
write.table(corrs$Xcor, 
            file=concat(OUTPUT_DIR,'/water soil cross correlations.csv'),
            append=TRUE,
            sep=",")
write('', 
      file=concat(OUTPUT_DIR,'/water soil cross correlations.csv'),
      append=TRUE)
write('Ycor', 
      file=concat(OUTPUT_DIR,'/water soil cross correlations.csv'),
      append=TRUE)
write.table(corrs$Ycor, 
            file=concat(OUTPUT_DIR,'/water soil cross correlations.csv'),
            append=TRUE,
            sep=",")
write('', 
      file=concat(OUTPUT_DIR,'/water soil cross correlations.csv'),
      append=TRUE)
write('XYcor', 
      file=concat(OUTPUT_DIR,'/water soil cross correlations.csv'),
      append=TRUE)
write.table(corrs$XYcor, 
            file=concat(OUTPUT_DIR,'/water soil cross correlations.csv'),
            append=TRUE,
            sep=",")
# show images of the correlations
png(concat(IMAGES_DIR,'/water soil - correlations - image1.png'), 
    width = 1024, height = 1024)
img.matcor(corrs, type=1)
dev.off()
png(concat(IMAGES_DIR,'/water soil - correlations - image2.png'), 
    width = 1024, height = 1024)
img.matcor(corrs, type=2)
dev.off()

# perform canonical correlation analysis
cc1 <- cc(water, soil)
# display the canonical correlations
write('Canonical Correlations', 
      file=concat(OUTPUT_DIR,'/water soil canonical correlations.csv'),
      append=FALSE)
write.table(cc1$cor, 
            file=concat(OUTPUT_DIR,'/water soil canonical correlations.csv'),
            append=TRUE,
            sep=",")
write('', 
      file=concat(OUTPUT_DIR,'/water soil canonical correlations.csv'),
      append=TRUE)
write('Raw Canonical Coefficients', 
      file=concat(OUTPUT_DIR,'/water soil canonical correlations.csv'),
      append=TRUE)
write('xcoef', 
      file=concat(OUTPUT_DIR,'/water soil canonical correlations.csv'),
      append=TRUE)
write.table(cc1$xcoef, 
            file=concat(OUTPUT_DIR,'/water soil canonical correlations.csv'),
            append=TRUE,
            sep=",")
write('ycoef', 
      file=concat(OUTPUT_DIR,'/water soil canonical correlations.csv'),
      append=TRUE)
write.table(cc1$ycoef, 
            file=concat(OUTPUT_DIR,'/water soil canonical correlations.csv'),
            append=TRUE,
            sep=",")

# compute the loadings of the variables on the canonical dimensions (variates).
# These loadings are the correlations between variables and the variates.
cc2 <- comput(water, soil, cc1)
write('Canonical Loadings', 
      file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
      append=FALSE)
write('corr.X.xscores', 
      file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
      append=TRUE)
write.table(cc2$corr.X.xscores, 
            file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
            append=TRUE,
            sep=",")
write('', 
      file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
      append=TRUE)
write('corr.Y.xscores', 
      file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
      append=TRUE)
write.table(cc2$corr.Y.xscores, 
            file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
            append=TRUE,
            sep=",")
write('', 
      file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
      append=TRUE)
write('corr.X.yscores', 
      file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
      append=TRUE)
write.table(cc2$corr.X.yscores, 
            file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
            append=TRUE,
            sep=",")
write('', 
      file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
      append=TRUE)
write('corr.Y.yscores', 
      file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
      append=TRUE)
write.table(cc2$corr.Y.yscores, 
            file=concat(OUTPUT_DIR,'/water soil canonical loadings.csv'),
            append=TRUE,
            sep=",")

# Test the null hypothesis that the canonical correlations are all equal to zero,
# that the second and third canonical correlations are equal to zero, and that 
# the third canonical correlation is equal to zero. 
ev <- (1 - cc1$cor^2)
n <- dim(water)[1]
p <- length(water)
q <- length(soil)
k <- min(p,q)
m <- n - 3/2 - (p + q) / 2
w <- rev(cumprod(rev(ev)))
# initialize
d1 <- d2 <- f <- vector("numeric", k)
for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2- 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}
pv <- pf(f, d1, d2, lower.tail = FALSE)
dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
write('Canonical Dimensions Test', 
      file=concat(OUTPUT_DIR,'/water soil canonical dimensions test.csv'),
      append=FALSE)
write.table(dmat, 
            file=concat(OUTPUT_DIR,'/water soil canonical dimensions test.csv'),
            append=TRUE,
            sep=",")

# calculate the standardized water canonical coefficients diagonal matrix of water sd's
s1 <- diag(sqrt(diag(cov(water))))
write('Standardized Water Canonical Coefficients', 
      file=concat(OUTPUT_DIR,'/water soil canonical standardized coefficients.csv'),
      append=FALSE)
write.table(s1 %*% cc1$xcoef, 
            file=concat(OUTPUT_DIR,'/water soil canonical standardized coefficients.csv'),
            append=TRUE,
            sep=",")
write('', 
      file=concat(OUTPUT_DIR,'/water soil canonical standardized coefficients.csv'),
      append=TRUE)
s2 <- diag(sqrt(diag(cov(soil))))
write('Standardized Soil Canonical Coefficients', 
      file=concat(OUTPUT_DIR,'/water soil canonical standardized coefficients.csv'),
      append=TRUE)
write.table(s2 %*% cc1$ycoef, 
            file=concat(OUTPUT_DIR,'/water soil canonical standardized coefficients.csv'),
            append=TRUE,
            sep=",")

#######################################################
# PROBLEM 3                                           #
#######################################################
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

####################################################
# problem3.txt
#  Country: Name of country
#  Agr: Percentage employed in agriculture
#  Min: Percentage employed in mining
#  Man: Percentage employed in manufacturing
#  PS: Percentage employed in power supply industries
#  Con: Percentage employed in construction
#  SI: Percentage employed in service industries
#  Fin: Percentage employed in finance
#  SPS: Percentage employed in social and personal services
#  TC: Percentage employed in transport and communications.
data <- read.table(concat(DATA_DIR,'/problem3.txt'), header=T, sep="\t")
str(data)
head(data)

# look at relationships between variables
load_package('car')
write.table(round(cor(data[,2:10]), 2), file=concat(OUTPUT_DIR,'/employment - correlations.csv'), sep=",")
png(concat(IMAGES_DIR,'/employment - scattplot matrix.png'), 
    width = 2048, height = 2048)
scatterplotMatrix(data[,2:10], diagonal="density")
dev.off()

# perform PCA
fit <- prcomp(data[,2:10], center=TRUE, scale=TRUE)
summary(fit)
# 5 principal components are required to explain 90% of the total variation

# variance charts
# from http://rstudio-pubs-static.s3.amazonaws.com/27823_dbc155ba66444eae9eb0a6bacb36824f.html
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}
png(concat(IMAGES_DIR,'/employment - variance graphs.png'), 
    width = 512, height = 512)
pcaCharts(fit)
dev.off()

# eigenvalues
eigenvalues <- fit$sdev^2
write.table(round(eigenvalues[1:5], 2), file=concat(OUTPUT_DIR,'/employment - eigenvalues.csv'), sep=",")
png(concat(IMAGES_DIR,'/employment - eigenvalues.png'), 
    width = 512, height = 512)
plot(eigenvalues, type='b', xlab="Principal Components", ylab="Eigenvalues")
dev.off()

# eigenvectors
cov_matrix <- cov(data[2:10])
eigs <- eigen(cov_matrix)
eigenvectors <- round(eigs$vectors, 4)
write.table(eigenvectors, file=concat(OUTPUT_DIR,'/employment - eigenvectors.csv'), sep=",")

# rotations
write.table(round(fit$rotation[,1:5], 4), file=concat(OUTPUT_DIR,'/employment - rotations.csv'), sep=",")

# bi-directional plot
load_package('ggplot2')
countries <- data[,1]
PCbiplot <- function(PC, rownames, x="PC1", y="PC2") {
  # code is modified but mostly borrowed from:
  #    http://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
  #    posted by http://stackoverflow.com/users/577462/crayola
  # PC being a prcomp object
  data <- data.frame(obsnames=countries, PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
  plot <- plot + geom_hline(alpha=0.4, size=.2, yintercept=0) + geom_vline(alpha=0.4, size=.2, xintercept=0)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plot
}
png(concat(IMAGES_DIR,'/employment - biplot.png'), 
    width = 1024, height = 1024)
PCbiplot(fit, countries)
dev.off()

# individual coordinates
write.table(fit$x, file=concat(OUTPUT_DIR,'/employment - coordinates.csv'), sep=",")
