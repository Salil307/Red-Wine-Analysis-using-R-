**RMarkdown** enables you to interleave Markdown text, blocks of code, and output.

If you're in the editor, the resulting notebook will be shown on the HTML tab to the right.

This R environment comes with all of CRAN preinstalled, as well as many other helpful packages

The environment is defined by the [kaggle/rstats docker image](https://github.com/kaggle/docker-rstats).

For example, here's several helpful packages to load in 

---
title: "Red Wine Analysis by R"
author: "Sagarnil Das"
date: "June 21, 2017"
output: rmarkdown::github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

```{r message = FALSE}
#Load all the required libraries
library("ggplot2")
library("dplyr")
library("gridExtra")
library(Simpsons)
library(GGally)
library(memisc)
library(pander)
library(corrplot)
```

## Target of this Project

In order to determine which factors affect wine quality, I will analyse the Red Wine Data for this project. I'll attempt to acquire a sense of the variables separately before attempting to determine how they relate to the wine quality when other variables are taken into account. In the end, I'll develop a linear model to forecast the results of test set data.

```{r }
#Loading the csv file
wine <- read.csv('../input/wineQualityReds.csv')

#Transforming Quality from an Integer to a Factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new Factored Variable called 'Rating'

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))

```

## Structure and summary of the Dataframe



```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
str(wine)
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
summary(wine)
```

## Univariate Plots

I'm going to plot the distribution of each variable first in order to get a feel for it before undertaking any analysis between the variables. Plotting different variables against each other will also help me get a feel of what to expect based on the distribution shape, such as Normal, Positive Skew, or Negative Skew. This dataset has severe outliers for a number of variables as well. To provide a more thorough study, I shall exclude the extreme outliers for those.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine, aes(x = quality)) +
  geom_bar(width = 1, color = 'black',fill = I('orange'))

```

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine, aes(x = rating)) +
  geom_bar(width = 1, color = 'black',fill = I('blue'))
```

 Let's look at the other plots.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = fixed.acidity ) ) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(4,14)),
ggplot(data = wine, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) + 
  scale_x_continuous(lim = c(4,14)),ncol = 2)
  

```



The distribution of Fixed Acidity is positively skewed. The median is around 8 with high concentration of wines with Fixed Acidity but due to some outliers, the mean has been dragged to around 9.4. The image has been rescaled to get rid of the high outliers.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = volatile.acidity ) ) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,1)),
ggplot(data = wine, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.05, color = 'black',fill = I('orange')) +
  scale_x_continuous(lim = c(0,1)), ncol = 2)
```


The distribution of Volatile acidity looks like Bimodal with two peaks around 0.4 and 0.6.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = citric.acid )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
ggplot(data = wine, aes(x = citric.acid)) +
  geom_histogram(binwidth = 0.08, color = 'black',fill = I('orange')) +
  scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)), ncol = 2)

```


Apart from some outliers, the distribution of Citric acid looks strange. Some higher values have no data at all and apart from them, the distribution looks almost rectangular. Maybe there was some error in the data or maybe the data collected was incomplete?

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = residual.sugar )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(1,8)),
ggplot(data = wine, aes(x = residual.sugar)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
  scale_x_continuous(lim = c(1,8)), ncol = 2)
```

The distribution of Residual Sugar is again positively skewed with high peaks at around 2.3 with many outliers present at the higher ranges.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = chlorides )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,0.25)),
ggplot(data = wine, aes(x = chlorides)) +
  geom_histogram(binwidth = 0.01, color = 'black',fill = I('orange')) +
  scale_x_continuous(lim = c(0,0.25)), ncol = 2)
```


For Chlorides also, we see a similar distribution like Residual Sugar. We have got rid of extreme outliers in this image.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = free.sulfur.dioxide )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,45)),
ggplot(data = wine, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) +
  scale_x_continuous(breaks = seq(0,80,5), lim = c(0,45)), ncol = 2)
```


For Free Sulphur Dioxide, there is a high peak at 7 but then it again follows the same positively skewed long tailed patterns with some outliers in the high range.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = total.sulfur.dioxide )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,180)),
ggplot(data = wine, aes(x = total.sulfur.dioxide)) +
  geom_histogram(binwidth = 5, color = 'black',fill = I('orange')) +
  scale_x_continuous(lim = c(0,180)), ncol = 2)
```


Being a superset of the previous variable, Total Sulphur Dioxide also follows a similar pattern.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = density)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
ggplot(data = wine, aes(x = density)) +
  geom_histogram(binwidth = 0.001, color = 'black',fill = I('orange')), ncol = 2)

```


For the Density variable, we see something new for the first time. This Variable has almost a perfect Normal Distribution.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = pH)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
ggplot(data = wine, aes(x = pH)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')), ncol = 2)

```


pH also has a very Normally distributed shape.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = sulphates)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0.3,1.6)),
ggplot(data = wine, aes(x = sulphates)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
  scale_x_continuous(lim = c(0.3,1.6)), ncol = 2)

```


Sulphates also exhibit a similar long tailed distribution like Chlorides or Free/Total Sulphur Dioxide. It has relatively less outliers.

```{r echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(wine, aes( x = 1, y = alcohol)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(8,14)),
ggplot(data = wine, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
  scale_x_continuous(lim = c(8,14)), ncol = 2)
```


Alcohol also follows a skewed distribution but here the skewness is less than that of Chlorides or Residual Sugars.

## Analysis of the Univariate Plots

### Dataset Structure

The Red Wine Dataset had 1599 rows and 13 columns originally. After I added a new column called 'rating', the number of columns became 14. Here our categorical variable is 'quality', and the rest of the variables are numerical variables which reflect the physical and chemical properties of the wine.

I also see that in this dataset, most of the wines belong to the 'average' quality with very few 'bad' and 'good' ones. Now this again raises my doubt if this dataset is a complete one or not. For the lack of these data, it might be challenging to build a predictive model as I don't have enough data for the Good Quality and the Bad Quality wines.

### Distribution and Outliers

1. Density and pH seems normally distributed with few outliers.

2. Residual sugar and Chloride seems to have extreme outliers.

3. Fixed and volatile acidity, total and free sulfur dioxides, alcohol and sulphates seem to be long-tailed for the outliers present.

4. Citric acid has large number of zero values. I wonder if this is due to incomplete data entry.

## Bivariate Plots

First I will create a correlation table between the variables present in this dataset so that I get some initial guidance about which ones may be correlated to each other.

```{r}
c <- cor(
  wine %>%
    # first we remove unwanted columns
    dplyr::select(-X) %>%
    dplyr::select(-rating) %>%
    mutate(
      # now we translate quality to a number
      quality = as.numeric(quality)
    )
)
emphasize.strong.cells(which(abs(c) > .3 & c != 1, arr.ind = TRUE))
pandoc.table(c)
```

1. The very first thing that caught my eye in this table is that Volatile acidity has a positive correlation with pH. But how can that be possible! We know that with the decrease in pH, acidity increases. So is it possible that a Simpson's Paradox is at play here? I will further investigate this peculiar characteristic.

2. Density has a very strong correlation with Fixed Acidity.

3. The variables most strongly correlated to quality are Volatile Acidity and Alcohol.

4. Alcohol has negative correlation with density. This is evident from the fact that the density of water is greater than the density of alcohol.

Now let us create some Box plots between these variables to see if I have missed anything from the correlation table.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine, aes(x = quality, y = fixed.acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
```


As we can see, Fixed Acidity has almost no effect on the Quality. The mean and median values of fixed acidity remains almost unchanged with increase in quality.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x = quality, y = volatile.acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
```

Volatile acid seems to have a negative impact on the quality of the wine. As volatile acid level goes up, the quality of the wine degrades.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=citric.acid)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
```


Citric acid seems to have a positive correlation with Wine Quality. Better wines have higher Citric Acid.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=residual.sugar)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,5)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)


```


Previously I thought that Residual Sugar may have an effect on the wine quality. But this plot contradicts that assumption and shows that Residual Sugar almost has no effect on the Quality of the Wine. The mean values for the residual sugar is almost the same for every quality of wine.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=chlorides)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,0.2)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

```

Even though weakly correlated, from the decrease in median values of the Chlorides with increase in quality, it seems that lower percent of Chloride seems to produce better wines.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=free.sulfur.dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,40)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

```

Now this is an interesting observation. We see that too low concentration of Free Sulphur Dioxide produces poor wine and too high concentration results in average wine.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=total.sulfur.dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,150)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

```


As this is a Subset of Free Sulphur Dioxide, we see a similar pattern here.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=density)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

```


Better wines seems to have lower densities. But may be it will be wise not to draw any conclusions here. Because there might be a possibility that the low density is due to higher alcohol content which actually is the driving factor for better wines.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=pH)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

```


Better wines seems to have less pH, i.e they are more acidic. But there are a quite a few outliers here. So maybe the next logical thing would be to see how the individual acids affects the pH.

```{r echo=FALSE, message=FALSE, warning=FALSE}

ggplot(data = wine, aes(x = fixed.acidity, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(5,15,1)) +
  xlab("Fixed Acidity in Log Scale") +
  geom_smooth(method="lm")
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine, aes(x = volatile.acidity, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("Volatile Acidity in Log Scale") +
  geom_smooth(method="lm")
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = subset(wine, citric.acid > 0), aes(x = citric.acid, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  xlab("Citric Acid in Log Scale") +
  geom_smooth(method="lm")
```

These three plots make us come back to our old question. Recall that we saw for Volatile Acid, pH has a positive correlation. But we know acidity has a negative correlation with pH. So is it possible, that we are seeing a Simpson's Paradox at play here? Let's investigate.

```{r echo=FALSE, message=FALSE, warning=FALSE}
simpsons <- Simpsons(volatile.acidity, pH, data=wine)
plot(simpsons)
```


Wow! So it is indeed Simpson's paradox which was responsible for the trend reversal of Volatile Acid vs pH. I clustered the data into 3 segments and calculated the regression coefficient. I see that there is indeed a sign reversal. This is due to a lurking variable which changes the overall coefficient.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=sulphates)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0.25,1)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)


```


Even though we see many outliers in the 'Average' quality wine, it seems that better wines have a stronger concentration of Sulphates.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(x=quality, y=alcohol)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)

```


The correlation is really distinct here. It is pretty evident that better wines have higher Alcohol content in it. But we see a great number of outliers here. So it might be possible that alcohol alone does not contribute to a wine being a good quality one. Let's make a simple linear model and try to get the statistics here.

```{r echo=FALSE, message=FALSE, warning=FALSE}
alcoholQualityLinearModel <- lm(as.numeric(quality) ~ alcohol,
                       data = wine)
summary(alcoholQualityLinearModel)
```


Based on the value of R squared, we see that Alcohol alone contributes to only about 22% of the Wine quality. So there must be other variables at play here. I have to figure them out in order to build a better regression model.

So now I will put a correlation test against each variable to the quality of the wine.

```{r echo=FALSE, message=FALSE, warning=FALSE}
simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

correlations <- c(
  simple_cor_test(wine$fixed.acidity, wine$quality),
  simple_cor_test(wine$volatile.acidity, wine$quality),
  simple_cor_test(wine$citric.acid, wine$quality),
  simple_cor_test(log10(wine$residual.sugar), wine$quality),
  simple_cor_test(log10(wine$chlorides), wine$quality),
  simple_cor_test(wine$free.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$total.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$density, wine$quality),
  simple_cor_test(wine$pH, wine$quality),
  simple_cor_test(log10(wine$sulphates), wine$quality),
  simple_cor_test(wine$alcohol, wine$quality))
names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                         'log10.residual.sugar',
                         'log10.chlordies', 'free.sulfur.dioxide',
                         'total.sulfur.dioxide', 'density', 'pH',
                         'log10.sulphates', 'alcohol')

correlations
```


From the correlation test, it seems that the following variables have a higher correlation to Wine Quality.

1. Alcohol
2. Sulphates(log10)
3. Volatile Acidity
4. Citric Acid

## Analysis of Bivariate Plots

### Observations

1. Fixed Acidity seems to have almost no effect on quality.
2. Volatile Acidity seems to have a negative correlation with the quality.
3. Better wines seem to have higher concentration of Citric Acid.
4. Better wines seem to have higher alcohol percentages. But when I created a linear model around it, I saw from the R squared value that alcohol by itself only contributes like 20% on the variance of the quality. So there may be some other factors at play here.
5. Even though it's a weak correlation, but lower percent of Chloride seems to produce better quality wines.
6. Better wines seem to have lower densities. But then again, this may be due to the higher alcohol content in them.
7. Better wines seem to be more acidic.
8. Residual sugar almost has no effect on the wine quality.

### Special features

Volatile acidity had a positive correlation with pH which at first was totally unexpected to me. Later I found out that this was due to the Simpson's Paradox.

## Multivariate Plots

As we saw, that alcohol plays a strong part in the quality of the wine even though it actually contributes only 22% of the total quality, now I will first make alcohol constant and try to insert a few more variables to see if they contribute to the overall quality in any other way.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = density, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```


With constant Alcohol, Density does not seem to play a prominet role in changing the quality of the alcohol. So our previous suspicion must be true that the correlation we were seeing of density with quality was due to alcohol percent.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = sulphates, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0.3,1.5)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```


It looks like Wines with higher alcohol content produce better wine if they have higher level of Sulphates.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = volatile.acidity, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```


It looks like Volatile acid has just the opposite effect. With less concentration of volatile acid and higher concentration of alcohol seems to produce better wines.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = pH, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```


Here also, low pH and high Alcohol percentage seems to produce better wines.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = residual.sugar, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```

No such correlation between residual sugar and quality.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = total.sulfur.dioxide, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```


In general lower Sulphur Dioxide seems to produces better wine even though some high outliers for better wine with high Sulphur Dioxide.


Now let us try to investigate the effect of Acids on the Quality of Wines.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = citric.acid, x = volatile.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```


Higher Citric Acid and low Volatile Acid seems to produce better Wines.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = citric.acid, x = fixed.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```

I don't see much correlations here.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = fixed.acidity, x = volatile.acidity,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
```


Again, I don't get much correlation with the quality here.

## Linear Modelling

Now after all these analysis, I am going to take the variables which are most strongly correlated with the quality of the wine and generate a linear model with them.

```{r}
set.seed(1221)
training_data <- sample_frac(wine, .6)
test_data <- wine[ !wine$X %in% training_data$X, ]
m1 <- lm(as.numeric(quality) ~ alcohol, data = training_data)
m2 <- update(m1, ~ . + sulphates)
m3 <- update(m2, ~ . + volatile.acidity)
m4 <- update(m3, ~ . + citric.acid)
m5 <- update(m4, ~ . + fixed.acidity)
m6 <- update(m2, ~ . + pH)
mtable(m1,m2,m3,m4,m5,m6)
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
wine_predict <- data.frame(
  test_data$quality,
  predict(m5, test_data) - as.numeric(test_data$quality)
)
names(wine_predict) <- c("quality", "error")
ggplot(data=wine_predict, aes(x=quality,y=error)) +
  geom_jitter(alpha = 0.3)
```

## Analysis of the Multivariate Plots

### Observations

1. High Alcohol and Sulaphate content seems to produce better wines.
2. Citric Acid, even though weakly correlated plays a part in improving the wine quality.


### Linear Models Created

I created a couple of linear models. But the main problem was there was not enough statistic to have a significant confidence level in the equations produced. Because of the low R squared value, I saw that alcohol contributes to only 22% of the Wine Quality and the most of the factors converged on the Average quality wines. This can be due to the fact that our dataset comprised mainly of 'Average' quality wines and as there were very few data about the 'Good' and the 'Bad' quality wines in the training dataset, that's why it was difficult to predict statistics for the edge cases. Maybe a more complete dataset would have helped me better in predicting the higher range values.

## Final Plots and Summary

I saw that the Alcohol and Sulphates played a major role in determining alcohol quality. Also in the final linear model I made, I also plotted the error value against the quality which shows us the variation in the error percentage with different qualities of Wine. I think these three plots are very crtical plots for this project. So I decided to include these three plots in the Final Plots and Summary section.

### Plot 1

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=wine, aes(y=alcohol, x=quality)) + 
  geom_jitter(alpha = .3)  +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4) +
  xlab("Quality") +
  ggtitle("Influence of alcohol on wine quality")
```

This plot tells us that Alcohol percentage has played a big role in determining the quality of Wines. The higher the alcohol percentage, the better the wine quality. In this dataset, even though most of the data pertains to average quality wine, we can see from the above plot that the mean and median coincides for all the boxes implying that for a particular Quality it is very normally distributed. So a very high value of the median in the best quality wines imply that almost all points have a high percentage of alcohol. But previously from our linear model test, we saw from the R Squared value that alcohol alone contributes to about 22% in the variance of the wine quality. So alcohol is not the only factor which is responsible for the improvement in Wine Quality.

### Plot 2

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wine,
       aes(y = sulphates, x = alcohol,
           color = quality)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0.3,1.5)) +
  ylab("potassium sulphate (g/dm3)") +
  xlab("Alcohol Percentage") +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality')) +
  ggtitle("Alcohol and sulphates over wine quality")
```



In this plot, we see that the best quality wines have high values for both Alcohol percentage and Sulphate concentration implying that High alcohol contents and high sulphate concentrations together seem to produce better wines. Although there is a very slight downwards slope maybe because in best quality wines, percentage of alcohol is slightly greater than the concentration of Sulphates.


### Plot 3


```{r}
df <- data.frame(
  test_data$quality,
  predict(m5, test_data) - as.numeric(test_data$quality)
)
names(df) <- c("quality", "error")
ggplot(data=df, aes(x=quality,y=error)) +
  geom_jitter(alpha = 0.3) +
  ggtitle("Linear model errors vs expected quality")
```


We see that the error is much more dense in the 'Average' quality section than the 'Good' and the 'Bad' quality wines. This is evident from the fact that most of our dataset contains 'Average' quality wines and there is not too many data in the extreme ranges. The linear model with the R squared value for m5 could only explain around 33% change in quality. Also the earlier models clearly shows that due to the lack of information, it is not the best model to predict both 'Good' and 'Bad' quality wines.


