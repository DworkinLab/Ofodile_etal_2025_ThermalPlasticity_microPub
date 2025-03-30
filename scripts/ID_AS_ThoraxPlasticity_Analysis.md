---
title: 'Stewart artificial selection lineages: thermal Plasticity experiment'
author: "Ian Dworkin"
date: "30 Mar 2025"
output:
  html_document:
    keep_md: yes
    number_sections: yes
    toc: yes
  pdf_document:
    toc: yes
editor_options:
  chunk_output_type: console
---




## Load libraries

``` r
require(car)
```

```
## Loading required package: car
```

```
## Loading required package: carData
```

``` r
require(emmeans)
```

```
## Loading required package: emmeans
```

```
## Welcome to emmeans.
## Caution: You lose important information if you filter this package's results.
## See '? untidy'
```

``` r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

``` r
require(lme4)
```

```
## Loading required package: lme4
```

```
## Loading required package: Matrix
```

``` r
require(glmmTMB)
```

```
## Loading required package: glmmTMB
```

``` r
require(lmerTest)
```

```
## Loading required package: lmerTest
```

```
## 
## Attaching package: 'lmerTest'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmer
```

```
## The following object is masked from 'package:stats':
## 
##     step
```

``` r
require(effects)
```

```
## Loading required package: effects
```

```
## lattice theme set by effectsTheme()
## See ?effectsTheme for details.
```

``` r
require(predictmeans)
```

```
## Loading required package: predictmeans
```

```
## Loading required package: nlme
```

```
## 
## Attaching package: 'nlme'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmList
```

``` r
require(ggbeeswarm)
```

```
## Loading required package: ggbeeswarm
```

``` r
require(ggdist)
```

```
## Loading required package: ggdist
```

``` r
require(ggridges)
```

```
## Loading required package: ggridges
```

```
## 
## Attaching package: 'ggridges'
```

```
## The following objects are masked from 'package:ggdist':
## 
##     scale_point_color_continuous, scale_point_color_discrete,
##     scale_point_colour_continuous, scale_point_colour_discrete,
##     scale_point_fill_continuous, scale_point_fill_discrete,
##     scale_point_size_continuous
```

``` r
require(cowplot)
```

```
## Loading required package: cowplot
```



``` r
pref_theme <- theme_classic() +
  theme(text = element_text(size = 14))
```


## load data and do quality checks


``` r
size_dat <- read.csv("../data/AS_LotithTempPlasticity.csv", h = T,
                     stringsAsFactors = TRUE)

str(size_dat)
```

```
## 'data.frame':	1641 obs. of  6 variables:
##  $ Treat : Factor w/ 5 levels "C","E","L","LHm",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Temp  : int  18 18 18 18 18 18 18 18 18 18 ...
##  $ Rep   : int  2 2 2 2 2 2 2 2 2 2 ...
##  $ Vial  : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Sex   : Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Thorax: num  0.941 0.797 0.921 0.861 1 ...
```

``` r
levels(size_dat$Treat)
```

```
## [1] "C"   "E"   "L"   "LHm" "S"
```

``` r
head(size_dat)
```

```
##   Treat Temp Rep Vial Sex Thorax
## 1     C   18   2    1   F  0.941
## 2     C   18   2    1   F  0.797
## 3     C   18   2    1   F  0.921
## 4     C   18   2    1   F  0.861
## 5     C   18   2    1   F  1.000
## 6     C   18   2    1   F  0.895
```

``` r
with(size_dat, table(Sex, Treat, Temp))
```

```
## , , Temp = 18
## 
##    Treat
## Sex  C  E  L LHm  S
##   F 57 52 60  60 59
##   M 60 60 57  60 41
## 
## , , Temp = 25
## 
##    Treat
## Sex  C  E  L LHm  S
##   F 54 60 54  47 49
##   M 49 56 38  54 54
## 
## , , Temp = 31
## 
##    Treat
## Sex  C  E  L LHm  S
##   F 60 58 58  59 53
##   M 60 60 58  52 42
```


### check the class of some objects so they are what we need for analysis (i.e. factors instead of integers)

sex, selection, replicate, sampling, trait, individual_ID and repeat_measure should all be factors.


``` r
str(size_dat)
```

```
## 'data.frame':	1641 obs. of  6 variables:
##  $ Treat : Factor w/ 5 levels "C","E","L","LHm",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Temp  : int  18 18 18 18 18 18 18 18 18 18 ...
##  $ Rep   : int  2 2 2 2 2 2 2 2 2 2 ...
##  $ Vial  : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Sex   : Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Thorax: num  0.941 0.797 0.921 0.861 1 ...
```

``` r
names(size_dat)
```

```
## [1] "Treat"  "Temp"   "Rep"    "Vial"   "Sex"    "Thorax"
```

``` r
size_dat$TempF <- size_dat$Temp # Temperature, coded as a factor.
names(size_dat)
```

```
## [1] "Treat"  "Temp"   "Rep"    "Vial"   "Sex"    "Thorax" "TempF"
```

``` r
# use lapply to convert strings to factors where we need them
size_dat[, c(3,4,7)] <- lapply(size_dat[, c(3,4,7)], factor)
str(size_dat) # check that it behaves as expected
```

```
## 'data.frame':	1641 obs. of  7 variables:
##  $ Treat : Factor w/ 5 levels "C","E","L","LHm",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Temp  : int  18 18 18 18 18 18 18 18 18 18 ...
##  $ Rep   : Factor w/ 2 levels "1","2": 2 2 2 2 2 2 2 2 2 2 ...
##  $ Vial  : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Sex   : Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Thorax: num  0.941 0.797 0.921 0.861 1 ...
##  $ TempF : Factor w/ 3 levels "18","25","31": 1 1 1 1 1 1 1 1 1 1 ...
```

``` r
size_dat$Treat <- relevel(size_dat$Treat, "LHm")
```



## log transform size

I may not use it, but we should always check.

From $mm$ to $\mu m$ and then $log_2$ transformed

``` r
size_dat$log2_thorax <- log(1000*size_dat$Thorax, base = 2)
```


labels

``` r
levels(size_dat$Treat) <- c("LHm", "Control", "Reversal", "Large", "Small")
  
treat.labs <- c(expression(LH^M), "Control", "Reversal", "Large", "Small")

names(treat.labs) <- c(levels(size_dat$Treat))
```



### Now check some basic data characteristics

Let's make sure we don't have any really obvious outliers (like something 10 times too big or ten times too small). Also make sure mean and median are pretty close.


``` r
with(size_dat,
     tapply(Thorax, interaction(Treat, TempF, Sex, drop = T ), quantile, 
            probs = c(0,0.005, 0.02, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95,0.98,0.99, 1)))
```

```
## $LHm.18.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.819 0.830 0.862 0.928 0.965 0.992 1.018 1.053 1.060 1.061 1.063 1.067 
## 
## $Control.18.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.797 0.814 0.859 0.893 0.921 0.953 0.974 0.996 1.001 1.018 1.056 1.102 
## 
## $Reversal.18.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.805 0.807 0.814 0.832 0.853 0.873 0.906 0.926 0.948 0.956 0.956 0.956 
## 
## $Large.18.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.865 0.868 0.877 0.950 0.997 1.042 1.065 1.091 1.099 1.106 1.107 1.108 
## 
## $Small.18.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.608 0.609 0.614 0.632 0.655 0.677 0.700 0.716 0.722 0.736 0.739 0.742 
## 
## $LHm.25.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.708 0.712 0.725 0.827 0.871 0.918 0.948 0.976 0.994 1.007 1.015 1.023 
## 
## $Control.25.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.703 0.707 0.719 0.789 0.829 0.863 0.902 0.928 0.938 0.960 0.967 0.974 
## 
## $Reversal.25.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.636 0.643 0.664 0.729 0.760 0.795 0.829 0.849 0.859 0.869 0.872 0.876 
## 
## $Large.25.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.743 0.756 0.795 0.864 0.912 0.956 1.000 1.033 1.046 1.060 1.067 1.074 
## 
## $Small.25.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.518 0.518 0.519 0.576 0.609 0.654 0.674 0.702 0.707 0.721 0.724 0.727 
## 
## $LHm.31.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.688 0.689 0.693 0.728 0.784 0.878 0.913 0.935 0.945 0.964 0.966 0.967 
## 
## $Control.31.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.722 0.728 0.744 0.792 0.820 0.863 0.905 0.917 0.927 0.933 0.946 0.964 
## 
## $Reversal.31.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.534 0.574 0.675 0.700 0.738 0.796 0.831 0.847 0.857 0.867 0.870 0.875 
## 
## $Large.31.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.676 0.681 0.697 0.759 0.852 0.911 0.966 0.988 0.998 1.022 1.038 1.056 
## 
## $Small.31.F
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.512 0.512 0.513 0.545 0.568 0.597 0.621 0.639 0.661 0.665 0.685 0.707 
## 
## $LHm.18.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.738 0.747 0.771 0.810 0.846 0.862 0.881 0.904 0.914 0.929 0.940 0.952 
## 
## $Control.18.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.735 0.736 0.743 0.814 0.832 0.851 0.870 0.881 0.885 0.895 0.897 0.897 
## 
## $Reversal.18.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.707 0.717 0.745 0.779 0.801 0.824 0.842 0.864 0.872 0.878 0.883 0.889 
## 
## $Large.18.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.731 0.732 0.743 0.832 0.883 0.920 0.944 0.966 0.975 0.982 0.984 0.986 
## 
## $Small.18.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.457 0.462 0.477 0.532 0.548 0.576 0.604 0.631 0.693 0.698 0.705 0.713 
## 
## $LHm.25.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.688 0.688 0.689 0.721 0.749 0.787 0.835 0.870 0.885 0.947 0.968 0.988 
## 
## $Control.25.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.622 0.629 0.650 0.715 0.751 0.779 0.803 0.824 0.833 0.855 0.858 0.860 
## 
## $Reversal.25.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.572 0.580 0.602 0.665 0.707 0.734 0.758 0.775 0.783 0.794 0.796 0.798 
## 
## $Large.25.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.651 0.660 0.689 0.743 0.782 0.829 0.869 0.895 0.916 0.928 0.940 0.952 
## 
## $Small.25.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.452 0.458 0.477 0.502 0.530 0.553 0.579 0.589 0.596 0.602 0.602 0.602 
## 
## $LHm.31.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.654 0.654 0.655 0.691 0.729 0.786 0.811 0.829 0.842 0.870 0.871 0.872 
## 
## $Control.31.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.574 0.585 0.616 0.673 0.711 0.767 0.778 0.793 0.806 0.817 0.827 0.838 
## 
## $Reversal.31.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.573 0.579 0.601 0.646 0.675 0.721 0.750 0.766 0.780 0.789 0.794 0.800 
## 
## $Large.31.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.645 0.647 0.656 0.711 0.763 0.821 0.858 0.891 0.895 0.907 0.913 0.919 
## 
## $Small.31.M
##    0%  0.5%    2%   10%   25%   50%   75%   90%   95%   98%   99%  100% 
## 0.433 0.436 0.445 0.470 0.513 0.540 0.565 0.587 0.592 0.600 0.613 0.627
```

``` r
with(size_dat,
     tapply(Thorax, interaction(Treat, TempF, Sex, drop = T ), mean))
```

```
##      LHm.18.F  Control.18.F Reversal.18.F    Large.18.F    Small.18.F 
##         0.989         0.948         0.877         1.026         0.676 
##      LHm.25.F  Control.25.F Reversal.25.F    Large.25.F    Small.25.F 
##         0.906         0.860         0.790         0.952         0.643 
##      LHm.31.F  Control.31.F Reversal.31.F    Large.31.F    Small.31.F 
##         0.855         0.858         0.782         0.895         0.596 
##      LHm.18.M  Control.18.M Reversal.18.M    Large.18.M    Small.18.M 
##         0.858         0.847         0.821         0.907         0.580 
##      LHm.25.M  Control.25.M Reversal.25.M    Large.25.M    Small.25.M 
##         0.796         0.770         0.725         0.822         0.551 
##      LHm.31.M  Control.31.M Reversal.31.M    Large.31.M    Small.31.M 
##         0.773         0.745         0.711         0.809         0.535
```



We could also just look at min and max for each as a check.

``` r
with(size_dat, tapply(Thorax,
                      interaction(Treat, TempF, Sex, drop = T ), 
                      range))
```

```
## $LHm.18.F
## [1] 0.819 1.067
## 
## $Control.18.F
## [1] 0.797 1.102
## 
## $Reversal.18.F
## [1] 0.805 0.956
## 
## $Large.18.F
## [1] 0.865 1.108
## 
## $Small.18.F
## [1] 0.608 0.742
## 
## $LHm.25.F
## [1] 0.708 1.023
## 
## $Control.25.F
## [1] 0.703 0.974
## 
## $Reversal.25.F
## [1] 0.636 0.876
## 
## $Large.25.F
## [1] 0.743 1.074
## 
## $Small.25.F
## [1] 0.518 0.727
## 
## $LHm.31.F
## [1] 0.688 0.967
## 
## $Control.31.F
## [1] 0.722 0.964
## 
## $Reversal.31.F
## [1] 0.534 0.875
## 
## $Large.31.F
## [1] 0.676 1.056
## 
## $Small.31.F
## [1] 0.512 0.707
## 
## $LHm.18.M
## [1] 0.738 0.952
## 
## $Control.18.M
## [1] 0.735 0.897
## 
## $Reversal.18.M
## [1] 0.707 0.889
## 
## $Large.18.M
## [1] 0.731 0.986
## 
## $Small.18.M
## [1] 0.457 0.713
## 
## $LHm.25.M
## [1] 0.688 0.988
## 
## $Control.25.M
## [1] 0.622 0.860
## 
## $Reversal.25.M
## [1] 0.572 0.798
## 
## $Large.25.M
## [1] 0.651 0.952
## 
## $Small.25.M
## [1] 0.452 0.602
## 
## $LHm.31.M
## [1] 0.654 0.872
## 
## $Control.31.M
## [1] 0.574 0.838
## 
## $Reversal.31.M
## [1] 0.573 0.800
## 
## $Large.31.M
## [1] 0.645 0.919
## 
## $Small.31.M
## [1] 0.433 0.627
```




### graphical summaries of the raw data

I am providing a few different ways of plotting this. You can choose which you think is best to make your points.

These all show the same thing, just in different ways. Mostly a matter of preference depending on what you want to highlight in your paper.


``` r
ggplot(size_dat, aes( y = Thorax, x = TempF, col = Treat:Sex)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(size = 1.25, alpha = 0.25, 
                position=position_jitterdodge(0.5), show.legend= FALSE) +
  labs( y = "length (mm)") +
  theme_classic() +
  theme(text = element_text(size = 24))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



``` r
# Same as above faceted by sex
ggplot(size_dat, aes( y = Thorax, x = Treat, col = TempF)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(size = 1.25, alpha = 0.25, 
                position=position_jitterdodge(0.5), show.legend= FALSE) +
  labs( y = "length (mm)") +
  facet_wrap(~ Sex) +
  theme_classic() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, size = 12, vjust = 0.6))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



``` r
# log transformed
ggplot(size_dat, aes( y = log2_thorax, x = Treat, col = TempF)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(size = 1.25, alpha = 0.25, 
                position=position_jitterdodge(0.5), show.legend= FALSE) +
  labs( y = "thorax length log2 (µm)") +
  facet_wrap(~ Sex) +
  theme_classic() +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle = 45, size = 12, vjust = 0.6))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



``` r
# violin plots as an alternative. Using log2 transformed. 
ggplot(size_dat, aes( y = log2_thorax, col = Treat, x = TempF)) +
  geom_violin(aes(fill = Treat)) +
  labs( y = "length, log2 (µm)") +
  facet_wrap(~ Sex) +
  theme_classic() +
  theme(text = element_text(size = 20))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Or beeswarm


``` r
ggplot(size_dat, aes( y = log2_thorax, col = Treat, x = TempF)) +
  geom_quasirandom(alpha = 0.5) +
  labs(y = "length, log2 (µm)") +
  facet_wrap(~ Sex) +
  theme_classic() +
  theme(text = element_text(size = 20))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->



``` r
ggplot(size_dat, aes( y = log2_thorax, x = Treat, col = TempF, shape = TempF)) +
  geom_quasirandom(alpha = 0.5) +
  labs(y = "length, log2 (µm)") +
  facet_wrap(~ Sex) +
  theme_classic() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, size = 12, vjust = 0.6))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



``` r
ggplot(size_dat, aes( y = log2_thorax, x = TempF, col = Sex, shape = Sex)) +
  geom_quasirandom(alpha = 0.5, method = "tukeyDense") +
  labs(#y = "length, log2 (µm)", 
       y = expression(thorax~length~log[2]~mu~m),
       x = "Temperature (°C)") +
  theme_bw() +
  facet_wrap(~ Treat, labeller = labeller(Treat = treat.labs)) +
  theme(text = element_text(size = 15),
        legend.position = "none")
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


## Additional plots of raw data to examine variation by vials and replicate block



``` r
ggplot(size_dat, 
       aes( y = Thorax, x = Treat, col = TempF:Sex)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(size = 1.25, alpha = 0.25, 
                position=position_jitterdodge(0.5), show.legend= FALSE) +
  labs( y = "length (mm)") +
  facet_wrap(~ Rep) +
  ggtitle("thorax") +
  theme_classic() +
  theme(text = element_text(size = 20),,
        axis.text.x = element_text(angle = 45, size = 12, vjust = 0.6))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



### Real models (these have the info you want)

 temperature as a continuous predictor, where 0 represents the lowest tempereature used in the experiment.
 

``` r
mean(size_dat$Temp)
```

```
## [1] 24.6
```

``` r
size_dat$TempC <- size_dat$Temp - min(size_dat$Temp)
mean(size_dat$TempC)
```

```
## [1] 6.63
```

``` r
size_dat$TempC_2 <- size_dat$Temp - mean(size_dat$Temp)
```


## About the analysis


Replicate Block is a bit hard to decide on whether to fit as fixed or random. It is obviously a nuisance variable, but with only two blocks it is not easy to estimate. I did include it as a random effect

Vial is nested within the joint levels of Treatment, TempF and Rep



## for the check of the evolved differences 
from the normal (what they evolved at) rearing temperature of 25°C



``` r
mod1_thorax__25C_log <- lmer(log2(1000*Thorax) ~ Sex*Treat +
                    + (1 | Rep) 
                    + (1| Rep:Treat:Vial),
           REML = FALSE,          
           data = size_dat,
           subset = size_dat$Temp == 25)

summary(mod1_thorax__25C_log)
```

```
## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
##   method [lmerModLmerTest]
## Formula: log2(1000 * Thorax) ~ Sex * Treat + +(1 | Rep) + (1 | Rep:Treat:Vial)
##    Data: size_dat
##  Subset: size_dat$Temp == 25
## 
##      AIC      BIC   logLik deviance df.resid 
##     -943     -888      485     -969      502 
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.778 -0.500  0.098  0.563  2.621 
## 
## Random effects:
##  Groups         Name        Variance Std.Dev.
##  Rep:Treat:Vial (Intercept) 0.000387 0.0197  
##  Rep            (Intercept) 0.002026 0.0450  
##  Residual                   0.008504 0.0922  
## Number of obs: 515, groups:  Rep:Treat:Vial, 30; Rep, 2
## 
## Fixed effects:
##                     Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)          9.81874    0.03557   2.93667  276.05  1.4e-07 ***
## SexM                -0.18198    0.01861 507.48433   -9.78  < 2e-16 ***
## TreatControl        -0.07021    0.02180  65.86799   -3.22  0.00199 ** 
## TreatReversal       -0.19551    0.02141  61.75187   -9.13  4.6e-13 ***
## TreatLarge           0.07723    0.02181  65.53077    3.54  0.00074 ***
## TreatSmall          -0.50603    0.02220  69.51166  -22.80  < 2e-16 ***
## SexM:TreatControl    0.02876    0.02605 501.28582    1.10  0.27006    
## SexM:TreatReversal   0.05441    0.02531 499.61208    2.15  0.03206 *  
## SexM:TreatLarge     -0.00665    0.02715 507.84681   -0.25  0.80649    
## SexM:TreatSmall     -0.03280    0.02604 500.49883   -1.26  0.20835    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) SexM   TrtCnt TrtRvr TrtLrg TrtSml SxM:TC SxM:TR SxM:TL
## SexM        -0.284                                                        
## TreatContrl -0.325  0.463                                                 
## TreatRevrsl -0.331  0.471  0.541                                          
## TreatLarge  -0.325  0.463  0.531  0.540                                   
## TreatSmall  -0.319  0.453  0.521  0.530  0.520                            
## SxM:TrtCntr  0.202 -0.714 -0.607 -0.336 -0.330 -0.324                     
## SxM:TrtRvrs  0.209 -0.735 -0.341 -0.608 -0.341 -0.333  0.525              
## SexM:TrtLrg  0.194 -0.683 -0.316 -0.322 -0.578 -0.312  0.489  0.502       
## SxM:TrtSmll  0.203 -0.714 -0.331 -0.336 -0.330 -0.625  0.510  0.525  0.489
```



``` r
estimates_25C <- emmeans(mod1_thorax__25C_log, ~ Treat | Sex)

estimates_25C
```

```
## Sex = F:
##  Treat    emmean     SE   df lower.CL upper.CL
##  LHm        9.82 0.0484 5.91     9.70     9.94
##  Control    9.75 0.0480 5.64     9.63     9.87
##  Reversal   9.62 0.0478 5.50     9.50     9.74
##  Large      9.90 0.0480 5.64     9.78    10.02
##  Small      9.31 0.0482 5.81     9.19     9.43
## 
## Sex = M:
##  Treat    emmean     SE   df lower.CL upper.CL
##  LHm        9.64 0.0480 5.64     9.52     9.76
##  Control    9.60 0.0482 5.81     9.48     9.71
##  Reversal   9.50 0.0480 5.59     9.38     9.62
##  Large      9.71 0.0490 6.42     9.59     9.83
##  Small      9.10 0.0481 5.66     8.98     9.22
## 
## Degrees-of-freedom method: kenward-roger 
## Results are given on the log2 (not the response) scale. 
## Confidence level used: 0.95
```

``` r
contrasts_To_control <- contrast(estimates_25C, method = "trt.vs.ctrl", ref = "Control")

contrasts_To_control
```

```
## Sex = F:
##  contrast           estimate     SE   df t.ratio p.value
##  LHm - Control         0.070 0.0230 77.5   3.050  0.0117
##  Reversal - Control   -0.125 0.0220 68.4  -5.710  <.0001
##  Large - Control       0.147 0.0223 70.4   6.600  <.0001
##  Small - Control      -0.436 0.0228 77.1 -19.150  <.0001
## 
## Sex = M:
##  contrast           estimate     SE   df t.ratio p.value
##  LHm - Control         0.041 0.0228 76.0   1.820  0.2210
##  Reversal - Control   -0.100 0.0226 76.6  -4.400  0.0001
##  Large - Control       0.112 0.0246 92.6   4.550  0.0001
##  Small - Control      -0.497 0.0228 76.4 -21.770  <.0001
## 
## Degrees-of-freedom method: kenward-roger 
## Results are given on the log2 (not the response) scale. 
## P value adjustment: dunnettx method for 4 tests
```

``` r
confint(contrasts_To_control)
```

```
## Sex = F:
##  contrast           estimate     SE   df lower.CL upper.CL
##  LHm - Control         0.070 0.0230 77.5    0.012    0.128
##  Reversal - Control   -0.125 0.0220 68.4   -0.180   -0.070
##  Large - Control       0.147 0.0223 70.4    0.091    0.204
##  Small - Control      -0.436 0.0228 77.1   -0.493   -0.379
## 
## Sex = M:
##  contrast           estimate     SE   df lower.CL upper.CL
##  LHm - Control         0.041 0.0228 76.0   -0.016    0.099
##  Reversal - Control   -0.100 0.0226 76.6   -0.156   -0.043
##  Large - Control       0.112 0.0246 92.6    0.051    0.173
##  Small - Control      -0.497 0.0228 76.4   -0.555   -0.440
## 
## Degrees-of-freedom method: kenward-roger 
## Results are given on the log2 (not the response) scale. 
## Confidence level used: 0.95 
## Conf-level adjustment: dunnettx method for 4 estimates
```

please note, to recapitulate figures from the paper, remove the `type = "response"` from the emmeans statement so it will be displayed on the log2 scale. 

first panel (p1)

``` r
p1 <- plot(contrasts_To_control) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
  xlab(expression(paste("∆thorax length, ", log[2], " µm"))) + 
  ylab("") +
  theme_bw() +
  theme(text = element_text(size = 15), 
        legend.position = "none",
        legend.title = element_blank())
  

p1
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


using log2 µm, (which will allow us to focus on proportional changes.)


``` r
mod1_thorax_log <- lmer(log2(1000*Thorax) ~ Sex*Treat*TempC +
                    + (1 | Rep) 
                    + (1 | Rep:Treat:TempF:Vial),
                    REML = FALSE,  
                    data = size_dat )

summary(mod1_thorax_log)
```

```
## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
##   method [lmerModLmerTest]
## Formula: log2(1000 * Thorax) ~ Sex * Treat * TempC + +(1 | Rep) + (1 |  
##     Rep:Treat:TempF:Vial)
##    Data: size_dat
## 
##      AIC      BIC   logLik deviance df.resid 
##    -3266    -3142     1656    -3312     1618 
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -5.446 -0.487  0.061  0.594  3.489 
## 
## Random effects:
##  Groups               Name        Variance Std.Dev.
##  Rep:Treat:TempF:Vial (Intercept) 0.00140  0.0374  
##  Rep                  (Intercept) 0.00189  0.0435  
##  Residual                         0.00713  0.0844  
## Number of obs: 1641, groups:  Rep:Treat:TempF:Vial, 90; Rep, 2
## 
## Fixed effects:
##                           Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)               9.94e+00   3.54e-02  3.36e+00  281.06  1.9e-08 ***
## SexM                     -2.07e-01   1.45e-02  1.56e+03  -14.30  < 2e-16 ***
## TreatControl             -7.70e-02   2.48e-02  1.26e+02   -3.11   0.0023 ** 
## TreatReversal            -1.87e-01   2.50e-02  1.30e+02   -7.46  1.1e-11 ***
## TreatLarge                5.83e-02   2.47e-02  1.24e+02    2.36   0.0198 *  
## TreatSmall               -5.44e-01   2.48e-02  1.25e+02  -21.95  < 2e-16 ***
## TempC                    -1.65e-02   2.04e-03  1.23e+02   -8.07  5.5e-13 ***
## SexM:TreatControl         5.59e-02   2.05e-02  1.56e+03    2.72   0.0065 ** 
## SexM:TreatReversal        1.08e-01   2.08e-02  1.57e+03    5.21  2.2e-07 ***
## SexM:TreatLarge           2.40e-02   2.06e-02  1.56e+03    1.16   0.2444    
## SexM:TreatSmall          -2.80e-02   2.16e-02  1.57e+03   -1.30   0.1951    
## SexM:TempC                4.48e-03   1.72e-03  1.57e+03    2.61   0.0093 ** 
## TreatControl:TempC        5.31e-03   2.89e-03  1.23e+02    1.83   0.0691 .  
## TreatReversal:TempC       3.04e-03   2.92e-03  1.27e+02    1.04   0.2998    
## TreatLarge:TempC          7.39e-04   2.89e-03  1.23e+02    0.26   0.7988    
## TreatSmall:TempC          2.91e-03   2.91e-03  1.25e+02    1.00   0.3193    
## SexM:TreatControl:TempC  -7.93e-03   2.41e-03  1.56e+03   -3.29   0.0010 ** 
## SexM:TreatReversal:TempC -7.42e-03   2.44e-03  1.57e+03   -3.04   0.0024 ** 
## SexM:TreatLarge:TempC    -2.00e-03   2.42e-03  1.56e+03   -0.83   0.4091    
## SexM:TreatSmall:TempC     9.19e-04   2.56e-03  1.57e+03    0.36   0.7199    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 20 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

``` r
Anova(mod1_thorax_log)
```

```
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: log2(1000 * Thorax)
##                   Chisq Df Pr(>Chisq)    
## Sex             1558.70  1    < 2e-16 ***
## Treat           2225.63  4    < 2e-16 ***
## TempC            264.34  1    < 2e-16 ***
## Sex:Treat         40.06  4    4.2e-08 ***
## Sex:TempC          1.70  1    0.19195    
## Treat:TempC        2.65  4    0.61830    
## Sex:Treat:TempC   22.38  4    0.00017 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



Generate model fitted values and their CIs

``` r
pred_vals_slopes <- as.data.frame(predictorEffect("TempC", mod1_thorax_log, 
                                                  x.levels = 13))

pred_vals_slopes$Temp <- pred_vals_slopes$TempC + 18

pred_vals_slopes$log2_thorax <- pred_vals_slopes$fit
```


### panel b


``` r
pred_vals_slopes$Treat <- relevel(pred_vals_slopes$Treat, "LHm")
```



``` r
p2 <- ggplot(data = pred_vals_slopes, aes(x = Temp, y = log2_thorax, colour = Sex)) +
  facet_wrap(~Treat, nrow = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_vals_slopes$lower, 
                  ymax = pred_vals_slopes$upper,
                  x = Temp,
                  colour = Sex), alpha = 0.175, 
               linetype = 0) +
  geom_jitter(data = size_dat,
              aes(y = log2_thorax, x = Temp), alpha = 0.2, width = 1) +
  ylab(expression(paste("Thorax length, ", log[2], " µm"))) + 
  xlab("Rearing temperature (°C)") + 
  theme_bw() +
  theme(text = element_text(size = 15), 
        legend.position = "top",
        legend.title = element_blank())


p2
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-24-1.png)<!-- -->



## Changes in slopes


### What is the magnitude of change in the slope of the plasticity response, by experimental treatment.

Note that in the paper, the figures show the contrast computed on the log2 scale. 


``` r
slopes2 <- emtrends(mod1_thorax_log,  var = "TempC",  
                            specs = ~ Treat | Sex)


plot(slopes2) + 
  labs(x = "Plasticity", y = "") +
  theme_bw() +
  theme(text = element_text(size = 18))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

``` r
# relative to LHm
slope_contrasts_LHm <- contrast(slopes2, 
                             "trt.vs.ctrl", ref = "LHm",
                                 by = "Sex")

#excluding LHm, relative to controls
slope_contrasts_control <- contrast(slopes2, 
                             "trt.vs.ctrl", ref = "Control", exclude = 1, 
                                 by = "Sex")

confint(slope_contrasts_control)
```

```
## Sex = F:
##  contrast           estimate      SE  df lower.CL upper.CL
##  Reversal - Control -0.00227 0.00306 142 -0.00957  0.00504
##  Large - Control    -0.00457 0.00303 138 -0.01181  0.00267
##  Small - Control    -0.00240 0.00305 139 -0.00968  0.00488
## 
## Sex = M:
##  contrast           estimate      SE  df lower.CL upper.CL
##  Reversal - Control -0.00176 0.00302 135 -0.00897  0.00546
##  Large - Control     0.00137 0.00303 138 -0.00587  0.00861
##  Small - Control     0.00645 0.00314 156 -0.00102  0.01393
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## Conf-level adjustment: dunnettx method for 3 estimates
```



``` r
p3 <- plot(slope_contrasts_control) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "∆plasticity relative to control", y = "") +
  scale_x_continuous(n.breaks = 4) + 
  theme_bw() +
  theme(text = element_text(size = 15), 
        legend.position = "none",
        legend.title = element_blank())

p3
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-26-1.png)<!-- -->



``` r
plot(slope_contrasts_LHm) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "∆plasticity relative to LHm", y = "") +
  theme_bw() +
  theme(text = element_text(size = 15))
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

Just to help understand what the output is telling us

``` r
mod1_thorax <- lmer(1000*Thorax ~ Sex*Treat*TempC +
                    + (1 | Rep) 
                    + (1| Rep:Treat:TempF:Vial),
                    REML = FALSE,
                    data = size_dat )


slopes2_b <- emtrends(mod1_thorax,  var = "TempC",  
                            specs = ~ Treat | Sex)

slopes2_b 
```

```
## Sex = F:
##  Treat    TempC.trend   SE  df lower.CL upper.CL
##  LHm           -10.32 1.21 131   -12.71    -7.93
##  Control        -6.94 1.21 133    -9.34    -4.55
##  Reversal       -7.57 1.23 140    -9.99    -5.14
##  Large         -10.21 1.21 132   -12.60    -7.82
##  Small          -5.94 1.22 134    -8.35    -3.53
## 
## Sex = M:
##  Treat    TempC.trend   SE  df lower.CL upper.CL
##  LHm            -6.67 1.22 138    -9.09    -4.26
##  Control        -7.94 1.21 130   -10.33    -5.56
##  Reversal       -8.60 1.21 130   -10.98    -6.21
##  Large          -7.79 1.21 134   -10.19    -5.39
##  Small          -3.16 1.29 167    -5.70    -0.62
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95
```

``` r
slope_contrasts_control_b <- contrast(slopes2_b,
                             "trt.vs.ctrl", ref = "Control", exclude = 1,
                                 by = "Sex")

confint(slope_contrasts_control_b)
```

```
## Sex = F:
##  contrast           estimate   SE  df lower.CL upper.CL
##  Reversal - Control    -0.62 1.72 136    -4.74     3.49
##  Large - Control       -3.27 1.71 132    -7.35     0.82
##  Small - Control        1.00 1.72 134    -3.10     5.11
## 
## Sex = M:
##  contrast           estimate   SE  df lower.CL upper.CL
##  Reversal - Control    -0.65 1.70 130    -4.72     3.42
##  Large - Control        0.16 1.71 132    -3.93     4.24
##  Small - Control        4.78 1.76 148     0.58     8.99
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## Conf-level adjustment: dunnettx method for 3 estimates
```


## stitching together for the plot


``` r
top_row <- plot_grid(p1, p3, 
          label_size = 20,        
          labels = c("A", "C"))

fig_paper <- plot_grid(top_row, p2,
          labels = c("", "B"),
          label_size = 20,
          ncol = 1)

fig_paper
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

``` r
save_plot("../outputs/fig_paper_file.png", fig_paper, base_height = 6.72, base_asp = 5/3, dpi = 1000)

save_plot("../outputs/fig_paper_file.tiff", fig_paper, base_height = 6.72, base_asp = 5/3, dpi = 500 )
```

## Note below, this is for ID's personal interest in changes in SSD, not about the paper!

The analysis below is because I was curious... Not about the paper *per se*
### compute differences in SSD for slopes slope relative to control


``` r
slope_treatment <- emtrends(mod1_thorax_log,  var = "TempC",  
                            pairwise ~ Sex*Treat)

SSD_for_slope_contrasts <- contrast(slope_treatment[[1]], 
                                 interaction = c(Treat = "trt.vs.ctrl1", Sex = "pairwise"),
                                 by = NULL)



confint(SSD_for_slope_contrasts)
```

```
##  Treat_trt.vs.ctrl1 Sex_pairwise estimate      SE   df lower.CL upper.CL
##  Control - LHm      F - M         0.00793 0.00242 1570  0.00319  0.01267
##  Reversal - LHm     F - M         0.00742 0.00245 1579  0.00262  0.01222
##  Large - LHm        F - M         0.00200 0.00243 1570 -0.00276  0.00676
##  Small - LHm        F - M        -0.00092 0.00257 1579 -0.00596  0.00412
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95
```

``` r
plot(SSD_for_slope_contrasts) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
  labs(x = "change in dimorphism plasticity relative to control lineages", y = "comparison") +
  theme_bw()
```

![](ID_AS_ThoraxPlasticity_Analysis_files/figure-html/unnamed-chunk-30-1.png)<!-- -->



``` r
sessionInfo()
```

```
## R version 4.4.1 (2024-06-14)
## Platform: aarch64-apple-darwin20
## Running under: macOS 15.1.1
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: America/Toronto
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] cowplot_1.1.3      ggridges_0.5.6     ggdist_3.3.2       ggbeeswarm_0.7.2  
##  [5] predictmeans_1.1.1 nlme_3.1-166       effects_4.2-2      lmerTest_3.1-3    
##  [9] glmmTMB_1.1.10     lme4_1.1-35.5      Matrix_1.7-1       ggplot2_3.5.1     
## [13] emmeans_1.10.5     car_3.1-3          carData_3.0-5     
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.2.1     viridisLite_0.4.2    farver_2.1.2        
##  [4] vipor_0.4.7          dplyr_1.1.4          fastmap_1.2.0       
##  [7] lazyeval_0.2.2       digest_0.6.37        estimability_1.5.1  
## [10] lifecycle_1.0.4      survival_3.7-0       magrittr_2.0.3      
## [13] compiler_4.4.1       rlang_1.1.4          sass_0.4.9          
## [16] tools_4.4.1          utf8_1.2.4           yaml_2.3.10         
## [19] data.table_1.16.2    knitr_1.49           labeling_0.4.3      
## [22] htmlwidgets_1.6.4    plyr_1.8.9           abind_1.4-8         
## [25] KernSmooth_2.23-24   withr_3.0.2          purrr_1.0.2         
## [28] numDeriv_2016.8-1.1  nnet_7.3-19          grid_4.4.1          
## [31] fansi_1.0.6          xtable_1.8-4         colorspace_2.1-1    
## [34] scales_1.3.0         MASS_7.3-61          insight_0.20.5      
## [37] cli_3.6.3            mvtnorm_1.3-2        survey_4.4-2        
## [40] rmarkdown_2.29       ragg_1.3.3           reformulas_0.4.0    
## [43] generics_0.1.3       rstudioapi_0.17.1    httr_1.4.7          
## [46] lmeSplines_1.1-12    minqa_1.2.8          DBI_1.2.3           
## [49] cachem_1.1.0         lmeInfo_0.3.2        splines_4.4.1       
## [52] parallel_4.4.1       HRW_1.0-5            mitools_2.4         
## [55] vctrs_0.6.5          boot_1.3-31          jsonlite_1.8.9      
## [58] pbkrtest_0.5.3       beeswarm_0.4.0       Formula_1.2-5       
## [61] systemfonts_1.1.0    plotly_4.10.4        jquerylib_0.1.4     
## [64] tidyr_1.3.1          glue_1.8.0           nloptr_2.1.1        
## [67] distributional_0.5.0 gtable_0.3.6         munsell_0.5.1       
## [70] tibble_3.2.1         splines2_0.5.3       pillar_1.9.0        
## [73] htmltools_0.5.8.1    R6_2.5.1             TMB_1.9.15          
## [76] textshaping_0.4.0    Rdpack_2.6.2         evaluate_1.0.1      
## [79] lattice_0.22-6       rbibutils_2.3        backports_1.5.0     
## [82] broom_1.0.7          bslib_0.8.0          Rcpp_1.0.13-1       
## [85] coda_0.19-4.1        mgcv_1.9-1           xfun_0.49           
## [88] pkgconfig_2.0.3
```

