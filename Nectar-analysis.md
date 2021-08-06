Analyses for honest signals MS
================

## Analyses

Question 1: Is flower size variation consistent between the field and
the greenhouse?

*Approach*: Comparing population means between the field and the
greenhouse. Are we trying to compare each individual population field to
greenhouse (e.g. two sample t-tests), or a trend across the populations
field vs. greenhouse (e.g. regression of population means between field
and greenhouse)?

Question 2: Are size and scent honest signals for nectar rewards?

*Approach*: Correlation between size and rewards, and between scent and
rewards. First do a regression of all plants, with plant nested within
population as a random effect. Then do regressions for each population.

Question 3: Are populations pollen limited? \[Q3\]

*Approach*: Paired t-tests comparing controlled vs. open pollinated seed
set in each population.

### Packages and start up code:

``` r
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(knitr)
library(lmerTest)
library(emmeans)
library(ggpubr)
library(plotrix)
```

### Q1: Is there population differentiation in size in the field and the greenhouse?

*Greenhouse* Loading in the data, filtering out the NAs, and selecting
the populations with 8 or more plants

``` r
nectar <- read_excel("nectar protocol.xlsx")

nectar_work <- nectar %>% filter(!is.na(Population)) %>%  filter(!is.na(nectar_μl)) %>% filter(!is.na(Corola_area_mm2))

nectar_work8 <- nectar_work %>% group_by(Population) %>% filter (length(unique(Plant_id))>=8)
```

Analyzing population differences

``` r
size_mod<- lmer(Corola_area_mm2~Population+(1|Plant_id), data = nectar_work8)

anova(size_mod)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##            Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## Population 250372  8941.9    28 291.19   37.99 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#emmeans(size_mod, pairwise~Population)
emms<-emmeans(size_mod, "Population")
#pwpm(emms)
plot(emms, comparisons = TRUE)
```

![](Nectar-analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

*Field*

``` r
field <- read_csv("field_data.csv")
field_work <- field %>% filter(!is.na(FlowerSize))

field_work %>% group_by(Population) %>% summarize(N=length(Id))
```

    ## # A tibble: 12 x 2
    ##    Population     N
    ##    <chr>      <int>
    ##  1 G1            30
    ##  2 G3            29
    ##  3 G6            30
    ##  4 G7            27
    ##  5 G8            28
    ##  6 G9            18
    ##  7 I10           23
    ##  8 I15           30
    ##  9 I16           28
    ## 10 I17           28
    ## 11 I4            30
    ## 12 I7            21

``` r
size_mod<- lm(FlowerSize~Population, data = field_work)

anova(size_mod)
```

    ## Analysis of Variance Table
    ## 
    ## Response: FlowerSize
    ##             Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## Population  11  797.59  72.508  13.761 < 2.2e-16 ***
    ## Residuals  310 1633.42   5.269                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
emms<-emmeans(size_mod, pairwise~Population)
plot(emms, comparisons = TRUE)
```

![](Nectar-analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

*comparing the field and the greenhouse*

``` r
Pops <- c("G3", "I10", "I15", "I16", "I17", "I4","It10", "It15", "It16", "It17", "It4")

field_combine <- field_work %>% filter(Population %in% Pops) %>%  group_by(Population) %>% summarize(FlSizeMean = mean(FlowerSize), FlSizeSE=std.error(FlowerSize)) %>% mutate(Source="Field")

gh_combine <- nectar_work8 %>% filter(Population %in% Pops) %>%  group_by(Population) %>% summarize(FlSizeMean = mean(Corola_area_mm2), FlSizeSE=std.error(Corola_area_mm2)) %>% mutate(Source="Greenhouse")

combine <- rbind(field_combine, gh_combine)
combine[8:12,1]<-c("I10", "I15", "I16", "I17", "I4")

combine_wide <- combine %>% pivot_wider(names_from=Source, values_from=c(FlSizeMean,FlSizeSE))

combine_mod <- lm(FlSizeMean_Greenhouse~FlSizeMean_Field, data=combine_wide)

ggplot(aes(x=FlSizeMean_Field, y=FlSizeMean_Greenhouse), data=combine_wide)+geom_point()+geom_errorbar(aes(ymin=FlSizeMean_Greenhouse-FlSizeSE_Greenhouse, ymax=FlSizeMean_Greenhouse+FlSizeSE_Greenhouse))+geom_errorbarh(aes(xmin=FlSizeMean_Field-FlSizeSE_Field, xmax=FlSizeMean_Field+FlSizeSE_Field))+xlab("Flower size in natural populations")+ylab("Flower size in greenhouse")+theme_classic()
```

![](Nectar-analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Q2: Are size and scent honest signals for nectar rewards?

#### scent work

Need to make an object with average nectar values per plant for those
populations with sufficient plants. Then, we can merge these data with
the scent data.

``` r
nectar <- read_excel("nectar protocol.xlsx")

nectar_work <- nectar %>% filter(!is.na(Population)) %>%  filter(!is.na(nectar_μl)) %>% filter(!is.na(Corola_area_mm2))

nectar_work8 <- nectar_work %>% group_by(Population) %>% filter (length(unique(Plant_id))>=8)

nectar_8_avg <- nectar_work8 %>% group_by(Population, Plant_id) %>% summarize(Mean_nectar=mean(nectar_μl))

#add scent into this df

#nectar_model<-lmer(nectar_μl~scent+(1|Population), data=nectar_8_avg)

#par(mfrow=c(1,2))
#hist(resid(nectar_model))
#plot(predict(nectar_model),resid(nectar_model)) ; abline(h=0)
```

``` r
#ggplot(aes(x=scent, y=nectar_μl), #data=nectar_8_avg)+geom_point(aes(color=Population))+geom_smooth(method="lm",color="black")+theme_classic()+xlab(expression(paste("Total scent emission (ng", h^-1, flower^-1, ")"))) + ylab(expression(paste("Nectar volume (", mu,"l)")))
```

#### nectar work

``` r
nectar <- read_excel("nectar protocol.xlsx")
```

#### Summarizing and subsetting the dataset

*summarizing the NAs*

``` r
#kable(nectar %>% filter(is.na(Population)) %>% group_by(Plant_id) %>% summarise(N=length(unique(flower_id))))
```

*removing flowers where we don’t have the population and we don’t have
nectar and/or flower size*

``` r
nectar_work <- nectar %>% filter(!is.na(Population)) %>%  filter(!is.na(nectar_μl)) %>% filter(!is.na(Corola_area_mm2))
```

*summarizing how many flowers and plants we have data for*

``` r
nectar_work_sum <- nectar_work %>% group_by(Population) %>%  summarise(N_plants=length(unique(Plant_id)), N_flowers=length(flower_id))
kable(nectar_work_sum)
```

| Population | N\_plants | N\_flowers |
|:-----------|----------:|-----------:|
| Aal04      |         8 |         18 |
| Aal12      |        11 |         31 |
| Aal29      |        13 |         42 |
| Aal34c     |        15 |         43 |
| Aal36      |         8 |         23 |
| AalDMA     |         1 |          3 |
| AalDMB     |         8 |         23 |
| AalFC      |        11 |         35 |
| AalPB      |         5 |         15 |
| AalSFH     |         8 |         22 |
| E3         |        11 |         33 |
| Fr1        |        13 |         39 |
| Fr2        |        11 |         29 |
| Fr3        |        12 |         33 |
| Fr4        |        11 |         32 |
| G1         |         3 |          9 |
| G3         |        16 |         50 |
| G4         |        33 |         92 |
| G5         |        13 |         36 |
| G6         |         4 |         12 |
| G7         |         3 |          8 |
| G8         |         1 |          3 |
| G9         |         2 |          4 |
| It10       |        13 |         38 |
| It13       |         9 |         27 |
| It15       |        11 |         33 |
| It16       |        11 |         27 |
| It17       |        10 |         30 |
| It18       |         9 |         22 |
| It2        |        12 |         35 |
| It4        |         9 |         20 |
| It5        |        10 |         26 |
| It6        |        11 |         32 |
| It8        |        10 |         27 |
| It9        |        10 |         27 |
| S1         |        15 |         33 |

*making a list of populations with 8 or more plants sampled*

``` r
nectar_list <- nectar_work_sum %>% filter(N_plants >= 8)
```

*filtering the dataset to only contain those populations*

``` r
nectar_work8 <- nectar_work %>% group_by(Population) %>% filter (length(unique(Plant_id))>=8)
```

### Analysis: nectar vs. size, all populations

``` r
nectar_model<-lmer(nectar_μl~Corola_area_mm2+(1|Population/Plant_id), data=nectar_work8)

par(mfrow=c(1,2))
hist(resid(nectar_model))
plot(predict(nectar_model),resid(nectar_model)) ; abline(h=0)
```

![](Nectar-analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

The residuals get a little better with a square-root transformation, but
it’s not a huge difference.

``` r
ggplot(aes(x=Corola_area_mm2, y=nectar_μl), data=nectar_work8)+geom_point(aes(color=Population))+geom_smooth(method="lm",color="black")+theme_classic()+xlab(expression(paste("Corolla area (", mm^2 ,")"))) + ylab(expression(paste("Nectar volume (", mu,"l)")))
```

![](Nectar-analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

I think this works for now, but if we use all of this data, it’s
probably not effective to show this many populations in colors. Maybe we
don’t need to color the points by population here if we do a population
by population analysis. Another option could be coloring the points by
mating system, or region.

## Analysis: nectar vs. size, population by population

``` r
f <- function(df) summary(lmer(nectar_μl ~ Corola_area_mm2 +(1|Plant_id), data = df))

v <- lapply(split(nectar_work8, nectar_work8$Population), f)

Pop=sort(as.factor(c(unique(nectar_work8$Population),unique(nectar_work8$Population))))

results<-data.frame("Estimate"=numeric(), "Std. Error"=numeric(), "df"=numeric(), "t value"=numeric(), "Pr(>|t|)"=numeric())


for (i in 1:29){
  z<-as.data.frame(v[[i]]$coefficients)
  results<-rbind(results,z)
}

results<-cbind(results, Pop)


results<-results %>% tibble::rownames_to_column("X") 
results <- results %>% filter(str_detect(X, "Corola"))  %>% mutate(Sig=if_else(`Pr(>|t|)`< 0.05, "Yes", "No")) %>% select(-X) %>% relocate(Pop) 
results.sig<-results %>% filter(Sig=="Yes")
kable(results)
```

| Pop    |   Estimate | Std. Error |        df |    t value | Pr(&gt;\|t\|) | Sig |
|:-------|-----------:|-----------:|----------:|-----------:|--------------:|:----|
| Aal04  |  0.0013040 |  0.0006967 | 15.881577 |  1.8717772 |     0.0797726 | No  |
| Aal12  |  0.0011256 |  0.0005457 | 28.989376 |  2.0626750 |     0.0482040 | Yes |
| Aal29  |  0.0014136 |  0.0002887 | 27.294673 |  4.8964210 |     0.0000392 | Yes |
| Aal34c |  0.0010282 |  0.0004307 | 35.858561 |  2.3871995 |     0.0223738 | Yes |
| Aal36  | -0.0001285 |  0.0004723 |  6.708009 | -0.2720635 |     0.7937559 | No  |
| AalDMB |  0.0021986 |  0.0006927 | 14.835149 |  3.1739284 |     0.0063630 | Yes |
| AalFC  |  0.0008291 |  0.0005083 | 32.523891 |  1.6311194 |     0.1125124 | No  |
| AalSFH |  0.0007254 |  0.0004025 | 12.128386 |  1.8024451 |     0.0963635 | No  |
| E3     |  0.0012568 |  0.0003398 | 30.475240 |  3.6982758 |     0.0008540 | Yes |
| Fr1    |  0.0014803 |  0.0006345 | 23.956013 |  2.3332194 |     0.0283610 | Yes |
| Fr2    |  0.0016901 |  0.0005446 | 23.384906 |  3.1035910 |     0.0049416 | Yes |
| Fr3    |  0.0006278 |  0.0002663 | 31.000000 |  2.3577303 |     0.0248775 | Yes |
| Fr4    |  0.0012537 |  0.0003083 | 19.562775 |  4.0660135 |     0.0006263 | Yes |
| G3     |  0.0006918 |  0.0001763 | 41.112670 |  3.9243248 |     0.0003240 | Yes |
| G4     |  0.0010025 |  0.0002107 | 89.308662 |  4.7578305 |     0.0000075 | Yes |
| G5     |  0.0000516 |  0.0005600 | 31.945869 |  0.0921136 |     0.9271830 | No  |
| It10   |  0.0014202 |  0.0005085 | 30.629420 |  2.7930009 |     0.0089190 | Yes |
| It13   |  0.0007681 |  0.0005410 | 23.747470 |  1.4197427 |     0.1686777 | No  |
| It15   |  0.0014179 |  0.0006498 | 29.592631 |  2.1819766 |     0.0371899 | Yes |
| It16   |  0.0015897 |  0.0005504 | 24.999998 |  2.8883078 |     0.0078836 | Yes |
| It17   |  0.0024936 |  0.0008308 | 28.000000 |  3.0012543 |     0.0056000 | Yes |
| It18   |  0.0013445 |  0.0002853 | 18.068962 |  4.7130481 |     0.0001718 | Yes |
| It2    |  0.0017286 |  0.0006156 | 30.465859 |  2.8078521 |     0.0086212 | Yes |
| It4    |  0.0008047 |  0.0003032 | 18.000000 |  2.6545406 |     0.0161333 | Yes |
| It5    | -0.0000534 |  0.0008654 | 23.818748 | -0.0617506 |     0.9512767 | No  |
| It6    |  0.0013939 |  0.0003363 | 16.792595 |  4.1454584 |     0.0006929 | Yes |
| It8    |  0.0018435 |  0.0003973 | 24.616943 |  4.6404598 |     0.0000979 | Yes |
| It9    |  0.0018697 |  0.0003466 | 25.000000 |  5.3942611 |     0.0000135 | Yes |
| S1     |  0.0014483 |  0.0004314 | 17.196609 |  3.3567465 |     0.0036941 | Yes |

``` r
lines<-nectar_work8 %>% filter(Population %in% results.sig$Pop)

ggplot(aes(x=Corola_area_mm2, y=nectar_μl, color=Population), data=nectar_work8)+geom_point()+
  facet_wrap(~Population, scales="free")+theme_classic()+xlab(expression(paste("Corolla area (", mm^2 ,")"))) + ylab(expression(paste("Nectar volume (", mu,"l)")))+geom_smooth(data=lines, method="lm", color="black")
```

![](Nectar-analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## If we want to only use the MSC data

``` r
nectar_msc <- nectar_work %>% filter(Population=="G3"|Population=="G4"|Population=="G5"|
                                       Population=="It10"|Population=="It13"|Population=="It15"|
                                       Population=="It16"|Population=="It17"|Population=="It18"|
                                       Population=="It2"|Population=="It4"|Population=="It5"|
                                       Population=="It6"|Population=="It8"|Population=="It9")

nectar_model<-lmer(nectar_μl~Corola_area_mm2+(1|Population/Plant_id), data=nectar_msc)
ggplot(aes(x=Corola_area_mm2, y=nectar_μl), data=nectar_msc)+geom_point(aes(x=Corola_area_mm2, y=nectar_μl, color=Population))+geom_smooth(method="lm")+theme_classic()+xlab(expression(paste("Corolla area (", mm^2 ,")"))) + ylab(expression(paste("Nectar volume (", mu,"l)")))
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Nectar-analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Q3: Are populations pollen-limited?

``` r
seeds <- read_csv("seed_data.csv")

seeds_work <- seeds %>% filter(Treatment == "hp" | Treatment == "c")

#looking at number of plants that had zeros in the hp fruits, to see if we really need to drop plants 
zees<-seeds_work %>% group_by(Population, Plant_ID) %>% filter(Treatment== "hp") %>% summarize(zeros=sum(Seeds_final==0), nonzeroes=sum(Seeds_final!=0))

zees2<-seeds_work %>% group_by(Population, Plant_ID, Treatment) %>%  summarize(zeros=sum(Seeds_final==0), nonzeroes=sum(Seeds_final!=0)) %>% pivot_wider(names_from=Treatment, values_from=c(zeros, nonzeroes))


#for now, we will only analyze plants where there were fewer than 2 zero-seed hand-pollinated fruits, following Sotiria's criteria
keep_list <- zees2 %>% filter(zeros_hp <2) %>% mutate(ID=paste(Population, Plant_ID))

seeds_work2<- seeds_work %>% mutate(ID=paste(Population, Plant_ID)) %>% filter(ID %in% keep_list$ID) %>% mutate(Treatment=factor(Treatment, levels=c("hp", "c")))

seeds_work2 %>% group_by(Population) %>% summarize(N=length(unique(Plant_ID)))
```

    ## # A tibble: 12 x 2
    ##    Population     N
    ##    <chr>      <int>
    ##  1 G1            22
    ##  2 G3            14
    ##  3 G6            28
    ##  4 G7            17
    ##  5 G8            23
    ##  6 G9            15
    ##  7 I10           16
    ##  8 I15           22
    ##  9 I16           24
    ## 10 I17           21
    ## 11 I4            21
    ## 12 I7             8

Run an lmer for each population

``` r
f <- function(df) as.data.frame(emmeans(lmer(Seeds_final ~ Treatment +(1|Plant_ID), data = df), "Treatment"))

v <- lapply(split(seeds_work2, seeds_work2$Population), f)


f2 <- function(df) as.data.frame(pairs(emmeans(lmer(Seeds_final ~ Treatment +(1|Plant_ID), data = df), "Treatment")))

v3 <- lapply(split(seeds_work2, seeds_work2$Population), f2)

v4 <-do.call(rbind.data.frame, v3)
v4$Pop<-rownames(v4)
rownames(v4)<-c()
v4$sig<-ifelse(v4$p.value<0.05, "*", "")

v2<-do.call(rbind.data.frame, v)

Pop=sort(as.factor(c(unique(seeds_work2$Population),unique(seeds_work2$Population))))


v2<-data.frame(v2, Pop, row.names = c())

kable(v2)
```

| Treatment |    emmean |       SE |       df |  lower.CL |  upper.CL | Pop |
|:----------|----------:|---------:|---------:|----------:|----------:|:----|
| hp        | 14.942885 | 1.818878 | 31.04051 | 11.233455 | 18.652314 | G1  |
| c         | 12.582700 | 1.808288 | 30.41375 |  8.891789 | 16.273612 | G1  |
| hp        | 15.363696 | 1.831281 | 22.06828 | 11.566534 | 19.160858 | G3  |
| c         | 11.807043 | 1.831281 | 22.06828 |  8.009881 | 15.604205 | G3  |
| hp        | 22.506316 | 1.454066 | 40.41734 | 19.568485 | 25.444147 | G6  |
| c         | 15.370490 | 1.467233 | 41.81546 | 12.409107 | 18.331873 | G6  |
| hp        | 16.800679 | 2.269341 | 19.96026 | 12.066313 | 21.535045 | G7  |
| c         | 13.782001 | 2.268339 | 19.90233 |  9.048841 | 18.515162 | G7  |
| hp        | 11.975090 | 1.612601 | 44.60462 |  8.726350 | 15.223829 | G8  |
| c         |  7.993084 | 1.609851 | 44.34126 |  4.749348 | 11.236820 | G8  |
| hp        | 16.158607 | 1.627895 | 29.92189 | 12.833639 | 19.483575 | G9  |
| c         |  7.312217 | 1.646973 | 31.17245 |  3.953947 | 10.670487 | G9  |
| hp        | 12.723741 | 1.398571 | 21.94362 |  9.822850 | 15.624631 | I10 |
| c         |  8.401876 | 1.437773 | 23.83682 |  5.433383 | 11.370368 | I10 |
| hp        | 18.433270 | 1.747025 | 32.84910 | 14.878302 | 21.988239 | I15 |
| c         | 18.153988 | 1.769396 | 33.95414 | 14.557964 | 21.750013 | I15 |
| hp        | 14.975390 | 1.556236 | 32.30146 | 11.806601 | 18.144179 | I16 |
| c         | 12.946049 | 1.560226 | 32.68158 |  9.770569 | 16.121529 | I16 |
| hp        | 21.970017 | 1.645420 | 35.48333 | 18.631263 | 25.308770 | I17 |
| c         | 19.830410 | 1.628076 | 34.15938 | 16.522329 | 23.138491 | I17 |
| hp        | 17.725440 | 1.877355 | 40.16199 | 13.931640 | 21.519239 | I4  |
| c         | 10.938649 | 1.760266 | 34.11451 |  7.361800 | 14.515499 | I4  |
| hp        |  6.547457 | 1.569241 | 17.31530 |  3.241235 |  9.853679 | I7  |
| c         |  8.059441 | 1.628184 | 21.73454 |  4.680401 | 11.438481 | I7  |

``` r
kable(v4)
```

| contrast |   estimate |       SE |        df |    t.ratio |   p.value | Pop | sig |
|:---------|-----------:|---------:|----------:|-----------:|----------:|:----|:----|
| hp - c   |  2.3601842 | 1.535166 | 139.53757 |  1.5374132 | 0.1264567 | G1  |     |
| hp - c   |  3.5566529 | 1.811759 |  94.53170 |  1.9630934 | 0.0525744 | G3  |     |
| hp - c   |  7.1358258 | 1.289801 | 200.74609 |  5.5325007 | 0.0000001 | G6  | \*  |
| hp - c   |  3.0186775 | 1.489123 | 115.18440 |  2.0271511 | 0.0449557 | G7  | \*  |
| hp - c   |  3.9820058 | 1.905295 | 101.62443 |  2.0899680 | 0.0391169 | G8  | \*  |
| hp - c   |  8.8463901 | 1.912805 | 104.63099 |  4.6248253 | 0.0000108 | G9  | \*  |
| hp - c   |  4.3218650 | 1.281442 |  95.53222 |  3.3726569 | 0.0010763 | I10 | \*  |
| hp - c   |  0.2792821 | 1.650001 | 145.67393 |  0.1692618 | 0.8658256 | I15 |     |
| hp - c   |  2.0293409 | 1.262732 | 153.94157 |  1.6071036 | 0.1100808 | I16 |     |
| hp - c   |  2.1396069 | 1.629993 | 178.81598 |  1.3126482 | 0.1909838 | I17 |     |
| hp - c   |  6.7867904 | 1.976656 | 105.58413 |  3.4334702 | 0.0008524 | I4  | \*  |
| hp - c   | -1.5119842 | 2.223285 |  35.59059 | -0.6800675 | 0.5008617 | I7  |     |

``` r
ggplot(aes(x=Treatment, y=emmean), data=v2) + facet_wrap(~Pop) +geom_point()+geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2) +ylab("Estimated marginal mean") + theme_classic()+geom_text(aes(x=1.5, y=20, label=sig),data=v4, size=6 )
```

![](Nectar-analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
