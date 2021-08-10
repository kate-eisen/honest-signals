Seed estimates and pollen limitation
================

## Purpose

In this document, I create the regressions to estimate seed set in the
fruits that were open in Italy. I then use these seed set estimates,
along with the seed counts, to calculate the extent of pollen limitation
in all populations.

``` r
library(tidyverse)
library(readxl)
library(knitr)
library(lme4)
library(lmerTest)
library(knitr)
library(emmeans)
library(ggpubr)
library(plotrix)
library(magicfor)
```

Loading in the data and filtering out rows with missing values. Created
two objects, one with the Italian populations and one with the Greek
populations.

``` r
seeds_E <- read_excel("Seed_estimate.xlsx")


seeds_E <- seeds_E %>% filter(!is.na(Missing_seeds)) %>% filter(!is.na(Treatment))

seeds_I <- seeds_E %>% filter(!str_detect(Population,"^G")) 
seeds_G <- seeds_E %>% filter(str_detect(Population,"^G")) 
```

The number of non HP and C fruits used in each population for the
regressions

``` r
kable(seeds_I %>% filter(Treatment != "c" & Treatment != "hp" & fruit_length >= 5) %>% group_by(Population, Treatment) %>% summarize(N=length(Missing_seeds)))
```

| Population | Treatment |   N |
|:-----------|:----------|----:|
| I10        | e         |  46 |
| I10        | n         |  65 |
| I15        | n         |  76 |
| I16        | n         |  82 |
| I17        | n         | 111 |
| I4         | n         |  76 |
| I7         | n         |  34 |

The percentages of fruits (from the control or hp treatments) that were
missing seeds

``` r
seeds_I %>% filter(Treatment == "c" | Treatment == "hp")  %>%  group_by(Population) %>% summarize(N_missing=sum(Missing_seeds), N_total=length(Missing_seeds)) %>% mutate(Per_missing=100*(N_missing/N_total))
```

    ## # A tibble: 6 x 4
    ##   Population N_missing N_total Per_missing
    ##   <chr>          <dbl>   <int>       <dbl>
    ## 1 I10               63     147        42.9
    ## 2 I15               97     189        51.3
    ## 3 I16              110     192        57.3
    ## 4 I17               60     238        25.2
    ## 5 I4                69     158        43.7
    ## 6 I7                 8      69        11.6

The number of fruits (broken out by treatments) that were intact

``` r
kable(seeds_I %>% filter(Treatment == "c" | Treatment == "hp" & fruit_length >= 5)  %>% group_by(Population, Treatment) %>% summarize(N_intact=length(Missing_seeds)-sum(Missing_seeds)))
```

| Population | Treatment | N\_intact |
|:-----------|:----------|----------:|
| I10        | c         |        46 |
| I10        | hp        |        24 |
| I15        | c         |        50 |
| I15        | hp        |        30 |
| I16        | c         |        42 |
| I16        | hp        |        35 |
| I17        | c         |        90 |
| I17        | hp        |        68 |
| I4         | c         |        51 |
| I4         | hp        |        28 |
| I7         | c         |        32 |
| I7         | hp        |        21 |

Okay so for a given population, we want to take the intact fruits where
fruit length was measured and was greater than or equal to 5, and run a
regression of fruit length vs. seed set with plant ID as a random effect

``` r
#creating a new object to contain only fruits that weren't missing seeds and that had a length greater than or equal to 5
seeds_est <- seeds_I %>% filter(Missing_seeds==0 & fruit_length >= 5 & Seeds_counted!="NA")

#creating a function to return the summary of the model we want to apply to all the population subsets
f <- function(df) summary(lmer(Seeds_counted ~ fruit_length +(1|Plant_ID), data = df))

#subsetting the data by population and applying the function
v <- lapply(split(seeds_est, seeds_est$Population), f)

#creating the list of populations and the empty data frame for the results
Pop=sort(as.factor(c(unique(seeds_est$Population),unique(seeds_est$Population))))

results<-data.frame("Estimate"=numeric(), "Std. Error"=numeric(), "df"=numeric(), "t value"=numeric(), "Pr(>|t|)"=numeric())

##extracting the coefficients from each model
for (i in 1:6){
  z<-as.data.frame(v[[i]]$coefficients)
  results<-rbind(results,z)
}

#putting the results together with population labels
results<-cbind(results, Pop)
results<-results %>% tibble::rownames_to_column("Coefficient") 
kable(results)
```

| Coefficient    |   Estimate | Std. Error |        df |    t value | Pr(&gt;\|t\|) | Pop |
|:---------------|-----------:|-----------:|----------:|-----------:|--------------:|:----|
| (Intercept)    | -4.9030311 |  1.1720448 |  43.22923 | -4.1833136 |     0.0001380 | I10 |
| fruit\_length  |  0.6442419 |  0.0404219 |  99.05419 | 15.9379396 |     0.0000000 | I10 |
| (Intercept)1   | -6.9302873 |  1.4834267 |  78.21704 | -4.6718097 |     0.0000122 | I15 |
| fruit\_length1 |  0.6157679 |  0.0313306 | 104.38553 | 19.6538812 |     0.0000000 | I15 |
| (Intercept)2   | -7.0853665 |  1.2992793 |  99.90579 | -5.4533052 |     0.0000004 | I16 |
| fruit\_length2 |  0.4996711 |  0.0303692 | 130.38271 | 16.4532449 |     0.0000000 | I16 |
| (Intercept)3   | -6.1124375 |  1.4477199 | 106.46340 | -4.2221133 |     0.0000511 | I17 |
| fruit\_length3 |  0.6502955 |  0.0291129 | 220.91176 | 22.3370509 |     0.0000000 | I17 |
| (Intercept)4   | -5.2894699 |  1.6483733 |  63.25084 | -3.2089029 |     0.0020933 | I4  |
| fruit\_length4 |  0.6191814 |  0.0435775 |  92.51235 | 14.2087489 |     0.0000000 | I4  |
| (Intercept)5   | -1.5641300 |  2.3399232 |  49.25099 | -0.6684535 |     0.5069651 | I7  |
| fruit\_length5 |  0.2489382 |  0.0474980 |  68.80198 |  5.2410270 |     0.0000017 | I7  |

``` r
#Using these regressions to calculate the estimated number of seeds for fruits that had missing seeds
#My approach was to loop over the populations, subset the data by that population, and then use the regression parameters for that population to create a column that contains either the counted seed number (if no seeds were missing), or the estimated seed number if seeds were missing
magic_for()
for (pop in unique(Pop)) {
  a <- seeds_I %>% filter(Population==pop) 
  aa <- results %>% filter(Pop==pop)
  b <- if_else(a$Missing_seeds==1, (a$fruit_length*aa[2,2])+aa[1,2], a$Seeds_counted)
  put(b)
}

#get the result of the loop and unlist it to make it a column of the seeds_I object
z<-magic_result()
magic_free()
seeds_I$estimates<-unlist(z)
```

Here we are extracting the predicted values from each model fit, to use
in plotting

``` r
pop_list <- unique(seeds_est$Population)

magic_for()
for (pop in pop_list) {
  a <- seeds_est %>% filter(Population==pop) 
  b <- lmer(Seeds_counted ~ fruit_length +(1|Plant_ID), data = a)
  c <- predict(b, type= "response")
  put(c)
  
}
#extracting the results and putting them into a column
z<-magic_result()
magic_free()
seeds_est$pred<-unlist(z)

#plotting the regressions for each population
ggplot(aes(x=fruit_length),data=seeds_est)+geom_point(aes(y=Seeds_counted)) + geom_smooth(aes(y=pred), method="lm")+facet_wrap(~Population)+theme_classic()+xlab("Fruit length (mm)")+ylab("Seed number")
```

![](Seed-estimates_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Now we need to take the `seeds_I$estimates` and put them into `seeds_E`,
to use for our calculations for pollen limitation for all populations

``` r
seeds_G$estimates <- seeds_G$Seeds_counted
seeds_work <- rbind(seeds_G, seeds_I)
```

Working with only the `hp` and `c` fruits, and calculating how many
plants had zeros

``` r
seeds_work <- seeds_work %>% filter(Treatment == "hp" | Treatment == "c")

#looking at number of plants that had zeros in the hp fruits, to see if we really need to drop plants 
zees<-seeds_work %>% group_by(Population, Plant_ID) %>% filter(Treatment== "hp") %>% summarize(zeros=sum(estimates==0), nonzeroes=sum(estimates!=0))

zees2<-seeds_work %>% group_by(Population, Plant_ID, Treatment) %>%  summarize(zeros=sum(estimates==0), nonzeroes=sum(estimates!=0)) %>% pivot_wider(names_from=Treatment, values_from=c(zeros, nonzeroes))


#for now, we will only analyze plants where there were fewer than 2 zero-seed hand-pollinated fruits, following Sotiria's criteria
#this is a list of the plants had less than 2 zeros in the hp fruits
keep_list <- zees2 %>% filter(zeros_hp <2) %>% mutate(ID=paste(Population, Plant_ID))

#making an object with only those plants
seeds_work2<- seeds_work %>% mutate(ID=paste(Population, Plant_ID)) %>% filter(ID %in% keep_list$ID) %>% mutate(Treatment=factor(Treatment, levels=c("hp", "c")))

#this tells us sample sizes
seeds_work2 %>% group_by(Population) %>% summarize(N=length(unique(Plant_ID)))
```

    ## # A tibble: 12 x 2
    ##    Population     N
    ##    <chr>      <int>
    ##  1 G1            22
    ##  2 G3            13
    ##  3 G6            28
    ##  4 G7            17
    ##  5 G8            23
    ##  6 G9            12
    ##  7 I10           16
    ##  8 I15           21
    ##  9 I16           20
    ## 10 I17           19
    ## 11 I4            23
    ## 12 I7             7

Run an lmer for each population

``` r
#creating a function that extracts the emmeans for models
f <- function(df) as.data.frame(emmeans(lmer(estimates ~ Treatment +(1|Plant_ID), data = df), "Treatment"))
#applying this function to the dataset subset by population
v <- lapply(split(seeds_work2, seeds_work2$Population), f)

#creating a function that extracts the pairwise comparisons of emmeans for models
f2 <- function(df) as.data.frame(pairs(emmeans(lmer(estimates ~ Treatment +(1|Plant_ID), data = df), "Treatment")))
#applying this function to the dataset subset by population
v3 <- lapply(split(seeds_work2, seeds_work2$Population), f2)

#making the contrasts object more usable, adding significance column
v4 <-do.call(rbind.data.frame, v3)
v4$Pop<-rownames(v4)
rownames(v4)<-c()
v4$sig<-ifelse(v4$p.value<0.05, "*", "")

#making the emmeans object more usable
v2<-do.call(rbind.data.frame, v)
Pop=sort(as.factor(c(unique(seeds_work2$Population),unique(seeds_work2$Population))))
v2<-data.frame(v2, Pop, row.names = c())

#printing these tables
kable(v2)
```

| Treatment |    emmean |       SE |       df |  lower.CL |  upper.CL | Pop |
|:----------|----------:|---------:|---------:|----------:|----------:|:----|
| hp        | 14.942885 | 1.818878 | 31.04051 | 11.233455 | 18.652314 | G1  |
| c         | 12.582700 | 1.808288 | 30.41375 |  8.891789 | 16.273612 | G1  |
| hp        | 15.589550 | 1.894391 | 19.93256 | 11.637063 | 19.542036 | G3  |
| c         | 12.138835 | 1.923416 | 20.67681 |  8.135063 | 16.142607 | G3  |
| hp        | 25.100876 | 2.605697 | 52.64807 | 19.873694 | 30.328057 | G6  |
| c         | 15.420692 | 2.642681 | 55.30285 | 10.125293 | 20.716092 | G6  |
| hp        | 16.800679 | 2.269341 | 19.96026 | 12.066313 | 21.535045 | G7  |
| c         | 13.782001 | 2.268339 | 19.90233 |  9.048841 | 18.515162 | G7  |
| hp        | 11.975090 | 1.612601 | 44.60462 |  8.726350 | 15.223829 | G8  |
| c         |  7.993084 | 1.609851 | 44.34126 |  4.749348 | 11.236820 | G8  |
| hp        | 16.849021 | 1.944671 | 20.03896 | 12.793014 | 20.905028 | G9  |
| c         |  6.423405 | 2.005466 | 22.50001 |  2.269675 | 10.577135 | G9  |
| hp        | 12.193909 | 1.380126 | 22.04706 |  9.332057 | 15.055762 | I10 |
| c         |  8.088747 | 1.413334 | 23.59607 |  5.169123 | 11.008371 | I10 |
| hp        | 17.232032 | 1.114000 | 45.20029 | 14.988595 | 19.475469 | I15 |
| c         | 15.025079 | 1.145358 | 48.38227 | 12.722653 | 17.327505 | I15 |
| hp        | 16.945741 | 1.794187 | 30.84421 | 13.285724 | 20.605758 | I16 |
| c         | 18.355129 | 1.775890 | 30.08162 | 14.728690 | 21.981567 | I16 |
| hp        | 19.815493 | 1.372928 | 45.74091 | 17.051509 | 22.579476 | I17 |
| c         | 18.877753 | 1.319608 | 41.25125 | 16.213242 | 21.542264 | I17 |
| hp        | 18.264904 | 1.724183 | 41.41548 | 14.783906 | 21.745902 | I4  |
| c         | 16.631915 | 1.659748 | 37.41123 | 13.270194 | 19.993636 | I4  |
| hp        |  6.661252 | 1.544884 | 15.42039 |  3.376212 |  9.946292 | I7  |
| c         |  8.763809 | 1.662033 | 19.45213 |  5.290598 | 12.237021 | I7  |

``` r
kable(v4)
```

| contrast |   estimate |       SE |        df |    t.ratio |   p.value | Pop | sig |
|:---------|-----------:|---------:|----------:|-----------:|----------:|:----|:----|
| hp - c   |  2.3601842 | 1.535166 | 139.53757 |  1.5374132 | 0.1264567 | G1  |     |
| hp - c   |  3.4507147 | 1.872154 |  90.81113 |  1.8431784 | 0.0685650 | G3  |     |
| hp - c   |  9.6801833 | 2.908255 | 201.41342 |  3.3285192 | 0.0010380 | G6  | \*  |
| hp - c   |  3.0186775 | 1.489123 | 115.18440 |  2.0271511 | 0.0449557 | G7  | \*  |
| hp - c   |  3.9820058 | 1.905295 | 101.62443 |  2.0899680 | 0.0391169 | G8  | \*  |
| hp - c   | 10.4256158 | 2.140999 |  82.56524 |  4.8695094 | 0.0000053 | G9  | \*  |
| hp - c   |  4.1051624 | 1.258279 |  96.41874 |  3.2625218 | 0.0015279 | I10 | \*  |
| hp - c   |  2.2069530 | 1.399481 | 139.23685 |  1.5769800 | 0.1170694 | I15 |     |
| hp - c   | -1.4093879 | 1.671339 | 134.00514 | -0.8432686 | 0.4005809 | I16 |     |
| hp - c   |  0.9377394 | 1.652255 | 155.02814 |  0.5675512 | 0.5711608 | I17 |     |
| hp - c   |  1.6329890 | 1.791575 | 119.98503 |  0.9114824 | 0.3638689 | I4  |     |
| hp - c   | -2.1025575 | 2.233296 |  32.91942 | -0.9414596 | 0.3533273 | I7  |     |

``` r
#plotting the pollen limitation results
ggplot(aes(x=Treatment, y=emmean), data=v2) + facet_wrap(~Pop) +geom_point()+geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2) +ylab("Seed set") + theme_classic()+geom_text(aes(x=1.5, y=20, label=sig),data=v4, size=6 )
```

![](Seed-estimates_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
