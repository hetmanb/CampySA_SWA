
This project describes additional analysis to accompany the manuscript detailing the attribution of source from clinical infection with *Campylobacter jejuni* in Alberta, Canada.

Source Attribution Analysis for C. jejuni from Southern Alberta, Canada
================
Benjamin Hetman
2022-06-01


## Step 0: Load libraries and custom functions

For this step, it is often useful to write a separate helper file. It
keeps the present workspace organized by reducing clutter. Here we’ll
use the `source()` command to call another file labelled `helper.R`
which contains many of our custom functions.

``` r
source("helper.R")
set.seed(900)
```

## Step 1: Load data from MS Excel file

``` r
#campy_raw <- readxl::read_xlsx(here("DATA","SWA_data_for_attribution.xlsx"), sheet = 1, col_names = TRUE, guess_max = 1000, na=c("?", "#N/A"))
#Dataset with isolates from SWA only: 
campy_raw <- readxl::read_xlsx(here("DATA","SWAhum_SWAnhum.xlsx"), sheet = 1, col_names = TRUE, guess_max = 1000, na=c("?", "#N/A"))
# Note: 2547 human isolates; 5405 non-human isolates

#Dataset with isolates from SWA(Human) and full CGFdb (nonhuman)
#campy_raw <- readxl::read_xlsx(here("DATA","SWAhum_CGFnhum.xlsx"), sheet = 1, col_names = TRUE, guess_max = 1000, na=c("?", "#N/A"))
# Note: 2547 human isolates; 13188 non-human isolates
```

------------------------------------------------------------------------

## Step 2: Verify data

``` r
campy_human <- campy_raw %>%  filter(Source == "Human")
campy_nonhuman <- campy_raw %>%  filter(Source != "Human")
```

Summarize the human data set by year, providing some descriptive
statistics.

``` r
# Summarize Human case data by year
# Note: the output of this filters out potential Year/Source combos with only 1 subtype 
campy_human %>%
  group_by(Year) %>%  
  summarize(n.isolates = n(), 
            n.subtypes = length(unique(BCM95.v2)), 
            min.size = min(BCM95.SA.size, na.rm = TRUE), 
            max.size = max(BCM95.SA.size, na.rm = TRUE),
            subtype.size = list(BCM95.SA.size[!is.na(BCM95.SA.size)]),
            .groups = "keep") %>%
  mutate(Year = as.factor(Year)) %>% 
  filter(n.subtypes >1) %>% 
  flextable() %>%  
  compose(j="subtype.size", value = as_paragraph(
    plot_chunk(value = subtype.size, type = "dens", col = "blue")
  )) %>%
  set_caption("Summary of human C.jejuni isolates") 
```

Summarize the non-human data, also by year.

``` r
# Summarize Human case data by year
# Note: the output of this filters out potential Year/Source combos with only 1 subtype 

campy_nonhuman %>%
  group_by(Year, Source) %>%  
  summarize(n.isolates = n(), 
            n.subtypes = length(unique(BCM95.v2)), 
            min.size = min(BCM95.SA.size, na.rm = TRUE), 
            max.size = max(BCM95.SA.size, na.rm = TRUE),
            subtype.size = list(BCM95.SA.size[!is.na(BCM95.SA.size)]),
            .groups = "keep") %>%  
  mutate(Year = as.factor(Year)) %>% 
  filter(n.subtypes >1) %>% 
  flextable() %>% 
  compose(j="subtype.size", value = as_paragraph(
    plot_chunk(value = subtype.size, type = "dens", col = "red")
  )) %>%
  set_caption("Summary of non-human C.jejuni isolates")  
```

## Step 3: Set up randomization for Source Attribution

Purpose of this analysis is to generate estimate and 95% confidence
intervals surrounding the attribution of source for each unique subtype.

The objective will be to iterate through each unique subtype, and
randomly sample a subset (i.e., subsample) to calculate the estimated
primary source frequency, then calculate a confidence interval around
that distribution.

``` r
f <- function(human.data, nonhuman.data, iterations) {
  # Initialize list to store results
  results_table <- tibble(
    subtype = unique(human.data$BCM95.v2),
    n.human = NA,
    n.nonhuman = NA,
    Chicken = NA,
    Chicken.low = NA,
    Chicken.high = NA,
    Chicken.sd = NA,
    Cattle = NA,
    Cattle.low = NA,
    Cattle.high =  NA,
    Cattle.sd = NA, 
    Water =  NA,
    Water.low = NA,
    Water.high = NA,
    Water.sd = NA
    )
  
  # Loop through subtypes from the human dataset
  for(j in unique(human.data$BCM95.v2)){
    #Subset non-human data based on human subtype
    p <- nonhuman.data %>% filter(BCM95.v2 == j)
    q <- human.data %>%  filter(BCM95.v2 == j) 
    
    results_table[results_table$subtype == j,]$n.nonhuman <- nrow(p)
    results_table[results_table$subtype == j,]$n.human <- nrow(q)
    #If less than 5 isolates in non-human subset, don't calculate stats and move
    # to the next subtype. 
    if(nrow(p) < 5) {
      next
    }
    # Use 50% of isolates from subtype to sample
    n <- round(nrow(p) * 0.75)
    
    # Initialize empty tibble and draw random samples
    x <- tibble(n.Cattle = numeric(), n.Chicken =numeric(), n.Water=numeric())
    for(i in 1:iterations){
      sub <- sample_n(p, n, replace = FALSE)
      x[i, "n.Chicken"] <- sub %>%  filter(Source == "Chicken") %>%  nrow()
      x[i, "n.Cattle"] <- sub %>%  filter(Source == "Cattle") %>%  nrow()
      x[i, "n.Water"] <- sub %>%  filter(Source == "Water") %>%  nrow()
    }
      
    # Calculate descriptive statistics
    #----------------------------------------------------------------------------
      mean.chicken <- mean(x$n.Chicken)
      mean.cattle <- mean(x$n.Cattle)
      mean.water <- mean(x$n.Water)
      sd.chicken <- sd(x$n.Chicken)
      sd.cattle <- sd(x$n.Cattle)
      sd.water <- sd(x$n.Water)
     #----------------------------------------------------------------------------
      ci.hi.chicken <- mean.chicken + 1.96*(sd.chicken/sqrt(length(x$n.Chicken)))
      ci.low.chicken <- mean.chicken - 1.96*(sd.chicken/sqrt(length(x$n.Chicken)))
      
      ci.hi.cattle <- mean.cattle + 1.96*(sd.cattle/sqrt(length(x$n.Cattle)))
      ci.low.cattle <- mean.cattle - 1.96*(sd.cattle/sqrt(length(x$n.Cattle)))
      
      ci.hi.water <- mean.water + 1.96*(sd.water/sqrt(length(x$n.Water)))
      ci.low.water <- mean.water - 1.96*(sd.water/sqrt(length(x$n.Water)))
     #----------------------------------------------------------------------------
      # Store results in final table:
      results_table[results_table$subtype == j,]$Chicken = round(mean.chicken/n, 3)
      results_table[results_table$subtype == j,]$Chicken.high = round(ci.hi.chicken/n, 3)
      results_table[results_table$subtype == j,]$Chicken.low = round(ci.low.chicken/n, 3)
      results_table[results_table$subtype == j,]$Chicken.sd = round(sd.chicken/n, 3)
      results_table[results_table$subtype == j,]$Cattle = round(mean.cattle/n, 3)
      results_table[results_table$subtype == j,]$Cattle.high =  round(ci.hi.cattle/n, 3)
      results_table[results_table$subtype == j,]$Cattle.low = round(ci.low.cattle/n, 3)
      results_table[results_table$subtype == j,]$Cattle.sd = round(sd.cattle/n, 3)
      results_table[results_table$subtype == j,]$Water =  round(mean.water/n, 3)
      results_table[results_table$subtype == j,]$Water.high = round(ci.hi.water/n, 3)
      results_table[results_table$subtype == j,]$Water.low = round(ci.low.water/n, 3)
      results_table[results_table$subtype == j,]$Water.sd = round(sd.water/n, 3)

  }
return(results_table)
}


subtype.table <- f(campy_human, campy_nonhuman, 100) 
subtype.table <- arrange(subtype.table, desc(n.human))

flextable(subtype.table)
```

## Step 4a: Random draws from human population

Next, we’ll set up some random draws from the human isolates only, and
use this to estimate the total proportion of human C. jejuni isolates
attributable to Cattle, Chicken, and Water sources in S. Alberta

``` r
# The following set of functions are based on the equations for pooled mean and standard deviations 
# described here: https://math.stackexchange.com/questions/2971315/how-do-i-combine-standard-deviations-of-two-groups
# They accurately combine multiple means and sd from different samples into a combined mean (c.mean) and combined st.dev (c.sd)

human_sampler <- function(human.data, subtype.data, iterations, sample.n){
  

  
  results_list <- list()
  for(i in 1:iterations){
  x1 <- sample_n(human.data, sample.n) %>% 
    select(Key, BCM95.v2) %>%
    group_by(BCM95.v2) %>%  
    summarize(n.human = n()) %>%  
    left_join(subtype.data, by=c("BCM95.v2"="subtype"))
  
  x2 <- x1 %>% 
    filter(n.nonhuman >= 5) %>% 
    select(n.human.x, n.nonhuman, Chicken, Cattle, Water) %>% 
    mutate(across(c(Chicken, Cattle, Water), ~ .x * n.human.x * n.nonhuman)) %>% 
    summarize(across(c(Chicken, Cattle, Water), ~ sum(.x, na.rm=TRUE)/sum(n.human.x *n.nonhuman, na.rm = TRUE)))
  
  Chicken.cmean <- x2[[1]]
  Cattle.cmean <- x2[[2]]  
  Water.cmean <- x2[[3]]
    
  x3 <- x1 %>%
    filter(n.nonhuman >= 5) %>% 
    select(n.human.x, n.nonhuman, Chicken, Chicken.sd, Cattle, Cattle.sd, Water, Water.sd) %>% 
    mutate(Chicken.q = (Chicken.sd^2)*(n.nonhuman-1) + (n.nonhuman*(Chicken^2))) %>% 
    mutate(Cattle.q = (Cattle.sd^2)*(n.nonhuman-1) + (n.nonhuman*(Cattle^2))) %>% 
    mutate(Water.q = (Water.sd^2)*(n.nonhuman-1) + (n.nonhuman*(Water^2))) %>% 
    summarize(n.assigned = sum(n.human.x),
              n.unassigned = sample.n - n.assigned,
              n.total = sum(n.nonhuman), 
              Chicken.csd = sqrt(abs(sum(Chicken.q, na.rm=TRUE) - sum(n.nonhuman, na.rm=TRUE)*Chicken.cmean^2) / (sum(n.nonhuman, na.rm=TRUE)-1) ),
              Cattle.csd = sqrt(abs(sum(Cattle.q, na.rm=TRUE) - sum(n.nonhuman, na.rm=TRUE)*Cattle.cmean^2) / (sum(n.nonhuman, na.rm=TRUE)-1) ),
              Water.csd = sqrt(abs(sum(Water.q, na.rm=TRUE) - sum(n.nonhuman, na.rm=TRUE)*Water.cmean^2) / (sum(n.nonhuman, na.rm=TRUE)-1) ),
              Chicken.cmean = Chicken.cmean,
              Chicken.clow = Chicken.cmean - 1.96*(Chicken.csd/sqrt(n.total)), 
              Chicken.chigh = Chicken.cmean + 1.96*(Chicken.csd/sqrt(n.total)), 
              Cattle.cmean = Cattle.cmean, 
              Cattle.clow = Cattle.cmean - 1.96*(Cattle.csd/sqrt(n.total)), 
              Cattle.chigh = Cattle.cmean + 1.96*(Cattle.csd/sqrt(n.total)), 
              Water.cmean = Water.cmean,
              Water.clow = Water.cmean - 1.96*(Water.csd/sqrt(n.total)), 
              Water.chigh = Water.cmean + 1.96*(Water.csd/sqrt(n.total)),
              mutate(across(Chicken.csd:Water.chigh, ~round(.x, digits = 3)))
              )
    

  results_list[[i]] <- x3
  }
  return(bind_rows(results_list))
}



sa.results <- human_sampler(campy_human, subtype.table, 30, 500)
flextable(sa.results)
```

## Step 4b: Getting a final estimate for attribution of source to the human population in SWA

Finally, we’ll estimate the combined mean and standard deviation for the
human population derived from the random draws.

``` r
# Calculate a single average estimate for the propotions of attributable source from the sa_table calculated above

final_sa_estimate <- function(sa_table){
  
  df <- sa_table
  
  Chicken.cmean <- round(sum(df$Chicken.cmean*df$n.total)/sum(df$n.total), 3)
  Cattle.cmean <- round(sum(df$Cattle.cmean*df$n.total)/sum(df$n.total), 3)
  Water.cmean <- round(sum(df$Water.cmean*df$n.total)/sum(df$n.total), 3)

  Chicken.q <- sum(df$Chicken.csd^2*df$n.total-1 + df$n.total*df$Chicken.cmean^2)
  Cattle.q <- sum(df$Cattle.csd^2*df$n.total-1 + df$n.total*df$Cattle.cmean^2)
  Water.q <- sum(df$Water.csd^2*df$n.total-1 + df$n.total*df$Water.cmean^2)

  
  Chicken.csd = sqrt((Chicken.q - sum(df$n.total)*Chicken.cmean^2)/(sum(df$n.total)-1))
  Cattle.csd = sqrt((Cattle.q - sum(df$n.total)*Cattle.cmean^2)/(sum(df$n.total)-1))
  Water.csd = sqrt((Water.q - sum(df$n.total)*Water.cmean^2)/(sum(df$n.total)-1))
 
  
  Chicken.low <- round(Chicken.cmean - 1.96*(Chicken.csd/sqrt(sum(df$n.total))), 3)
  Cattle.low <- round(Cattle.cmean - 1.96*(Cattle.csd/sqrt(sum(df$n.total))), 3)
  Water.low <- round(Water.cmean - 1.96*(Water.csd/sqrt(sum(df$n.total))), 3)

  Chicken.high <- round(Chicken.cmean + 1.96*(Chicken.csd/sqrt(sum(df$n.total))), 3)
  Cattle.high <- round(Cattle.cmean + 1.96*(Cattle.csd/sqrt(sum(df$n.total))), 3)
  Water.high <- round(Water.cmean + 1.96*(Water.csd/sqrt(sum(df$n.total))), 3)

  
  
  return(c("Mean Chicken" = paste0(Chicken.cmean, "; (", Chicken.low, "-", Chicken.high, ")"),
           "Mean Cattle" = paste0(Cattle.cmean, "; (", Cattle.low, "-", Cattle.high, ")"),
           "Mean Water" = paste0(Water.cmean, "; (", Water.low, "-", Water.high, ")"))
  )
  
}

final_sa_estimate(sa.results)
```

    ##           Mean Chicken            Mean Cattle             Mean Water 
    ##  "0.087; (0.084-0.09)"   "0.8; (0.799-0.801)" "0.112; (0.111-0.113)"

## Step 5: Plot the data for S. Alberta

### 5a Randomized draws from each subtype to estimate source

``` r
subtype.df <- 
 subtype.table %>%
  filter(!is.na(Chicken)) %>% 
  select(-n.human) %>%
  arrange(desc(Cattle), desc(Chicken)) %>% 
  rowid_to_column("Index") %>% 
   group_by(subtype) %>% 
   mutate(Chicken.low = Chicken.low + Cattle,
         Chicken.high = Chicken.high + Cattle,
         Water.low = Water.low + Chicken + Cattle,
         Water.high = Water.high + Chicken + Cattle) %>% 
  ungroup()

  
st.chicken.df <- subtype.df %>% 
  select(subtype, Index, n.nonhuman, starts_with("Chicken")) %>%  
  mutate(Source = "Chicken") %>% 
  rename(c("subtype"="subtype", "Mean"="Chicken", "Low"="Chicken.low", "High"="Chicken.high", "Source"="Source"))

st.cattle.df <- subtype.df %>% 
  select(subtype, Index, n.nonhuman, starts_with("Cattle")) %>%  
  mutate(Source = "Cattle") %>% 
  rename(c("subtype"="subtype", "Mean"="Cattle", "Low"="Cattle.low", "High"="Cattle.high", "Source"="Source"))

st.water.df <- subtype.df %>% 
  select(subtype,Index, n.nonhuman, starts_with("Water")) %>%  
  mutate(Source = "Water") %>% 
  rename(c("subtype"="subtype", "Mean"="Water", "Low"="Water.low", "High"="Water.high", "Source"="Source"))
  
st.plot.df <- bind_rows(st.chicken.df, st.cattle.df, st.water.df) %>%  
  mutate(Source = factor(Source, levels = c("Water", "Chicken", "Cattle")))
```

``` r
ggplot(st.plot.df, aes(x = reorder(subtype, Index) , y = Mean)) + 
  geom_bar(stat = "identity", aes(fill = Source), color = "black", width=1) +
  geom_errorbar(aes(x = subtype, ymin=Low, ymax=High), color = "black", width = 0.2) +
  hrbrthemes::theme_ipsum() + 
  labs(title = "Estimated source attribution for CGF subtypes from South Alberta, Canada",
       subtitle = "(Randomized draws of 50% from each subtype, sampled 100 times)",
       y = "Proportion",
       x = "Subtype") +
  theme(axis.text.x = element_text(angle = 90, size = 9, vjust = 0.5, )) +
  scale_fill_manual(values = c("#67727e","#d4674c", "#a8a355"))
```

![](SWA_Analysis_files/figure-gfm/Plot%20the%20subtype%20source%20distributions%20and%20uncertainties-1.png)<!-- -->

### 5b Randomized draws from Human dataset to estimate proportion attributable

Finally, we’ll want to actually visualize the results. The first code
chunk here will reconfigure the data into a format more suitable for
plotting. Then, we’ll use a separate chunk to actually draw the chart.

``` r
start.df <- 
 sa.results %>%
  select(Chicken.cmean, Chicken.clow, Chicken.chigh,
         Cattle.cmean, Cattle.clow, Cattle.chigh,
         Water.cmean, Water.clow, Water.chigh) %>% 
  rowid_to_column("Index") %>% 
  group_by(Index) %>%  
  mutate(Chicken.clow = Chicken.clow + Cattle.cmean,
         Chicken.chigh = Chicken.chigh + Cattle.cmean,
         Water.clow = Water.clow + Chicken.cmean + Cattle.cmean,
         Water.chigh = Water.chigh + Chicken.cmean + Cattle.cmean) %>% 
  ungroup()
  
  

chicken.df <- start.df %>% 
  select(Index, starts_with("Chicken")) %>%  
  mutate(Source = "Chicken") %>% 
  rename(c("Index"="Index", "Mean"="Chicken.cmean", "Low"="Chicken.clow", "High"="Chicken.chigh", "Source"="Source"))

cattle.df <- start.df %>% 
  select(Index, starts_with("Cattle")) %>%  
  mutate(Source = "Cattle") %>% 
  rename(c("Index"="Index", "Mean"="Cattle.cmean", "Low"="Cattle.clow", "High"="Cattle.chigh", "Source"="Source"))

water.df <- start.df %>% 
  select(Index, starts_with("Water")) %>%  
  mutate(Source = "Water") %>% 
  rename(c("Index"="Index", "Mean"="Water.cmean", "Low"="Water.clow", "High"="Water.chigh", "Source"="Source"))
  
plot.df <- bind_rows(chicken.df, cattle.df, water.df) %>%  
  arrange(Index) %>%
  mutate(Source = factor(Source, levels = c("Water", "Chicken", "Cattle")))
```

``` r
ggplot(plot.df, aes(x = Index, y = Mean)) + 
  geom_bar(stat = "identity", aes(fill = Source), color = "black", width=1) +
  geom_errorbar(aes(x = Index, ymin=Low, ymax=High), color = "black", width = 0.2) +
  hrbrthemes::theme_ipsum() + 
  labs(title = "Attributed source proportions of randomly sampled human clinical isolates 
       from southern Alberta, Canada",
       subtitle = "(Results from n=30 draws of 500 C. jejuni isolates)",
       y = "Proportion",
       x = "Random Draw") +
  scale_fill_manual(values = c("#67727e","#d4674c", "#a8a355"))
```

![](SWA_Analysis_files/figure-gfm/Plot%20the%20data-1.png)<!-- -->
