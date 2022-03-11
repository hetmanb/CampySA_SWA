Source Attribution Analysis for C. jejuni from Southern Alberta, Canada
================
Benjamin Hetman
2022-03-10

# Source attribution analysis for *C. jejuni* from southern Alberta, Canada

## Step 0: Load libraries and custom functions

For this step, it is often useful to write a separate helper file. It
keeps the present workspace organized by reducing clutter. Here we’ll
use the `source()` command to call another file labelled `helper.R`
which contains many of our custom functions.

``` r
source("helper.R")
```

## Step 1: Load data from MS Excel file

``` r
campy_raw <- readxl::read_xlsx(here("DATA","SWA_data_for_attribution.xlsx"), sheet = 1, col_names = TRUE, guess_max = 1000, na=c("?", "#N/A"))
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
campy_human %>%
  group_by(Year) %>%  
  summarize(n.isolates = n(), 
            n.subtypes = length(unique(BCM95.v2)), 
            min.size = min(BCM95.SA.size, na.rm = TRUE), 
            max.size = max(BCM95.SA.size, na.rm = TRUE),
            subtype.size = list(BCM95.SA.size[!is.na(BCM95.SA.size)]),
            .groups = "keep") %>%
  mutate(Year = as.factor(Year)) %>% 
  flextable() %>%  
  compose(j="subtype.size", value = as_paragraph(
    plot_chunk(value = subtype.size, type = "dens", col = "blue")
  )) %>%
  set_caption("Summary of human C.jejuni isolates") 
```

Summarize the non-human data, also by year.

``` r
# Summarize Human case data by year
campy_nonhuman %>%
  group_by(Year, Source) %>%  
  summarize(n.isolates = n(), 
            n.subtypes = length(unique(BCM95.v2)), 
            min.size = min(BCM95.SA.size, na.rm = TRUE), 
            max.size = max(BCM95.SA.size, na.rm = TRUE),
            subtype.size = list(BCM95.SA.size[!is.na(BCM95.SA.size)]),
            .groups = "keep") %>%  
  mutate(Year = as.factor(Year)) %>%  
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
    Cattle = NA,
    Cattle.low = NA,
    Cattle.high =  NA,
    Water =  NA,
    Water.low = NA,
    Water.high = NA,
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
    n <- round(nrow(p) * 0.5)
    
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
      results_table[results_table$subtype == j,]$Chicken = round(mean.chicken/n, 2)
      results_table[results_table$subtype == j,]$Chicken.high = round(ci.hi.chicken/n, 2)
      results_table[results_table$subtype == j,]$Chicken.low = round(ci.low.chicken/n, 2)
      results_table[results_table$subtype == j,]$Cattle = round(mean.cattle/n, 2)
      results_table[results_table$subtype == j,]$Cattle.high =  round(ci.hi.cattle/n, 2)
      results_table[results_table$subtype == j,]$Cattle.low = round(ci.low.cattle/n, 2)
      results_table[results_table$subtype == j,]$Water =  round(mean.water/n, 2)
      results_table[results_table$subtype == j,]$Water.high = round(ci.hi.water/n, 2)
      results_table[results_table$subtype == j,]$Water.low = round(ci.low.water/n, 2)
  }
return(results_table)
}


subtype.table <- f(campy_human, campy_nonhuman, 100) 
subtype.table <- arrange(subtype.table, desc(n.human))

flextable(subtype.table)
```

## Step 4: Random draws from human population

Next, we’ll set up some random draws from the human isolates only, and
use this to estimate the total proportion of human C. jejuni isolates
attributable to Cattle, Chicken, and Water sources in S. Alberta

``` r
# Set up random sampler for human data only: 
human_sampler <- function(human.data, subtype.data, iterations, sample.n){
  
  subtype.data <- subtype.data %>%  filter(!is.na(Chicken))
  human.data <- human.data %>%  filter(BCM95.v2 %in% subtype.data$subtype)
  
  results_list <- list()
  for(i in 1:iterations){
  x <- 
    sample_n(human.data, sample.n) %>%  
    select(Key, BCM95.v2) %>%
    group_by(BCM95.v2) %>%  
    summarize(n.human = n()) %>%  
    left_join(subtype.data, by=c("BCM95.v2"="subtype")) %>% 
    select(-n.human.y, -n.nonhuman) %>% 
    mutate(across(Chicken:Water.high, ~ .x * n.human.x)) %>% 
    summarize(across(n.human.x:Water.high, ~ sum(.x, na.rm=TRUE)))
  
  results_list[[i]] <- x  
  }
  return(bind_rows(results_list))
}

sa.results <- human_sampler(campy_human, subtype.table, 30, 500)
flextable(sa.results)
```

## Step 5: Plot the human data for S. Alberta

Finally, we’ll want to actually visualize the results. The first code
chunk here will reconfigure the data into a format more suitable for
plotting. Then, we’ll use a separate chunk to actually draw the chart.

``` r
start.df <- 
 sa.results %>%
  mutate(across(Chicken:Water.high, ~ .x / n.human.x)) %>% 
  select(-n.human.x) %>% 
  rowid_to_column("Index") %>% 
  group_by(Index) %>%  
  mutate(Chicken.low = Chicken.low + Cattle,
         Chicken.high = Chicken.high + Cattle,
         Water.low = Water.low + Chicken + Cattle,
         Water.high = Water.high + Chicken + Cattle) %>% 
  ungroup()
  
  

chicken.df <- start.df %>% 
  select(Index, starts_with("Chicken")) %>%  
  mutate(Source = "Chicken") %>% 
  rename(c("Index"="Index", "Mean"="Chicken", "Low"="Chicken.low", "High"="Chicken.high", "Source"="Source"))

cattle.df <- start.df %>% 
  select(Index, starts_with("Cattle")) %>%  
  mutate(Source = "Cattle") %>% 
  rename(c("Index"="Index", "Mean"="Cattle", "Low"="Cattle.low", "High"="Cattle.high", "Source"="Source"))

water.df <- start.df %>% 
  select(Index, starts_with("Water")) %>%  
  mutate(Source = "Water") %>% 
  rename(c("Index"="Index", "Mean"="Water", "Low"="Water.low", "High"="Water.high", "Source"="Source"))
  
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