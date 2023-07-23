---
title: "Foraging Simulation following Geometric Framework Models  - Part 1"
author: "Patrick Lauer, Urs Kalbitzer"
date: '2023-07-22'
slug: food-simulation-I
categories: []
tags:
  - R
subtitle: ''
summary: ''
authors: []
lastmod: "2023-07-23 16:23:27 CEST"
featured: no
projects: []
commentable: true
bibliography: references.bib
toc: true
number-sections: true
---

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/viz/viz.js"></script>
<link href="{{< blogdown/postref >}}index_files/DiagrammeR-styles/styles.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/grViz-binding/grViz.js"></script>

Conception, Idea, and Writing: Patrick Lauer and Urs Kalbitzer. Realization and Programming: Patrick Lauer.

## Introduction

Animals forage to meet their nutritional needs while avoiding potentially harmful components, such as toxins. **Geometric Framework Models**, which have gained popularity in recent years, provide a framework that takes into account that there is rarely one single resource that satisfies an animal’s nutritional requirements and animals have to balance the consumption of different food resources with different nutritional compositions (Simpson and Raubenheimer 2011; Lambert and Rothman 2015). Thus, following Geometric Framework Models, the selection of food resources is not just based on general preferences for specific food items and their availability in the environment. Rather it depends on the animal’s nutritional goals, the composition (i.e. nutrients, harmful components) as well as the availability of food resources, and the animal’s current nutritional state (i.e. which nutrients have recently been consumed), see @fig-dia-assumptions .

Empirical studies on animal foraging behavior often focus on estimating preferences for different food items, which is usually done by calculating **electivity indices** (Manly et al. 2007). Such indices have been developed with the assumption that animals consume different food items proportionally to their preferences for, and the availability of, the different items in the environment. This, however, hardly captures the complex decision-making processes involved in foraging.

Assuming that Geometric Framework Models better reflect animal foraging decisions than just preference models, the applicability of such electivity indices is unclear. Therefore, **we here simulate animals foraging according to such Geometric Framework Models to explore nutrient acquisition in different scenarios** **(Part 1), and then calculate electivity indices based on the observed consumption of different food items to compare these indices with what we know about the foraging behavior of our simulated animals (Part 2).**

<div class="figure">

<div class="grViz html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-1" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"diagram":"\n digraph flowchart {\n  node [shape=box, style = filled];\n   \n    7 [label = \"Electivity Scores\", fillcolor = PaleGreen]\n\n  node[fillcolor = LightCoral]\n    2 [label = \"Availability of Food Resources in the Environment\"];\n    3 [label = \"Composition of Food Resources\"];\n    4 [label = \"Overall Nutritional Goals\"];\n   \n  node[fillcolor = lightblue]\n    1 [label = \"Diet\"];\n    6 [label = \"Subjective Availability of Food Resources\"];\n    5 [label = \"Current Nutritional State\"];\n    \n\n    2 -> 6[arrowhead = \"none\"];\n    6 -> 1;\n    3 -> 1;\n    4 -> 1;\n    5 -> 1;\n    1 -> 5;\n    1 -> 7;\n    2 -> 7;\n    \nsubgraph {\n rank = same; 2; 3; 4;\n } \n  }\n  \n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
<p class="caption">
<span id="fig:fig-dia-assumptions"></span>Figure 1: Diagram illustrating the assumptions simulated for Geometric Framework Models. The red coloured boxes highlight set parameters in the simulation.
</p>

</div>

## General concept of the simulation

The basic idea is that the (simulated) animals have an **optimal, or target, ratio** of different nutrients, for example a ratio of proteins (P) to carbohydrates (C) of **1P:1C**. There are **different food items** in the environment, and these items vary with regard to their **nutritional content**, for example item A has 3 units protein and 1 unit carbohydrates (= 3P:1C), whereas another item B has 1 unit proteins and 3 units carbohydrates (= 1P:3C). Thus, **neither item will allow an animal to stay close to their target ratio of 1P:1C** and they have to feed from both items. In other words, after each feeding event, the ratio of consumed nutrients will deviate from the target ratio in one or another direction. This affects which food item should be eaten next. However, in our simulation (and in nature), different items can vary with regard to their **availability**, it is thereby ‘easier’ for animals to consume more abundant items. This means that which item is selected in each feeding event depends on *(i)* how much the ratio of consumed nutrients would deviate from the target ratio after eating that item and *(ii)* the availability of different food items.

This simulation is done using the function **`geometric_framework()`**, for which the code is provided below. For each individual, the simulation runs through *n* feeding events. For each feeding event, the function calculates the cumulative units of each nutrient that would be consumed if a particular food item was eaten, and how much the new nutrient ratio would deviate from the target ratio. Based on this deviation and the availability of the item, the function calculates the relative probability of consuming each item, which is then used to determine which item is actually consumed (more details are provided below). The cumulative nutrient units consumed are updated for that individual after each feeding event.

This simulation is done for a given number of individuals, which currently all have the same nutritional target. Furthermore, all individuals can feed on the same food items, each item with a defined content of different nutrients. However, we simulate inter-individual variability in the availability of food items (subjective food availability) to mimic natural variation between individuals due to differences in feeding position, cognitive capacities (i.e., skills to find and utilize specific items) and other factors. This variability is realized using a beta-distribution. The function can be used to simulate different kinds of scenarios to explore how animals would move through their nutritional landscapes.

Currently, some options are still missing but we will implement them at a later stage:

1.  Assigning potentially harmful components that an animal wants to avoid.

2.  Simulating the expenditure of resources while foraging.

3.  Defining more than two different nutrients per item.

4.  Adding a “consumption effect”, thus a decrease in availability if an item was consumed.

Most of these options should be straightforward to implement the way we set up the function.

## Calculating the deviation from optimal nutrient ratio and probability of selecting food items

As described above, for each potentially consumed food item, the deviation from the optimal ratio of consumed nutrients has to be calculated. This however, comes with one challenge that is illustrated in <a href="#fig:fig-dev-optimal">2</a>. Consider a simple example with only two nutrients, a and b, where an animal has an optimal ratio of 1a:1b for these nutrients. If the animal has the option between an item with 3a:1b and another item with 1a:3b, the deviations resulting from consuming each item should be symmetrical, with 3 times more of nutrient a in one case and 3 times more of nutrient b in the other. However, differences between the slope of the original/optimal line (s0 = 1) and the slopes of the two new lines (s1 = 3 for Item 1 and s2 = 1/3 for Item 2) would be 3 - 1 = 2 and 1/3 - 1 = -2/3, respectively.

<div class="figure">

<img src="{{< blogdown/postref >}}index_files/figure-html/fig-dev-optimal-1.png" alt="Potential deviation from optimal nutrient ratio when consuming different food items." width="672" />
<p class="caption">
<span id="fig:fig-dev-optimal"></span>Figure 2: Potential deviation from optimal nutrient ratio when consuming different food items.
</p>

</div>

To overcome the challenge explained above, we can use the magnitudes of differences between the optimal slope `\(s\\{optimal} =\frac{optimal{Nutrient1}}{optimal{Nutrient2}}\)` and the potential new slope that arises when consuming a particular item `\(s\\{new} =\frac{potential{Nutrient1}}{potential{Nutrient2}}\)` . This can be done by using the log of the ratio between the new slope and the original slopes `\(log(\frac{s\\{new}}{s\\{optimal}})\)` . Using this approach, the deviations become symmetrical and can be compared, as illustrated in @fig-log-for-dev.

``` r
snew <- c(1/6:1, 2:6)
s_opt_1 <- 1
s_opt_2 <- 0.5
plot(x = log(snew), y = log(snew/s_opt_1),
     main = "For optimal ratio of 1a:1b",
     xaxt = "n",
     xlab = "New ratio",
     ylab = "Deviation: log(s_new/s_optimal)")
axis(side = 1, at = log(snew), labels = round(snew, 2),
     las = 2)
# Line for deviations
lines(x = log(c(0.5, 1, 1, 1, 2)),
            y = log(c(0.5, 0.5, 1, 2, 2)/s_opt_1),
            lty = 1, col = "red")
# Optimal line
lines(x = c(log(1), log(1)), y = c(log(min(snew)/1), log(max(snew)/1)),
      lty = 2, col = "grey20")
```

<div class="figure">

<img src="{{< blogdown/postref >}}index_files/figure-html/fig-log-for-dev-1.png" alt="Potential deviation from optimal nutrient ratio when consuming different food items illustrated as magnitude of slope differences." width="672" />
<p class="caption">
<span id="fig:fig-log-for-dev"></span>Figure 3: Potential deviation from optimal nutrient ratio when consuming different food items illustrated as magnitude of slope differences.
</p>

</div>

The x-axis indicates the new ratio after consuming a food item with the vertical dashed line showing the optimal ratio. The y-axis illustrates the deviation calculated as described above. By using this approach the absolute value of the deviation between 1 (the optimal ratio) and the new ratio 1:2 (0.5) is the same as between the optimal ratio of 1 and the new ratio of 2:1 (2).

To put this into a formula, the deviation from the optimal ratio after consuming item *k* by individual *j* in feeding event *i* is:

$$
deviation\\{i, j, k} = log(\frac{slope\\{optimal}}{slope\\{i, j, k}})
$$

Here, *s**l**o**p**e*<sub>*i*, *j*, *k*</sub> is the slope of the line between 0 and consumed units of nutrients after the feeding event *i*.

It is important to note that the change in this deviation after each new food item becomes less severe if an individual has already consumed many items. That’s because we are using the cumulative amount of consumed nutrients which decreases the *relative* importance of each new item for the ratio of already consumed nutrients. In other words, an individual becomes less picky if it has already consumed many nutrients because each new item would only have little impact on the ratio of consumed nutrients.

We then define the probability of choosing item *k* by individual *j* in feeding event *i* (*P*<sub>*i*, *j*, *k*</sub>) as:

$$
P\\{i, j, k} = (1 - (\frac{deviation\\{i, j, k}}{threshold})^2) \\ availabilty\\{i,k}
$$

Here, *t**h**r**e**s**h**o**l**d* is the maximum tolerable deviation of the nutritional ratio compared to the optimal ratio before an individual stops feeding on a particular item. And \*a**v**a**i**l**a**b**i**l**i**t\*\*y<sub>i*,* k</sub> is the availability of food item i\* for individual *k*. Note that these are relative probabilities (or weights) calculated for each food item (per individual and feeding event) and they don’t sum up to 1 for each potential feeding event. In other words, if the deviation from the optimal value increases, the probability of selecting this specific food item decreases. The multiplication term for food availability `\((1 - (\frac{deviation\\{i, j, k}}{threshold})^2)\)` is illustrated in @fig-threshold with different threshold values.

``` r
par(mfrow = c(1,2))
curve((1 - (x/0.5)^2), from = -0.5, to = 0.5,
      main = "Threshold = 0.5",
      xlab = "Deviation from optimal nutrient value",
      ylab = "Multiplicator for food availability")
curve((1 - (x/1)^2), from = -1, to = 1,
      main = "Threshold = 1",
      xlab = "Deviation from optimal nutrient value",
      ylab = "Multiplicator for food availability")
```

<div class="figure">

<img src="{{< blogdown/postref >}}index_files/figure-html/fig-threshold-1.png" alt="Weight of multiplication term for food availability with different threshold values." width="864" />
<p class="caption">
<span id="fig:fig-threshold"></span>Figure 4: Weight of multiplication term for food availability with different threshold values.
</p>

</div>

## Define the function

The function below to simulate animals following Geometric Framework Models follows the logic explained above.

``` r
geometric_framework <- function(items_nutrients = list(c(3,1), c(1,3), c(1,1)),
                                items_availability = c(0.8, 0.8, 0.8),
                                optimal_ratio = c(2,1),
                                threshold_deviation = 1,
                                n_feeding_events = 100,
                                n_individuals = 10,
                                initial_nutrients = c(0,0),
                                return_class = c("list", "df")) {
  
  # If no return class is specified, use first type in vector (here a list)  
  return_class <- match.arg(return_class)
  
  ## Do some checks
  if (length(items_nutrients) != length(items_availability))
    stop("Length of items_nutrients and items_availability differ.
         Provide nutrients and availability for all items included.")
  if(any(lapply(items_nutrients, length) != 2)) stop("Currently, only two types of nutrients per item allowed.")
  if(any(lapply(items_availability, length) != 1)) stop("Provide one availability value per item.")
  
  
  ## Define Beta density function parameterized with "prob" and "theta" for adding
  # "noise" to food availability. Taken from
  # https://rdrr.io/github/rmcelreath/rethinking/src/R/distributions.r
  rbeta2 <- function( n , prob , theta ) {
    a <- prob * theta
    b <- (1-prob) * theta
    rbeta( n , shape1 = a , shape2 = b)
  }
  
  ## Initialize Variables and Objects
  
  # Prior to the first feeding event, animals current nutrients are the set to initial_nutrients
  current_nutrients_per_individual <- rep(list(initial_nutrients), n_individuals)
  
  # Set up helper variables and vector for probabilities to choose each item
  n_nutrients <- length(items_nutrients[[1]])
  n_items <- length(items_nutrients)
  probability_weight_choosing_item <- rep(NA, n_items)
  
  # Set up matrix for items consumed with feeding events as rows and individuals as columns
  items_consumed <- matrix(NA, nrow = n_feeding_events, ncol = n_individuals)
  
  # Set up array for consumed nutrients with feeding events as rows (first
  # index), individuals as columns (second index), and the different nutrients in
  # the third dimension (third index)
  nutrients_consumed <- array(NA, dim = c(n_feeding_events, n_individuals, n_nutrients))
  
  dimnames(nutrients_consumed) <-  list( 1:n_feeding_events,
                                         1:n_individuals,
                                         1:n_nutrients)
  
  # Save parameters as attributes
  attr(nutrients_consumed, "items_nutrients") <- items_nutrients
  attr(nutrients_consumed, "items_availability") <- items_availability
  attr(nutrients_consumed, "optimal_ratio") <- optimal_ratio
  
  
  # For each item, add inter-individual variability in food availability using the above defined function rbeta2
  availability_per_individual <- t(sapply(items_availability, function(x)
    rbeta2(n = n_individuals, prob = x, theta = 500)))
  
  ## Apply Geometric Framework assumptions to select which food item to include in diet
  
  # Go though all feeding events...
  for (event_i in 1:n_feeding_events) { 
    
    #... and for each feeding event, through all individuals...
    for (individual_j in 1:n_individuals) { 
      
       # ... and for each individual through all items.
      for (item_k in 1:n_items) {
        
  # First, determine how nutrients would look like if item_k is added to current nutrients
    potential_nutrients <- current_nutrients_per_individual[[individual_j]] + items_nutrients[[item_k]]
        
  # Transform optimal ratio and potential ratio from vector to numeric value
    optimal_ratio_numeric <- optimal_ratio[1] / optimal_ratio[2]
    potential_nutrient_ratio_numeric <- potential_nutrients[1] / potential_nutrients[2]

  
  # Calculate the deviation from the optimal nutrient acquisition if item_k was consumed 
    deviation_from_optimal_nutrients <- log(optimal_ratio_numeric/ potential_nutrient_ratio_numeric)
        
  # Set probability to select item_k according to how much the nutrients
  # deviate from the optimal amount.  Note that the probabilities
  # for all different items don't have to sum up to 1 to be
  # used in `sample()`, they are rather considered as relative weights.
        
  # If deviation > threshold, set value to 0
    if(abs(deviation_from_optimal_nutrients) > threshold_deviation){
          probability_weight_choosing_item[item_k] <- 0
    } else {
     # Explanation in text, but in short: if deviation is 0, just use food
     # availability as probability. With increasing deviation, put a larger
     # penalty on item and decrease probability accordingly.The severity of 
     # the penalty is dependent on the threshold of maximum "allowed" deviation 
      probability_weight_choosing_item[item_k] <- (1 - (deviation_from_optimal_nutrients/threshold_deviation)^2) *
      availability_per_individual[[item_k, individual_j]]
     }
        
      }
      
  # Based on probability/weight, select one of the food items 
      item_selected <- sample(x = 1:n_items, size = 1, prob = probability_weight_choosing_item)
  
  # Add the nutrients of the selected item to current nutrients
      current_nutrients_per_individual[[individual_j]] <- current_nutrients_per_individual[[individual_j]] +
        items_nutrients[[item_selected]]
      
  # Assign new current nutrient values to cells in array
      nutrients_consumed[event_i, individual_j, ] <- current_nutrients_per_individual[[individual_j]]
      
  # Store information about which item was included in the diet in a matrix   
      items_consumed[event_i, individual_j] <- item_selected
      
    }
  }
  
  # Finally, return object according to return_class
  if(return_class == "list"){
    
    geom_frame_list <- list(nutrients_consumed, items_consumed)
    names(geom_frame_list) <- c("Consumed Nutrient Units", "Items in Diet")
    
    return(geom_frame_list)
    
  }else if(return_class == "df"){
    
    nutrients_consumed_df <- as.data.frame(as.table(nutrients_consumed[,,1]))
    names(nutrients_consumed_df) <- c("Feeding_event", "Individual", "Nutrient1")
    nutrients_consumed_df$Nutrient2 <- as.vector(nutrients_consumed[,,2])
    nutrients_consumed_df$Item_in_diet <- as.data.frame(as.table(items_consumed))[[3]]
    
    return(nutrients_consumed_df)
  }
}
```

## Running the simulation for different scenarios

We test our function in a simple example. In a group of 10 individuals, each individual has the nutritional target of obtaining protein and carbohydrates in a 2P : 1C ratio. The three available food items in the environment have the following nutrient composition:

- item 1’s nutritional composition is 3P:1C

- item 2’s nutritional composition is 1P:3C

- item 3’s nutritional composition is 1P:1C

To explore how the nutritional acquisition happens depending on limited resources, the individuals are “provided” with these three items in varying levels of availability in 3 different scenarios:

1.  Scenario: All items are equally scarce.

2.  Scenario: Only item 1 (3P:1C) is highly abundant, the remaining items are scarce.

3.  Scenario: Only item 2 (1P:3C) is highly abundant, the remaining items are scarce.

### Preparation to run the simulation

``` r
## setup for function
items_nutrients <- list(c(3,1), c(1,3), c(1,1))
optimal_ratio <- c(2,1)
threshold_deviation <- 1
n_feeding_events <- 150
n_individuals <- 10
initial_nutrients <-  c(0,0)
return_class <- "df"

## setup for plotting
library(ggplot2)
theme_set(theme_minimal())
optimal_carbohydrates <- 1:200
optimal_protein <- 2* optimal_carbohydrates 
```

### Scenario 1: All items equally scarce

In this scenario we set each item to the same, scarce (0.2) availability value.

``` r
items_availability_scenario1 <- c(0.2, 0.2, 0.2)

gf_df1 <- geometric_framework(items_nutrients = items_nutrients,
                              items_availability = items_availability_scenario1,
                              optimal_ratio = optimal_ratio,
                              threshold_deviation = threshold_deviation,
                              n_feeding_events = n_feeding_events,
                              n_individuals = n_individuals,
                              initial_nutrients = initial_nutrients,
                              return_class = return_class)
  
  
head(gf_df1)
```

    ##   Feeding_event Individual Nutrient1 Nutrient2 Item_in_diet
    ## 1             1          1         3         1            1
    ## 2             2          1         4         4            2
    ## 3             3          1         7         5            1
    ## 4             4          1         8         8            2
    ## 5             5          1        11         9            1
    ## 6             6          1        12        12            2

We illustrate the nutrient acquisition over feeding events with equally highly available items in the individuals environment.

``` r
ggplot() +
  geom_point(aes(x = Nutrient1, y = Nutrient2, color = Individual),
             alpha = 0.7, data = gf_df1) +
  geom_line(aes(x = optimal_protein, y = optimal_carbohydrates)) +
  ylim(0, 200) +
  theme(legend.position = "none") +
  labs(title = "Nutrient acquisition when all items are equally scarce",
       x = "Units Proteins",
       y = "Units Carbohydrates") 
```

<div class="figure">

<img src="{{< blogdown/postref >}}index_files/figure-html/fig-acq-sc1-1.png" alt="Scenario 1 (items are equally scare) nutrient acquisition. The plot shows the acquisition of proteins on the x-axis in relation to the acquisition of carbohydrates on the y-axis over feeding events. The individuals nutrient acquisition is highlighted as dots. Different colors distinguish the simulated individuals. The nutritional goal of the individuals is highlighted as black line" width="672" />
<p class="caption">
<span id="fig:fig-acq-sc1"></span>Figure 5: Scenario 1 (items are equally scare) nutrient acquisition. The plot shows the acquisition of proteins on the x-axis in relation to the acquisition of carbohydrates on the y-axis over feeding events. The individuals nutrient acquisition is highlighted as dots. Different colors distinguish the simulated individuals. The nutritional goal of the individuals is highlighted as black line
</p>

</div>

@fig-acq-sc1 illustrates that the cumulative nutrient acquisition of our simulated individuals deviates from the target ratio when all items are equally scarce in the environment. The nutritional goal of our simulated animals is to consume 2P:1C. We included two items (item 2 with 1P:3C and item 3 with 1P:1C) that would lead to a higher carbohydrate consumption compared to the nutrient target, and one item (item 1 with a 3P:1C) that would lead to a higher protein consumption compared to the nutrient target.

When animals experience food scarcity they are not able to maintain their target ratio, but still consume more units proteins than expected by the relative availability. Especially in the first few feeding events, when the total nutrient acquisition is still low, they stay closer to the target ratio compared to later feeding events when already many nutrients have been consumed. Individuals become less selective the more total nutrients they have already consumed.

### Scenario 2: Only item 1 abundant

In this scenario we set Item 1 (3P:1C) to a high availability of 0.8, whereas item 2 (1P:3C) and item 3 (1P:1C) are both set to a low availability of 0.2.

``` r
items_availability_scenario2 <- c(0.8, 0.2, 0.2)

gf_df2 <- geometric_framework(items_nutrients = items_nutrients,
                              items_availability = items_availability_scenario2,
                              optimal_ratio = optimal_ratio,
                              threshold_deviation = threshold_deviation,
                              n_feeding_events = n_feeding_events,
                              n_individuals = n_individuals,
                              initial_nutrients = initial_nutrients,
                              return_class = return_class)

  
head(gf_df2)
```

    ##   Feeding_event Individual Nutrient1 Nutrient2 Item_in_diet
    ## 1             1          1         3         1            1
    ## 2             2          1         6         2            1
    ## 3             3          1         7         3            3
    ## 4             4          1         8         6            2
    ## 5             5          1        11         7            1
    ## 6             6          1        14         8            1

We illustrate the nutrient acquisition over feeding events with only item 1, which has a high protein content and is abundant in the individuals’ environment. The remaining items are scarce.

``` r
ggplot() +
  geom_point(aes(x = Nutrient1, y = Nutrient2, color = Individual),
             alpha = 0.7, data = gf_df2) +
  geom_line(aes(x = optimal_protein, y = optimal_carbohydrates)) +
    ylim(0, 200) +
  theme(legend.position = "none") +
  labs(title = "Nutrient acquisition when only item 1 (3P:1C) is abundant",
       x = "Units Proteins",
       y = "Units Carbohydrates")
```

<div class="figure">

<img src="{{< blogdown/postref >}}index_files/figure-html/fig-acq-sc2-1.png" alt="Scenario 2 (only item 1 (3P:1C) is abundant) nutrient acquisition. The plot shows the acquisition of proteins on the x-axis in relation to the acquisition of carbohydrates on the y-axis over feeding events. The individuals nutrient acquisition is highlighted as dots. Different colors distinguish the simulated individuals. The nutritional goal of the individuals is highlighted as black line" width="672" />
<p class="caption">
<span id="fig:fig-acq-sc2"></span>Figure 6: Scenario 2 (only item 1 (3P:1C) is abundant) nutrient acquisition. The plot shows the acquisition of proteins on the x-axis in relation to the acquisition of carbohydrates on the y-axis over feeding events. The individuals nutrient acquisition is highlighted as dots. Different colors distinguish the simulated individuals. The nutritional goal of the individuals is highlighted as black line
</p>

</div>

@fig-acq-sc2 illustrates that the cumulative nutrient acquisition of our simulated individuals deviates from the target ratio when only item 1, which provides the highest ratio of proteins compared to carbohydrates (3P:1C), is abundant in the environment. The nutritional goal of our simulated animals is to consume proteins and carbohydrates in a 2 to 1 ratio. We included two items (item 2 with a 1P:3C and item 3 with 1P:1C) that would lead to a higher carbohydrate consumption compared to the nutrient target, and one item (item 1 with a 3P:1C ) that would lead to a higher protein consumption compared to the nutrient target.

If animals are provided with enough protein sources (item 1 abundant) they maintain their target ratio. Especially in the first few feeding events, when the total nutrient acquisition is still low, staying close to the target ratio is more important than in later feeding events when already many nutrients have been consumed. As before, individuals become less picky the more they have consumed.

### Scenario 3: Only item 2 abundant

In this scenario we set Item 2 (1P:3C) to a high availability of 0.8, whereas item 1 (3P:1C) and item 3 (1P:1C) are both set to a low availability of 0.2.

``` r
items_availability_scenario3 <- c(0.2, 0.8, 0.2)

gf_df3 <- geometric_framework(items_nutrients = items_nutrients,
                              items_availability = items_availability_scenario3,
                              optimal_ratio = optimal_ratio,
                              threshold_deviation = threshold_deviation,
                              n_feeding_events = n_feeding_events,
                              n_individuals = n_individuals,
                              initial_nutrients = initial_nutrients,
                              return_class = return_class)

head(gf_df3)
```

    ##   Feeding_event Individual Nutrient1 Nutrient2 Item_in_diet
    ## 1             1          1         1         1            3
    ## 2             2          1         4         2            1
    ## 3             3          1         5         5            2
    ## 4             4          1         6         6            3
    ## 5             5          1         7         9            2
    ## 6             6          1        10        10            1

We illustrate the nutrient acquisition over feeding events with only item 2, which has a high carbohydrate content and is abundant in the individuals’ environment. The remaining items are scarce.

``` r
ggplot() +
  geom_point(aes(x = Nutrient1, y = Nutrient2, color = Individual),
             alpha = 0.7, data = gf_df3) +
  geom_line(aes(x = optimal_protein, y = optimal_carbohydrates)) +
    ylim(0, 200) +
  theme(legend.position = "none") +
  labs(title = "Nutrient acquisition when only item 2 (1P:3C) is abundant",
       x = "Units Proteins",
       y = "Units Carbohydrates")
```

<div class="figure">

<img src="{{< blogdown/postref >}}index_files/figure-html/fig-acq-sc3-1.png" alt="Scenario 3 (only item 2 (1P:3C) is abundant) nutrient acquisition. The plot shows the acquisition of proteins on the x-axis in relation to the acquisition of carbohydrates on the y-axis over feeding events. The individuals nutrient acquisition is highlighted as dots. Different colors distinguish the simulated individuals. The nutritional goal of the individuals is highlighted as black line" width="672" />
<p class="caption">
<span id="fig:fig-acq-sc3"></span>Figure 7: Scenario 3 (only item 2 (1P:3C) is abundant) nutrient acquisition. The plot shows the acquisition of proteins on the x-axis in relation to the acquisition of carbohydrates on the y-axis over feeding events. The individuals nutrient acquisition is highlighted as dots. Different colors distinguish the simulated individuals. The nutritional goal of the individuals is highlighted as black line
</p>

</div>

@fig-acq-sc3 illustrates that the cumulative nutrient acquisition of our simulated individuals deviates from the target ratio when only item 1, which provides the highest ratio of proteins compared to carbohydrates, is abundant in the environment.The nutritional goal of our simulated animals is to consume proteins and carbohydrates in a 2 to 1 ratio. We included two items (item 2 with a 1P:3C and item 3 with 1P:1C) that would lead to a higher carbohydrate consumption compared to the nutrient target, and one item (item 1 with 3P:1C) that would lead to a higher protein consumption compared to the nutrient target.

If animals are limited in their protein source (item 1 scarce) and primarily encounter carbohydrates while moving through their nutritional landscape (only item 2 abundant) they can’t maintain their target ratio and over consume carbohydrates.

## Summary

In this example, we only included 3 items and simulated animals that aim to follow a nutritional ratio of 2P:1C. We included two items (item 2 with 1P:3C and item 3 with 1P:1C ) that would lead to a higher carbohydrate consumption compared to the nutrient target, and one item (item 1 with 3P:1C) that would lead to a higher protein consumption compared to the nutrient target. Under our set conditions, the animals can only maintain their target ratio when item 1 is more abundant than the other items (Scenario 2). The animals deviate from the target ratio when there is protein scarcity (Scenario 1) and especially when they primarily encounter items with different nutritional signatures, they may reach their limiting deviation to the optimal nutrient ratio (Scenario 3), while moving through their nutritional landscape.

In **Part 2** we explore how well commonly used preference measures (namely electivity indices) can capture the preference of animals following Geometric Framework Models for different food items and how preference might change depending on resource limitations.

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-lambert2015" class="csl-entry">

Lambert, Joanna E., and Jessica M. Rothman. 2015. “Fallback Foods, Optimal Diets, and Nutritional Targets: Primate Responses to Varying Food Availability and Quality.” *Annual Review of Anthropology* 44 (1): 493–512. <https://doi.org/10.1146/annurev-anthro-102313-025928>.

</div>

<div id="ref-manly2007" class="csl-entry">

Manly, Bryan F J, Lyman L McDonald, Dana L Thomas, Trent L McDonald, and Wallace P Erickson. 2007. “Resource Selection by Animals,” 231.

</div>

<div id="ref-simpson2011" class="csl-entry">

Simpson, Stephen J., and David Raubenheimer. 2011. “The Nature of Nutrition: A Unifying Framework.” *Australian Journal of Zoology* 59 (6): 350. <https://doi.org/10.1071/ZO11068>.

</div>

</div>
