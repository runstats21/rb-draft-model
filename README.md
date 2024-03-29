# RB NFL Draft Model
### Mock NFL Draft for CFB running backs using Gradient Boosting Machine algorithm

During a 3-month Sports Data Science Course, I scraped CFB and 
NFL running back data from [sportsreference.com](https://www.sports-reference.com/), to build a replicable RB NFL Draft Model. 

<br>

This project culminated in the creation of two Stochastic Gradient Boosting Machine (GBM) models, one to predict college win percentage, and another to predict NFL average rushing yards per carry,
all based on college rushing data. The models were fit using the `gbm` library within R, with predictions translated into Z-scores to allow for aggregation of win-percentage and yard-per-carry metrics.

<br>

I then created a Shiny App to display model results, and allow for a "mock draft", with the aggregated model results ranking college running backs againsts eachother for a given season. The code to create this Shiny App using the aggregated GBM models can be found in this repository.
