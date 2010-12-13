#A method for obtaining 2 year seasonal volume forecasts


- q = length of years leading up 
- p = length of years after

New ideas to not suck:

- For each specific transition, fit a model (optimized locfit) to the from and
  to data. Not quite sure yet how to deal with multiple to years, maybe
  vectorized glm. Need to abstract the model building for each from and to 
  states.
  
- There is evidence that conditioning on transitions reveals some 
  statistically significant information in trasitioning to both the first and 
  the second year.  Need to think of how to combine this with the existing knn 
  or just replace?
  
- The existing method very accurately predicts the current year magnitude from
  the current year state and magnitude feature vector. This is not necessarily 
  a trivial result, it means that using only the current conditional pools and 
  nearest neighbor criteria, sampled from years tend to be similar to the last 
  year in the from years. *What else can be made from this?*