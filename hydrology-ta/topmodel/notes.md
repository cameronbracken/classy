## Some Language of Mathematical Modeling 

__Simulation Model__
: Abstraction of a real-world system to a conceptual model 

__Conceptual Model__
: A series of mathematical and logical relationships concerning components and structure of a system
: e.g. Linear relationship between elevation and precipitation

__Operational Model (the model)__
: Implementation of a conceptual model
: e.g. The R code that carries out the linear regression

__Model Parameter__
: Quantity that relates unknown functions and variables
: eg. albedo of snow

__Lumped Parameter__
: Constant with respect to independent variables (usually space or time)
: eg. albedo of snow 

__Distributed Parameter__
: Non-Constant with respect to independent variables
: eg. albedo of snow ;)

__Calibration__
: Process of obtaining an optimal set of model parameters.  An optimal parameter set minimizes observed - predicted values.

__Verification__
: Process of determining weather an implementation of a conceptual model is correct
: Requires data known as training set

__Validation__
: Process of determining if a conceptual model is a good representation of a real-world system
: Requires data other than training set

__Sensitivity Analysis__
: Process of varying model parameters to asses the effect on model output

__Monte Carlo Simulation__
: Process of randomly varying model input (according to known probability distributions) 
: Used for calibration and for generating ensembles

## Some Language of Hydrologic Modeling

__Watershed__
: A contiguous area such that net rainfall or runoff over that area will contribute to runoff at the outlet.  Flow runs perpendicular to elevation contours. 

__Hydrologic Response__
: Amount of surface runoff generated within a watershed that becomes streamflow for a given input rainfall pattern. 

__Hydrograph__
: Plot of flow rate versus time for a given location within a stream. Main Hydrologic response function.
: Represents integrated effects of climate, hydrologic losses, surface runoff and baseflow  
: Characteristics affecting hydrologic response: size, shape, slope, soil type, storage and land use
: Common units: cms, cfs, ac-ft/month
: V = ∫Q(t)dt

__Hydrologic loss__
: Water that does not directly contribute to runoff 

__Rainfall Runoff Model__
: Predictive relationship between amount of surface runoff generated in a watershed and a given rainfall pattern

__Nash-Sutcliff Model Efficiency__
: Measures how well a hydrologic model represents an observed hydrograph
: Ranges from (-infty, 1]
: E = 1 - Perfect match between model and data
: E = 0 - Model predicts mean of observed data
: E < 0 - Model is worse than observed mean

            T                2
    E = 1 - ∑ (Q_o,t - Q_m,t)
           t=1  
            ----------------
            T          _   2
            ∑ (Q_o,t - Q_m)
           t=1

## TopModel

- (Beven and Kirkby, 1979)
- Variable Contributing Area 
	- Some areas may infiltrate, store or immediately contribute rainfall to runoff

### Components
- Surface interception and depression storage, S1 and max storage SD.  When S1 > SD, infiltration takes place to S2.
- S2 has constant leakage to S3, i0
 	- Case 1: S1 is saturated, input takes place at rainfall rate i. 
	- Case 2: rainfall greater than max infiltration,  rainfall directly runs off.  
	- Case 3: S2 = SC, rainfall directly runs off
- S2 has returns flow using the relationship qb = q0 exp(S3/m)

### R Package 'topmodel'
- Interface to 