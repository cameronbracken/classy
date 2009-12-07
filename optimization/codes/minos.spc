BEGIN
    MAXAMIZE
    NONLINEAR VARIABLES 10
    DERIVATIVE LEVEL 0 
*    UPPER BOUND 100
END
NAME          LSO
ROWS                 
 G  DEM     
COLUMNS                 
    Q1        DEM       1           
    Q2        DEM       1   
    Q3        DEM       1   
    Q4        DEM       1   
    Q5        DEM       1   
    Q6        DEM       1   
    Q7        DEM       1   
    Q8        DEM       1   
    Q9        DEM       1   
    Q10       DEM       1      
RHS 
    RHS1      DEM       1
BOUNDS  
 UP BND1      Q1        1
 UP BND1      Q2        1
 UP BND1      Q3        1
 UP BND1      Q4        1
 UP BND1      Q5        1
 UP BND1      Q6        1
 UP BND1      Q7        1
 UP BND1      Q8        1
 UP BND1      Q9        1
 UP BND1      Q10       1
 FX INITIAL   Q1       .05
 FX INITIAL   Q10       .05
ENDATA
