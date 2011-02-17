# Exam 1 Notes

## Chapter 1

- Research is composed of Observations, Theory and Models
- 1904 - First description of atmosphere 7 equations and 7 unknowns
- Richardson
  - Father of nwp 
  - finite diff by hand to integrate eq of motion
  - failed because no geostrophic balance between mass and motion
- Von Neuman
  - Electronic computing project 
- 1950 - first NWP model on computer

## Chapter 2
- 7 Basic Equations 
  - Wind velocity, Temperature, density, heat, pressure
- Reynolds equations
  - Break state variables into mean and fluxuating components
  - Modify equations to apply to only large scale (non-turbulent) processes
  - Results in a covariance term that represents "turbulent" heat flux
  - Usually ignore molecular viscosity since it is small compared to eddy v
- Approximations
  - Hydrostatic
    - small vertical acceleration 
    - relates pressure and density field 
    - applicable only for synoptic and global scales 
  - Boussinesq and anelastic 
    - filter sound waves but tie pressure and density
    - can be used for high resolution
  - Shallow fluid
    - useful simplification for testing
    - incompressible, hydrostatic, 
    - barotropic (no horizontal variation in vertical pg)
    - usually written in 1-d in terms of a mean east-west flow

## Chapter 3 

### Basic Concepts

- space derivatives
  - Grid point methods estimate at descrete points
  - spectral methods estimate state variables by analytic functions 
  - choose grid increment to represent smallest feature
    - 10 grid points to a wave
- Time derivatives 
  - estimate with finite differences
  - limit based on numerical stability
    - CFL <= 1
    - adaptive
  - centered difference is the leap frog 
- Computational requirements
  - Depend on grid increment/num points
  - time step depends on grid increment
- Boundary Conditions
  - Lateral boundary conditions (LBC)
    - needed for local area models
    - use previous global forecast or observations, data assimilation
    - use old forecasts in tests of improving skill
  - Upper boundary conditions 
    - need finite limit to atmosphere
    - prevent artificial reflection of waves
  - Lower BC
    - fluxes to land and ocean surface
- Initial conditions
  - initialization - process of defining initial conditions
  - ic - initial state for a forecast
  - balancing ic - constraint to ensure the mass and wind field are realistic
  - static initilization - interpoalte ob to a grid, balance
  - cold start- ic with no vertical motion
  - dynamic initialization - uses a model in initialization
  - hot start - includes vertical motions
  - spin up - process of developing vertical motions
  - warm start - somewhere in between
- Physical process parameterizations
  - represent a process not through solving physical equations
  - clouds, turbulence, radiation, heat flux, convective precip
  - high resolution may combat this

### Numerical frameworks
- Grid point methods
  - structured grids, unstructured, adaptive grids, movable meshes
  - map projections
    - geometric/mathematical transformations properties to flat plane
    - used for x-y coordinates on a cartesian grid instead of spherical
    - Mercator
      - project to a cylinder
      - good for equator
    - Lambert conformal 
      - cone 
      - mid latitudes
    - Polar steriographic 
      - plane near pole
      - high/low latitudes
    - Secant projection
      - longitudes at which projection is true
    - tangent projection
      - image surface is tangent to sphere
    - preserving geometric properties
  
  
  
  
  
  
  
  
  
  
  
  
  