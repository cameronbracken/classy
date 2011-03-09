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
      - map scale factor determins magnitude of distorition
    - Useful to display output on a geographic map
    - changes physical equations
    - composite grids that interact can be used
  - Latitude-longitude grids
    - no issues with distortion
    - large density of points near poles, small time steps everywhere
    - singularity at poles
    - can use reduced grid near poles
  - spherical geodesic grid 
    - divide into triangles and project
    - not perfectly uniform vertecies
    - uniform distribution of points
    - no nice rows and columns
    - can use voronoi cells
  - differential grid resolution 
    - nesting to capture local features of interest
    - easy to code
    - waves can distort moving between grid resolution 
    - may require different physics
    - parasitic nesting
    - numerical problems with differential vertial/horiz res
    - also need some dz consistancy
- Spectral methods
  - sub analyic functions for state variables
  - derivatives calculated analytically
  - must truncate infinite series (truncation error)
    - triangular or rhomboidal truncation 
    - defines resolution
  - nonlinear terms are too computationally demanding
  - many physical processes can only be simulated in physical space
  - pseudo spectral methods do both
    - spectral transformation method 
  - no nonlinear instability, or computational diffusion
  - do not exactly conserve mass and energy

### Finite difference
- Time differencing 
  - explicit
    - cfl applies
    - both single and multi-step methods
  - split explicit
    - different time steps for different variables
  - single step methods
  - multi step methods
    - predictor and corrector step 
    - better numerical properties 
    - euler, runge-kutta
  - implicit
    - no cfl restrictions
  - semi-implicit
    - implicit for some variables, explicit for others
  - unconditionally stable, conditionally stable, absolutely unstable 
- Space differencing
  - Eulerian 
    - fixed grid points
  - Lagrangian 
    - points move with parcels 
    - must interpolate parcels after time, due to divergence/convergence
  - Grid staggering
    - all variables not defined at same points

### Effects of numerical approximations
- Truncation error
  - errors from not including all terms in differencing
  - first order accuracy, only include first order terms
  - forward in space 
  - centered in space df(a)/dx = (f(x+a)-f(x-a))/2dx
    - second order accuracy
  - compare error by comparing to analytical value
  - every calculation involves error 
  - can fuck up pressure gradient forces
- Linear stability and damping 
  - stable - amplitude unaffected or damps 
  - unstable - amplitude grows exponentially
  - damping depends on courant number, strongest near .5
- Phase/Group speed errors
  - phase speed speed of a wave
  - group speed speed of a wave packet
  - for Cp < U phase speed is too high 
  - for Cp > U phase speed is too low
  - numerical dispersion causes phase speed dependence on wavelength
    - non physical 
  - Waves should move at U the group speed
  - poorly resolved waves move slower
  - shortest waves move backwards
  - larger waves are better at higher cfl numbers
  - higher order schemes have better properties
- Aliasing
  - nonlinear interaction between two waves
  - two waves m+n if not resolvable, create folding of energy to lower wave #
  - energy locates at wrong scales
  - can cause nonlinear instability
    - most energy growth is at small scales
  - does not happen in spectral models
  - use a diffusion term to mitigate
- Diffusion
  - Physical diffusion
    - turbulence reduced gradients and smoothes out maxima and minima
  - Explicit numerical diffusion
    - artificial term to selectively damp out short wavelengths
    - prevents instability 
    - may damage solution
  - Implicit numerical diffusion
    - damping associated with finite difference scheme
  - Grid diffusion 
    - spread of information due to influence of ajacent points 
    - non-physical and may be rapid
- Vertical coordinate
  - height above sea level, pressure, potential temp, sigma-p
  - sigma, terrain following
- Time smoothers
  - control separation of solution due to centered in time calculations 

### Lateral Boundary conditions
- Properties
  - Features should propagate from larger scales 
  - gravity waves should pass through boundary and not reflect
  - should have no artificial feedback
- Sources of error 
  - Poor resolution of lbc data
  - errors in meteorology
  - lack of feedback
  - noise from LBC
  - inconsistant parameterizations 
  - phase/group speed contrasts
- Types 
  - Forecasts/re-anlysis
  - two way interaction/parasitic nesting

## Chapter 4 

- paramterization - represent process through statistical or algorithmic rel
- Why?
  - Computationally expensive due to complexity
  - insufficient knowlegdge of process
  - too small scale
- What?
  - Land surface process, cloud microphysics, rad, turbulent flux, cumulus 
- Grid increments
  - only need to prameterize processes which cannot be resolved
  - must be careful of double counting
- Cloud microphysics
  - rep processes on scale of cloud droplets
  - affects precip 
  - separate convective and cloud precip
  - variables ust be spun up
- cumulus convection 
  - 1-2 km is needed
  - many kinds, all necessary due to model resolution
- Turbulence 
  - aka bounday layer parameterization 
  - vertical mixing of heat, moisture and momentum
  - also anywhere in atmosphere, jet stream
- Radiation
  - solar, use sun position 
- cloud cover 
  - may occur partially when RH < 100%
  

  
  
  
  
  
  
  
  
  
  
  
  
  