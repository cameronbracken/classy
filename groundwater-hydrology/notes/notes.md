Title: Groundwater Hydrology Notes
Author: Cameron Bracken
LaTeX XSLT: memoir-twosided.xslt
Pagestyle: plain
Chapterstyle: dash

<!-- 
% !TEX TS-program = MultiMarkdown 
% !TEX encoding = UTF-8 Unicode
-->

# Groundwater Flow Theory

## Porosity, Hydrogeologic units - 24 August 2010 [lecture1]
> _Groundwater Hydrology_: The study of the occurrence, movement and distribution of groundwater and the movement of chemicals in the water

#### Objectives - 24 August 2010
- Calculate the porosity of a porous medium 
- Describe the effects on porosity of 
	- grain shape, grain size, pocketing, sorting
- Define the three main hydrologic units 
    - aquifer, aquitard, water table, confined aquifer, unconfined aquifer, capilary fringe, vadose zone

#### Porosity 

<< n = V_v/V_T >>

<< V_v >> = Volume of voids

<< V_T >> = Total volume

##### Porosity doesn't depend on

- Size of grains

##### Porosity depends on

- Shape of the grains 
- packing

#### Packing

- Cubic n=0.48
	
<!--
\includegraphics{figs/1-cubic-packing.pdf}
-->

- Rombohedral n=0.26

<!--
\includegraphics{figs/1-rom-packing.pdf}
-->

Aquifer
: Material that transmits water

Aquitard 
: Very impermeable region 

Capillary Fringe
: saturated zone where saturation is due to capillary rise where pressure is negative

Vadose 
: unsaturated zone above an unconfined aquifer

Unconfined aquifer
: free surface, friadic aquifer


##Head, Hydraulic gradient - 26 August 2010 [lecture2]

#### Objectives - 26 August 2010

1. Calculate head, elevation head, pressure head, and pressure. 
2. Calculate the hydraulic gradient
3. Use the hydraulic gradient to determine the flow direction 
4. Draw a potentiometric surface and use it to determine flow direction

#### Hydraulic head

Hydraulic head = Pressure potential energy + elevation head + kinetic energy  
Hydraulic head = Pressure head + elevation head + velocity head  

<< h = \frac{\rho}{g}+z+\frac{V^2}{2g} >>


<< p >> = pressure
	
<< \gamma >> = specific weight

<< z >> = elevation
	
<< V >> = velocity
	
<< g >> = gravitational constant


For groundwater we can ignore velocity head because the fluid moves so slowly. So

<< h = \frac{\rho}{g}+z+\frac{V^2}{2g} >>

- << h >> = Height of water in piezometer above the point of interest  
- << \frac{\rho}{g} >> = Height of water in piezometer above the point of interest
- << z >> = height of a point of interest above the datum

Water flows in a direction opposite of the gradient. 

#### Hydraulic Gradient

<< \nabla h = \frac{\partial h}{\partial x}\hat{i} + \frac{\partial h}{\partial y}\hat{j} + \frac{\partial h}{\partial z}\hat{k} >>

- vector (magnitude and direction)
- water is flowing in opposite direction of gradient

##### Horizontal gradient

<< \nabla h = \frac{\partial h}{\partial x}\hat{i} + \frac{\partial h}{\partial y}\hat{j} >>

- need three points minimum

<!--
\includegraphics{figs/2-gradient.pdf}
-->

<< \frac{\partial h}{\partial x} = \frac{h_2-h_1}{x_2-x_1} >>

<< \frac{\partial h}{\partial y} = \frac{h_2-h_1}{x_2-x_1} >>

Also can use a graphical solution.

if three points are not aligned along a coordinate systems calculate the equation of the surface between the points then calculate gradients. 

All wells must be measured at the same elevation.

##### Potentiometric Surface

- Lines of constant head
- Uniform contour increment 

##Darcy’s Law - 31 August 2010

#### Objectives - 31 August 2010
- State of Darcy's law in terms of flow rate specific discharge and pore velocity 
- Use Darcy's law to calcualte parameters in a 1-D flow column
- Use a potentiometric surface to identify regions of high and low hydraulic conductivity. 

##### Experiments 
- << Q >> goes up as << \Delta h>> goes up
- << Q >> goes up as << L >> goes up
- << Q >> goes up as << A >> goes up

<< Q >> = Flow rate 
	
<< \Delta h >> = head drop
	
<< L >> = Flow length

<< A >> = Cross sectional area

#### Darcy's Law 

<< Q \propto A \frac{\Delta h}{L}>>

<< Q = - K A \frac{dh}{dL}>>

<<\frac{dh}{dL}>> = hydraulic gradient

K - Hydraulic conductivity

Hydraulic conductivity 
: The ability of porus material to transmit fluid

The hydraulic conductivity of materials varies over many (10) orders of magnitude. 

##### Calculate K of sand

Given:
<!--
\noindent$A = 63 \mbox{cm}^2\\
h_1=5.75 \mbox{in}\\
h_2=0$
-->

<!--
\[
\frac{dh}{dL} = \frac{dh}{dz} = \frac{h_1 - h_2}{z_1-z_2} = \frac{5.75 in - 0 in}{2.5 in - 0 in} 
\]
-->

<!--
\[
Q = \frac{V}{\Delta t} = \frac{120 \mbox{cm}^3}{60 s} \approx -2\mbox{cm/s} 
\]
-->

<!--
\[
K = \frac{-Q}{A\,\frac{dh}{dL}} = \frac{-2 \mbox{cm}/\mbox{s}}{(63 \mbox{cm}^2) (2.3)} = - 0.014 \mbox{cm/s} 
\]
-->
	
#### Specific discharge

<<q = \frac{Q}{A}>>
	
<<q = -K\nabla h>>
	
<!--
\begin{equation}
\mathbf{q} = -K\left[\begin{array}{c}\partial h/\partial x\\\partial h/\partial y\\\partial h/\partial z\\\end{array}\right]= \left[\begin{array}{c}q_x\\q_y\\q_z\end{array}\right]
\end{equation}
-->

#### Pore velocity 

<<v = \frac{Q}{nA} = \frac{k}{n}\nabla h = \frac{q}{n}>>

- Also known as: Average velocity  of water molecules, average linear velocity, groundwater velocity, or just velocity 

#### 1-D Sand column

#### Potentiometric surface 

Steeper gradient indicates lower hydraulic conductivity

##Hydraulic conductivity: Heterogeneity - 2 September 2010 [lecture4]

#### Objectives - 2 September 2010

1. Explain the difference between hydraulic conductivity and permiability 
2. Develop equationsfor average K parallel and perpendicular to layering and use them to calculate average K
3. Develop an equation for flow direction across an interface between two materials and use it to calculate flow direction
4. State the approxamate flow direction inan aquitard and justify your statement

#### Hydraulic conductivity
	
<!--
\begin{equation}
K = \frac{k\rho g}{\mu}
\end{equation}
-->
	
- << \mathcal{k} >> - permeability, property of porus material (<< L^2 >>)
- << \rho >> - density
- << \mu >> - viscosity
- 1 darcy - 9.87e-9 cm2

#### Heterogeneity 

Homogeneous
: material properties do not vary with spatial position

Heterogeneous
: material properties vary with spatial position << K(x,y) >>

#### Layered system

- Depends on flow direction 
- Conductivity never lower than lowest conductivity layer and never higher than highest conductivity layer

##### Relative K

<< K_{min} > K_{\bot} > K_G < K_{||} < K_{max}>>

- << K_{min} >> - Minimum conductivity 
- << K_{\bot} >> - average conductivity fot flow perpendicular to layers
- << K_{G} >> - gemetric mean (average conductivity for mix of materials)
- << K_{||} >> - average conductivity for flow parallel to layering 
- << K_{max} >> - Maximum conductivity 

!!!!!!!!!HELLA CRAP IN NOTES!!!!!!!!!!!!!!!!!!!!

##### Perpendicualr discharge

##### Parallel discharge

##### Angled discharge

!!!!!!!!!!! See derivation in notes !!!!!!!!!!

<< \frac{K_1}{\tan\alpha_1}=\frac{K_2}{\tan\alpha_2} >>

In natural systems flow in aquifer approxamately horizontal (low vertical head gradient) and flow through an aquitard is approxamately vertical (high vertical head gradient).

##Hydraulic conductivity: Anisotropy - 7 September 2010

#### Objectives
- Define principle direction of anisotropy
- State darcy's law in three dimensions for and anisotropic medium, and explain each term
- Calculate components of the hydraulic conductivity tensor
- State the conditions that result in a diagonal << K >> tensor. 
- Describe the difference in flow direction of an isotropic  medium and an anisotropic medium for a given hydraulic gradient. 

#### Layered material

Anisotropic
: Material properties vary with direction 

Isotropic
: Material properties do not vary with direction

#### Principal Directions

- << K_{||}>> - Along direction of layer
- << K_{\bot} >> - along the direction perpendicular to layers

Principal directions of anisotropy
: direction parallel and perpendicular to layering

<< K_3 >> - anisotropy ratio

<< K_3 = \frac{K_{||}}{K_{\bot}} >>

#### Darcy's in 3D for Anisotropic Materials

<< q_x = -K_{x x} \frac{\partial h}{\partial x} -K_{xy} \frac{\partial h}{\partial y}-K_{xz} \frac{\partial h}{\partial z}>>

<< q_y = -K_{yx} \frac{\partial h}{\partial x} -K_{yy} \frac{\partial h}{\partial y}-K_{yz} \frac{\partial h}{\partial z}>>

<< q_z = -K_{zx} \frac{\partial h}{\partial x} -K_{zy} \frac{\partial h}{\partial y}-K_{zz} \frac{\partial h}{\partial z}>>

- << q_z >> - Component of flow in << z >> direction
- << K_{zx} >> - contribution of << q_z >> die to gradient in x



<!--
\[
 \left[\begin{array}{c}q_x\\q_y\\q_z\end{array}\right] =  \left[\begin{array}{ccc}
K_{xx} & K_{xy} & K_{xz}\\
K_{yx} & K_{yy} & K_{yz}\\
K_{zx} & K_{zy} & K_{zz}
\end{array}\right]
\left[\begin{array}{c}\partial h/\partial x\\\partial h/\partial y\\\partial h/\partial z\\\end{array}\right]
\]
-->

<!--
\[
\mathbf{q} = -\underline{\underline{K}} \nabla h
\]
-->

!!!!!!!!!more derivation in notes

#### Conductivity Tensor components in 2D

<< K_{x x} = K_{||} cos^2\theta + K_{\bot} sin^2\theta>>

<< K_{xy} = (K_{||} - K_{\bot})\sin\theta\cos\theta >>

<< K_{yy} = K_{\bot} cos^2\theta + K_{||} sin^2\theta>>

<< \underline{\underline{K}}>> is symmetric

#### Darcy's law matrix form
<!--
\[
 \left[\begin{array}{c}q_x\\q_y\end{array}\right] =  \left[\begin{array}{ccc}
K_{x x} & K_{xy} \\
K_{yx} & K_{yy} 
\end{array}\right]
\left[\begin{array}{c}\partial h/\partial x\\\partial h/\partial y\end{array}\right]
\]
-->

##### Principal direction of anisotropy aligned with coordinate axis.

<!--
\[
 \underline{\underline{K}} = \left[\begin{array}{ccc}
K_{||} & 0\\
0 & K_{\bot} 
\end{array}\right]
\]
-->

##### Isotropic material

<!--
\[
 \underline{\underline{K}} = \left[\begin{array}{ccc}
K & 0\\
0 & K
\end{array}\right]
\]
-->

* Flow direction is in opposite direction of << \nabla h>> only for isotropic materials. 

##### Homogeneous anisotropic

<!--
\[
 \left[\begin{array}{c}q_x\\q_y\end{array}\right] =  \left[\begin{array}{ccc}
K_{x x}(x,y) & K_{xy}(x,y) \\
K_{yx}(x,y) & K_{yy}(x,y) 
\end{array}\right]
\left[\begin{array}{c}\partial h/\partial x\\\partial h/\partial y\end{array}\right]
\]
-->

##Hydraulic conductivity: Laboratory measurements
##Storage Properties
##Flow equations: Confined aquifers
##Flow equations: Unconfined aquifers
##MODFLOW/MODPATH  

# Well Hydraulics
##Slug Test
##Steady state well hydraulics 
##Theis equation
##Cooper-Jacob approximation
##Pump tests for leaky aquifers and confined aquifers
##Image well theory 
##Capture zones
##Simulation of capture zones 
 
# Solute Transport
##Types of contamination and contaminants
##Advection, diffusion, dispersion
##Advection-dispersion equation
##Solutions to the advection-dispersion equation
##Solutions of the ADE, macrodispersion

#Vadose Zone Hydrology
##Unsaturated hydraulic properties
##Unsaturated flow equations
##1-D unsaturated flow
##Effects of aquifer heterogeneity
