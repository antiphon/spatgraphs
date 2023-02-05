# spatgraphs
This is an R-package for computing various graphs (or networks) where the edges are related to the **spatial** configuration of 2D or 3D point location data. 

## The idea

Consider the Ripley's K-function, or the local species richness index ISAR, or any other functional summary for point pattern data. They usually have a parameter "r", determining the spatial scale of interest. This leads to analysis of a kind "...in the r-neighbourhood, we have so and so...".

The commonly used r-neighbourhood is the neighbourhood structure of a graph known in graph theory as the *geometric graph*, and is a well studied graph in itself. The r-neighbourhood however is not always optimal, and can even be misleading in cases where homogeneity of the pattern does not hold.

**This package provides alternative ways for computing neighbourhoods** and hence allows more flexible analysis of the local scale structures in spatial point pattern data. 

## Highlights

* Supports 2D as well as 3D point locations (point patterns)
* Supports many different neighbourhood definitions, for example
  - geometric with locally varying range
  - k-nearest neighbours
  - k-Gabriel
  - Spheres of Influence
  - Radial spanning tree
  - Class-Cover-Catch
* Connected component computations

## New version (3.*)

Complete rewrite of the package, reducing complexity of the C-side representation of the point pattern. Result is faster and lighter. 

Input handling has changed, now you can feed the point patterns in as pretty much any format.

Parameter handling is changed (to accommodate pattern input flexibility), make note!

