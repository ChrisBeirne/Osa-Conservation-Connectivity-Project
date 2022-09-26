--- 
title: "Connectivity modelling"
subtitle: "Final Report"
date: "`July 2022`"
site: bookdown::bookdown_site
output: bookdown::pdf_book
documentclass: book
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
description: "An exploration of the ridge-to-reef connectivity of central America"
---

# Introduction {#intro}

Placeholder


## Executive Summary
## Approach
## Report structure

<!--chapter:end:index.Rmd-->


# Data assembly {#data-assembly}

Placeholder


## Define the focal area
## Data products
### Protected areas
#### Types of protected area
#### Marine protected areas
### Elevation
### Forest height
### Mangrove cover (current)
### Human disturbance:  
### Current land-use and habitat
## Start and end nodes
### Start nodes
#### Mangroves >1km2
#### Coastal Protected areas
### End nodes
#### High elevation protected areas
### Existing corridors
#### Corridors according to CBM
### Other sources
#### Key Biodiversity Areas

<!--chapter:end:02-data_assembly.Rmd-->


# Methods {#methods}

Placeholder


## Connectivity
### Least Cost Paths
#### Distance between nodes
#### How we could use this framework
#### Costa Rica example
## Probalistic approach

<!--chapter:end:03-method.Rmd-->


# Resistance surface {#cost-surface}

Placeholder


## Creating the resistance surface
### Step 1: Coarse resoluton
### Step 2: High resolution landcover {#cost-surface_high}
#### Interactive map
#### Adjusting forest conductance
## High resolution raster re-classification
### Osa Peninsula case study
#### Add the modifier for biomass

<!--chapter:end:04-Resistance-surface.Rmd-->


# Least-cost paths {#lcp}

Placeholder


## 'End' Nodes
## Protected area connectivity
### Country characteristics
#### Number of coastal protected areas
#### Starting protected area size
#### End protected area size
#### Multinational connectivity
#### Least cost path lengths
#### Average conductance
#### Least-suitable habitat
#### Proportion of the path which is already protected
#### Proportion of forest cover 
### Multivariate summary
#### The 'sweet spot'
## Mangrove connectivity
### Country characteristics
#### Number of mangroves fragments
#### Starting mangrove patch size
#### End protected area size
#### Multinational connectivity
#### Least cost path lengths
#### Average conductance
#### Least-suitable habitat
#### Proportion of the path which is already protected
#### Proportion of forest cover 
### Multivariate summary
#### The 'sweet spot'

<!--chapter:end:05-least-cost-paths.Rmd-->


# Circuitscape {#circuitscape}

Placeholder


## Methods
## Broad scale maps (1 km)
### Protected areas
#### Normalised current
### Mangroves
## Fine scale (100m)
#### Case Study 1: Osa peninsula

<!--chapter:end:06-circuitscape.Rmd-->


# References {-}


<!--chapter:end:07-references.Rmd-->


# Synthesis 

Placeholder


## Priority areas
### Protected area vs. Mangrove
### Least-cost vs. Circuitscape
### Protected areas
### Mangroves
## High elevation areas vs. existing corridors
## High resolution case study 1

<!--chapter:end:07-synthesis.Rmd-->

---
title: "08-discussion"
author: "Chris B"
date: "27/04/2022"
output: html_document
editor_options: 
  chunk_output_type: console
---

# Discussion {#discussion}

**Place Holder**



## Pitfalls

- Steep terrain not account for 
- Roads
- Other low elevation sources?
- Context of high elevation protected areas -> should it be more about getting to a protected area that contains or is connected to high elevation forests
- Non-protected contiguous high elevation forest
- Need to buffer the focal area to minimise edge effects (a strong issue for panama, Belize)
- Landcover -> currently biased towards wet ecosystems with highest biomass: https://www.nature.com/articles/s42003-021-02359-9 -> Must read! I might have to standardise by vegetation type. This would be the best way.




## Extentions

**Place Holder**




<!--chapter:end:08-summary.Rmd-->

