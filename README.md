# atlantisdiagnostics

<!-- badges: start -->

[![gitleaks](https://github.com/NOAA-EDAB/atlantisdiagnostics/workflows/gitleaks/badge.svg)](https://github.com/NOAA-EDAB/atlantisdiagnostics/actions) 
[![pkgdown](https://github.com/NOAA-EDAB/atlantisdiagnostics/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/NOAA-EDAB/atlantisdiagnostics/actions/workflows/pkgdown.yaml) [![R-CMD-check](https://github.com/NOAA-EDAB/atlantisdiagnostics/workflows/R-CMD-check/badge.svg)](https://github.com/NOAA-EDAB/atlantisdiagnostics/actions)

<!-- badges: end -->

The diagnostics package primarily focuses on two aspects:

  * Processing and plotting a suite of model properties
  * Performance diagnostics (guided by Kaplan and Marshall<sup>1</sup>, 2016) to quickly identify potential issues, for example:
  
      * Persistence: Do all species/groups in the model persist through a set period of time?
      * Stability: Do all species/groups reach an acceptable steady state or equilibrium?
      * Reasonability: Do all species/groups population biomass lie within reasonable bounds?


<sup>1</sup> [A guinea pig's tale](https://doi.org/10.1093/icesjms/fsw047): learning to review end-to-end marine ecosystem models for management application. ICES Journal of Marine Science (2016) 73(7), 1715-1724




## Installation

``` r
remotes::install_github("NOAA-EDAB/atlantisdiagnostics")
```

or

``` r
pak::pak("NOAA-EDAB/atlantisdiagnostics")
```

### Team members

Members of the Atlantis team in alphabetical order:

| [andybeet](https://github.com/andybeet) | [jcaracappa1](https://github.com/jcaracappa1) | [sgaichas](https://github.com/sgaichas) | [gambler1650](https://github.com/gambler1650) | [slarge](https://github.com/slarge) | [RMORSEcode](https://github.com/RMORSEcode) |
|---|---|---|---|---|---|
| [![andybeet avatar](https://avatars1.githubusercontent.com/u/22455149?s=100&v=4)](https://github.com/andybeet) | [![jcaracappa1 avatar](https://avatars1.githubusercontent.com/u/57966543?s=100&v=4)](https://github.com/jcaracappa1) | [![sgaichas avatar](https://avatars1.githubusercontent.com/u/8172302?s=100&v=4)](https://github.com/sgaichas) | [![andybeet avatar](https://avatars1.githubusercontent.com/u/5949383?s=100&v=4)](https://github.com/gambler1650) | [![slarge avatar](https://avatars1.githubusercontent.com/u/5000131?s=100&v=4)](https://github.com/slarge) | [![RMORSEcode avatar](https://avatars1.githubusercontent.com/u/10620840?s=100&v=4)](https://github.com/RMORSEcode) |



#### Legal disclaimer

*This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.*

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [NOAA Fisheries](https://www.fisheries.noaa.gov/)

