# interlimb
*An R Package for Assessing Interlimb Asymmetries*

## Introduction

Interlimb asymmetries, or interlimb imbalances, of the lower-body are associated with increased incidences of injury among fall-risk and post-surgery ACL reconstruction patients. Recent research has also established that athletes displaying interlimb asymmetries greater than 10-15% in strength or power also experience decrements in repeated sprint and jumping ability, and general athletic performance.

Currently, calculating interlimb asymmetries requires custom, and sometimes proprietary, MS Excel Sheets. The {interlimb} R Stats package (R, version 4.1.0) is an open-source tool that helps researchers, rehabilitation specialists, and sports scientists effortlessly assess limb imbalances. Regardless of the test, be it range-of-motion, force, or power, {interlimb} returns multiple symmetry and asymmetry indices, reliability and validity measures, and changes in asymmetry values nearly instantaneously. Moreover, this publication and the package documentation provides users insight into which calculations are appropriate given different testing modalities.

## Installing {interlimb}

Since the package is not published on CRAN, users must download it from GitHub using:

```r
devtools::install_github("aaronzpearson/interlimb")
```

## Usage

The package is built for all users, regardless of their experience with R.    

Every function is documented and examples are provided. The exported (readilly available) functions are:    
- `interlimb()` automates the entire process and returns a data frame that includes the original data, asymmetry percentages, and reliability measures    
- `interlimb.wide()` returns a data with calculations for each test and trial per asymmetry index     
- `interlimb.long()` returns a *long* version of the wide data set for easy data manipulations    
- `track.asym()` provides changes in asymmetries between tests and against the baseline test, and various reliability measures    

### Load the Package

```r
library(interlimb)
```

### Run Examples

Run examples to understand and visualize outputs. 

```r
example("interlimb")    
example("interlimb.wide") # example("interlimb.long") returns the same output    
example("track.asym")    
```
    
{interlimb} was written to compliment Aaron Pearson's MSc thesis that explored interlimb asymmetries in professional men's ice-hockey. 
