# Version 1.4
__Release date__: 23 January 2018

## Major Changes

- __ggplot2__ is now being used for the (majority of) time series graphs

## Bug fixes
- various


# Version 1.3
__Release date__: 02 October 2017

- Bug fixes and package improvements to pass CRAN checks

## Patches

### 1.3-1 - 20/10/2017

- Modify missing-values error message


***

# Version 1.2
__Release date__: 24 March 2017

- Main change is addition of smoothing parameter to plots
- Improved time variable detection, including weekly, daily, hourly data
- Better use of x and y axis labels
- Other minor bug fixes


***

# Version 1.1
__Release date__: 18 January 2014

- No user-visible changes to the package, however re-released with the
  launch of iNZight 2.0.

## Patches

### 1.1.1 - 27/03/2014

- New functionality using proportion values to calcualate `x`,
  and calculated using order to control the x-axis.

- Changed some of the graphical parameter settings to avoid
  over-writing objects with a "white" background.

### 1.1.2 - 4/04/2014

- y-axis label support now on all plots.
- Various bug fixes to accomodate multiplicative time series.

### 1.1.3 - 17/09/2014

- added x label to additional plots

### 1.1.5 - 12/01/2015

- fixed a missed function export for `compareplot`

### 1.1.6 - 02/02/2015

- reposition the axis label

### 1.1.7 - 23/09/2016

- catch errors trying to guess TS structure
