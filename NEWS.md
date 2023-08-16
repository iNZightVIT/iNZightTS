- Add option to seasonally adjust the series before plotting (default plot)

# iNZightTS 2.0.0

Large update with major refactoring of most/all plots and processes. Notably,

- rebuild package with features from **tidyverts**
- new: sub-series plot
- enhanced: introduce key for multiple time series
- enhanced: changed the default behaviour of multiple variables in time series plots
- added readme with example usage (#56)

# iNZightTS 1.5.9

- fix bug that occurs if data passed to `iNZightTS()` is a tibble

# iNZightTS 1.5.8

- change arguments to avoid deprecation arguments with next ggplot2 release (#62)

# iNZightTS 1.5.7

- print comparison plots so they are always shown in the graphics device

# iNZightTS 1.5.6

- add colour legend to subtitle of decomposition plot

# iNZightTS 1.5.5

- fix bug preventing basic plots from working for annual data

# iNZightTS 1.5.4

- fix bug which meant passing `recompose = TRUE` to the plot method for decomposition objects was being ignored

# iNZightTS 1.5.3

- fix small bug in detecting time information where `[` subset operator returns tibble/dataframe (not vector)

# iNZightTS 1.5.2

- disable multiplicative series if any values are zero or lower
- add appropriate subtitle to trend panel of decomposition plot
- fix a bug preventing `iNZightTS` from accepting a valid `stats::ts` object
- specify `stringsAsFactors = TRUE` for upcoming R 4.0.0
- fix documentation for CRAN

# iNZightTS 1.5.1

**Release date**: 14 November 2019

- fix a bug in `seasonplot()` where a strange interaction between Windows' `Cairo()` graphics device and `egg::ggarrange()` was resulting in the graphs not being displayed

# iNZightTS 1.5

**Release date**: 11 November 2019

- add argument to `plot()` functions to show smoother (default is `TRUE`)
- add UTF-8 encoding to DESCRIPTION file as requested by roxygen :)
- add ggplot2 version of season plot and decompose plot
- for compare plot with additive effects, make main and effects y-limits equally scaled
- fix bug where smoothing parameter wasn't being passed to compare effects graph

# iNZightTS 1.4.3

**Release date**: 2 September 2019

- automatically detect 5-day week data (as opposed to 7-day week)

# iNZightTS 1.4.2

**Release date**: 15 July 2019

- specify (time/horizontal) axes limits to view part of a time series (#16)
- use only part of a series to create model + forecast (#17)
- recode forecast plot using `ggplot2`

# iNZightTS 1.4.1

**Release date**: 29 May 2018

- add the seasonal lines for each cycle back into the separate-series comparison plot

# iNZightTS 1.4

**Release date**: 23 January 2018

- **ggplot2** is now being used for the (majority of) time series graphs
- various bugfixes

# iNZightTS 1.3-1

**Release date**: 20 October 2017

- Modify missing-values error message

# iNZightTS 1.3

**Release date**: 02 October 2017

- Bug fixes and package improvements to pass CRAN checks

# iNZightTS 1.2

**Release date**: 24 March 2017

- Main change is addition of smoothing parameter to plots
- Improved time variable detection, including weekly, daily, hourly data
- Better use of x- and y-axis labels
- Other minor bug fixes

# iNZightTS 1.1.7

**Release date**: 23 September 2016

- catch errors trying to guess TS structure

# iNZightTS 1.1.6

**Release date**: 02 February 2015

- reposition the axis label

# iNZightTS 1.1.5

**Release date**: 12 January 2015

- fixed a missed function export for `compareplot`

# iNZightTS 1.1.3

**Release date**: 17 September 2014

- added x label to additional plots

# iNZightTS 1.1.2

**Release date**: 4 April 2014

- y-axis label support now on all plots.
- Various bug fixes to accommodate multiplicative time series.

# iNZightTS 1.1.1

**Release date**: 27 March 2014

- New functionality using proportion values to calculate `x`,
  and calculated using order to control the x-axis.

- Changed some graphical parameter settings to avoid
  over-writing objects with a "white" background.

# iNZightTS 1.1

**Release date**: 18 January 2014

- No user-visible changes to the package, however re-released with the
  launch of iNZight 2.0.
