% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{Tune_SS_Comps}
\alias{Tune_SS_Comps}
\title{Tune_SS_Comps}
\usage{
Tune_SS_Comps(
  Origin = NULL,
  SS_dr = NULL,
  SS_loc = "/Users/jonathansmart/Documents/SS/",
  ss_exe = "ss3_osx_arm64",
  over_write_model = FALSE,
  method = "Francis",
  copy_to_origin = TRUE,
  auto_run = FALSE,
  n_iters = NULL,
  plots = TRUE,
  ...
)
}
\arguments{
\item{Origin}{The working directory on a network or hard drive where the
Stock Synthesis assessment is being performed. The SS files will be copied
from here to `SS_dr` where the model is run. If `copy_to_origin == TRUE`
(default) then the SS model files are copied back to this directory.}

\item{SS_dr}{The location on the hard drive where the SS model will be run locally.
make sure there is no white space in this filepath, especially if using a
non Windows computer.}

\item{SS_loc}{The location of the SS executable file.}

\item{ss_exe}{The name of the SS executable. The default is 'ss3_osx_arm64'
which corresponds to my own operating system.}

\item{over_write_model}{Should the existing tuned model be overwritten. `FALSE`
.    will generate a copy of the model to be tuned. `TRUE` will overwrite the
.    existing tuned model (necessary for iterative tuning steps)}

\item{method}{`Francis` or `MI` for McCallister and Ianelli method}

\item{copy_to_origin}{Do you want the SS files (and `r4ss` plots if requested)
copied back to the `Origin`? This can take a while if working remotely with
an agency network.}

\item{auto_run}{Keep running tuning until further iterations no longer improve
.    the model. Maxes out at 50 runs}

\item{n_iters}{Pre-set number of tuning iterations. Ignored if `auto-run == TRUE`}

\item{plots}{Do you want to produce the `r4ss` outputs?}

\item{...}{Function calls to be passed to `r4ss`}
}
\value{
A tuned SS model following the instructions given. No R objects are returned.
}
\description{
Length and age compositions are tuned using either the Francis (default) or MI
    methods. This function is a wrapper around the `r4ss::tune_comps` function
    which 1) copies files to necessary locations with the correct permisions
    to use SS (see description for `Run_Stock_Synthesis`), and 2) will perform
    numerous iterations of the tuning in an attempt to achieve a stable tuned
    model. The user can specify the number of iterations to run or set
    `auto_run == TRUE` so that the model is tuned until criteria are met. These
    include 1) changes in total log likelihood are less than 2, and 2) the model
    has evidence of convergence.
}
