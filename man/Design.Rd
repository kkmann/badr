% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Design.R, R/plots.R
\docType{class}
\name{Design}
\alias{Design}
\alias{sample_size}
\alias{critical_value}
\alias{futility_region}
\alias{efficacy_region}
\alias{continuation_region}
\alias{pmf}
\alias{probability_to_reject}
\alias{expected_sample_size}
\alias{sample_space}
\alias{reject}
\alias{as_tibble.Design}
\alias{plot.Design}
\alias{plot_designs}
\alias{plot_power}
\alias{plot_ess}
\alias{plot_compare_designs}
\title{Generic two-stage designs for single-arm trials with binary endpoint}
\usage{
Design(n2, c2, label = "")

sample_size(design, x1 = NULL)

critical_value(design, x1)

futility_region(design)

efficacy_region(design)

continuation_region(design)

pmf(design, x1, x2, p)

probability_to_reject(design, p, x1 = NULL)

expected_sample_size(design, p)

sample_space(design)

reject(x1, x2, design)

\method{as_tibble}{Design}(x, ...)

\method{plot}{Design}(x, tbl_power_annotations = NULL, ...)

plot_designs(
  ...,
  textsize = 1.75,
  ystep = 10,
  yexpandlower = 0.1,
  skip_listing = NULL
)

plot_power(
  ...,
  annotations = NULL,
  min.segment.length = Inf,
  annotation_text_size = 1.5
)

plot_ess(
  ...,
  annotations = NULL,
  min.segment.length = Inf,
  annotation_text_size = 2
)

plot_compare_designs(
  ...,
  annotations = NULL,
  min.segment.length = Inf,
  annotation_text_size = 2,
  args_design_plot = list(textsize = 1.75, ystep = 10),
  args_power_plot = list(annotations = annotations, min.segment.length =
    min.segment.length, annotation_text_size = annotation_text_size),
  args_ess_plot = list(annotations = annotations, min.segment.length =
    min.segment.length, annotation_text_size = annotation_text_size)
)
}
\arguments{
\item{n2}{numeric vector with stage-two sample sizes for x1 = 0, 1, ..., n1}

\item{c2}{numeric vectot with stage-two decision boundary for x1 = 0, 1, ..., n1;
the design rejects H0: p <= p0 iff X2 > c_2(X1); use +/- Inf for early futility/efficacy (n2 = 0).}

\item{label}{an optional label to use for pretty plotting etc.}

\item{design}{a \code{Design} object}

\item{x1}{stage-one number of results}

\item{x2}{stage-two number of responses}

\item{p}{response probability}

\item{...}{one more more \code{Design} objects to plot}

\item{textsize}{size of c2/n2 annotation in design plot}

\item{ystep}{tick marks step in design plot y axis}

\item{yexpandlower}{factor for (lower) y axis expansion in design plot,}

\item{skip_listing}{character vector with design labels for which to not
include the c2/n2 listings}

\item{annotations}{vector of response prbabilities to annotate}

\item{min.segment.length}{minimal lenght of arrow to annotations}

\item{annotation_text_size}{annotation text size}

\item{args_design_plot}{list of arguments for the design plot}

\item{args_power_plot}{list of arguments for the power plot}

\item{args_ess_plotlist}{of arguments for the expected sample size plot}
}
\description{
[todo]
}
