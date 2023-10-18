
<!-- README.md is generated from README.Rmd. Please edit that file -->

# movenetapp

The movenet app provides a graphical user interface to the \[movenet
package\] (<https://digivet-consortium.github.io/movenet/>), in the form
of a Shiny app.

The goal of movenet and the movenet app is to simplify the effective use
of livestock movement data in veterinary public health. It facilitates
the dataflow from livestock movement data to social network analysis and
disease transmission models, while addressing common data issues such as
the diversity of data formats and privacy preservation.

<figure>
<img src="man/figures/movenet_summary.png"
alt="Flow chart showing the workflows of the movenet package, as well as how movenet addresses some common data challenges." />
<figcaption aria-hidden="true">Flow chart showing the workflows of the
movenet package, as well as how movenet addresses some common data
challenges.</figcaption>
</figure>

movenet and the movenet app are being developed in the context of the
[NordForsk Digitalisation of livestock data to improve veterinary public
health (DigiVet)
project](https://www.nordforsk.org/projects/digitalisation-livestock-data-improve-veterinary-public-health).

**Disclaimer:** The movenet app is under active development. The
interface may change without prior warning.

## Workflows

movenet and the movenet app include a range of workflows for the
processing and analysis of livestock movement data and (optional)
holding data:

- **standardisation** of data into a single format, allowing for
  interoperability and integration of data from different countries or
  systems

- **pseudonymisation** of livestock movement and/or holding data, to
  improve the potential for data sharing and collaborative analysis

- generation of network representations, and **social network
  analysis**, of livestock movement data

- integration of livestock movement and/or holding data into
  **transmission models**

- exploration of the effects of different pseudonymisation strategies on
  network properties, so as to allow users to find a suitable balance
  between the identifiability of the data and the accuracy of these
  properties.

For more detail on each of these workflows, see the vignettes on the
\[movenet package website\]
(<https://digivet-consortium.github.io/movenet/>).

## Installation

You can install the development version of the movenet app from
[GitHub](https://github.com/).

Before you install the movenet app, you first need to install the latest
version of movenet, which contains the basic functions behind the
movenet app.

The process is as follows:

``` r
# install.packages("devtools")
devtools::install_github("digivet-consortium/movenet")
devtools::install_github("digivet-consortium/movenetapp")
```

## Usage

To launch the movenet app, run the following:

``` r
movenetapp::runMovenetApp()
```
