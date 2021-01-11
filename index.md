aweSOM
------

<img src="man/figures/logo.png" align="right" alt="" width="150" />

**aweSOM** is an R package offering a set of tools to explore and
analyze datasets with Self-Organizing Maps (also known as [Kohonen
maps](https://en.wikipedia.org/wiki/Self-organizing_map)), a form of
artificial neural network originally created by [Teuvo
Kohonen](https://en.wikipedia.org/wiki/Teuvo_Kohonen) in the 1980s. The
package introduces interactive plots, making analysis of the SOM easier.

**aweSOM** provides a variety of functions to analyze and visualize
SOMs. 

* Initializing SOMs on training data and training these using the kohonen package.
* Interactive visualizations of structural characteristics and distribution of numeric as well as categorical variables of SOMs
* Exporting interactive SOM visualizations as standalone HTML file, .png or .svg.
* Performing and graphically evaluating super-clustering on SOM using hierarchical and PAM clustering methods 
* Ability of generating replicable R-code using the web-based interface

These can be used either through the web-based interface (called by
`aweSOM()`) or through command-line functions. The package relies on the
`kohonen` package for the training of SOMs.








### Install

Install the developer version of aweSOM from Github using the devtools
package

    devtools::install_github("jansodoge/awesom_dev_version")

### aweSOM web-based interface

An intuitive and user-friendly approach to training and analyzing
datasets with self-organizing maps is enabled by the aweSOM web-based
interface. It allows the user to import data, train a SOM, analyze via
the interactive visualizations and download results. Plots can be exported 
as interactive versions (HTML format), or static image files (.png or.svg).
Additionally, it creates replicable R-code to perform the operations 
using command-line functions of **aweSOM**.





![](shiny_workflow.png)

The web-based interface can be launched using the following function.

    aweSOM::aweSOM()

### aweSOM command-line functions

The aweSOM package provides command-line functions that enable
functionality similar to the web-based interface. Detailed documentation
of the command-line functions and their usage within a typical workflow
is described [here](articles/aweSOM-package.html).

    


