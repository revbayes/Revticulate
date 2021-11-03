
[![Build Status](https://travis-ci.com/Paleantology/Revticulate.svg?branch=master)](https://travis-ci.com/Paleantology/Revticulate)


## RevBayes and R

Calling external software from R can be a bit tricky, and RevBayes is no exception. In order to successfully use RevBayes in R, you must have RevBayes [installed](https://revbayes.github.io/download) on your computer. You must also know the system path to the RevBayes executeable. For example, on my computer, my RevBayes executeable is called "rb" (this will be the case for Mac and Linux users), and it is stored in my software folder. If you are on PC, your copy will be called "rb.exe."

## Setup

In the above chunk, you will see the command `initRev`. This is a function in the `Revticulate` R package. This R package can be installed using the popular `devtools` R package like so:

```{r}
devtools::install_github("revbayes/Revticulate")
library(Revticulate)
```

To function properly, Revticulate package must be connected to the underlying RevBayes executeable. This is done via the `initRev` function, which accepts an absolute path to RevBayes. For example, below, my rb executeable is stored in my (April) user software folder.

```
initRev("/Users/april/software/rb")
KnitRev()
```

The `initRev` function creates a RevBayes running environment, which will allow you to interact with RevBayes through R. `initRev` takes one argument: where RevBayes lives on your computer. Delete my sample path and enter your path above.

The next line, `KnitRev` establishes a KnitR environment to render Rev code in the attractive KnitR format many of us are used to.

## Using RevBayes in a KnitR Chunk

RevBayes can be used in a KnitR chunk by changing the header to `rb` instead of `r`. In the below chunk, we create an object called `example` and use the assignment operator to give it the value 1. Then we print it. 
```{rb}
example <- 1.0
example
```
This is not an overtly useful thing to do, however. Let's erase the previous chunk using the `clearRev()` function. This removes prior code and objects from the RevBayes environment. Very handy if you make a mistake!

```{r}
clearRev()
```

We could, instead, choose to do something a little more useful. How about reading in a data matrix and making a quick starting tree? 

```{rb}
morpho <- readDiscreteCharacterData("../data/Cinctans.nex")
num_taxa <- morpho.size()
num_branches <- 2 * num_taxa - 2
taxa <- morpho.names()
br_len_lambda ~ dnExp(0.2)
phylogeny ~ dnUniformTopologyBranchLength(taxa, branchLengthDistribution=dnExponential(br_len_lambda))
phylogeny
```
Anything entered in an `rb` block will be interepreted as Rev code, and all the normal Rev syntax will apply. For a nice overview of Rev language and syntax, please see [this tutorial](https://revbayes.github.io/tutorials/intro/rev). 

One thing researchers are often interested in doing is making an object in Rev and then viewing it in R. Right now, the best way to do this is to use phytools or ape to read in a RevBayes tree and plot to screen. Coercion of tree objects from RevBayes to R is still an area under active development.
```{r}
phylogeny <- getRevObj(name = "phylogeny", coerce = TRUE)

phytools::plotTree(phylogeny)
```



One nice facet of having RevBayes running in an R notebook is the ability to flip to visualizations of the different distributions we use. For example, here is the code for a common parameterization of the discrete Gamma distribution on site rates.

```{rb}

alpha_morpho ~ dnUniform( 0, 1E6 );
rates_morpho := fnDiscretizeGamma( alpha_morpho, alpha_morpho, 4 )
```

Might not mean much to you, in terms of what this distribution looks like. But it is important to develop intuitions for what common distributions look like. So, we can use R's built-in graphics capabilities to have a look at what 1000 draws from this gamma will look like. 

```{r}
library(ggplot2)
alpha_morpho <- runif(1, 0, 1E6 )

draws <- rgamma(1000, shape = alpha_morpho, rate = alpha_morpho)
hist(draws)
```



### Simulate coin flips

It's adviseable if you're switching gears to a new activity to clear the Rev environment of workspace objects from old activities:

```{r}

ClearRev()
```


Note that `ClearRev` is an R function, and must be executed in an R chunk.

```{rb}
# The number of coin flips
n <- 100

# The number of heads
x <- 50
```

### Initialize Markov Chain
We have to start MCMC off with some initial parameter values. One way to do this is to randomly draw values of the parameter ($p$) from the prior distribution. We assume a flat beta prior distribution ($\alpha = 1$ and $\beta = 1$).

```{rb}
alpha <- 1
beta <- 1
p <- rbeta (n=1, alpha, beta) [1]
```

### Likelihood Function
We next specify the likelihood function. We use the binomial probability for the likelihood function. Since the likelihood is defined only for $p$ between 0 and 1, we return 0 if $p$ is outside this range.

```{rb}
function likelihood(p) {
    if(p < 0 || p > 1)
        return 0

    l = dbinomial(x,p,n,log=false)
    return l
}
```

The function can then be executed in the next cell:

```{rb}
likelihood(p)
```
## The RepRev() environment

The function `RepRev()` can be called in the console (or in non-RStudio versions of R) to use RevBayes directly to program interactively. The RepRev() environment is denoted with `rb>>>`. To exit, type Ctrl + C. 

```{r}

RepRev()

# rb>>> 1+2

# [1] 3
```

## Limitations of Revticulate

We're still working on object passing between RevBayes and R. So, as we saw above, passing trees from Rev to R is a little clunky. This is an area we expect to have working shortly.

Using Revticulate for long computations is not advisable. To run your actual MCMC, it's probably best to save your code to a script and run it in the terminal.

We also have a known whitespace bug in function definition. It should be corrected over the weekend.
