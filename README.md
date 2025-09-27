
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arsampler

<!-- badges: start -->

<!-- badges: end -->

## Overview

`arsampler` is a package to help perform the acceptance-rejection
sampling without having to code the entire algorithm itself. The current
version of this package can only accept a proposal densities from
pre-defined templates while the user can define their own target
densities (see examples below).

## Installation

You can install the released version of `arsampler` along with the unit
tests with:

``` r
# Install the relevant library (if not already)
install.packages("devtools")

# Load the relevant library
library(devtools)

# Install the arsampler package from GitHub
devtools::install_github("enggelin/arsampler", INSTALL_opts = "--install-tests")

# Load the arsampler package
library(arsampler)
```

## Get started with arsampler

Read the short introductory vignette to get you started with `arsampler`
and have a look at the examples below.

``` r
# Read the short vignette
#vignette("introduction-to-arsampler)

# Reproduce a simple example
#example(arsampler)
```

## Example

This is a basic example of the usage of `arsampler` package:

``` r
library(arsampler)

# Target density: Normal(0,1); Proposal density: Normal(0,1)
f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
q_norm <- "rnorm(n, 0, 1)"

# Run the main function ar()
example_norm <- ar(f_norm, q_norm, c=1, n=10)

# Print the raw values of the fitted densities
example_norm
#> Data of  x :
#> 
#> $params
#> $params$f
#> [1] "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
#> 
#> $params$q
#> [1] "rnorm(n, 0, 1)"
#> 
#> $params$c
#> [1] 1
#> 
#> $params$n
#> [1] 10
#> 
#> 
#> $data
#>    index           x         f         q           y r          u
#> 1      1 -0.12596041 0.3957900 0.3957900 -0.12596041 1 0.16511915
#> 2      2  0.65087736 0.3227881 0.3227881  0.65087736 1 0.81443629
#> 3      3  1.05144770 0.2295327 0.2295327  1.05144770 1 0.73004210
#> 4      4  0.19838225 0.3911687 0.3911687  0.19838225 1 0.06482781
#> 5      5  0.03761018 0.3986602 0.3986602  0.03761018 1 0.99893473
#> 6      6 -0.13493207 0.3953271 0.3953271 -0.13493207 1 0.23551828
#> 7      7  1.04458511 0.2311895 0.2311895  1.04458511 1 0.49856997
#> 8      8 -0.06864086 0.3980036 0.3980036 -0.06864086 1 0.63826861
#> 9      9  0.47099088 0.3570588 0.3570588  0.47099088 1 0.95916749
#> 10    10  1.44675186 0.1400881 0.1400881  1.44675186 1 0.41172633
#> 
#> $acceptance_rate
#> [1] 1
#> 
#> $expected_values_f
#> [1] 0.4571112
#> 
#> $variance_f
#> [1] 0.3250348
#> 
#> $suggested_c
#> [1] "NA"

# Provide a summary of the fitted densities
ar_summary(example_norm)
#> Summary of  example_norm :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  0.4571 
#>      Empirical variance of target distribution:  0.325 
#>      Suggested minimum c:  NA 
#> ----------------------
```

The above output can also be achieved by using the commands `print()`
and `summary()`, which is an S3 object-oriented method in R. Those
commands are override if the user passes an object of class `ar`, which
is the object class of the output from the function `ar()`.

``` r
# Print the raw values of the fitted densities using S3 method print()
print(example_norm)
#> Data of  example_norm :
#> 
#> $params
#> $params$f
#> [1] "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
#> 
#> $params$q
#> [1] "rnorm(n, 0, 1)"
#> 
#> $params$c
#> [1] 1
#> 
#> $params$n
#> [1] 10
#> 
#> 
#> $data
#>    index           x         f         q           y r          u
#> 1      1 -0.12596041 0.3957900 0.3957900 -0.12596041 1 0.16511915
#> 2      2  0.65087736 0.3227881 0.3227881  0.65087736 1 0.81443629
#> 3      3  1.05144770 0.2295327 0.2295327  1.05144770 1 0.73004210
#> 4      4  0.19838225 0.3911687 0.3911687  0.19838225 1 0.06482781
#> 5      5  0.03761018 0.3986602 0.3986602  0.03761018 1 0.99893473
#> 6      6 -0.13493207 0.3953271 0.3953271 -0.13493207 1 0.23551828
#> 7      7  1.04458511 0.2311895 0.2311895  1.04458511 1 0.49856997
#> 8      8 -0.06864086 0.3980036 0.3980036 -0.06864086 1 0.63826861
#> 9      9  0.47099088 0.3570588 0.3570588  0.47099088 1 0.95916749
#> 10    10  1.44675186 0.1400881 0.1400881  1.44675186 1 0.41172633
#> 
#> $acceptance_rate
#> [1] 1
#> 
#> $expected_values_f
#> [1] 0.4571112
#> 
#> $variance_f
#> [1] 0.3250348
#> 
#> $suggested_c
#> [1] "NA"

# Provide a summary of the fitted densities using S3 method summary()
summary(example_norm)
#> Summary of  example_norm :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  0.4571 
#>      Empirical variance of target distribution:  0.325 
#>      Suggested minimum c:  NA 
#> ----------------------
```

## Test plan: unit testing

In this version, we will test several scenarios involving varieties of
pairs of target densities and pre-defined proposal densities and how
they will perform.

The unit tests can be run directly from the tests folder obtained when
installing the package. There are one test file in the tests folder,
which is used to test the main function of this package, `ar()`.

To run the unit tests from the tests folder, run the following command:

``` r
# Install the relevant library (if not already)
install.package("testthat")

# Load the relevant library
library(testthat)

# Set the working directory to the package root, and run this command:
testthat::test_dir(system.file("tests", package = "arsampler"))
```

The above command will test several scenarios of the input from the
user, which will be described below:

…

## Test plan: user input

Besides testing using the unit tests embedded in the installed package,
user can also perform relevant testings by their own using their own
input. The details are explain below.

You will notice that in the above example, the expected mean and
variance are rather nonsense, as we only run the sampler for a few
iterations. To be able to provide a rather accurate results, we need to
run the sampler long enough. An iteration of minimum of `n=1000` should
provide suffice, although for some densities it needs to be run long
enough (more about that later). In this test plan, we will try several
scenarios of combination pair of target-proposal densities.

### Test 1: Proposal densities with Acceptance rate: 1, Suggested minimum c: NA, and Empirical expected value of target distribution: $`\approx`$ its mean

The package has several pre-defined probability densities for user to
use. They are: `normal`, `uniform`, `gamma`, `beta`, and `exponential`.
To check whether this proposal densities give the correct output for its
counterpart target densities written in its pdf form, you can run these
tests below (notice that we will run it for longer iterations,
i.e. `n=1000` and no parameters declaration are done as instructed in
the vignette):

``` r
# pair of standard normal distributions
f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
q_norm <- "rnorm(n, 0, 1)"

example_norm <- ar(f_norm, q_norm, c=1, n=1000)
summary(example_norm)
#> Summary of  example_norm :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  0.033 
#>      Empirical variance of target distribution:  0.9916 
#>      Suggested minimum c:  NA 
#> ----------------------
```

``` r
# pair of uniform distributions
f_unif <- "1"
q_unif <- "runif(n, 0, 1)"

example_unif <- ar(f_unif, q_unif, c=1, n=1000)
summary(example_unif)
#> Summary of  example_unif :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  0.4988 
#>      Empirical variance of target distribution:  0.0869 
#>      Suggested minimum c:  NA 
#> ----------------------
```

``` r
# pair of gamma distributions, alpha=3, beta=2
f_gamma <- "((2^3)/(gamma(3)))*x^(3-1)*exp(-2*x)"
q_gamma <- "rgamma(n, 3, 2)"

example_gamma <- ar(f_gamma, q_gamma, c=1, n=1000)
summary(example_gamma)
#> Summary of  example_gamma :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  1.5746 
#>      Empirical variance of target distribution:  0.7996 
#>      Suggested minimum c:  NA 
#> ----------------------
```

``` r
# pair of beta distributions, alpha=3, beta=2
f_beta <- "((gamma(3+2))/gamma(3)*gamma(2))*x^(3-1)*(1-x)^(2-1)"
q_beta <- "rbeta(n, 3, 2)"

example_beta <- ar(f_beta, q_beta, c=1, n=1000)
summary(example_beta)
#> Summary of  example_beta :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  0.5982 
#>      Empirical variance of target distribution:  0.0384 
#>      Suggested minimum c:  NA 
#> ----------------------
```

``` r
# pair of exp distributions, lambda=3
f_exp <- "3*exp(-3*x)"
q_exp <- "rexp(n, 3)"

example_exp <- ar(f_exp, q_exp, c=1, n=1000)
summary(example_exp)
#> Summary of  example_exp :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  0.3189 
#>      Empirical variance of target distribution:  0.1063 
#>      Suggested minimum c:  NA 
#> ----------------------
```

Notice that for the above examples, we do not use the command `print()`,
as we do not want to clutter the **Example** section. Another S3 method,
`head()`, will be implemented in a later version to accommodate this
shortcoming.

If the examples run correctly, you will notice that each printed summary
gives `Acceptance rate: 1`, `Suggested minimum c: NA`, and
`Empirical expected value of target distribution: $\approx$ its mean`.
This means that the proposal densities perfectly match their own target
densities, as they are the exact same distributions with the exact same
parameters. `Acceptance rate: 1` means all the proposed values are
accepted as coming from the corresponding target distribution,
`Suggested minimum c: NA` means that the proposal distribution envelopes
the target distribution entirely with the given value of `c`, and
`Empirical expected value of target distribution: $\approx$ its mean`
means that the accepted `x` values from the proposal distributions are
able to approximate the mean of the target distribution. You can input
your own parameters and see if the expected value match with the
corresponding distributions. For reference:

Normal distribution with $`\mu=0`$ and $`\sigma^2=1`$:
$`X \sim N(0,1)`$; $`E(X)=\mu`$, $`Var(X)=\sigma^2`$;

Uniform distribution with $`a=0`$ and $`b=1`$: $`X \sim U(0,1)`$;
$`E(X)=\frac{a+b}{2}`$, $`Var(X)=\frac{(b-a)^2}{12}`$;

Gamma distribution with $`\alpha=3`$ and $`\beta=2`$:
$`X \sim Gamma(3,2)`$; $`E(X)=\frac{\alpha}{\beta}`$,
$`Var(X)=\frac{\alpha}{\beta^2}`$;

Beta distribution with $`\alpha=3`$ and $`\beta=2`$:
$`X \sim Beta(3,2)`$; $`E(X)=\frac{\alpha}{\alpha+\beta}`$,
$`Var(X)=\frac{\alpha\beta}{(\alpha+\beta)^2(\alpha+\beta+1)}`$;

Exponential distribution with $`\lambda=3`$: $`X \sim Exp(3)`$;
$`E(X)=\frac{1}{\lambda}`$, $`Var(X)=\frac{1}{\lambda^2}`$;

You can also try different parameters to see how it works.

### Test 2: Proposal densities with different form than the target densities

In the real world usage, the case is not that simple; most of the time
we need to estimate the target distribution from a different, usually
easier, proposal distribution (which also coincides with the purpose of
this package). We can test several simple pair of target-proposal
densities, as instructed below:

``` r
# pair of standard normal density (f) and uniform density (q)
f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
q_unif <- "runif(n, -4, 4)" # to accommodate the lower and upper limit of N(0,1) distribution

example_norm_unif <- ar(f_norm, q_unif, c=1, n=10000) # run longer for convergence
summary(example_norm_unif)
#> Summary of  example_norm_unif :
#> ----------------------
#>      Acceptance rate:  0.5067 
#>      Empirical expected value of target distribution:  -0.0018 
#>      Empirical variance of target distribution:  1.5952 
#>      Suggested minimum c:  3.1915 
#> ----------------------
```

From the result above, we can see that the proposal density does not
fully envelope the target density, determined by the value of
`Suggested minimum c`. Also, the expected value and variance are
nonsense. We will follow the suggested value of `c` and re-run the
example code:

``` r
# pair of standard normal density (f) and uniform density (q)
f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
q_unif <- "runif(n, -4, 4)" # to accommodate the lower and upper limit of N(0,1) distribution

example_norm_unif <- ar(f_norm, q_unif, c=3.2, n=10000) # change the value of c to the suggested minimum.
summary(example_norm_unif)
#> Summary of  example_norm_unif :
#> ----------------------
#>      Acceptance rate:  0.3092 
#>      Empirical expected value of target distribution:  0.0114 
#>      Empirical variance of target distribution:  1.0025 
#>      Suggested minimum c:  NA 
#> ----------------------
```

The suggested minimum c is now NA, which means the proposal density now
envelopes the target density fully, and the expected value and variance
are now valid. ($`\hat{E(X)} \approx \mu`$ and
$`\hat{Var(X)} \approx \sigma^2`$).

You can try using different combinations of target-density pair for this
part.
