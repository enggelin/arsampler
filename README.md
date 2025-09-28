
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arsampler

<!-- badges: start -->

<!-- badges: end -->

## Overview

`arsampler` is a package to help perform the acceptance-rejection
sampling without having to code the entire algorithm itself. The current
version of this package (Version 0.1.0) can only accept a proposal
densities from pre-defined templates while the user can define their own
target densities (see examples below).

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

Read the short introductory vignette to get you started with
`arsampler`. To look at examples, follow the section **Example** right
after this section.

``` r
# Read the short vignette
vignette("introduction-to-arsampler")
```

The vignette above basically explains that the package, in its current
version, has 3 functions: `ar()`, which is the main function and outputs
an object of class `ar`; `ar_summary()`, which receives input of an
object of class `ar` and outputs the summary of the fitted densities;
and `ar_plot()`, which receives input of an object of class `ar` and
outputs the overlay plot of both the target and proposal densities
(scaled with `c`, if defined) in a single plot. In the current version,
only two functions are ready to be used and tested, i.e. `ar()` and
`ar_summary()`.

## Example

This is a basic example of the usage of `arsampler` package:

``` r
library(arsampler)

# Target density: Normal(0,1); Proposal density: Normal(0,1)
f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
q_norm <- "rnorm(n, 0, 1)"

# Run the main function ar()
example_norm <- ar(f_norm, q_norm, c=1, n=10)
```

``` r
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
#>    index            x          f          q            y r         u
#> 1      1 -1.288202027 0.17400508 0.17400508 -1.288202027 1 0.5693442
#> 2      2  1.334124251 0.16383716 0.16383716  1.334124251 1 0.6846918
#> 3      3 -0.617325437 0.32972910 0.32972910 -0.617325437 1 0.4977059
#> 4      4 -1.992615179 0.05479282 0.05479282 -1.992615179 1 0.5697501
#> 5      5 -0.164634641 0.39357217 0.39357217 -0.164634641 1 0.1621674
#> 6      6  1.008997609 0.23979363 0.23979363  1.008997609 1 0.2169549
#> 7      7  0.003358717 0.39894003 0.39894003  0.003358717 1 0.5156591
#> 8      8  0.670684617 0.31859089 0.31859089  0.670684617 1 0.5599517
#> 9      9 -0.454254780 0.35983408 0.35983408 -0.454254780 1 0.5258619
#> 10    10 -0.396117998 0.36883966 0.36883966 -0.396117998 1 0.7526849
#> 
#> $acceptance_rate
#> [1] 1
#> 
#> $expected_values_f
#> [1] -0.1895985
#> 
#> $variance_f
#> [1] 1.032194
#> 
#> $suggested_c
#> [1] "NA"
```

``` r
# Provide a summary of the fitted densities
ar_summary(example_norm)
#> Summary of  example_norm :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  -0.1896 
#>      Empirical variance of target distribution:  1.0322 
#>      Suggested minimum c:  NA 
#> ----------------------
```

The above output can also be achieved by using the commands `print()`
and `summary()`, which is an S3 object-oriented method in R. Those
commands are overridden if the user passes an object of class `ar`,
which is the object class of the output from the function `ar()`.

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
#>    index            x          f          q            y r         u
#> 1      1 -1.288202027 0.17400508 0.17400508 -1.288202027 1 0.5693442
#> 2      2  1.334124251 0.16383716 0.16383716  1.334124251 1 0.6846918
#> 3      3 -0.617325437 0.32972910 0.32972910 -0.617325437 1 0.4977059
#> 4      4 -1.992615179 0.05479282 0.05479282 -1.992615179 1 0.5697501
#> 5      5 -0.164634641 0.39357217 0.39357217 -0.164634641 1 0.1621674
#> 6      6  1.008997609 0.23979363 0.23979363  1.008997609 1 0.2169549
#> 7      7  0.003358717 0.39894003 0.39894003  0.003358717 1 0.5156591
#> 8      8  0.670684617 0.31859089 0.31859089  0.670684617 1 0.5599517
#> 9      9 -0.454254780 0.35983408 0.35983408 -0.454254780 1 0.5258619
#> 10    10 -0.396117998 0.36883966 0.36883966 -0.396117998 1 0.7526849
#> 
#> $acceptance_rate
#> [1] 1
#> 
#> $expected_values_f
#> [1] -0.1895985
#> 
#> $variance_f
#> [1] 1.032194
#> 
#> $suggested_c
#> [1] "NA"
```

``` r
# Provide a summary of the fitted densities using S3 method summary()
summary(example_norm)
#> Summary of  example_norm :
#> ----------------------
#>      Acceptance rate:  1 
#>      Empirical expected value of target distribution:  -0.1896 
#>      Empirical variance of target distribution:  1.0322 
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
testthat::test_dir(system.file("tests/testthat", package = "arsampler"))
```

The above command will test several scenarios defined by the author of
this package. The unit testing comprises of several scenario which may
arise in certain circumstances and the expected output from it. Inside
the directory `tests/testthat/`, there are two test files named
`test-ar.R` and `test-ar_summary.R`. The contents of the files and the
context of the tests are as follows:

``` r
# test-ar.R

test_that("The test of the output for the exact same pair of target and proposal densities.", {

  ## initialise variables for testing
  # pair of standard normal distributions
  f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
  q_norm <- "rnorm(n, 0, 1)"

  # pair of uniform distribution
  f_unif <- "1"
  q_unif <- "runif(n, 0, 1)"

  # pair of gamma distribution, alpha=3, beta=2
  f_gamma <- "((2^3)/(gamma(3)))*x^(3-1)*exp(-2*x)"
  q_gamma <- "rgamma(n, 3, 2)"

  # pair of beta distribution, alpha=3, beta=2
  f_beta <- "((gamma(3+2))/gamma(3)*gamma(2))*x^(3-1)*(1-x)^(2-1)"
  q_beta <- "rbeta(n, 3, 2)"

  # pair of exp distribution, lambda=3
  f_exp <- "3*exp(-3*x)"
  q_exp <- "rexp(n, 3)"

  # expect silent (correct input)
  expect_silent(ar(f_norm, q_norm, c=1, n=1000))
  expect_silent(ar(f_unif, q_unif, c=1, n=1000))
  expect_silent(ar(f_gamma, q_gamma, c=1, n=1000))
  expect_silent(ar(f_beta, q_beta, c=1, n=1000))
  expect_silent(ar(f_exp, q_exp, c=1, n=1000))

  # expect equal: acceptance rate = 1
  expect_equal(ar(f_norm, q_norm, c=1, n=1000)$acceptance_rate, 1)
  expect_equal(ar(f_unif, q_unif, c=1, n=1000)$acceptance_rate, 1)
  expect_equal(ar(f_gamma, q_gamma, c=1, n=1000)$acceptance_rate, 1)
  expect_equal(ar(f_beta, q_beta, c=1, n=1000)$acceptance_rate, 1)
  expect_equal(ar(f_exp, q_exp, c=1, n=1000)$acceptance_rate, 1)

  # expect equal: rounded mean from standard distributions; longer run for convergence
  expect_equal(abs(round(ar(f_norm, q_norm, c=1, n=10000)$expected_values_f, 1)), 0) # need to put abs() on the standard normal distribution because the possible value may range from negative around zero to positive around zero.
  expect_equal(round(ar(f_unif, q_unif, c=1, n=10000)$expected_values_f, 1), 0.5)
  expect_equal(round(ar(f_gamma, q_gamma, c=1, n=10000)$expected_values_f, 1), 1.5)
  expect_equal(round(ar(f_beta, q_beta, c=1, n=10000)$expected_values_f, 1), 0.6)
  expect_equal(round(ar(f_exp, q_exp, c=1, n=10000)$expected_values_f, 1), 0.3)

  # expect match: suggested c = NA
  expect_match(ar(f_norm, q_norm, c=1, n=1000)$suggested_c, "NA")
  expect_match(ar(f_unif, q_unif, c=1, n=1000)$suggested_c, "NA")
  expect_match(ar(f_gamma, q_gamma, c=1, n=1000)$suggested_c, "NA")
  expect_match(ar(f_beta, q_beta, c=1, n=1000)$suggested_c, "NA")
  expect_match(ar(f_exp, q_exp, c=1, n=1000)$suggested_c, "NA")
})
```

The first test file, `test-ar.R` runs several tests regarding the output
from several input scenarios. Basically, if the target and proposal
distribution are of the same shape written in different form (in pdf
form for the target distribution and following the templates of the
proposal distribution) then the result should be the same. In the test
file, we expect that the output from the target and proposal densities
with the same shapes are correct and hence does not return any error
messages (tested using `expect_silent()`), the acceptance rate of the
proposal density with the same form as the target density is 1,
indicating perfect match (tested using `expect_equal()`) and the
expected value, when rounded to 1 decimal place, converges to the mean
of the distribution (also tested using `expect_equal()`), and again, for
target and proposal densities with the same shapes, the suggested
minimum c equals to `NA`, indicating the proposal envelopes the target
fully (tested using `expect_match()`, since `NA` is returned as a
string).

``` r
# test-ar_summary.R

test_that("Test of the input of the function ar_summary().", {

  # expect error
  expect_error(ar_summary("A"), "Input must be an object of class \"ar\"")
  expect_error(ar_summary(1), "Input must be an object of class \"ar\"")
  expect_error(ar_summary(c(1,2,3)), "Input must be an object of class \"ar\"")
})
```

The next test file, `test-ar_summary.R`, tested the input passed to the
function `ar_summary()`. Since it can only accept an object of class
`ar` as input, passing object of different class will results in an
error message `Input must be an object of class "ar"`. The test was
conducted using different type of input, i.e. a string `"A"`, a numeric
`1`, and a list `c(1,2,3)`.

If both test files run successfully, the console will return `23` passed
test items in total, with `20` tested items come from `test-ar.R` file
and the remaining 3 items from `test-ar_summary.R` file.

The content tested by the test file can be done by the user using the
instruction below.

## Test plan: user input

Besides testing using the unit tests embedded in the installed package,
user can also perform relevant testings by their own using their own
input. You will notice that in the previous example, the expected mean
and variance are rather nonsense, as we only run the sampler for a few
iterations. To be able to provide a rather accurate results, we need to
run the sampler long enough. An iteration of minimum of `n=1000` should
provide suffice (this is also the default value for `n`), although for
some densities it needs to be run long enough (more about that later).
In this test plan, we will try several scenarios of combination pair of
target-proposal densities.

### Test 1: Proposal densities with Acceptance rate: 1, Suggested minimum c: NA, and Empirical expected value of target distribution $`\approx`$ its mean

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
#>      Empirical expected value of target distribution:  0.0208 
#>      Empirical variance of target distribution:  1.0342 
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
#>      Empirical expected value of target distribution:  0.5102 
#>      Empirical variance of target distribution:  0.0847 
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
#>      Empirical expected value of target distribution:  1.4751 
#>      Empirical variance of target distribution:  0.6905 
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
#>      Empirical expected value of target distribution:  0.6184 
#>      Empirical variance of target distribution:  0.0406 
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
#>      Empirical expected value of target distribution:  0.3354 
#>      Empirical variance of target distribution:  0.1172 
#>      Suggested minimum c:  NA 
#> ----------------------
```

Notice that for the above examples, we do not use the command `print()`,
as we do not want to clutter this section. Another S3 method, `head()`,
will be implemented in a later version to accommodate this shortcoming.

If the examples run correctly, you will notice that each printed summary
gives `Acceptance rate: 1`, `Suggested minimum c: NA`, and
`Empirical expected value of target distribution: (approximately its mean)`.
This means that the proposal densities perfectly match their own target
densities, as they are the exact same distributions with the exact same
parameters. `Acceptance rate: 1` means all the proposed values are
accepted as coming from the corresponding target distribution,
`Suggested minimum c: NA` means that the proposal distribution envelopes
the target distribution entirely with the given value of `c`, and
`Empirical expected value of target distribution: (approximately its mean)`
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
#>      Acceptance rate:  0.5079 
#>      Empirical expected value of target distribution:  0.0138 
#>      Empirical variance of target distribution:  1.5206 
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
#>      Acceptance rate:  0.3063 
#>      Empirical expected value of target distribution:  -0.0115 
#>      Empirical variance of target distribution:  0.982 
#>      Suggested minimum c:  NA 
#> ----------------------
```

The suggested minimum `c` is now `NA`, which means the proposal density
now envelopes the target density fully, and the expected value and
variance are now valid. ($`\hat{E(X)} \approx \mu`$ and
$`\hat{Var(X)} \approx \sigma^2`$).

You can try using different combinations of target-density pair for this
part, such as Gamma and Uniform, or using more complicated target
distribution, such as Kumaraswamy distribution (pdf:
$`abx^{a-1}(1-x^a)^{b-1}`$) which has a support of $`x \in (0,1)`$ using
$`U(0,1)`$ as the proposal distribution (make sure to convert $`a`$ and
$`b`$ into numerics in the argument).

### Test 3: Input to `ar_summary()` function

The next part of the test is about the input to the function
`ar_summary()`. As mentioned before, the input needs to be an object of
class `ar` which is the output from the function `ar()`. Any other input
to this function will result in an error message
`Input must be an object of class "ar"`. The user can test this function
with several inputs, such as string characters, numerical, list, etc. as
the example below:

``` r
  ar_summary("A")
  ar_summary(1)
  ar_summary(c(1,2,3))
})
```
