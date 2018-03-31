# algebench
A tool for benchmarking libraries/systems for doing commutative algebra


## Installation

With Homebrew: 

```
brew install PoslavskySV/rings/algebench
```

Or just clone this repo and type `sbt` to proceed with sbt console.


## Example

Generate a set of GCD problems:

```bash
algebench generate gcd uniform --characteristic 0 --bit-length 128 --n-variables 3 --n-problems 100 --min-size 50 --max-size 50 --min-degree 0 --max-degree 50 gcd.problems
```

Solve them with different tools and get statistics:

```bash
algebench solve  --rings --singular --mathematica --form --fermat gcd.problems gcd.timigs.tsv
```

## General usage

```
Usage: algebench action [OPTION]...
Options:
  -k, --keep-temp-files
      --nokeep-temp-files
  -h, --help                Show help message
  -v, --version             Show version of this program

Subcommand: generate

Usage: algebench generate [gcd|factor] [OPTIONS] output_file
Generates problem data and writes it to output_file

      --rnd-seed  <arg>   Random seed
  -h, --help              Show help message

Subcommand: generate gcd

Usage: algebench generate gcd [uniform|sharp|custom] [OPTIONS] output_file
Generates gcd problems data using specified method (uniform/sharp/custom) and writes to output_file

  -h, --help   Show help message

Subcommand: generate gcd uniform

Generates polynomials with uniformly distributed exponents
      --bit-length  <arg>       Bit length of coefficients (only for
                                characteristic 0)
  -c, --characteristic  <arg>   Field characteristic
      --max-degree  <arg>       Maximal exponent of each variable in monomials
      --max-size  <arg>         Maximal number of terms in factors
      --min-degree  <arg>       Minimal exponent of each variable in monomials
      --min-size  <arg>         Minimal number of terms in factors
  -n, --n-problems  <arg>       Number of problems to generate
      --n-variables  <arg>      Number of variables
  -s, --size  <arg>             Size of factors and gcd
  -h, --help                    Show help message

 trailing arguments:
  output (required)   Output file


Subcommand: generate gcd sharp

Generates polynomials with sharp exponents
      --bit-length  <arg>       Bit length of coefficients (only for
                                characteristic 0)
  -c, --characteristic  <arg>   Field characteristic
      --max-size  <arg>         Maximal number of terms in factors
      --min-size  <arg>         Minimal number of terms in factors
  -n, --n-problems  <arg>       Number of problems to generate
      --n-variables  <arg>      Number of variables
  -s, --size  <arg>             Size of factors and gcd
      --total-degree  <arg>     Total degree of polynomials
  -h, --help                    Show help message

 trailing arguments:
  output (required)   Output file


Subcommand: generate gcd custom

Generates polynomials with custom distributions
      --bit-length  <arg>       Bit length of coefficients (only for
                                characteristic 0)
  -c, --characteristic  <arg>   Field characteristic
  -f, --factor1  <arg>          JSON string for first factor distribution
      --factor2  <arg>          JSON string for second factor distribution
  -g, --gcd  <arg>              JSON string for gcd distribution
  -n, --n-problems  <arg>       Number of problems to generate
      --n-variables  <arg>      Number of variables
  -h, --help                    Show help message

 trailing arguments:
  output (required)   Output file
Subcommand: generate factor

Usage: algebench generate factor [uniform|sharp|custom] [OPTIONS] output_file
Generates factorization problems data using specified method (uniform/sharp/custom) and writes to output_file

  -h, --help   Show help message

Subcommand: generate factor uniform

Generates polynomials with uniformly distributed exponents
      --bit-length  <arg>       Bit length of coefficients (only for
                                characteristic 0)
  -c, --characteristic  <arg>   Field characteristic
      --max-degree  <arg>       Maximal exponent of each variable in monomials
      --max-size  <arg>         Maximal number of terms in factors
      --min-degree  <arg>       Minimal exponent of each variable in monomials
      --min-size  <arg>         Minimal number of terms in factors
      --n-factors  <arg>        Number of factors
  -n, --n-problems  <arg>       Number of problems to generate
      --n-variables  <arg>      Number of variables
      --no-factors              All input polynomials are irreducible
      --nono-factors
  -s, --size  <arg>             Size of factors and gcd
  -h, --help                    Show help message

 trailing arguments:
  output (required)   Output file


Subcommand: generate factor sharp

Generates polynomials with sharp exponents
      --bit-length  <arg>       Bit length of coefficients (only for
                                characteristic 0)
  -c, --characteristic  <arg>   Field characteristic
      --max-size  <arg>         Maximal number of terms in factors
      --min-size  <arg>         Minimal number of terms in factors
      --n-factors  <arg>        Number of factors
  -n, --n-problems  <arg>       Number of problems to generate
      --n-variables  <arg>      Number of variables
      --no-factors              All input polynomials are irreducible
      --nono-factors
  -s, --size  <arg>             Size of factors and gcd
      --total-degree  <arg>     Total degree of polynomials
  -h, --help                    Show help message

 trailing arguments:
  output (required)   Output file


Subcommand: generate factor custom
Generates polynomials with custom distributions
      --bit-length  <arg>       Bit length of coefficients (only for
                                characteristic 0)
  -c, --characteristic  <arg>   Field characteristic
  -d, --dist  <arg>             JSON string for factor distribution
      --n-factors  <arg>        Number of factors
  -n, --n-problems  <arg>       Number of problems to generate
      --n-variables  <arg>      Number of variables
      --no-factors              All input polynomials are irreducible
      --nono-factors
  -h, --help                    Show help message

 trailing arguments:
  output (required)   Output file
Subcommand: solve

Usage: algebench solve [OPTIONS] input_file output_file
Solves generated probles with provided solvers and writes timing statistics to output_file

      --fermat                    Fermat (http://home.bway.net/lewis/)
      --nofermat
      --fermat-exec  <arg>        Path to Fermat executable
      --form                      FORM (https://www.nikhef.nl/~form/)
      --noform
      --form-exec  <arg>          Path to FORM executable
      --mathematica               Wolfram Mathematica
      --nomathematica
      --mathematica-exec  <arg>   Path to Mathematica executable
      --rings                     Rings (http://ringsalgebra.io)
      --norings
      --rings-exec  <arg>         Path to Rings executable
      --singular                  Singular (https://www.singular.uni-kl.de)
      --nosingular
      --singular-exec  <arg>      Path to Singular executable
      --threads  <arg>            Number of threads for running solvers, if
                                  euqual to 1 (default) all solvers will be run
                                  sequentially
  -h, --help                      Show help message

 trailing arguments:
  input (required)    Input file
  output (required)   Output file


```
