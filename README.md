
<!-- README.md is generated from README.Rmd. Please edit this file -->

# tftfetch

The goal of tftfetch is to provide straightforwards functions to connect
to the Riot API and return data in a usable format. This package focuses
on endpoints concerning the Riot game Team Fight Tactics (TFT).

## Installation

You can install the released version of tftfetch from
<a href="https://CRAN.R-project.org" target="_blank">CRAN</a> with:

``` r
install.packages("tftfetch")
```

and the development version of tftfetch from
<a href="https://github.com/" target="_blank">GitHub</a> with:

``` r
# install.packages("pak")
pak::pak("tidymodels/tftfetch")
```

## API Key Setup

To use `tftfetch`, you will need a Riot Games API key.

Obtain an API Key:

- Visit the
  <a href="https://developer.riotgames.com/" target="_blank">Riot Games
  Developer Portal</a>.

- Register for an account (if you don’t have one).

- Go to “Dashboard” to Generate a “Development API Key” or apply for a
  “Production API Key” (depending on your usage needs). Note that
  development keys expire every 24 hours and must be refreshed.

Set Up Your `.env` File:

- It is best practice to store your API key as an environment variable
  to avoid hardcoding it directly into your scripts or committing it to
  version control (like Git).

- Create a file named .env in the root directory of your R project.

- Add your API key to this file in the following format (replace
  YOUR_ACTUAL_RIOT_API_KEY with the key you obtained):

``` r
RIOT_KEY=YOUR_ACTUAL_RIOT_API_KEY
```

- Ensure there are no spaces before or after the equals sign.

Load the API Key in R:

- It is recommended to us a package like `dotenv` to load this `.env`
  file.

- For example, including `load_dot_env()` at the beginning of your R
  scripts or R Markdown documents where you use `tftfetch` functions.
  This makes your `RIOT_KEY` available via `Sys.getenv("RIOT_KEY")`.

## Example

``` r
# First, ensure dotenv is installed: install.packages("dotenv")
library(tftfetch)
library(dotenv)

load_dot_env() # Load environment variables from your .env file
api_key <- Sys.getenv("RIOT_KEY") # Retrieve your API key

# Example: Get account data for "Wander#HENRO" in the Americas region
account <- get_account_by_riot_id("Wander", "HENRO", "americas", api_key)

# If you prefer not to save the API as a variable, you can use Sys.getenv("RIOT_KEY") directly in the function
account <- get_account_by_riot_id("Wander", "HENRO", "americas", Sys.getenv("RIOT_KEY"))

print(account)
```
