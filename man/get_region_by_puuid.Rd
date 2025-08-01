% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_region_by_puuid.R
\name{get_region_by_puuid}
\alias{get_region_by_puuid}
\title{Fetch Player's Active Shard by Game and PUUID}
\usage{
get_region_by_puuid(puuid, routing_region, game, api_key)
}
\arguments{
\item{puuid}{A character string representing the player's PUUID.}

\item{routing_region}{A character string indicating the regional API cluster to use.
Valid choices are "americas", "asia", or "europe".}

\item{game}{A character string specifying the game for which to retrieve the active shard.
Valid values are "lol" or "tft".}

\item{api_key}{A character string containing your valid Riot API key.}
}
\value{
A list containing the player's \code{puuid}, the \code{game} queried, and their \code{activeShard} (game region).
Example: \code{list(puuid = "...", game = "tft", activeShard = "na1")}
}
\description{
Retrieves the active shard (game region) for a player in a specific Riot game
(League of Legends or Teamfight Tactics) using their PUUID.
This endpoint is part of the Riot Account-V1 API.
}
\details{
This function interacts with the \verb{https://\{routing_region\}.api.riotgames.com/riot/account/v1/region/by-game/\{game\}/by-puuid/\{puuid\}}
endpoint.

The \code{routing_region} ("americas", "asia", "europe") determines
which API cluster the request is sent to. For this endpoint, the data
is globally accessible, so any of these routing values can be used to query
any player's active shard, regardless of their actual in-game region.

Valid \code{game} values include:
\itemize{
\item "lol" (League of Legends)
\item "tft" (Teamfight Tactics)
}

The \code{puuid} parameter is the Player Universally Unique ID, which can be
obtained from \code{get_account_by_riot_id()}.
}
\examples{
\dontrun{
# Replace with your actual API key and a valid PUUID
api_key <- "YOUR_API_KEY"
example_puuid <- "YOUR_EXAMPLE_PUUID_HERE"

# Get active shard for a TFT player in the Americas region
active_shard_tft <- get_active_shard_by_puuid(
  puuid = example_puuid,
  routing_region = "americas",
  game = "tft",
  api_key = api_key
)
print(active_shard_lor$region)
}

}
