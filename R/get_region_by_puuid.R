#' Fetch Player's Active Shard by Game and PUUID
#'
#' Retrieves the active shard (game region) for a player in a specific Riot game
#' (League of Legends or Teamfight Tactics) using their PUUID.
#' This endpoint is part of the Riot Account-V1 API.
#'
#' @details
#' This function interacts with the `https://{routing_region}.api.riotgames.com/riot/account/v1/region/by-game/{game}/by-puuid/{puuid}`
#' endpoint.
#'
#' The `routing_region` ("americas", "asia", "europe") determines
#' which API cluster the request is sent to. For this endpoint, the data
#' is globally accessible, so any of these routing values can be used to query
#' any player's active shard, regardless of their actual in-game region.
#'
#' Valid `game` values include:
#' * "lol" (League of Legends)
#' * "tft" (Teamfight Tactics)
#'
#' The `puuid` parameter is the Player Universally Unique ID, which can be
#' obtained from `get_account_by_riot_id()`.
#'
#' @param game A character string specifying the game for which to retrieve the active shard.
#'   Valid values are "lol" or "tft".
#' @param puuid A character string representing the player's PUUID.
#' @param routing_region A character string indicating the regional API cluster to use.
#'   Valid choices are "americas", "asia", or "europe".
#' @param api_key A character string containing your valid Riot API key.
#'
#' @return A list containing the player's `puuid`, the `game` queried, and their `activeShard` (game region).
#'   Example: `list(puuid = "...", game = "tft", activeShard = "na1")`
#' @examples
#' \dontrun{
#' # Replace with your actual API key and a valid PUUID
#' api_key <- "YOUR_API_KEY"
#' example_puuid <- "YOUR_EXAMPLE_PUUID_HERE"
#'
#' # Get active shard for a TFT player in the Americas region
#' active_shard_tft <- get_active_shard_by_puuid(
#'   puuid = example_puuid,
#'   routing_region = "americas",
#'   game = "tft",
#'   api_key = api_key
#' )
#' print(active_shard_lor$region)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom checkmate check_character assert_choice
#' @importFrom httr2 request req_url_path_append req_headers_redacted req_error req_perform resp_status resp_body_json
#'
get_region_by_puuid <- function(puuid, routing_region, game, api_key) {

  # Input validation
  check_character(game, len = 1, any.missing = FALSE)
  assert_choice(game, choices = c("lol", "tft")) # Valid games for active-shards

  check_character(puuid, len = 1, any.missing = FALSE)

  check_character(routing_region, len = 1, any.missing = FALSE)
  assert_choice(routing_region, choices = c("americas", "asia", "europe")) # Regional routing values

  check_character(api_key, len = 1, any.missing = FALSE)

  # Base URL for the active-shards endpoint
  base_url <- paste0(
    "https://", routing_region,
    ".api.riotgames.com/riot/account/v1/region/by-game"
  )

  response <- request(base_url) %>%
    req_url_path_append(game, "by-puuid", puuid) %>%
    req_headers_redacted("X-Riot-Token" = api_key) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()

  # Check the status code
  status_code <- resp_status(response)

  if (status_code == 200) {
    # Success: Return the JSON body as a simplified R object (list/data frame)
    resp_body_json(response, simplifyVector = TRUE)
  } else {
    # Error handling for non-200 status codes
    error_details <- tryCatch({
      resp_body_json(response, simplifyVector = TRUE)
    }, error = function(e) {
      paste("Could not parse error response body. Status:", status_code)
    })

    if (is.list(error_details) && !is.null(error_details$status$message)) {
      error_message <- error_details$status$message
    } else {
      error_message <- "Unknown API error."
    }

    stop(paste0(
      "Riot API request failed (Status ", status_code, "): ", error_message, sep = ""), call. = FALSE)
  }
}
