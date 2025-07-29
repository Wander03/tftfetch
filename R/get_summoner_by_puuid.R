#' Fetch TFT Summoner Data by PUUID
#'
#' Retrieves a player's TFT summoner information (PUUID, profileIconId,
#' revisionDate, summonerLevel) using the TFT Summoner-V1 endpoint,
#' specifically by providing their PUUID.
#'
#' @details
#'
#' This function interacts with the `https://{region}.api.riotgames.com/tft/summoner/v1/summoners/by-puuid/{puuid}`
#' endpoint.
#'
#' The `region` parameter specifies the game server region (e.g., "na1", "euw1")
#' where the summoner exists. This is **distinct** from the broader regional
#' routing values ("americas", "asia", "europe") used by Account-V1 endpoints.
#' It can be obtained from `get_region_by_puuid()`.
#'
#' Valid Region List:
#'
#' * br1 (Brazil)
#' * eun1 (Europe Nordic & East)
#' * euw1 (Europe West)
#' * jp1 (Japan)
#' * kr (Korea)
#' * la1 (Latin America North)
#' * la2 (Latin America South)
#' * me1 (Middle East)
#' * na1 (North America)
#' * oc1 (Oceania)
#' * ru (Russia)
#' * sg2 (Singapore)
#' * th2 (Thailand)
#' * tr1 (Turkey)
#' * tw2 (Taiwan)
#' * vn2 (Vietnam)
#'
#' The `puuid` is an encrypted Player Universally Unique ID, which is a unique
#' identifier for a player across all Riot Games titles and regions. It can be
#' obtained from the Account-V1 API (e.g., using `get_account_by_riot_id()`)
#' or from match data.
#'
#' Revision Date: The `revisionDate` field in the returned data indicates when
#' the summoner was last modified, specified as epoch milliseconds. Events that
#' update this timestamp include: profile icon changes, playing the tutorial or
#' advanced tutorial, finishing a game, or a summoner name change.
#'
#' @param puuid A character string representing the player's encrypted PUUID.
#'   This PUUID is typically obtained from the Account-V1 API or Match-V5 API.
#' @param region A character string representing the platform (server) region
#'   where the summoner exists (e.g., "na1", "euw1", "kr", "br1", "jp1").
#'   This is NOT a regional routing value like "americas".
#' @param api_key A character string containing your valid Riot API key.
#'
#' @return A list containing the summoner's PUUID, profileIconId, revisionDate,
#'   and summonerLevel.
#' @examples
#' \dontrun{
#' # Replace with your actual API key and a valid PUUID for the specified platform
#' api_key <- "YOUR_API_KEY"
#' example_puuid <- "YOUR_ENCRYPTED_PUUID_HERE"
#' summoner_info <- get_summoner_by_puuid(puuid = example_puuid, region = "na1", api_key = api_key)
#' print(summoner_info$profileIconId)
#' print(summoner_info$summonerLevel)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom checkmate assert_character assert_choice
#' @importFrom httr2 request req_url_path_append req_headers_redacted req_error req_perform resp_status resp_body_json
#'
get_summoner_by_puuid <- function(puuid, region, api_key) {

  # Input validation
  assert_character(puuid, len = 1, any.missing = FALSE)
  assert_character(region, len = 1, any.missing = FALSE)

  # Assert that 'region' is one of the valid game-specific platform IDs
  valid_platforms <- c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "me1", "na1", "oc1", "ru", "sg2", "tr1", "tw2", "vn2")
  assert_choice(tolower(region), choices = valid_platforms)

  assert_character(api_key, len = 1, any.missing = FALSE)

  # Base URL for the TFT Summoner-V1 endpoint by PUUID
  # Use 'platform' for the subdomain here
  base_url <- paste0("https://", tolower(region), ".api.riotgames.com/tft/summoner/v1/summoners/by-puuid")

  # Construct the request
  response <- request(base_url) %>%
    req_url_path_append(puuid) %>%
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

    # Try to extract error message from JSON body
    error_details <- tryCatch({
      resp_body_json(response, simplifyVector = TRUE)
    }, error = function(e) {
      # If the body isn't JSON (e.g., HTML error page), return a standard message
      paste("Could not parse error response body. Status:", status_code)
    })

    # Extract the Riot API specific status message if available
    if (is.list(error_details) && !is.null(error_details$status$message)) {
      error_message <- error_details$status$message
    } else {
      error_message <- "Unknown API error."
    }

    # Stop the function and provide a descriptive error message
    stop(paste0("Riot API request failed (Status ", status_code, "): ", error_message),
         call. = FALSE)
  }
}
