#' Fetch Riot Account Data by Riot Game Name and Tag Line
#'
#' Retrieves a player's Riot Account information (PUUID, gameName, and tagLine)
#' using the Riot Account-V1 endpoint.
#'
#' @details
#' This function interacts with the `https://{routing_region}.api.riotgames.com/riot/account/v1/accounts/by-riot-id/{game_name}/{tag_line}`
#' endpoint.
#'
#' Riot IDs are formatted as "GameName#TagLine" (e.g., "Wander#HENRO").
#' This function requires the two parts (`game_name` and `tag_line`) to be
#' provided as separate arguments.
#'
#' @param game_name A character string representing the player's game name (the part of the Riot ID *before* the '#').
#' @param tag_line A character string representing the player's tag line (the part of the Riot ID *after* the '#').
#' @param routing_region A character string representing the API routing region (e.g., "americas", "asia", "europe").
#' @param api_key A character string containing your valid Riot API key.
#'
#' The `routing_region` parameter (e.g., "americas", "asia", "europe") determines
#' which API cluster the request is sent to. For Account-V1 endpoints, the account
#' data is globally accessible, meaning you can query any Riot ID through any
#' of these regional routing values, and you will receive the same result.
#' It is recommended to use the regional routing value closest to your service's
#' physical location to minimize latency.
#'
#' @return A list containing the player's PUUID, gameName, and tagLine.
#' @examples
#' \dontrun{
#' # Replace with actual credentials
#' api_key <- "YOUR_API_KEY"
#' account_info <- get_account_by_riot_id(game_name = "Wander", tag_line = "HENRO", routing_region = "americas", api_key)
#' print(account_info$puuid)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom checkmate assert_character assert_choice
#' @importFrom httr2 request req_url_path_append req_headers_redacted req_error req_perform resp_status resp_body_json
#'
get_account_by_riot_id <- function(game_name, tag_line, routing_region, api_key) {

  # Input validation
  assert_character(game_name, len = 1, any.missing = FALSE)
  assert_character(tag_line, len = 1, any.missing = FALSE)
  assert_character(routing_region, len = 1, any.missing = FALSE)
  assert_choice(tolower(routing_region), choices = c("americas", "asia", "europe"))
  assert_character(api_key, len = 1, any.missing = FALSE)

  # Base URL for the account-v1 endpoint
  base_url <- paste0("https://", tolower(routing_region), ".api.riotgames.com/riot/account/v1/accounts/by-riot-id")

  # Construct the request
  response <- request(base_url) %>%
    req_url_path_append(game_name, tag_line) %>%
    req_headers_redacted("X-Riot-Token" = api_key) %>%
    # Use req_error() to prevent httr2 from stopping on 4xx/5xx
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

    # Stop the function and provide a error message
    stop(paste0("Riot API request failed (Status ", status_code, "): ", error_message),
         call. = FALSE)
  }
}
