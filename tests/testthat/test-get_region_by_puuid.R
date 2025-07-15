httptest2::with_mock_dir("fixtures/region_by_puuid", {

  # --- TEST 1: Valid Inputs ---
  test_that("get_region_by_puuid returns expected data for valid Riot ID", {

    result <- get_region_by_puuid(puuid = "qXLBEI63Edhj_pF8IEzDU4F-R7qKE_CqMiuZ4MTSEEXzezC-NjPk6O7UezMSeomll5icuPvimp_5RA",
                                  routing_region = "americas",
                                  game = "tft",
                                  api_key = "MOCKED_API_KEY")

    expect_type(result, "list")
    expect_true("puuid" %in% names(result))
    expect_true("game" %in% names(result))
    expect_true("region" %in% names(result))
    expect_equal(result$game, "tft")
    expect_equal(result$region, "na1")
  })

  # --- TEST 2: Invalid Inputs / API Errors ---
  test_that("get_region_by_puuid handles invalid inputs gracefully", {

    # Test 1: Input validation
    expect_error(get_region_by_puuid(puuid = 123, routing_region = "na1", game = "tft", api_key = "MOCKED_API_KEY"))
    expect_error(get_region_by_puuid(puuid = "123",
                                     routing_region = "atlantic ocean",
                                     game = "tft",
                                     api_key = "MOCKED_API_KEY")
    )
    expect_error(get_region_by_puuid(puuid = "123",
                                     routing_region = "americas",
                                     game = "lor",
                                     api_key = "MOCKED_API_KEY")
    )

    # Test 2: API 404/429 error handling
    expect_error(
      get_region_by_puuid(puuid = "NonExistentPuuid", routing_region = "americas", game = "tft", api_key = "MOCKED_API_KEY"),
      regexp = "Riot API request failed"
    )
  })

})
