httptest2::with_mock_dir("fixtures/match_ids_by_puuid", {

  # --- TEST 1: Valid Inputs ---
  test_that("get_match_ids_by_puuid returns expected data for valid Riot ID", {

    result <- get_match_ids_by_puuid(puuid = "qXLBEI63Edhj_pF8IEzDU4F-R7qKE_CqMiuZ4MTSEEXzezC-NjPk6O7UezMSeomll5icuPvimp_5RA",
                                     routing_region = "americas",
                                     api_key = "MOCKED_API_KEY",
                                     count = 50)

    result_2 <- get_match_ids_by_puuid(puuid = "qXLBEI63Edhj_pF8IEzDU4F-R7qKE_CqMiuZ4MTSEEXzezC-NjPk6O7UezMSeomll5icuPvimp_5RA",
                                     routing_region = "americas",
                                     api_key = "MOCKED_API_KEY",
                                     start = 10,
                                     count = 40)

    expect_type(result, "character")
    expect_equal(length(result), 50)
    expect_equal(result[11:50], result_2)
  })

  # --- TEST 2: Invalid Inputs / API Errors ---
  test_that("get_match_ids_by_puuid handles invalid inputs gracefully", {

    # Test 1: Input validation
    expect_error(get_match_ids_by_puuid(puuid = 123, routing_region = "americas", api_key = "MOCKED_API_KEY"))
    expect_error(get_match_ids_by_puuid(puuid = "qXLBEI63Edhj_pF8IEzDU4F-R7qKE_CqMiuZ4MTSEEXzezC-NjPk6O7UezMSeomll5icuPvimp_5RA",
                                      routing_region = "atlantic ocean",
                                      api_key = "MOCKED_API_KEY")
    )
    expect_error(get_match_ids_by_puuid(puuid = "qXLBEI63Edhj_pF8IEzDU4F-R7qKE_CqMiuZ4MTSEEXzezC-NjPk6O7UezMSeomll5icuPvimp_5RA",
                                        routing_region = "americas",
                                        api_key = "MOCKED_API_KEY",
                                        count = 201)
                 )

    # Test 2: API 404/429 error handling
    expect_error(
      get_match_ids_by_puuid(puuid = "NonExistentPuuid", routing_region = "americas", api_key = "MOCKED_API_KEY"),
      regexp = "Riot API request failed"
    )
  })

})
