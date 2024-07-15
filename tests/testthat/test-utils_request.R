with_mock_api({
  test_that("We can obtain a token", {
    #Get the token
    token <- get_token(Sys.getenv("CLIENT_ID"), Sys.getenv("CLIENT_SECRET"))

    #Check its bearer
    expect_equal(token$token_type, "Bearer")

    #Check the creation time
    expect_true(difftime(Sys.time(), token$creation_time, units = "mins") < 1)

  })
})
