library(tidyverse)
library(httr)
library(boxr)


box_auth(client_id = "jemswi6nx6bie6xep36kgrcvibbgpw71", 
         client_secret = "2AHfw8XXGPU6I37a5KYQ7aF2a10quWXM")

### failed attempts with httr
myapp <- oauth_app(
    "box", 
    key = "6a15rlvm5w3au3l3p09o5o68soo8slhd", 
    secret = "8khpqPzalF6M0WpvMOg60xYTvGT2lbCS")

box_endpoint <- oauth_endpoint(
    request = NULL,
    authorize = "https://account.box.com/api/oauth2/authorize?client_id=214593079&response_type=cod",
    access = "https://account.box.com/api/oauth2/authorize?client_id=214593079&response_type=cod", 
    base_url = "https://account.box.com/api/oauth2/authorize"
)


box_token <- oauth2.0_token(box_endpoint, myapp)
