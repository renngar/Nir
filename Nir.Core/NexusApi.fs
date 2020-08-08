module Nir.NexusApi

open System

open FSharp.Data

open Nir.Utility
open Utility.INI

let NexusSection = "Nexus"
let ApiKeyProp = "ApiKey"

/// Get the Nexus Mods API Key, if any, from the ini
let nexusApiKey ini: IniPropertyValue * Ini =
    ini
    |> section NexusSection
    |> property ApiKeyProp
    |> propertyValue

let setNexusApiKey ini value = setIniProperty ini NexusSection ApiKeyProp value

/// The rate limiting data returned by the Nexus Mods APIs. The limits are as follows:
///
/// 2,500 requests within 24 hours and once this limit has been exceeded by a user they are then restricted to 100
/// requests per hour.
///
/// Once a client's limits are reached, the API will return a HTTP 429 code, along with the headers above, allowing the
/// client to determine which limit has been breached.
///
/// A 429 status code can also be served by nginx if the client sends more than 30 requests per second. Nginx will
/// however allow bursts over this for very short periods of time.
///
/// Some routes don't count towards the hourly limit. Currently this includes the `/v1/users/validate.json` call.
type RateLimits =
    { HourlyLimit: int
      HourlyRemaining: int
      HourlyReset: DateTime
      DailyLimit: int
      DailyRemaining: int
      DailyReset: DateTime }
    // Assume the full rate quota when initializing RateLimits.  The actual values will be retrieved soon enough.
    static member initialLimits =
        let now = DateTime.UtcNow
        { HourlyLimit = 100
          HourlyRemaining = 100
          HourlyReset = now
          DailyLimit = 2500
          DailyRemaining = 2500
          DailyReset = now }

/// A Nexus API key
type ApiKey = string

/// Data specific to Nexus
type Nexus =
    { ApiKey: ApiKey
      RateLimits: RateLimits }

/// A map of HTTP header and values
type Headers = Map<string, string>

/// Represents an HTTP status code
type StatusCode = int

/// A Nexus Mods API call succeeded returning the given rate limits and value
type ApiSuccess<'T> =
    { Nexus: Nexus
      Result: 'T }

/// A Nexus Mods API call failed with a given status code and message
type ApiError =
    { StatusCode: StatusCode
      Message: string }

/// Returns an `ApiError` wrapped in a Result<'T,ApiError>.Error
let apiError statusCode msg =
    Error
        { StatusCode = statusCode
          Message = msg }

type ApiResult<'T> = Result<ApiSuccess<'T>, ApiError>

/// Parses the `RateLimit` data returned in the Nexus Mod API calls `Headers`
let private rateLimit (headers: Headers): RateLimits =
    let toInt (headers: Headers) value = headers.[value] |> int
    let toDateTime (headers: Headers) value = DateTime.Parse(headers.[value])
    { HourlyLimit = toInt headers "X-RL-Hourly-Limit"
      HourlyRemaining = toInt headers "X-RL-Hourly-Remaining"
      HourlyReset = toDateTime headers "X-RL-Hourly-Reset"
      DailyLimit = toInt headers "X-RL-Daily-Limit"
      DailyRemaining = toInt headers "X-RL-Daily-Remaining"
      DailyReset = toDateTime headers "X-RL-Daily-Reset" }

type NexusErrorProvider = JsonProvider<"../Data/error.json", RootName="NexusError">

type NexusError = NexusErrorProvider.NexusError

let callApi nexus parser apiUrl =
    async {
        try
            // Setting silentHttpErrors returns the error response rather than throwing an exception.
            let! result = Http.AsyncRequest
                              (apiUrl,
                               headers =
                                   [ "Accept", "application/json"
                                     "apikey", nexus.ApiKey ], silentHttpErrors = true)

            return match result.Body with
                   | Text json ->
                       match result.StatusCode with
                       | HttpStatusCodes.OK ->
                           Ok
                               { Nexus = { nexus with RateLimits = rateLimit result.Headers }
                                 Result = parser (json) }
                       | status -> NexusErrorProvider.Parse(json).Message |> apiError status
                   | Binary data ->
                       apiError result.StatusCode
                           (sprintf "Expected text, but got a %d byte binary response" data.Length)
        with exn -> return apiError exn.HResult exn.Message
    }

////////////////////////////////////////////////////////////////////////////////
// Mods
////////////////////////////////////////////////////////////////////////////////

// /v1/games/{game_domain_name}/mods/md5_search/{md5_hash}.json
////////////////////////////////////////////////////////////////////////////////

type Md5SearchProvider = JsonProvider<"../Data/md5_search.json", RootName="Md5Search">

type Md5Search = Md5SearchProvider.Md5Search

let md5Search (nexus, game, file) =
    try
       Md5sum.md5sum file
       |> sprintf "https://api.nexusmods.com/v1/games/%s/mods/md5_search/%s.json" game
       |> callApi nexus Md5SearchProvider.Parse
    with _ -> async { return apiError -1 "Non-existent file" }

////////////////////////////////////////////////////////////////////////////////
// Games
////////////////////////////////////////////////////////////////////////////////

// /v1/games.json?include_unapproved=<bool>
////////////////////////////////////////////////////////////////////////////////

type GamesProvider = JsonProvider<"../Data/games.json", RootName="Games">

type Game = GamesProvider.Game

let games (nexus, includeUnapproved) =
    if includeUnapproved then "true" else "false"
    |> sprintf "https://api.nexusmods.com/v1/games.json?include_unapproved=%s"
    |> callApi nexus GamesProvider.Parse

////////////////////////////////////////////////////////////////////////////////
// User
////////////////////////////////////////////////////////////////////////////////

// /vi/users/validate.json
////////////////////////////////////////////////////////////////////////////////

type ValidateProvider = JsonProvider<"../Data/validate.json", RootName="User">

type User = ValidateProvider.User

/// Validates the user's `apiKey` with Nexus
let usersValidate nexus: Async<ApiResult<User>> =
    callApi nexus ValidateProvider.Parse "https://api.nexusmods.com/v1/users/validate.json"
