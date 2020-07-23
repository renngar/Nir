module Nir.NexusMods

open FSharp.Data

open Utility.INI
open System

let Nexus = "Nexus"
let ApiKey = "ApiKey"

/// Get the Nexus Mods API Key, if any, from the ini
let nexusApiKey ini: IniPropertyValue * Ini =
    ini
    |> section Nexus
    |> property ApiKey
    |> propertyValue

let setNexusApiKey ini value = setIniProperty ini Nexus ApiKey value

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

type ApiResult<'T> = RateLimits * 'T

type Headers = Map<string, string>

/// Parses the `RateLimit` data returned in the Nexus Mod API calls `Headers`
let rateLimit (headers: Headers): RateLimits =
    let toInt (headers: Headers) value = headers.[value] |> int
    let toDateTime (headers: Headers) value = DateTime.Parse(headers.[value])
    { HourlyLimit = toInt headers "X-RL-Hourly-Limit"
      HourlyRemaining = toInt headers "X-RL-Hourly-Remaining"
      HourlyReset = toDateTime headers "X-RL-Hourly-Reset"
      DailyLimit = toInt headers "X-RL-Daily-Limit"
      DailyRemaining = toInt headers "X-RL-Daily-Remaining"
      DailyReset = toDateTime headers "X-RL-Daily-Reset" }

type ValidateProvider = JsonProvider<"../Data/validate.json", RootName="User">

type User = ValidateProvider.User

/// Validates the user's `apiKey` with Nexus, returns
let usersValidate apiKey =
    async {
        let! result = Http.AsyncRequest
                          ("https://api.nexusmods.com/v1/users/validate.json",
                           headers =
                               [ "Accept", "application/json"
                                 "apikey", apiKey ])
        return match result.Body with
               | Text json -> rateLimit result.Headers, ValidateProvider.Parse(json)
               | Binary data -> failwithf "Expected text, but got a %d byte binary response" data.Length
    }
