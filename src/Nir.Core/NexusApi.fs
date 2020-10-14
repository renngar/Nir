module Nir.NexusApi

open System

open FSharp.Data

/// A map of HTTP headers and values
type Headers = Map<string, string>

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
type RateLimits() =

    // Assume the full rate quota when initializing RateLimits.  The actual values will be retrieved soon enough.
    member val HourlyLimit = 100 with get, set
    member val HourlyRemaining = 100 with get, set
    member val HourlyReset = DateTime.Now with get, set
    member val DailyLimit = 2500 with get, set
    member val DailyRemaining = 2500 with get, set
    member val DailyReset = DateTime.Now with get, set

    /// Parses the `RateLimit` data returned in the Nexus Mod API calls `Headers`
    member this.Update(headers: Headers) =
        let toInt (headers: Headers) value = headers.[value] |> int
        let toDateTime (headers: Headers) value = DateTime.Parse(headers.[value])

        this.HourlyLimit <- toInt headers "X-RL-Hourly-Limit"
        this.HourlyRemaining <- toInt headers "X-RL-Hourly-Remaining"
        this.HourlyReset <- toDateTime headers "X-RL-Hourly-Reset"
        this.DailyLimit <- toInt headers "X-RL-Daily-Limit"
        this.DailyRemaining <- toInt headers "X-RL-Daily-Remaining"
        this.DailyReset <- toDateTime headers "X-RL-Daily-Reset"

/// Represents an HTTP status code
type StatusCode = int

/// A Nexus Mods API call failed with a given status code and message
type ApiError =
    { StatusCode: StatusCode
      Message: string }

/// Returns an `ApiError` wrapped in a Result<'T,ApiError>.Error
let apiError statusCode msg =
    Error
        { StatusCode = statusCode
          Message = msg }

type ApiResult<'T> = Result<'T, ApiError>

type Parser<'T> = string -> 'T

type private NexusErrorProvider = JsonProvider<"../../src/Nir.Core/ApiSamples/error.json", RootName="NexusError">

type private NexusError = NexusErrorProvider.NexusError

type private Md5SearchProvider = JsonProvider<"../../src/Nir.Core/ApiSamples/md5_search.json", RootName="Md5Search">

type Md5Search = Md5SearchProvider.Md5Search

type private GamesProvider = JsonProvider<"../../src/Nir.Core/ApiSamples/games.json", RootName="Games">

type Game = GamesProvider.Game

type private ValidateProvider = JsonProvider<"../../src/Nir.Core/ApiSamples/validate.json", RootName="User">

type User = ValidateProvider.User

/// Data specific to Nexus
type Nexus(apiKey: string) =
    let mutable verified, key = false, apiKey

    member this.ApiKey
        with get () = if verified then key else ""
        and set (value) =
            verified <- false
            key <- value

    member val RateLimits = RateLimits()

    member private this.AsyncRequest (parser: Parser<'T>) verify apiUrl: Async<ApiResult<'T>> =
        async {
            try
                let mutable headers = [ "Accept", "application/json" ]
                if verified || verify then headers <- ("apikey", key) :: headers

                // Setting silentHttpErrors returns the error response rather than throwing an exception.
                let! result = Http.AsyncRequest(apiUrl, headers = headers, silentHttpErrors = true)

                return
                    match result.Body with
                    | Text json ->
                        match result.StatusCode with
                        | HttpStatusCodes.OK ->
                            this.RateLimits.Update(result.Headers)

                            Ok(parser json)
                        | status ->
                            NexusErrorProvider.Parse(json).Message
                            |> apiError status
                    | Binary data ->
                        apiError
                            result.StatusCode
                            (sprintf "Expected text, but got a %d byte binary response" data.Length)
            with exn -> return apiError exn.HResult exn.Message
        }

    ////////////////////////////////////////////////////////////////////////////////
    // Mods
    ////////////////////////////////////////////////////////////////////////////////

    // /v1/games/{game_domain_name}/mods/md5_search/{md5_hash}.json
    ////////////////////////////////////////////////////////////////////////////////
    member this.md5Search(game, hash) =
        try
            sprintf "https://api.nexusmods.com/v1/games/%s/mods/md5_search/%s.json" game hash
            |> this.AsyncRequest Md5SearchProvider.Parse false

        with _ -> async { return apiError -1 "Non-existent file" }


    ////////////////////////////////////////////////////////////////////////////////
    // Games
    ////////////////////////////////////////////////////////////////////////////////

    // /v1/games.json?include_unapproved=<bool>
    ////////////////////////////////////////////////////////////////////////////////
    member this.games(includeUnapproved) =
        if includeUnapproved then "true" else "false"
        |> sprintf "https://api.nexusmods.com/v1/games.json?include_unapproved=%s"
        |> this.AsyncRequest GamesProvider.Parse false

    ////////////////////////////////////////////////////////////////////////////////
    // User
    ////////////////////////////////////////////////////////////////////////////////

    // /vi/users/validate.json
    ////////////////////////////////////////////////////////////////////////////////

    /// Validates the user's `apiKey` with Nexus
    member this.usersValidate(apiKey: string): Async<ApiResult<User>> =
        async {
            verified <- false
            key <- apiKey

            let! result =
                this.AsyncRequest ValidateProvider.Parse true "https://api.nexusmods.com/v1/users/validate.json"

            match result with
            | Ok _ -> verified <- true
            | Error _ -> ()

            return result
        }
