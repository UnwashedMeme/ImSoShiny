module Tests.Server
open Expecto
open FsCheck
open Splendor.Models
open Splendor.Events
open Splendor.Server

type ServerGen() =
    static member Player() : Arbitrary<VPs> = 
        Gen.choose (0,6) |> Arb.fromGen

let config = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ServerGen>] }
