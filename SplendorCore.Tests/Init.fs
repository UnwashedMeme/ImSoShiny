module Tests.Init
open Expecto
open FsCheck
open Splendor.Gamestate
open Splendor.Bank

let chooseFromList xs =
  gen { let! i = Gen.choose (0, List.length xs-1)
        return List.item i xs }


let genRandomCoin = Arb.generate<Coin>
let genRandomMine = Arb.generate<Mine>

let genRandomPlayerCoins =
    let gen s =
        Gen.resize ( s / 10) (genRandomCoin |> Gen.listOf)
    gen |> Gen.sized

type ActiveGameState = GameState

type ModelGen() =
    static member VictoryPoints() : Arbitrary<VPs> =
        Gen.choose (0,6) |> Arb.fromGen

    static member Count(): Arbitrary<Count> =
        Gen.choose (0,4) |> Arb.fromGen

    static member CoinList(): Arbitrary<Coin list> =
        genRandomPlayerCoins |> Arb.fromGen

    static member MineList(): Arbitrary<Mine list> =
        let gen s =
            Gen.resize (s/8) (Arb.generate<Mine> |> Gen.listOf)
        gen |> Gen.sized |> Arb.fromGen

    static member TurnData(): Arbitrary<TurnData> =
        let gen = gen {
            let! players = Arb.generate<Player> |> Gen.nonEmptyListOf
            let! player = Gen.elements players
            let makeTurnData phase bank tier1 tier2 tier3 nobles targetVictoryPoints = {
                players = players;
                currentPlayer = player;
                phase = phase;
                bank = bank;
                tier1 = tier1;
                tier2 = tier2;
                tier3 = tier3;
                nobles = nobles;
                targetVictoryPoints = targetVictoryPoints;
            }
            return! makeTurnData
                <!> Arb.generate<TurnPhase>
                <*> Arb.generate<Bank>
                <*> Arb.generate<Card list>
                <*> Arb.generate<Card list>
                <*> Arb.generate<Card list>
                <*> Arb.generate<Noble list>
                <*> Arb.generate<VPs>
        }
        (function s -> Gen.resize (s/15) gen) |> Gen.sized |> Arb.fromGen

    static member ActiveGameState(): Arbitrary<GameState> =
        let gen = gen {
            let makeGameState players bank tier1 tier2 tier3 nobles targetVictoryPoints = {
                Turns = [];
                Players = players;
                ActivePlayer = players.Head; // TODO: better sampling
                Bank = bank;
                Tier1Cards = tier1;
                Tier2Cards = tier2;
                Tier3Cards = tier3;
                Nobles = nobles;
                TargetVictoryPoints = targetVictoryPoints;
            }
            return! makeGameState
                <!> Arb.generate<Player list>
                <*> Arb.generate<Bank>
                <*> Arb.generate<Card list>
                <*> Arb.generate<Card list>
                <*> Arb.generate<Card list>
                <*> Arb.generate<Noble list>
                <*> Arb.generate<VPs>
        }
        (function s -> Gen.resize (s/15) gen) |> Gen.sized |> Arb.fromGen
    // static member Turn(): Arbitrary<Turn> =
    //     let gen = gen {
    //         let! players = Arb.generate<Player> |> Gen.nonEmptyListOf
    //         let! player = Gen.elements players
    //     }


let BaseConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ModelGen>] }
