// An "enum"-type union for card suit.
type CardSuit = 
    | Spades 
    | Clubs
    | Diamonds
    | Hearts

// Kinds: 1 = Ace, 2 = Two, ..., 11 = Jack, 12 = Queen, 13 = King.
type Card = {suit : CardSuit; kind : int}

// The state of a single game of blackjack. Tracks the current deck, the player's hand, and the dealer's hand.
type GameState = {deck : Card list; playerHand : Card list; dealerHand : Card list}

// A log of results from many games of blackjack.
type GameLog = {playerWins : int; dealerWins : int; draws : int}

// Identifying who owns a given hand.
type HandOwner = 
    | Player
    | Dealer


// UTILITY METHODS

// Returns a string describing a card.
let cardToString card =
    // TODO: replace the following line with logic that converts the card's kind to a string.
    // Reminder: a 1 means "Ace", 11 means "Jack", 12 means "Queen", 13 means "King".
    // A "match" statement will be necessary. (The next function below is a hint.)
    let kind = string card.kind


    // "%A" can print any kind of object, and automatically converts a union (like CardSuit)
    // into a simple string.
    sprintf "%s of %A" kind card.suit
    
// Returns the "value" of a card in a poker hand, where all three "face" cards are worth 10
// and an Ace has a value of 11.
let cardValue card =
    let value = match card.kind with
                | 1 -> 11
                | 11 | 12 | 13 -> 10  // This matches 11, 12, or 13.
                | n -> n
    value


// Calculates the total point value of the given hand (Card list). 
// Find the sum of the card values of each card in the hand. If that sum
// exceeds 21, and the hand has aces, then some of those aces turn from 
// a value of 11 to a value of 1, and a new total is computed.
// TODO: fill in the marked parts of this function.
let handTotal hand =
    // TODO: modify the next line to calculate the sum of the card values of each
    // card in the list. Hint: List.map and List.sum. (Or, if you're slick, List.sumBy)
    let sum = 0

    // TODO: modify the next line to count the number of aces in the hand.
    // Hint: List.filter and List.length. 
    let numAces = 0

    // Adjust the sum if it exceeds 21 and there are aces.
    if sum <= 21 then
        // No adjustment necessary.
        sum
    else 
        // Find the max number of aces to use as 1 point instead of 11.
        let maxAces = (float sum - 21.0) / 10.0 |> ceil |> int
        // Remove 10 points per ace, depending on how many are needed.
        sum - (10 * (min numAces maxAces))


// FUNCTIONS THAT CREATE OR UPDATE GAME STATES

// Creates a new, unshuffled deck of 52 cards.
// A function with no parameters is indicated by () in the parameter list. It is also invoked
// with () as the argument.
let makeDeck () =
    // Make a deck by calling this anonymous function 52 times, each time incrementing
    // the parameter 'i' by 1.
    // The Suit of a card is found by dividing i by 13, so the first 13 cards are Spades.
    // The Kind of a card is the modulo of (i+1) and 13. 
    List.init 52 (fun i -> let s = match i / 13 with
                                   | 0 -> Spades
                                   | 1 -> Clubs
                                   | 2 -> Diamonds
                                   | 3 -> Hearts
                           {suit = s; kind = i % 13 + 1})


// This global value can be used as a source of random integers by writing
// "rand.Next(i)", where i is the upper bound (exclusive) of the random range.
let rand = new System.Random()

// Creates a new game state by creating and shuffling a deck, and dealing 2 cards to 
// each player.
// Call this function by writing "newGame ()".
let newGame () =

    // Shuffles a list. Don't worry about this.
    let shuffleList list =
        let arr = List.toArray list

        let swap (a: _[]) x y =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp
    
        Array.iteri (fun i _ -> swap arr i (rand.Next(i, Array.length arr))) arr
        Array.toList arr

    // Create the deck, and then shuffle it
    let deck = makeDeck ()
               |> shuffleList

    // Construct the starting hands for player and dealer.
    let player = [deck.Head ; deck.Tail.Tail.Head] // First and third cards.
    let dealer = [deck.Tail.Head ; deck.Tail.Tail.Tail.Head] // Second and fourth.

    // Return a fresh game state.
    {playerHand = player; 
     dealerHand = dealer; 
     deck = List.skip 4 deck}


// Given a current game state and an indication of which player is "hitting", deal one
// card from the deck and add it to the given person's hand. Return the new game state.
let hit (handOwner : HandOwner) (gameState : GameState) = // these type annotations are for your benefit, not the compiler

    // TODO: take the top (first) card from the gameState's deck and cons it onto the hand
    // for whichever person is identified by "handOwner". 
    // Return the new game state, *including* new the deck with the top card removed.
    
    // TODO: this is just so the code compiles; fix it.
    gameState

// Take the dealer's turn by repeatedly taking a single action, hit or stay, until 
// the dealer busts or stays.
let rec dealerTurn gameState =
    let dealer = gameState.dealerHand
    let score = handTotal dealer

    // TODO: the following line prints the cards in the dealer's hand, but with ugly output
    // because F# doesn't know how to format a Card variable for output. Transform each card
    // in the dealer's hand into a string using cardToString and use those for output.
    // Hint: List.map
    printfn "Dealer's hand: %A; %d points" dealer score
    
    // Dealer rules: must hit if score < 17.
    if score > 21 then
        printfn "Dealer busts!"
        // The game state is unchanged because we did not hit. 
        // The dealer does not get to take another action.
        gameState
    elif score < 17 then
        printfn "Dealer hits"
        // The game state is changed; the result of "hit" is the new state.
        // The dealer gets to take another action using the new state.
        gameState
        |> hit Dealer
        |> dealerTurn
    else
        // The game state is unchanged because we did not hit. 
        // The dealer does not get to take another action.
        printfn "Dealer must stay"
        gameState

// Take the player's turn by repeatedly taking a single action until they bust or stay.
let rec playerTurn (playerStrategy : GameState->bool) (gameState : GameState) =
    // TODO: code this method using dealerTurn as a guide. Follow the same standard
    // of printing output. This function must return the new game state after the player's
    // turn has finished, like dealerTurn.

    // Unlike the dealer, the player gets to make choices about whether they will hit or stay.
    // The "elif score < 17" code from dealerTurn is inappropriate; in its place, we will
    // allow a "strategy" to decide whether to hit. A "strategy" is a function that accepts
    // the current game state and returns true if the player should hit, and false otherwise.
    // playerTurn must call that function (the parameter playerStrategy) to decide whether
    // to hit or stay.


    // The next line is just so the code compiles. Remove it when you code the function.
    gameState

// Plays one game with the given player strategy. Returns a GameLog recording the winner of the game.
let oneGame playerStrategy gameState =
    // TODO: print the first card in the dealer's hand to the screen, because the Player can see
    // one card from the dealer's hand in order to make their decisions.
    printfn "Dealer is showing: %A" 0 // fix this line

    // TODO: play the game! First the player gets their turn. The dealer then takes their turn,
    // using the state of the game after the player's turn finished.



    // TODO: determine the winner! Get the hand scores for the dealer and the player.
    // The player wins if they did not bust (score <= 21) AND EITHER:
    //                                                        - the dealer busts; or
    //                                                        - player's score > dealer's score
    // If neither side busts and they have the same score, the result is a "draw".
    // Return a GameLog object with a value of 1 for the correct winner.

    // TODO: this is a "blank" GameLog. Return something more appropriate for each of the outcomes
    // described above.
    {playerWins = 0; dealerWins = 0; draws = 0}

// Recursively plays n games using the given playerStrategy.
let manyGames n playerStrategy =
    // This tail-recursive helper implements the manyGames logic.
    let rec manyGamesTail n playerStrategy logSoFar =
        // TODO: construct a new game using newGame ().
        // Then play that game using oneGame.
        // Take the result of that and combine it with the logSoFar, by summing the playerWins, dealerWins, and draws
        // from the result and from the logSoFar.
        // If this is the last game (n = 1), then the combined log is the answer.
        // Otherwise, the combined log becomes the new logSoFar in a recursive call to manyGamesTail,
        // with n reduced by 1.

        // TODO: this is a "blank" GameLog. Return something more appropriate.
        {playerWins = 0; dealerWins = 0; draws = 0}

    // Start the tail recursion with a blank logSoFar.
    manyGamesTail n playerStrategy {playerWins = 0; dealerWins = 0; draws = 0}
            

// PLAYER STRATEGIES
let interactivePlayerStrategy gameState =
    printfn "Hit? y/n"
    let answer = System.Console.ReadLine()
    // Return true if they entered "y", false otherwise.
    answer = "y"

[<EntryPoint>]
let main argv =
    

    // manyGames 1000 (fun x -> handTotal x.playerHand < 16)
    // |> printfn "%A"
    0 // return an integer exit code
