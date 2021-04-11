#![feature(asm)]
#![feature(const_panic)]
#[macro_use]
extern crate impl_ops;

mod agents;
mod algorithms;
mod attacks;
mod boards;
mod chess_errors;
mod engine;
mod game;
mod game_state;
mod magic_number_tables;
mod moves;
mod pieces;
mod positional_tables;
mod positions;
mod utils;

use agents::*;
use boards::*;
use game::*;
use game_state::GameState;
use pieces::Color;
use positions::Position;
use std::io::{stdout, Write};
use text_io::read;

// GENERAL PLAN
// The program shall consist of the following parts:
// START:
// ✓ Board Representation – BitBox and MailBox
// ✓ Simple CLI for interactive play. Shall include:
//   ✓ Print Board state
//   ✓ Make Move, update state
//   Advanced:
//   ✓ Check legality of move
// ✓ Include tests
//
// MIDDLE:
// * Create Chess Engine:
//   ✓ Move generator & validator
//   ✓ Move heuristic
//   ✓ Search strategy
//   * Advanced Moves
//     ✓ castling
//     * en passant
//     ✓ promotion
//   ✓ Move preordering
//   * Lazy move generation
//   ✓ Quiescent search
//   ✓ Let the engine checkmate
//   * Transposition table
// * Include tests
// * More fine grained heuristics
// * End game positionals
// * Improve documentation
//
// EXTRA COOL STUFF:
// * Undo moves in play with humans
// * Protocols of games with replays
// * Logging for debugging
//
// END:
// * Abstract engine into its own form
// ✓ Write Algebraic Notation Parser
// * Interfaces
//   * Simple CLI
//   * Simple GUI
// * Universal Chess Interface (UCI)
// * Improvements to search strategy: End tables & Opening Books
// * Include tests
//

// ---------------------------------------------
// Main
// ---------------------------------------------

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut g = Game::new(
        HumanAgent::new(),
        AlphaBetaAgent::new(5, AlphaBetaMovePreordering(Color::Black)),
    );
    g.play();
    Ok(())
}
