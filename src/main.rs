#![feature(asm)]
#![feature(const_panic)]
// #![feature(const_eval_limit)]
// #![const_eval_limit = "50000000"]
#[macro_use]
extern crate impl_ops;

mod attacks;
mod boards;
mod chess_errors;
mod engine;
mod game;
mod magic_number_tables;
mod pieces;
mod utils;

use boards::*;
use engine::BitBoardGame;
use game::Game;
use pieces::*;

// GENERAL PLAN
// The program shall consist of the following parts:
// START:
// ✓ Board Representation – BitBox and MailBox
// * Simple CLI for interactive play. Shall include:
//   ✓ Print Board state
//   ✓ Make Move, update state
//   Advanced:
//   * Check legality of move
// ✓ Include tests
//
// MIDDLE:
// * Create Chess Engine:
//   * Move generator & validator
//   * Move heuristic
//   * Search strategy
// * Include tests
//
// END:
// * Abstract engine into its own form
// * Write Algebraic Notation Parser
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

#[cfg(debug_assertions)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut g = BitBoardGame::standard_setup();
    println!("{:?}", g);
    g.player_move(Piece::KnightWhite, 57, 42)?;
    println!("{:?}", g);
    for _ in 0..10 {
        g.play_random_turn()?;
        println!("{:?}", g);
    }
    Ok(())
}

#[cfg(not(debug_assertions))]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut g = BitBoardGame::standard_setup();
    println!("{}", g);
    g.player_move(Piece::KnightWhite, 57, 42)?;
    println!("{}", g);
    for _ in 0..10 {
        let m = g.play_random_turn()?;
        println!("Move: {}\n, {}", m, g);
    }
    Ok(())
}
