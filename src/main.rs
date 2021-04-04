#![feature(asm)]
#![feature(const_panic)]
#[macro_use]
extern crate impl_ops;

mod agents;
mod attacks;
mod boards;
mod chess_errors;
mod engine;
mod game;
mod game_state;
mod magic_number_tables;
mod moves;
mod pieces;
mod positions;
mod utils;

use agents::*;
use boards::*;
use game::*;
use game_state::GameState;
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
//   * Move heuristic
//   * Search strategy
//   * Advanced Moves (e.p., castling)
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

fn random_play_debug() -> Result<(), Box<dyn std::error::Error>> {
    let mut g = GameState::standard_setup();
    println!("{:?}", g);
    g.player_move(57.into(), 42.into())?;
    println!("{:?}", g);
    for _ in 0..10 {
        g.play_random_turn()?;
        println!("{:?}", g);
    }
    Ok(())
}

fn random_play() -> Result<(), Box<dyn std::error::Error>> {
    let mut g = GameState::standard_setup();
    println!("{}", g);
    g.player_move(57.into(), 42.into())?;
    println!("{}", g);
    for _ in 0..10 {
        let m = g.play_random_turn()?;
        println!("Move: {}\n, {}", m, g);
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut g = Game::new(
        SlowAgent::new(GreedyMaterialAgent::new(), 500),
        SlowAgent::new(GreedyMaterialAgent::new(), 500),
    );
    g.play();
    Ok(())
}
