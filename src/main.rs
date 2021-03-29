#![feature(asm)]
#![feature(const_trait_impl)]
#![feature(const_panic)]
#[macro_use]
extern crate impl_ops;

mod boards;
mod chess_errors;
mod engine;
mod game;
mod pieces;

use boards::*;
use game::*;
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
// * Include tests
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

fn main() {
    let mut g = BitBoardGame::standard_setup();
    println!("{}", g);
    g.make_move(Piece::KnightBlack, 1, 17);
}
