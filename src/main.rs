#![feature(asm)]
#[macro_use]
extern crate impl_ops;

mod boards;
mod chess_errors;
mod engine;
mod game;
mod pieces;

use boards::*;
use engine::temp_examine;
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
    let b: BitBoard = BitBoard::from(0b1);
    println!("{}", b);
    println!("{}", b.make_move(0, 17));

    let mut g = BitBoardGame::standard_setup();
    println!("{}", g);
    g.make_move(Piece::KnightBlack, 1, 17);
    println!("{}", g);
    println!("{:b}", 0x8000000000000000u64);
    println!("{}", BitBoard::from(0x8000000000000000u64 >> 10));
    temp_examine();
}
