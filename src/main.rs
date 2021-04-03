#![feature(asm)]
#![feature(const_panic)]
// #![feature(const_eval_limit)]
// #![const_eval_limit = "50000000"]
#[macro_use]
extern crate impl_ops;

mod attacks;
mod boards;
mod chess_errors;
mod game;
mod pieces;
mod utils;

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

    println!("{}", attacks::get_bishop_attack_board(49, bitboard!(35)));
    println!("{}", attacks::get_rook_attack_board(49, bitboard!(35)));
}
