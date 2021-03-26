#![feature(asm)]
#[macro_use]
extern crate impl_ops;

use array_init::array_init;
use std::fmt::{self, Display};
use std::ops;

type Position = usize;
const BOARD_SIZE: u8 = 8;

// GENERAL PLAN
// The program shall consist of the following parts:
// START:
// * Board Representation – BitBox and MailBox
// * Simple CLI for interactive play. Shall include:
//   * Print Board state
//   * Make Move, update state
//   Advanced:
//   * Check legality of move
//
// MIDDLE:
// * Create Chess Engine:
//   * Move generator & validator
//   * Move heuristic
//   * Search strategy
// END:
// * Abstract engine into its own form
// * Interfaces
//   * Simple CLI
//   * Simple GUI
// * Universal Chess Interface (UCI)
// * Improvements to search strategy: End tables & Opening Books

// ---------------------------------------------
// Error Handling
// ---------------------------------------------
#[derive(Debug, Clone)]
struct ChessError(String);

type ChessResult<T> = std::result::Result<T, ChessError>;

impl From<String> for ChessError {
    fn from(s: String) -> ChessError {
        ChessError(s)
    }
}

impl fmt::Display for ChessError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Chess Error occured: {}", self.0)
    }
}

// ---------------------------------------------
// Pieces
// ---------------------------------------------
#[derive(Debug, Clone)]
enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    Empty,
}

#[derive(Debug, Clone)]
enum Color {
    Black,
    White,
    None,
}

impl Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            Piece(PieceType::King, Color::White) => '\u{2654}',
            Piece(PieceType::Queen, Color::White) => '\u{2655}',
            Piece(PieceType::Rook, Color::White) => '\u{2656}',
            Piece(PieceType::Bishop, Color::White) => '\u{2657}',
            Piece(PieceType::Knight, Color::White) => '\u{2658}',
            Piece(PieceType::Pawn, Color::White) => '\u{2659}',
            Piece(PieceType::King, Color::Black) => '\u{265a}',
            Piece(PieceType::Queen, Color::Black) => '\u{265b}',
            Piece(PieceType::Rook, Color::Black) => '\u{265c}',
            Piece(PieceType::Bishop, Color::Black) => '\u{265d}',
            Piece(PieceType::Knight, Color::Black) => '\u{265e}',
            Piece(PieceType::Pawn, Color::Black) => '\u{265f}',
            Piece(PieceType::Empty, _) => ' ',
            _ => panic!(format!(
                "Illegal combination of piece and color: {:#?}",
                self
            )),
        };
        write!(f, "{}", symbol);
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Piece(PieceType, Color);

// ---------------------------------------------
// Board Types
// ---------------------------------------------

// Displays the first 64 items from an iterator in a chessboard style:
//
//   a  b  c  d  e  f  g
// 8 i1 i2 i3 ...        8
// 7 ....
//
// Where i1,...i64 are the items of the iterator.
// It is required that the iterator has at least 64 items, else we will return with an error.
fn display_chessboard_style<I, C>(it: &mut I, f: &mut fmt::Formatter<'_>) -> fmt::Result
where
    I: Iterator<Item = C>,
    C: Display,
{
    write!(f, " ")?;
    for c in 'a'..'i' {
        write!(f, " {}", c)?;
    }
    for row in 0..BOARD_SIZE {
        write!(f, "\n{} ", 8 - row)?;
        for _col in 0..BOARD_SIZE {
            let i = it.next().expect("Iterator ended too early");
            write!(f, "{} ", i)?;
        }
        write!(f, "{} ", 8 - row)?;
    }
    write!(f, "\n ")?;
    for c in 'a'..'i' {
        write!(f, " {}", c)?;
    }
    Ok(())
}

impl Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_chessboard_style(&mut bit_vec(self.0).iter(), f)
    }
}

struct MailboxBoard {
    pieces: [Piece; (BOARD_SIZE * BOARD_SIZE) as usize],
}

impl MailboxBoard {
    fn empty() -> MailboxBoard {
        MailboxBoard {
            pieces: array_init(|_| Piece(PieceType::Empty, Color::None)),
        }
    }

    fn add(&mut self, pos: Position, piece: Piece) -> ChessResult<()> {
        let current = &mut self.pieces[pos as usize];
        if let Piece(PieceType::Empty, _) = current {
            *current = piece;
            Ok(())
        } else {
            Err(format!("Piece at {} is not empty but {}", pos, current).into())
        }
    }

    fn add_bitboard(&mut self, b: &BitBoard, piece: Piece) -> ChessResult<()> {
        for (pos, val) in b.bit_vec().iter().enumerate() {
            if *val == 1 {
                self.add(pos, piece.clone())?
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
struct BitBoard(u64);

enum Rotation {
    Deg90,
    Deg180,
    Deg270,
}

// This is very wasteful, only use it when performance is of no concern!
fn bit_vec(i: u64) -> Vec<u8> {
    let mut res: Vec<u8> = Vec::with_capacity(64);
    let mut i_ = i;
    for _ in 0..64 {
        res.push((i_ % 2) as u8);
        i_ = i_ / 2;
    }
    res.into_iter().rev().collect()
}

impl BitBoard {
    fn bit_vec(&self) -> Vec<u8> {
        bit_vec(self.0)
    }

    fn rotate(&self, r: Rotation) -> BitBoard {
        use Rotation::*;
        match r {
            Deg180 => self.flip_vert().flip_horz(),
            _ => todo!(),
        }
    }

    // Uses fast assembly bit swap, thus only works on x86_64
    #[cfg(any(target_arch = "x86_64"))]
    fn flip_vert(&self) -> BitBoard {
        let c: u64;
        unsafe {
            asm!(
                "mov {0}, {1}",
                "bswap {0}",
                out(reg) c,
                in(reg) self.0,
            );
        };
        c.into()
    }

    #[cfg(not(any(target_arch = "x86_64")))]
    fn flip_vert(&self) -> BitBoard {
        incomplete!()
    }

    /// Straight-forward implementation of a parallel prefix algorithm.
    /// Source: https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating
    fn flip_horz(&self) -> BitBoard {
        let k1: u64 = 0x5555555555555555;
        let k2: u64 = 0x3333333333333333;
        let k4: u64 = 0x0f0f0f0f0f0f0f0f;
        let mut x = self.0;
        x = ((x >> 1) & k1) | ((x & k1) << 1);
        x = ((x >> 2) & k2) | ((x & k2) << 2);
        x = ((x >> 4) & k4) | ((x & k4) << 4);
        return x.into();
    }
}

impl From<u64> for BitBoard {
    fn from(u: u64) -> Self {
        BitBoard(u)
    }
}

impl Display for MailboxBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_chessboard_style(&mut self.pieces.iter(), f)
    }
}

// ---------------------------------------------
// Game
// ---------------------------------------------

// Note on this struct: property access must be highly efficient,
// so we do not use a hashmap to access the BitBoards for the
// different pieces. This sadly makes some of the code a bit bloated.
struct BitBoardGame {
    // The individual bit boards for the pieces
    white_pieces: PieceBitBoards,
    black_pieces: PieceBitBoards,
    // Bit board showing where all the white pieces are
    all_whites: BitBoard,
    all_blacks: BitBoard,
}

impl Game for BitBoardGame {
    fn make_move(&mut self, start: Position, end: Position) {}
}

impl Display for BitBoardGame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.state_as_mailbox().fmt(f)
    }
}

impl BitBoardGame {
    fn state_as_mailbox(&self) -> MailboxBoard {
        use Color::*;
        use PieceType::*;

        let mut res = MailboxBoard::empty();
        res.add_bitboard(&self.white_pieces.pawns, Piece(Pawn, White));
        res.add_bitboard(&self.white_pieces.knights, Piece(Knight, White));
        res.add_bitboard(&self.white_pieces.bishops, Piece(Bishop, White));
        res.add_bitboard(&self.white_pieces.rooks, Piece(Rook, White));
        res.add_bitboard(&self.white_pieces.queens, Piece(Queen, White));
        res.add_bitboard(&self.white_pieces.kings, Piece(King, White));
        res.add_bitboard(&self.black_pieces.pawns, Piece(Pawn, Black));
        res.add_bitboard(&self.black_pieces.knights, Piece(Knight, Black));
        res.add_bitboard(&self.black_pieces.bishops, Piece(Bishop, Black));
        res.add_bitboard(&self.black_pieces.rooks, Piece(Rook, Black));
        res.add_bitboard(&self.black_pieces.queens, Piece(Queen, Black));
        res.add_bitboard(&self.black_pieces.kings, Piece(King, Black));
        res
    }

    /// Returns a game with the figures placed on standard chess starting positions
    fn standard_setup() -> BitBoardGame {
        // White Setup
        let pawns: BitBoard = 0b1111111100000000.into();
        let knights: BitBoard = 0b01000010.into();
        let bishops: BitBoard = 0b00100100.into();
        let rooks: BitBoard = 0b10000001.into();
        let queens: BitBoard = 0b00010000.into();
        let kings: BitBoard = 0b00001000.into();

        // Note: King and queen position need to be swapped to get the correct position for black
        // as those two pieces are mirrored, not rotated (see a chess board :) )
        // See below.
        let white_vec = vec![pawns, knights, bishops, rooks, queens, kings];
        let black_vec: Vec<BitBoard> = white_vec
            .iter()
            .map(|b| b.rotate(Rotation::Deg180))
            .collect();

        let whites = PieceBitBoards {
            pawns: pawns,
            knights: knights,
            bishops: bishops,
            rooks: rooks,
            queens: queens,
            kings: kings,
        };
        let blacks = PieceBitBoards {
            pawns: black_vec[0],
            knights: black_vec[1],
            bishops: black_vec[2],
            rooks: black_vec[3],
            // Swapping knight and queen as explained
            queens: black_vec[5],
            kings: black_vec[4],
        };

        BitBoardGame {
            all_whites: whites.combine(),
            all_blacks: blacks.combine(),
            white_pieces: whites,
            black_pieces: blacks,
        }
    }
}

struct PieceBitBoards {
    pawns: BitBoard,
    knights: BitBoard,
    bishops: BitBoard,
    rooks: BitBoard,
    queens: BitBoard,
    kings: BitBoard,
}

impl PieceBitBoards {
    /// Returns board with all pieces occupied by white pieces.
    /// Does NOT check for double occupancies.
    fn combine(&self) -> BitBoard {
        (self.pawns | self.knights | self.bishops | self.rooks | self.queens | self.kings).into()
    }
}

impl ops::BitOr<BitBoard> for BitBoard {
    type Output = Self;

    fn bitor(self, rhs: BitBoard) -> Self::Output {
        (self.0 | rhs.0).into()
    }
}

impl_op_ex_commutative!(| |a: &BitBoard, b: &u64| -> BitBoard {BitBoard::from(a.0 | b)});
impl_op_ex_commutative!(^ |a: &BitBoard, b: &u64| -> BitBoard {BitBoard::from(a.0 ^ b)});

trait Game: Display {
    fn make_move(&mut self, start: Position, end: Position);
}

// ---------------------------------------------
// Main
// ---------------------------------------------

fn main() {
    // println!("{}", BitBoardGame::standard_setup());
    println!("{}", Piece(PieceType::Rook, Color::White));
}