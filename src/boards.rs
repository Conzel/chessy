use crate::chess_errors::*;
use crate::pieces::*;
use array_init::array_init;
use std::fmt::{self, Display};
use std::ops;

// ---------------------------------------------
// Board Types
// ---------------------------------------------

pub const BOARD_SIZE: u8 = 8;
pub type Position = usize;

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

pub struct MailboxBoard {
    pieces: [Piece; (BOARD_SIZE * BOARD_SIZE) as usize],
}

impl MailboxBoard {
    pub fn empty() -> MailboxBoard {
        MailboxBoard {
            pieces: array_init(|_| Piece::Empty),
        }
    }

    pub fn add(&mut self, pos: Position, piece: Piece) -> ChessResult<()> {
        let current = &mut self.pieces[pos as usize];
        if let Piece::Empty = current {
            *current = piece;
            Ok(())
        } else {
            Err(format!("Piece at {} is not empty but {}", pos, current).into())
        }
    }

    pub fn add_bitboard(&mut self, b: &BitBoard, piece: Piece) -> ChessResult<()> {
        for (pos, val) in b.bit_vec().iter().enumerate() {
            if *val == 1 {
                self.add(pos, piece.clone())?
            }
        }
        Ok(())
    }
}

// TODO: Make this enum private later - we just need it public at the moment for quick debugging.
#[derive(Debug, Clone, Copy)]
pub struct BitBoard(u64);

// TODO: Delete?
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
    pub fn bit_vec(&self) -> Vec<u8> {
        bit_vec(self.0)
    }

    // Could've also been done in a match, but multiple functions is more efficient (saves the enum
    // and comparison)
    pub fn rotate90(&self) -> BitBoard {
        self.flip_horz().flip_diag()
    }

    pub fn rotate180(&self) -> BitBoard {
        self.flip_vert().flip_horz()
    }

    pub fn rotate270(&self) -> BitBoard {
        self.flip_vert().flip_antidiag()
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

    /// Diagonal goes from top right to bottom left
    /// Source: https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating
    fn flip_diag(&self) -> BitBoard {
        let k1: u64 = 0x5500550055005500;
        let k2: u64 = 0x3333000033330000;
        let k4: u64 = 0x0f0f0f0f00000000;
        let mut x = self.0;
        let mut t = k4 & (x ^ (x << 28));
        x ^= t ^ (t >> 28);
        t = k2 & (x ^ (x << 14));
        x ^= t ^ (t >> 14);
        t = k1 & (x ^ (x << 7));
        x ^= t ^ (t >> 7);
        x.into()
    }

    /// Antidiagonal goes from top left to bottom right
    /// Source: https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating
    fn flip_antidiag(&self) -> BitBoard {
        let k1: u64 = 0xaa00aa00aa00aa00;
        let k2: u64 = 0xcccc0000cccc0000;
        let k4: u64 = 0xf0f0f0f00f0f0f0f;
        let mut x = self.0;
        let mut t = x ^ (x << 36);
        x ^= k4 & (t ^ (x >> 36));
        t = k2 & (x ^ (x << 18));
        x ^= t ^ (t >> 18);
        t = k1 & (x ^ (x << 9));
        x ^= t ^ (t >> 9);
        x.into()
    }

    /// Moves a piece on the board. Does NOT check whether the move is legal (i.e.
    /// the start position is indeed occupied, the end position unoccupied).
    /// If the move is not legal, this function makes no guarantees on the obtained
    /// bitboard.
    /// Positions by definition start from left to right, so for
    /// 0b10100
    /// we would index
    ///   01234
    // TODO: Also make this private again
    pub fn make_move(&self, start: Position, end: Position) -> BitBoard {
        // Move implemented via flipping the bits and start and end position
        (self.0 ^ (0x8000000000000000u64 >> start) ^ (0x8000000000000000u64 >> end)).into()
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

impl ops::BitOr<BitBoard> for BitBoard {
    type Output = Self;

    fn bitor(self, rhs: BitBoard) -> Self::Output {
        (self.0 | rhs.0).into()
    }
}

impl_op_ex_commutative!(| |a: &BitBoard, b: &u64| -> BitBoard {BitBoard::from(a.0 | b)});
impl_op_ex_commutative!(^ |a: &BitBoard, b: &u64| -> BitBoard {BitBoard::from(a.0 ^ b)});
