use crate::chess_errors::*;
use crate::pieces::*;
use crate::positions::*;
use array_init::array_init;
use std::fmt::{self, Display};
use std::ops;

// ---------------------------------------------
// General
// ---------------------------------------------

pub const BOARD_SIZE: u8 = 8;

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

// ---------------------------------------------
// PieceBitBoards
// ---------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct PieceBitBoards {
    pub pawns: BitBoard,
    pub knights: BitBoard,
    pub bishops: BitBoard,
    pub rooks: BitBoard,
    pub queens: BitBoard,
    pub kings: BitBoard,
}

impl PieceBitBoards {
    /// Returns board all positions where this color occupies a spot.
    /// Does NOT check for double occupancies.
    pub fn combine(&self) -> BitBoard {
        (self.pawns | self.knights | self.bishops | self.rooks | self.queens | self.kings).into()
    }
}

// ---------------------------------------------
// MailboxBoard
// ---------------------------------------------
type MailboxArray = [Piece; (BOARD_SIZE * BOARD_SIZE) as usize];

#[derive(Clone, PartialEq)]
pub struct MailboxBoard {
    pieces: MailboxArray,
}

impl MailboxBoard {
    pub fn empty() -> MailboxBoard {
        MailboxBoard {
            pieces: array_init(|_| Piece::Empty),
        }
    }

    pub fn add(&mut self, pos: Position, piece: Piece) -> ChessResult<()> {
        let current = &mut self.pieces[pos];
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
                self.add(pos.into(), piece.clone())?
            }
        }
        Ok(())
    }

    pub fn from_piece_bitboards(
        whites: &PieceBitBoards,
        blacks: &PieceBitBoards,
    ) -> ChessResult<MailboxBoard> {
        use Piece::*;

        let mut res = MailboxBoard::empty();
        res.add_bitboard(&whites.pawns, PawnWhite).unwrap();
        res.add_bitboard(&whites.knights, KnightWhite).unwrap();
        res.add_bitboard(&whites.bishops, BishopWhite).unwrap();
        res.add_bitboard(&whites.rooks, RookWhite).unwrap();
        res.add_bitboard(&whites.queens, QueenWhite).unwrap();
        res.add_bitboard(&whites.kings, KingWhite).unwrap();
        res.add_bitboard(&blacks.pawns, PawnBlack).unwrap();
        res.add_bitboard(&blacks.knights, KnightBlack).unwrap();
        res.add_bitboard(&blacks.bishops, BishopBlack).unwrap();
        res.add_bitboard(&blacks.rooks, RookBlack).unwrap();
        res.add_bitboard(&blacks.queens, QueenBlack).unwrap();
        res.add_bitboard(&blacks.kings, KingBlack).unwrap();

        Ok(res)
    }

    // Makes a move by moving the piece at start to the piece at end.
    // Not required to check for legality.
    pub fn make_move(&mut self, start: Position, end: Position) {
        debug_assert!(start != end);
        self.pieces[end] = self.pieces[start];
        self.pieces[start] = Piece::Empty;
    }

    // Exchanges piece at position
    pub fn exchange(&mut self, to: Piece, at: Position) {
        debug_assert!(self.pieces[at] != Piece::Empty);
        self.pieces[at] = to;
    }
}

impl ops::Index<usize> for MailboxBoard {
    type Output = Piece;
    fn index<'a>(&'a self, i: usize) -> &'a Piece {
        debug_assert!(i < 64);
        &self.pieces[i]
    }
}

impl ops::Index<Position> for MailboxBoard {
    type Output = Piece;
    fn index<'a>(&'a self, i: Position) -> &'a Piece {
        &self.pieces[i]
    }
}

pub struct MailboxIterator {
    current_pos: u8,
    array_iter: std::array::IntoIter<Piece, 64>,
}

impl Iterator for MailboxIterator {
    type Item = (Position, Piece);

    fn next(&mut self) -> Option<Self::Item> {
        let old_pos = self.current_pos;
        if old_pos > 63 {
            None
        } else {
            self.current_pos = old_pos + 1u8;
            Some((old_pos.into(), self.array_iter.next()?))
        }
    }
}

impl<'a> IntoIterator for &'a MailboxBoard {
    type Item = (Position, Piece);
    type IntoIter = MailboxIterator;

    fn into_iter(self) -> Self::IntoIter {
        MailboxIterator {
            current_pos: 0.into(),
            array_iter: std::array::IntoIter::new(self.pieces),
        }
    }
}

// ---------------------------------------------
// BitBoard
// ---------------------------------------------

#[derive(Clone, Copy, PartialEq)]
pub struct BitBoard(u64);

impl BitBoard {
    pub fn bit_vec(&self) -> Vec<u8> {
        bit_vec(self.0)
    }

    pub const fn new(u: u64) -> BitBoard {
        BitBoard(u)
    }

    pub const fn singular(at: Position) -> BitBoard {
        BitBoard(0x8000000000000000 >> at.get())
    }

    pub const fn empty() -> BitBoard {
        BitBoard(0)
    }

    /// Returns true if no bit is set in the board
    pub const fn is_empty(&self) -> bool {
        self.0 == 0
    }

    /// Returns number of bits set in bit board
    pub const fn bits_set(self) -> u8 {
        // Brian Kernighan algorithm
        let mut count = 0;
        let mut n = self.0;
        while n != 0 {
            n = n & (n - 1);
            count += 1;
        }
        count
    }

    // Allows access to underlying data – should not be used when possible
    pub const fn get(self) -> u64 {
        self.0
    }

    /// Returns true if the bit at position is 1
    pub const fn bit_set_at(self, pos: Position) -> bool {
        self.0 & (0x8000000000000000 >> pos.get()) != 0
    }

    /// Returns Bitboard where the bit at pos is set
    pub fn set_bit_at(self, pos: Position) -> BitBoard {
        (self.0 | (0x8000000000000000 >> pos.get())).into()
    }

    /// Returns Bitboard where the bit at pos is unset
    pub fn unset_bit_at(self, pos: Position) -> BitBoard {
        (self.0 & !(0x8000000000000000 >> pos.get())).into()
    }

    // Could've also been done in a match, but multiple functions is more efficient (saves the enum
    // and comparison)
    /// Rotates Bitboard 90 degrees CLOCKWISE
    pub fn rotate90(self) -> BitBoard {
        self.flip_horz().flip_diag()
    }

    /// Rotates Bitboard 180 degrees
    pub fn rotate180(self) -> BitBoard {
        self.flip_vert().flip_horz()
    }

    /// Rotates Bitboard 270 (-90) degrees CLOCKWISE
    pub fn rotate270(self) -> BitBoard {
        self.flip_horz().flip_antidiag()
    }

    /// Rotates Bitboard 270 (-90) degrees CLOCKWISE
    pub const fn rotate270_const(self) -> BitBoard {
        self.flip_horz_const().flip_antidiag()
    }

    pub const fn rotate90_const(self) -> BitBoard {
        self.flip_horz_const().flip_diag()
    }

    // Uses fast assembly bit swap, thus only works on x86_64
    #[cfg(any(target_arch = "x86_64"))]
    fn flip_horz(self) -> BitBoard {
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
    fn flip_horz(self) -> BitBoard {
        self.flip_horz_const()
    }

    // const fn version of flip_vert, a bit slower
    const fn flip_horz_const(self) -> BitBoard {
        let x = self.0;
        BitBoard::new(
            (x << 56)
                | (x << 40 & 0x00ff000000000000)
                | (x << 24 & 0x0000ff0000000000)
                | (x << 8 & 0x000000ff00000000)
                | (x >> 8 & 0x00000000ff000000)
                | (x >> 24 & 0x0000000000ff0000)
                | (x >> 40 & 0x000000000000ff00)
                | (x >> 56),
        )
    }

    /// Straight-forward implementation of a parallel prefix algorithm.
    /// Source: https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating
    const fn flip_vert(self) -> BitBoard {
        let k1: u64 = 0x5555555555555555;
        let k2: u64 = 0x3333333333333333;
        let k4: u64 = 0x0f0f0f0f0f0f0f0f;
        let mut x = self.0;
        x = ((x >> 1) & k1) | ((x & k1) << 1);
        x = ((x >> 2) & k2) | ((x & k2) << 2);
        x = ((x >> 4) & k4) | ((x & k4) << 4);
        BitBoard::new(x)
    }

    /// Diagonal (mirror axis) goes from top left to bottom right
    /// Source: https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating
    const fn flip_diag(self) -> BitBoard {
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
        BitBoard::new(x)
    }

    /// Antidiagonal (mirror axis) goes from top right to bottom left
    /// Source: https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating
    const fn flip_antidiag(self) -> BitBoard {
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
        BitBoard::new(x)
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
    pub fn make_move(self, start: Position, end: Position) -> BitBoard {
        self.0 & (!BitBoard::singular(start)) | BitBoard::singular(end)
    }

    pub const fn const_xor(&self, rhs: BitBoard) -> BitBoard {
        BitBoard(self.0 ^ rhs.0)
    }

    pub const fn const_or(&self, rhs: BitBoard) -> BitBoard {
        BitBoard(self.0 | rhs.0)
    }

    pub const fn const_and(&self, rhs: BitBoard) -> BitBoard {
        BitBoard(self.0 & rhs.0)
    }
}

impl From<u64> for BitBoard {
    fn from(u: u64) -> Self {
        BitBoard(u)
    }
}

// ---------------------------------------------
// Displays
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

impl fmt::Debug for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // makes for nicer messages in rust asserts
        write!(f, "\n")?;
        fmt::Display::fmt(self, f)
    }
}

impl Display for MailboxBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_chessboard_style(&mut self.pieces.iter(), f)
    }
}

impl fmt::Debug for MailboxBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Mailbox Board: [\n")?;
        for (pos, piece) in self.into_iter() {
            if piece != Piece::Empty {
                write!(f, "At {}: {:?}\n", pos, piece)?;
            }
        }
        write!(f, "]")
    }
}

// ---------------------------------------------
// Operator Impls
// ---------------------------------------------

impl ops::BitOr<BitBoard> for BitBoard {
    type Output = Self;

    fn bitor(self, rhs: BitBoard) -> Self::Output {
        (self.0 | rhs.0).into()
    }
}

impl ops::BitXor<BitBoard> for BitBoard {
    type Output = Self;

    fn bitxor(self, rhs: BitBoard) -> Self::Output {
        (self.0 ^ rhs.0).into()
    }
}

impl ops::BitAnd<BitBoard> for BitBoard {
    type Output = Self;

    fn bitand(self, rhs: BitBoard) -> Self::Output {
        (self.0 & rhs.0).into()
    }
}

impl ops::Shr<u8> for BitBoard {
    type Output = Self;

    fn shr(self, rhs: u8) -> Self::Output {
        (self.0 >> rhs).into()
    }
}

impl ops::Shl<u8> for BitBoard {
    type Output = Self;

    fn shl(self, rhs: u8) -> Self::Output {
        (self.0 << rhs).into()
    }
}

impl ops::Not for BitBoard {
    type Output = Self;

    fn not(self) -> Self::Output {
        (!self.0).into()
    }
}

impl_op_ex_commutative!(| |a: &BitBoard, b: &u64| -> BitBoard {BitBoard::from(a.0 | b)});
impl_op_ex_commutative!(^ |a: &BitBoard, b: &u64| -> BitBoard {BitBoard::from(a.0 ^ b)});
impl_op_ex_commutative!(&|a: &BitBoard, b: &u64| -> BitBoard { BitBoard::from(a.0 & b) });

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitboard;
    use crate::utils::*;
    use rand::prelude::*;

    #[test]
    fn test_pos_from_string() {
        assert_eq!("h1".parse::<Position>().unwrap().get(), 63);
        assert_eq!("a8".parse::<Position>().unwrap().get(), 0);
        assert_eq!("b6".parse::<Position>().unwrap().get(), 17);
        assert!("sdlfj".parse::<Position>().is_err());
        assert!("a9".parse::<Position>().is_err());
    }

    #[test]
    fn test_pos_row_col_conversion() {
        assert_eq!(Position::to_row_col(0.into()), (0, 0));
        assert_eq!(Position::to_row_col(1.into()), (0, 1));
        assert_eq!(Position::to_row_col(8.into()), (1, 0));
        assert_eq!(Position::to_row_col(9.into()), (1, 1));
        let (r1, c1) = Position::to_row_col(9.into());
        assert_eq!(Position::from_row_col(r1, c1), 9.into());
        let (r2, c2) = Position::to_row_col(23.into());
        assert_eq!(Position::const_from_row_col(r2, c2), 23.into());
    }

    #[test]
    fn test_bit_set_at() {
        assert!(BitBoard::from(0b1).bit_set_at(63.into()));
        assert!(BitBoard::from(0b11).bit_set_at(63.into()));
        assert!(BitBoard::from(0b11).bit_set_at(62.into()));
        assert!(!BitBoard::from(0b11).bit_set_at(0.into()));
        assert!(!BitBoard::from(0b11).bit_set_at(61.into()));
        assert!(!BitBoard::from(0b11).bit_set_at(58.into()));
    }

    #[test]
    fn test_flip_antidiag() {
        assert_eq!(bitboard!(0).flip_diag(), bitboard!(0));
        assert_eq!(bitboard!(9).flip_diag(), bitboard!(9));
        assert_eq!(bitboard!(63).flip_diag(), bitboard!(63));
        assert_eq!(bitboard!(7).flip_diag(), bitboard!(56));
        assert_eq!(bitboard!(56).flip_diag(), bitboard!(7));
        assert_eq!(bitboard!(8).flip_diag(), bitboard!(1));

        assert_eq!(
            (bitboard!(4) ^ bitboard!(5)).flip_antidiag(),
            bitboard!(23) ^ bitboard!(31)
        );
    }

    #[test]
    fn test_flip_diag() {
        assert_eq!(bitboard!(0).flip_antidiag(), bitboard!(63));
        assert_eq!(bitboard!(9).flip_antidiag(), bitboard!(54));
        assert_eq!(bitboard!(63).flip_antidiag(), bitboard!(0));
        assert_eq!(bitboard!(7).flip_antidiag(), bitboard!(7));
        assert_eq!(bitboard!(56).flip_antidiag(), bitboard!(56));
        assert_eq!(bitboard!(8).flip_antidiag(), bitboard!(62));

        assert_eq!(
            (bitboard!(4) ^ bitboard!(5)).flip_diag(),
            bitboard!(32) ^ bitboard!(40)
        );
    }

    #[test]
    fn test_rotate_90() {
        assert_eq!(bitboard!(0).rotate90(), bitboard!(7));
        assert_eq!(bitboard!(9).rotate90(), bitboard!(14));
        assert_eq!(bitboard!(63).rotate90(), bitboard!(56));
        assert_eq!(bitboard!(7).rotate90(), bitboard!(63));
        assert_eq!(bitboard!(56).rotate90(), bitboard!(0));

        assert_eq!(
            (bitboard!(4) ^ bitboard!(5)).rotate90(),
            bitboard!(39) ^ bitboard!(47)
        );
    }

    #[test]
    fn test_rotate_180() {
        assert_eq!(bitboard!(0).rotate180(), bitboard!(63));
        assert_eq!(bitboard!(9).rotate180(), bitboard!(54));
        assert_eq!(bitboard!(63).rotate180(), bitboard!(0));
        assert_eq!(bitboard!(7).rotate180(), bitboard!(56));
        assert_eq!(bitboard!(56).rotate180(), bitboard!(7));

        assert_eq!(
            (bitboard!(4) ^ bitboard!(5)).rotate180(),
            bitboard!(58) ^ bitboard!(59)
        );
    }

    #[test]
    fn rot_180_twice_identity() {
        is_id(|x: BitBoard| x.rotate180().rotate180(), id, random_board);
    }

    // Based on rot_180 being an identity
    #[test]
    fn rot_90_twice_is_rot_180() {
        is_id(
            |x: BitBoard| x.rotate90().rotate90(),
            BitBoard::rotate180,
            random_board,
        );
    }

    #[test]
    fn flip_vert() {
        assert_eq!(bitboard!(0).flip_vert(), bitboard!(7));
        assert_eq!(bitboard!(9).flip_vert(), bitboard!(14));
        assert_eq!(bitboard!(63).flip_vert(), bitboard!(56));
        assert_eq!(bitboard!(7).flip_vert(), bitboard!(0));
        assert_eq!(bitboard!(56).flip_vert(), bitboard!(63));

        assert_eq!(
            (bitboard!(4) ^ bitboard!(5)).flip_vert(),
            bitboard!(2) ^ bitboard!(3)
        );
    }

    #[test]
    fn flip_vert_twice_identity() {
        is_id(|x: BitBoard| x.flip_vert().flip_vert(), id, random_board);
    }

    #[test]
    fn flip_horz() {
        assert_eq!(bitboard!(0).flip_horz(), bitboard!(56));
        assert_eq!(bitboard!(9).flip_horz(), bitboard!(49));
        assert_eq!(bitboard!(63).flip_horz(), bitboard!(7));
        assert_eq!(bitboard!(7).flip_horz(), bitboard!(63));
        assert_eq!(bitboard!(56).flip_horz(), bitboard!(0));

        assert_eq!(
            (bitboard!(4) ^ bitboard!(5)).flip_horz(),
            bitboard!(60) ^ bitboard!(61)
        );
    }

    #[test]
    fn flip_horz_twice_identity() {
        is_id(|x: BitBoard| x.flip_horz().flip_horz(), id, random_board);
    }

    #[test]
    fn rot_270_rot_180_90_identity() {
        is_id(
            |x: BitBoard| x.rotate90().rotate180(),
            BitBoard::rotate270,
            random_board,
        );
    }

    #[test]
    fn rot_90_const_rot_90_identity() {
        is_id(BitBoard::rotate90_const, BitBoard::rotate90, random_board);
    }

    #[test]
    fn rot_270_const_rot_270_identity() {
        is_id(BitBoard::rotate270_const, BitBoard::rotate270, random_board);
    }

    #[test]
    fn test_make_bitboard_move() {
        assert_eq!(
            bitboard!(57, 58, 56).make_move(57.into(), 27.into()),
            bitboard!(27, 58, 56)
        );
    }

    #[test]
    fn make_mailbox_move() {
        let mut board = MailboxBoard::empty();
        board.add(25.into(), Piece::PawnWhite).unwrap();
        board.add(28.into(), Piece::PawnWhite).unwrap();
        board.make_move(25.into(), 37.into());

        let mut moved_board_desired = MailboxBoard::empty();
        moved_board_desired
            .add(28.into(), Piece::PawnWhite)
            .unwrap();
        moved_board_desired
            .add(37.into(), Piece::PawnWhite)
            .unwrap();
        assert_eq!(board, moved_board_desired);
    }

    #[test]
    fn test_count_bits_set() {
        assert_eq!(bitboard!(12, 14, 15).bits_set(), 3);
        assert_eq!(bitboard!(0).bits_set(), 1);
        assert_eq!(bitboard!().bits_set(), 0);
        assert_eq!(BitBoard::from(0xFFFFFFFFFFFFFFFF).bits_set(), 64);
    }
}
