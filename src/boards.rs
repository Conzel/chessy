use crate::chess_errors::*;
use crate::pieces::*;
use array_init::array_init;
use std::fmt::{self, Display};
use std::ops;

// ---------------------------------------------
// General
// ---------------------------------------------

pub const BOARD_SIZE: u8 = 8;
pub type Position = u8;

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
// Positions
// ---------------------------------------------

pub fn pos_from_string(s: &str) -> ChessResult<u8> {
    // Error is rather big, so we use a closure to avoid copies
    let err_closure = || -> ChessError { format!("Invalid Chess position {}", s).into() };
    let mut chars = s.chars();

    let col = chars.next().ok_or_else(err_closure)?;
    let row = chars
        .next()
        .map(|r| r.to_digit(10))
        .flatten()
        .ok_or_else(err_closure)?;

    // We need to catch invalid early rows, else we will have a panic on unsigned integer underflow
    //    Too many characters || row is invalid
    if chars.next().is_some() || row > 8 {
        // Too many characters
        return Err(err_closure());
    }

    // number part v               v letter part
    let pos = (8 - row) * 8 + col as u32 - 'a' as u32;
    if pos >= 8 * 8 {
        Err(err_closure())
    } else {
        Ok(pos as u8)
    }
}

/// Returns row and col from position.
/// Example: Position 63 (H1 in chess board) is mapped to (7,7)
pub const fn pos_to_row_col(p: Position) -> (u8, u8) {
    (p / 8, p % 8)
}

pub const fn row_col_to_pos(row: u8, col: u8) -> Position {
    row * 8 + col
}

// ---------------------------------------------
// MailboxBoard
// ---------------------------------------------

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
                self.add(pos as u8, piece.clone())?
            }
        }
        Ok(())
    }
}

// ---------------------------------------------
// BitBoard
// ---------------------------------------------

// TODO: Make this enum private later - we just need it public at the moment for quick debugging.
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
        BitBoard(0x8000000000000000 >> at)
    }

    pub const fn empty() -> BitBoard {
        BitBoard(0)
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
        flip_horz_const(self);
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
        // Move implemented via flipping the bits and start and end position
        self.0 ^ BitBoard::singular(start) ^ BitBoard::singular(end)
    }

    pub const fn const_xor(&self, rhs: BitBoard) -> BitBoard {
        BitBoard(self.0 ^ rhs.0)
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

impl_op_ex_commutative!(| |a: &BitBoard, b: &u64| -> BitBoard {BitBoard::from(a.0 | b)});
impl_op_ex_commutative!(^ |a: &BitBoard, b: &u64| -> BitBoard {BitBoard::from(a.0 ^ b)});

#[cfg(test)]
mod tests {
    use super::*;
    use rand::prelude::*;

    fn id<T>(x: T) -> T {
        x
    }

    /// Tests if F and H are identical functions using generated values by G.
    /// Tests 1000 cycles.
    fn is_id<T, F, G, H>(f: F, h: H, g: G)
    where
        F: Fn(T) -> T,
        H: Fn(T) -> T,
        G: Fn() -> T,
        T: fmt::Debug + std::cmp::PartialEq<T> + Clone,
    {
        for _ in 1..1000 {
            let v = g();
            assert_eq!(f(v.clone()), h(v));
        }
    }

    fn random_board() -> BitBoard {
        let mut rng = thread_rng();
        BitBoard::new(rng.gen_range(std::u64::MIN..std::u64::MAX))
    }

    #[test]
    fn test_pos_from_string() {
        assert_eq!(pos_from_string("h1").unwrap(), 63);
        assert_eq!(pos_from_string("a8").unwrap(), 0);
        assert_eq!(pos_from_string("b6").unwrap(), 17);
        assert!(pos_from_string("sdlfj").is_err());
        assert!(pos_from_string("a9").is_err());
    }

    #[test]
    fn test_pos_row_col_conversion() {
        assert_eq!(pos_to_row_col(0), (0, 0));
        assert_eq!(pos_to_row_col(1), (0, 1));
        assert_eq!(pos_to_row_col(8), (1, 0));
        assert_eq!(pos_to_row_col(9), (1, 1));
        let (r1, c1) = pos_to_row_col(9);
        assert_eq!(row_col_to_pos(r1, c1), 9);
        let (r2, c2) = pos_to_row_col(23);
        assert_eq!(row_col_to_pos(r2, c2), 23);
    }

    #[test]
    fn test_flip_antidiag() {
        assert_eq!(BitBoard::singular(0).flip_diag(), BitBoard::singular(0));
        assert_eq!(BitBoard::singular(9).flip_diag(), BitBoard::singular(9));
        assert_eq!(BitBoard::singular(63).flip_diag(), BitBoard::singular(63));
        assert_eq!(BitBoard::singular(7).flip_diag(), BitBoard::singular(56));
        assert_eq!(BitBoard::singular(56).flip_diag(), BitBoard::singular(7));
        assert_eq!(BitBoard::singular(8).flip_diag(), BitBoard::singular(1));

        assert_eq!(
            (BitBoard::singular(4) ^ BitBoard::singular(5)).flip_antidiag(),
            BitBoard::singular(23) ^ BitBoard::singular(31)
        );
    }

    #[test]
    fn test_flip_diag() {
        assert_eq!(
            BitBoard::singular(0).flip_antidiag(),
            BitBoard::singular(63)
        );
        assert_eq!(
            BitBoard::singular(9).flip_antidiag(),
            BitBoard::singular(54)
        );
        assert_eq!(
            BitBoard::singular(63).flip_antidiag(),
            BitBoard::singular(0)
        );
        assert_eq!(BitBoard::singular(7).flip_antidiag(), BitBoard::singular(7));
        assert_eq!(
            BitBoard::singular(56).flip_antidiag(),
            BitBoard::singular(56)
        );
        assert_eq!(
            BitBoard::singular(8).flip_antidiag(),
            BitBoard::singular(62)
        );

        assert_eq!(
            (BitBoard::singular(4) ^ BitBoard::singular(5)).flip_diag(),
            BitBoard::singular(32) ^ BitBoard::singular(40)
        );
    }

    #[test]
    fn test_rotate_90() {
        assert_eq!(BitBoard::singular(0).rotate90(), BitBoard::singular(7));
        assert_eq!(BitBoard::singular(9).rotate90(), BitBoard::singular(14));
        assert_eq!(BitBoard::singular(63).rotate90(), BitBoard::singular(56));
        assert_eq!(BitBoard::singular(7).rotate90(), BitBoard::singular(63));
        assert_eq!(BitBoard::singular(56).rotate90(), BitBoard::singular(0));

        assert_eq!(
            (BitBoard::singular(4) ^ BitBoard::singular(5)).rotate90(),
            BitBoard::singular(39) ^ BitBoard::singular(47)
        );
    }

    #[test]
    fn test_rotate_180() {
        assert_eq!(BitBoard::singular(0).rotate180(), BitBoard::singular(63));
        assert_eq!(BitBoard::singular(9).rotate180(), BitBoard::singular(54));
        assert_eq!(BitBoard::singular(63).rotate180(), BitBoard::singular(0));
        assert_eq!(BitBoard::singular(7).rotate180(), BitBoard::singular(56));
        assert_eq!(BitBoard::singular(56).rotate180(), BitBoard::singular(7));

        assert_eq!(
            (BitBoard::singular(4) ^ BitBoard::singular(5)).rotate180(),
            BitBoard::singular(58) ^ BitBoard::singular(59)
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
        assert_eq!(BitBoard::singular(0).flip_vert(), BitBoard::singular(7));
        assert_eq!(BitBoard::singular(9).flip_vert(), BitBoard::singular(14));
        assert_eq!(BitBoard::singular(63).flip_vert(), BitBoard::singular(56));
        assert_eq!(BitBoard::singular(7).flip_vert(), BitBoard::singular(0));
        assert_eq!(BitBoard::singular(56).flip_vert(), BitBoard::singular(63));

        assert_eq!(
            (BitBoard::singular(4) ^ BitBoard::singular(5)).flip_vert(),
            BitBoard::singular(2) ^ BitBoard::singular(3)
        );
    }

    #[test]
    fn flip_vert_twice_identity() {
        is_id(|x: BitBoard| x.flip_vert().flip_vert(), id, random_board);
    }

    #[test]
    fn flip_horz() {
        assert_eq!(BitBoard::singular(0).flip_horz(), BitBoard::singular(56));
        assert_eq!(BitBoard::singular(9).flip_horz(), BitBoard::singular(49));
        assert_eq!(BitBoard::singular(63).flip_horz(), BitBoard::singular(7));
        assert_eq!(BitBoard::singular(7).flip_horz(), BitBoard::singular(63));
        assert_eq!(BitBoard::singular(56).flip_horz(), BitBoard::singular(0));

        assert_eq!(
            (BitBoard::singular(4) ^ BitBoard::singular(5)).flip_horz(),
            BitBoard::singular(60) ^ BitBoard::singular(61)
        );
    }

    #[test]
    fn flip_horz_twice_identity() {
        let mut rng = thread_rng();
        is_id(|x: BitBoard| x.flip_horz().flip_horz(), id, random_board);
    }

    #[test]
    fn rot_270_rot_180_90_identity() {
        let mut rng = thread_rng();
        is_id(
            |x: BitBoard| x.rotate90().rotate180(),
            BitBoard::rotate270,
            random_board,
        );
    }

    #[test]
    fn rot_90_const_rot_90_identity() {
        let mut rng = thread_rng();
        is_id(BitBoard::rotate90_const, BitBoard::rotate90, random_board);
    }

    #[test]
    fn rot_270_const_rot_270_identity() {
        let mut rng = thread_rng();
        is_id(BitBoard::rotate270_const, BitBoard::rotate270, random_board);
    }
}
