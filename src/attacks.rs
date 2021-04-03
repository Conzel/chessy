use crate::boards::*;
use crate::game::*;
use crate::make_usize_wrapper;
use crate::pieces::*;
use array_const_fn_init::array_const_fn_init;
use array_init::array_init;
use lazy_static::lazy_static;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};

// GOALS:
// * Create Chess Engine:
//   * Move generator & validator
//   * Move heuristic
//   * Search strategy
// * Include tests
//

// For most of this, a board with positions will be helpful for visualizations.
// Here is an example:
//     a   b   c   d   e   f   g   h
//    --------------------------------
// 8 | 0   1   2   3   4   5   6   7  | 8
// 7 | 8   9   10  11  12  13  14  15 | 7
// 6 | 16  17  18  19  20  21  22  23 | 6
// 5 | 24  25  26  27  28  29  30  31 | 5
// 4 | 32  33  34  35  36  37  38  39 | 4
// 3 | 40  41  42  43  44  45  46  47 | 3
// 2 | 48  49  50  51  52  53  54  55 | 2
// 1 | 56  57  58  59  60  61  62  63 | 1
//    --------------------------------
//    a   b   c   d   e   f   g   h

const MAGIC_NUMBER_SEED: u64 = 42;

type BlockerMasksTable = [BitBoard; 64];
type MagicNumbersTable = [u64; 64];
// Bishop staying on the middle of the board can attack 9 squares, excluding
// the border (which doesn't matter, as it is always attacked)
const MAX_BISHOP_ATTACKED_RELEVANT: u8 = 9;
const MAX_NUM_BISHOP_OCCS: usize = 1 << MAX_BISHOP_ATTACKED_RELEVANT;
type BishopAttackingTable = [[BitBoard; MAX_NUM_BISHOP_OCCS]; 64];

// Rook on the edge of the board can attack 13 squares, excluding the corner squares
const MAX_ROOK_ATTACKED_RELEVANT: u8 = 12;
const MAX_NUM_ROOK_OCCS: usize = 1 << MAX_ROOK_ATTACKED_RELEVANT;
type RookAttackingTable = [[BitBoard; MAX_NUM_ROOK_OCCS]; 64];

// Move to game
struct Move {
    start: Position,
    end: Position,
    piece: Piece,
}

impl Move {
    fn new(start: Position, end: Position, piece: Piece) -> Self {
        Move {
            start: start,
            end: end,
            piece: piece,
        }
    }
}

impl BitBoardGame {
    fn gen_moves() -> Vec<Move> {
        todo!()
    }
}

// ---------------------------------------------------------------------
// Jumping Attack Boards
// ---------------------------------------------------------------------

/// Calculates an attacking board, a bitboard where 1 is set true if the piece
/// at pos can attack the space. The attacking board is calculated using offsets
/// (spaces the piece can attack relative to pos)
/// so this is viable for Knight, Pawn and King (jumping, not gliding pieces).
/// As this will be precomputed and stored in a table, it does not matter too much
/// how efficient it is.
/// Offsets are saved as (row,col).
const fn offset_attack_board<'a, const N: usize>(
    pos: Position,
    offsets: [(i16, i16); N],
) -> BitBoard {
    // A board that has a 1 at position pos
    let mut res = BitBoard::empty();
    let (row_u, col_u) = pos_to_row_col(pos);
    let (row, col) = (row_u as i16, col_u as i16);

    let mut i = 0;
    // const fns need while loop
    while i < N {
        let (off_row, off_col) = offsets[i];
        let new_row = row + off_row;
        let new_col = col + off_col;
        if in_board(new_row, new_col) {
            // Check guarantees that new_row, new_col are u8
            res = BitBoard::singular(row_col_to_pos(new_row as u8, new_col as u8)).const_xor(res);
        }
        i += 1;
    }
    res
}

/// TODO: precompute this in a table
const fn knight_attack_board(pos: Position) -> BitBoard {
    const KNIGHT_OFFSETS: [(i16, i16); 8] = [
        (1, 2),
        (2, 1),
        (-1, 2),
        (2, -1),
        (1, -2),
        (-2, 1),
        (-1, -2),
        (-2, -1),
    ];
    offset_attack_board(pos, KNIGHT_OFFSETS)
}

/// TODO: precompute this in a table
const fn king_attack_board(pos: Position) -> BitBoard {
    const KING_OFFSETS: [(i16, i16); 8] = [
        (0, 1),
        (1, 0),
        (0, -1),
        (-1, 0),
        (1, 1),
        (-1, -1),
        (-1, 1),
        (1, -1),
    ];
    offset_attack_board(pos, KING_OFFSETS)
}

/// TODO: precompute this in a table
const fn white_pawn_attack_board(pos: Position) -> BitBoard {
    const PAWN_OFFSETS: [(i16, i16); 2] = [(-1, -1), (-1, 1)];
    offset_attack_board(pos, PAWN_OFFSETS)
}

/// TODO: precompute this in a table
const fn black_pawn_attack_board(pos: Position) -> BitBoard {
    const PAWN_OFFSETS: [(i16, i16); 2] = [(1, -1), (1, 1)];
    offset_attack_board(pos, PAWN_OFFSETS)
}

// ---------------------------------------------------------------------
// Sliding Attack Boards
// ---------------------------------------------------------------------
//

// ---------------------------------------------------------------------
// Magic Bitboards
// ---------------------------------------------------------------------
//
// Bishops
// ----------------

//
// Guidance can be found f.e. here:
// https://stackoverflow.com/questions/30680559/how-to-find-magic-bitboards
//
// For a given positions, returns a mask that contains 1s where relevant blockers are located
const fn blocker_mask_bishop(pos: Position) -> BitBoard {
    // Very stupid and slow solution, but it works
    #[rustfmt::skip]
    let offsets = [ (-6, -6), (-5, -5), (-4, -4), (-3, -3), (-2, -2), (-1, -1),
        (6, 6), (5, 5), (4, 4), (3, 3), (2, 2), (1, 1), 
        (6, -6), (5, -5), (4, -4), (3, -3), (2, -2), (1, -1), 
        (-6, 6), (-5, 5), (-4, 4), (-3, 3), (-2, 2), (-1, 1),
    ];
    // Board that masks the edges since occupancy is
    // irrelevant there                                           v Edges mask
    offset_attack_board(pos, offsets).const_and(BitBoard::new(!0xFF818181818181FF))
}

make_usize_wrapper!(blocker_mask_bishop_wrapper, blocker_mask_bishop);
const BLOCKER_MASKS_BISHOP: BlockerMasksTable =
    array_const_fn_init![blocker_mask_bishop_wrapper; 64];

/// For a given position, returns an occupancy as bitboards.
/// Const fn makes filling vectors impossible, so we loop through with an index.
/// Every blocker mask for a bishop allows for a max. of 9 possible positions
/// (max diagonal - edges), so idx is max. 2^9 == 512
/// Example:
///      Assume that the Bishop is on position 54,
///      then the positions that are relevant for blocking (not erased by the
///      blocker mask) are 9, 17, 18, 27, 36, 45.
///      We can now describe all the relevant occupancies by setting these
///      squares to blocked / not blocked. This is exactly done via
///      our index. Assume idx = 000101, then squares 27 and 45 are blocked,
///      while the rest are unblocked.
///
const fn all_occupancies(
    idx: u16,
    pos: Position,
    blocker_masks: &BlockerMasksTable,
) -> Option<BitBoard> {
    let blocker_mask = blocker_masks[pos as usize];

    let mut num_bits_set_board = 0;
    let mut board = BitBoard::empty();

    let mut board_pos = 63;
    while board_pos > 0 {
        if blocker_mask.bit_set_at(board_pos) {
            if (idx >> num_bits_set_board) % 2 == 1 {
                board = board.const_xor(BitBoard::singular(board_pos))
            }

            num_bits_set_board += 1;
        }

        board_pos -= 1;
    }

    // I don't know if this is really more efficient or elegant than just
    // returning any weird bitboard if the index is not in range (since this
    // will be irrelevant for the hashing function in the magic bitboards).
    // This will return a blocker
    if ((1 << num_bits_set_board) - 1) < idx {
        None
    } else {
        Some(board)
    }
}

/// Calculates an attack board for a sliding pieces and given occupancy bitboard
/// via scanning the given directions
/// Not very efficient, used in magic number collision detection.
const fn calculate_attack_board_sliding(
    pos: Position,
    occ: BitBoard,
    directions: [(i16, i16); 4],
) -> BitBoard {
    let (row, col) = pos_to_row_col(pos);
    let mut board = BitBoard::empty();

    let mut i = 0;
    // Diagonal scan in every direction
    while i < 4 {
        let (delta_row, delta_col) = directions[i];
        let mut new_row = row as i16 + delta_row;
        let mut new_col = col as i16 + delta_col;

        while in_board(new_row, new_col) {
            // in_board being true guarantees that new_row and new_col are u8
            let new_pos = row_col_to_pos(new_row as u8, new_col as u8);
            board = board.const_xor(BitBoard::singular(new_pos));
            if occ.bit_set_at(new_pos) {
                // we encountered the first piece in the occupancy and stop scanning in the
                // direction
                break;
            }

            new_row = new_row + delta_row;
            new_col = new_col + delta_col;
        }
        i += 1;
    }
    board
}

const fn calculate_attack_board_bishop(pos: Position, occ: BitBoard) -> BitBoard {
    calculate_attack_board_sliding(pos, occ, [(1, 1), (-1, 1), (1, -1), (-1, -1)])
}

// More can be read here
// https://www.chessprogramming.org/Magic_Bitboards
/// Expects MASKED Occupancy and returns hash value for magic number
const fn hash_board_to_index(
    pos: Position,
    masked_occ: BitBoard,
    magic: u64,
    max_relevant_attacked: u8,
) -> usize {
    // Truncation & Overflow is on purpose
    (masked_occ.get().wrapping_mul(magic) >> (64 - max_relevant_attacked)) as usize
}

fn find_magic_number_bishops(pos: Position, r: &mut impl Rng) -> u64 {
    loop {
        let current_magic = crate::utils::random_u64_few_bits(r);
        if !magic_number_collision_bishop(current_magic, pos) {
            return current_magic;
        }
    }
}

/// Checks whether the the current magic number for pos has collisions
/// on relevant occupancy boards
fn magic_number_collision_bishop(magic: u64, pos: Position) -> bool {
    let mut i = 0;
    let mut temp_occ_map: [Option<BitBoard>; MAX_NUM_BISHOP_OCCS] = [None; MAX_NUM_BISHOP_OCCS];
    let mut collision_detected = false;

    while let Some(occ) = all_occupancies(i, pos, &BLOCKER_MASKS_BISHOP) {
        i += 1;
        let current_entry =
            &mut temp_occ_map[hash_board_to_index(pos, occ, magic, MAX_BISHOP_ATTACKED_RELEVANT)];
        let current_attacking_board = calculate_attack_board_bishop(pos, occ);

        if current_entry.is_none() || current_entry.unwrap().get() == current_attacking_board.get()
        {
            *current_entry = Some(current_attacking_board);
        } else {
            return true;
        };
    }
    false
}

fn magic_numbers_table_bishop() -> MagicNumbersTable {
    let r = &mut rand::thread_rng();
    let mut arr: MagicNumbersTable = [0; 64];
    for p in 0..64 {
        arr[p] = find_magic_number_bishops(p as u8, r);
    }
    arr
}

lazy_static! {
    static ref MAGIC_NUMBERS_BISHOP: Box<MagicNumbersTable> =
        Box::new(magic_numbers_table_bishop());
}

fn bishop_attacking_table() -> BishopAttackingTable {
    let mut attacking_array: BishopAttackingTable = [[BitBoard::empty(); MAX_NUM_BISHOP_OCCS]; 64];

    let mut pos = 0u8;
    while pos < 64 {
        let magic_number = MAGIC_NUMBERS_BISHOP[pos as usize];
        let mut occ_map: [BitBoard; MAX_NUM_BISHOP_OCCS] = [BitBoard::empty(); MAX_NUM_BISHOP_OCCS];

        let mut i = 0;
        while let Some(occ) = all_occupancies(i, pos, &BLOCKER_MASKS_BISHOP) {
            i += 1;
            occ_map[hash_board_to_index(pos, occ, magic_number, MAX_BISHOP_ATTACKED_RELEVANT)] =
                calculate_attack_board_bishop(pos, occ);
        }
        attacking_array[pos as usize] = occ_map;
        pos += 1;
    }
    attacking_array
}

// TODO: Would be cool to do this as const on release and lazy static on debug.
lazy_static! {
    static ref BISHOP_ATTACKING_TABLE: Box<BishopAttackingTable> =
        Box::new(bishop_attacking_table());
}

//  Rooks
// ----------------

const fn blocker_mask_rook(pos: Position) -> BitBoard {
    #[rustfmt::skip]
    let offsets = [ 
        (0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (0,7), 
        (0,-1), (0,-2), (0,-3), (0,-4), (0,-5), (0,-6), (0,-7), 
        (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), 
        (-1,0), (-2,0), (-3,0), (-4,0), (-5,0), (-6,0), (-7,0), 
    ];
    let (row, col) = pos_to_row_col(pos);
    // Weakest edge mask, we add more if we can
    let mut edge_mask: u64 = 0xFFFFFFFFFFFFFFFF;
    if row != 0 {
        edge_mask = edge_mask & !0xFF00000000000000;
        // Edge of the board
    }
    if col != 0 {
        edge_mask = edge_mask & !0x8080808080808080;
    }
    if col != 7 {
        edge_mask = edge_mask & !0x0101010101010101;
    }
    if row != 7 {
        edge_mask = edge_mask & !0x00000000000000FF;
    }
    offset_attack_board(pos, offsets).const_and(BitBoard::new(edge_mask))
}

make_usize_wrapper!(blocker_mask_rook_wrapper, blocker_mask_rook);
const BLOCKER_MASKS_ROOK: BlockerMasksTable = array_const_fn_init![blocker_mask_rook_wrapper; 64];

const fn calculate_attack_board_rook(pos: Position, occ: BitBoard) -> BitBoard {
    calculate_attack_board_sliding(pos, occ, [(0, 1), (0, -1), (1, 0), (-1, 0)])
}

// Copy paste from the Bishop part, but array sizes need to be known at compile time so
// we cannot just pass them in through the function. And doing the whole part with const generics
// honestly just looks uglier.
fn find_magic_number_rooks(pos: Position, r: &mut impl Rng) -> u64 {
    loop {
        let current_magic = crate::utils::random_u64_few_bits(r);
        if !magic_number_collision_rook(current_magic, pos) {
            return current_magic;
        }
    }
}

/// Checks whether the the current magic number for pos has collisions
/// on relevant occupancy boards
fn magic_number_collision_rook(magic: u64, pos: Position) -> bool {
    let mut i = 0;
    let mut temp_occ_map: [Option<BitBoard>; MAX_NUM_ROOK_OCCS] = [None; MAX_NUM_ROOK_OCCS];

    while let Some(occ) = all_occupancies(i, pos, &BLOCKER_MASKS_ROOK) {
        i += 1;
        let current_entry =
            &mut temp_occ_map[hash_board_to_index(pos, occ, magic, MAX_ROOK_ATTACKED_RELEVANT)];
        let current_attacking_board = calculate_attack_board_rook(pos, occ);

        if current_entry.is_none() || current_entry.unwrap().get() == current_attacking_board.get()
        {
            *current_entry = Some(current_attacking_board);
        } else {
            return true;
        };
    }
    false
}

fn magic_numbers_table_rook() -> MagicNumbersTable {
    let r = &mut rand::thread_rng();
    let mut arr: MagicNumbersTable = [0; 64];
    for p in 0..64 {
        arr[p] = find_magic_number_rooks(p as u8, r);
    }
    arr
}

lazy_static! {
    static ref MAGIC_NUMBERS_ROOK: Box<MagicNumbersTable> = Box::new(magic_numbers_table_rook());
}

fn rook_attacking_table() -> RookAttackingTable {
    let mut attacking_array: RookAttackingTable = [[BitBoard::empty(); MAX_NUM_ROOK_OCCS]; 64];

    let mut pos = 0u8;
    while pos < 64 {
        let magic_number = MAGIC_NUMBERS_ROOK[pos as usize];
        let mut occ_map: [BitBoard; MAX_NUM_ROOK_OCCS] = [BitBoard::empty(); MAX_NUM_ROOK_OCCS];

        let mut i = 0;
        while let Some(occ) = all_occupancies(i, pos, &BLOCKER_MASKS_ROOK) {
            i += 1;
            occ_map[hash_board_to_index(pos, occ, magic_number, MAX_ROOK_ATTACKED_RELEVANT)] =
                calculate_attack_board_rook(pos, occ);
        }
        attacking_array[pos as usize] = occ_map;
        pos += 1;
    }
    attacking_array
}

// TODO: Would be cool to do this as const on release and lazy static on debug.
lazy_static! {
    static ref ROOK_ATTACKING_TABLE: Box<RookAttackingTable> = Box::new(rook_attacking_table());
}

// ---------------------------------------------------------------------
// Public Interface
// ---------------------------------------------------------------------

pub fn get_bishop_attack_board(pos: Position, occ: BitBoard) -> BitBoard {
    let masked_occ = BLOCKER_MASKS_BISHOP[pos as usize].const_and(occ);
    BISHOP_ATTACKING_TABLE[pos as usize][hash_board_to_index(
        pos,
        masked_occ,
        MAGIC_NUMBERS_BISHOP[pos as usize],
        MAX_BISHOP_ATTACKED_RELEVANT,
    )]
}

pub fn get_rook_attack_board(pos: Position, occ: BitBoard) -> BitBoard {
    let masked_occ = BLOCKER_MASKS_ROOK[pos as usize].const_and(occ);
    ROOK_ATTACKING_TABLE[pos as usize][hash_board_to_index(
        pos,
        masked_occ,
        MAGIC_NUMBERS_ROOK[pos as usize],
        MAX_ROOK_ATTACKED_RELEVANT,
    )]
}

pub fn get_queen_attack_board(pos: Position, occ: BitBoard) -> BitBoard {
    get_rook_attack_board(pos, occ) | get_bishop_attack_board(pos, occ)
}

// ---------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitboard;
    use crate::utils::*;

    #[test]
    fn test_knight_attack_board() {
        assert_eq!(
            knight_attack_board(0),
            BitBoard::from(0b0000000000100000010000000000000000000000000000000000000000000000)
        );
        assert_eq!(
            knight_attack_board(27),
            BitBoard::from(0b0000000000101000010001000000000001000100001010000000000000000000)
        );
    }

    #[test]
    fn test_king_attack_board() {
        assert_eq!(
            king_attack_board(0),
            BitBoard::from(0b0100000011000000000000000000000000000000000000000000000000000000)
        );
        assert_eq!(
            king_attack_board(27),
            BitBoard::from(0b0000000000000000001110000010100000111000000000000000000000000000)
        );
    }

    #[test]
    fn test_pawn_attack_board() {
        assert_eq!(white_pawn_attack_board(62), BitBoard::from(0b10100000000));
        assert_eq!(
            black_pawn_attack_board(1),
            BitBoard::from(0b0000000010100000000000000000000000000000000000000000000000000000)
        );
        assert_eq!(
            black_pawn_attack_board(0),
            BitBoard::from(0b0000000001000000000000000000000000000000000000000000000000000000)
        );
    }

    #[test]
    fn test_blocker_mask_bishop() {
        assert_eq!(
            blocker_mask_bishop(0),
            BitBoard::from(0b0000000001000000001000000001000000001000000001000000001000000000)
        );
        assert_eq!(
            blocker_mask_bishop(9),
            BitBoard::from(0b0000000000000000001000000001000000001000000001000000001000000000)
        );
        assert_eq!(
            blocker_mask_bishop(1),
            BitBoard::from(0b0000000000100000000100000000100000000100000000100000000000000000)
        );
        assert_eq!(blocker_mask_bishop(17), bitboard!(10, 26, 35, 44, 53));
    }

    #[test]
    fn test_blocker_mask_rook() {
        assert_eq!(
            blocker_mask_rook(0),
            bitboard!(1, 2, 3, 4, 5, 6, 8, 16, 24, 32, 40, 48)
        );
        assert_eq!(
            blocker_mask_rook(7),
            bitboard!(1, 2, 3, 4, 5, 6, 15, 23, 31, 39, 47, 55)
        );
        assert_eq!(
            blocker_mask_rook(9),
            bitboard!(10, 11, 12, 13, 14, 17, 25, 33, 41, 49)
        );
        assert_eq!(
            blocker_mask_rook(16),
            bitboard!(8, 24, 32, 40, 48, 17, 18, 19, 20, 21, 22)
        );
        assert_eq!(
            blocker_mask_rook(23),
            bitboard!(17, 18, 19, 20, 21, 22, 15, 31, 39, 47, 55)
        );
        assert_eq!(
            blocker_mask_rook(4),
            bitboard!(1, 2, 3, 5, 6, 12, 20, 28, 36, 44, 52)
        );
        assert_eq!(
            blocker_mask_rook(60),
            bitboard!(57, 58, 59, 61, 62, 12, 20, 28, 36, 44, 52)
        );
        assert_eq!(
            blocker_mask_rook(30),
            bitboard!(14, 22, 25, 26, 27, 28, 29, 38, 46, 54)
        );
        assert_eq!(
            blocker_mask_rook(62),
            bitboard!(57, 58, 59, 60, 61, 54, 46, 38, 30, 22, 14)
        );
    }

    #[test]
    fn test_all_occupanices() {
        assert_eq!(
            all_occupancies(0b10101, 54, &BLOCKER_MASKS_BISHOP).unwrap(),
            bitboard!(9, 27, 45)
        );
        assert_eq!(
            all_occupancies(0b00000, 54, &BLOCKER_MASKS_BISHOP).unwrap(),
            BitBoard::empty()
        );
        assert_eq!(
            all_occupancies(0b101010, 63, &BLOCKER_MASKS_BISHOP).unwrap(),
            bitboard!(9, 27, 45)
        );
        assert_eq!(
            all_occupancies(0b101010, 0, &BLOCKER_MASKS_ROOK).unwrap(),
            bitboard!(8, 24, 40)
        );
        assert_eq!(
            all_occupancies(0b010101010, 63, &BLOCKER_MASKS_BISHOP),
            None
        );
        assert_eq!(
            all_occupancies(0b010101010000000, 63, &BLOCKER_MASKS_ROOK),
            None
        );
    }

    #[test]
    fn test_calc_attack_board_bishop() {
        assert_eq!(
            calculate_attack_board_bishop(0, bitboard!(18, 27)),
            bitboard!(9, 18)
        );
        assert_eq!(
            calculate_attack_board_bishop(0, bitboard!(27, 55)),
            bitboard!(9, 18, 27)
        );
        assert_eq!(
            calculate_attack_board_bishop(0, bitboard!(9, 18, 27, 22, 26, 28)),
            bitboard!(9)
        );
        assert_eq!(
            calculate_attack_board_bishop(0, bitboard!()),
            bitboard!(9, 18, 27, 36, 45, 54, 63)
        );
        assert_eq!(
            calculate_attack_board_bishop(49, bitboard!(35)),
            bitboard!(35, 42, 40, 56, 58)
        );
    }

    #[test]
    fn test_calc_attack_board_rook() {
        assert_eq!(
            calculate_attack_board_rook(0, bitboard!(18, 27)),
            bitboard!(1, 2, 3, 4, 5, 6, 7, 8, 16, 24, 32, 40, 48, 56)
        );
        assert_eq!(
            calculate_attack_board_rook(0, bitboard!(1, 8)),
            bitboard!(1, 8)
        );
        assert_eq!(
            calculate_attack_board_rook(0, bitboard!(1, 8, 9, 16, 24)),
            bitboard!(1, 8)
        );
        assert_eq!(
            calculate_attack_board_rook(0, bitboard!()),
            bitboard!(1, 2, 3, 4, 5, 6, 7, 8, 16, 24, 32, 40, 48, 56)
        );
    }

    // Tests are very expensive in debug mode
    #[test]
    #[cfg(not(debug_assertions))]
    fn test_calc_get_attack_bishop_id() {
        for p in 0..64 {
            is_id(
                |x: BitBoard| calculate_attack_board_bishop(p, x),
                |y: BitBoard| get_bishop_attack_board(p, y),
                random_board,
            );
        }
        assert_eq!(
            get_bishop_attack_board(49, bitboard!(35)),
            bitboard!(35, 42, 40, 56, 58)
        );
    }

    // WARNING: Creates a HUGE table. If you get a Stack Overflow, increase Rust stack size:
    // export RUST_MIN_STACK=8388608
    #[test]
    #[cfg(not(debug_assertions))]
    fn test_calc_get_attack_rook_id() {
        for p in 0..64 {
            is_id(
                |x: BitBoard| calculate_attack_board_rook(p, x),
                |y: BitBoard| get_rook_attack_board(p, y),
                random_board,
            );
        }
    }

    #[test]
    #[cfg(not(debug_assertions))]
    fn test_get_queen_attack_board() {
        assert_eq!(
            get_queen_attack_board(0, bitboard!(18, 27)),
            bitboard!(1, 2, 3, 4, 5, 6, 7, 8, 16, 24, 32, 40, 48, 56, 9, 18)
        );
        assert_eq!(
            get_queen_attack_board(0, bitboard!(1, 8)),
            bitboard!(1, 8, 9, 18, 27, 36, 45, 54, 63)
        );
        assert_eq!(
            get_queen_attack_board(0, bitboard!(1, 8, 9, 16, 24)),
            bitboard!(1, 8, 9)
        );
    }
}
