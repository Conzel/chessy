// GOALS:
// * Create Chess Engine:
//   * Move generator & validator
//   * Move heuristic
//   * Search strategy
// * Include tests
use crate::attacks::*;
use crate::boards::*;
use crate::chess_errors::*;
use crate::game::*;
use crate::moves::*;
use crate::pieces::*;
use crate::positional_tables::*;
use crate::positions::*;
use std::fmt::{self, Debug, Display};

// -------------------------------------
// BitBoardGame
// ------------------------------------

/// A Game State is an object that represents the current GameState.
/// Implements basic operations (executing one move forward, backwards, legal move generation)
/// and information about game statistics.
#[derive(Clone, PartialEq)]
pub struct GameState {
    // The individual bit boards for the pieces
    white_pieces: PieceBitBoards,
    black_pieces: PieceBitBoards,
    // Bit board showing where all the white pieces are
    all_whites: BitBoard,
    all_blacks: BitBoard,
    mailbox_repr: MailboxBoard,
    turn_count: u16,
    current_player: Color,
}

// Public Interface
impl GameState {
    // This could be reimplemented with specialized move functions for each piece later.
    pub fn player_move(&mut self, pm: &PlayerMove) -> ChessResult<()> {
        let m = self
            .find_player_move(pm)
            .ok_or(ChessError::from("Illegal move"))?;
        self.make_move(&m);
        Ok(())
    }

    pub fn player_move_legal(&self, pm: &PlayerMove) -> bool {
        self.find_player_move(pm).is_some()
    }

    /// Returns a game with the figures placed on standard chess starting positions
    pub fn standard_setup() -> GameState {
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
        let black_vec: Vec<BitBoard> = white_vec.iter().map(|b| b.rotate180()).collect();

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

        let all_whites = whites.combine();
        let all_blacks = blacks.combine();
        let mailbox_repr = MailboxBoard::from_piece_bitboards(&whites, &blacks)
            .expect("Standard setup failed; board in invalid state.");

        GameState {
            all_whites: all_whites,
            all_blacks: all_blacks,
            white_pieces: whites,
            black_pieces: blacks,
            mailbox_repr: mailbox_repr,
            turn_count: 0,
            current_player: Color::White,
        }
    }
}

/// Valuation result of a board state. Contains one value for white
/// and one for black
pub struct BoardValuation(u16, u16);

impl BoardValuation {
    pub fn white(&self) -> u16 {
        self.0
    }

    pub fn black(&self) -> u16 {
        self.1
    }

    pub fn value(&self, c: Color) -> u16 {
        match c {
            Color::White => self.white(),
            Color::Black => self.black(),
            _ => panic!("Tried to get material value of empty color"),
        }
    }
}

impl std::ops::Add for BoardValuation {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

// Statistical things
impl GameState {
    fn material_value_piecebit(p: &PieceBitBoards) -> u16 {
        // TODO: Could pre-calculate this in a table and together with position advantage
        100 * p.pawns.bits_set() as u16
            + 320 * (p.knights.bits_set()) as u16
            + 330 * (p.bishops.bits_set()) as u16
            + 525 * p.rooks.bits_set() as u16
            + 1000 * p.queens.bits_set() as u16
            + 10000 * p.kings.bits_set() as u16
    }

    /// Returns material value in centipawns
    pub fn material_value(&self) -> BoardValuation {
        BoardValuation(
            GameState::material_value_piecebit(&self.white_pieces),
            GameState::material_value_piecebit(&self.black_pieces),
        )
    }

    /// Returns positional value of both sides (a lot more efficient to do both in one run)
    /// Positional value is guaranteed to not make a piece have negative value
    pub fn positional_value(&self) -> BoardValuation {
        let mut total_white = 0;
        let mut total_black = 0;

        // TODO: Scale to end game tables
        for (pos, piece) in self.mailbox_repr.into_iter() {
            let c = piece.get_color();

            if c == Color::White {
                total_white += Self::positional_value_piece_pos(piece, pos);
            } else if c == Color::Black {
                total_black += Self::positional_value_piece_pos(piece, pos);
            }
        }
        BoardValuation(total_white, total_black)
    }

    pub fn get_current_player(&self) -> Color {
        self.current_player
    }

    fn positional_value_piece_pos(piece: Piece, pos: Position) -> u16 {
        use PieceType::*;

        let rel_pos = match piece.get_color() {
            Color::Black => (64 - pos.get() - 1).into(),
            Color::White => pos,
            _ => panic!("Tried to get value of empty colored piece"),
        };

        match piece.get_type() {
            Pawn => PAWN_POSITIONAL_TABLE[rel_pos],
            Bishop => BISHOPS_POSITIONAL_TABLE[rel_pos],
            Queen => QUEEN_POSITIONAL_TABLE[rel_pos],
            King => KING_POSITIONAL_TABLE[rel_pos],
            Rook => ROOKS_POSITIONAL_TABLE[rel_pos],
            Knight => KNIGHTS_POSITIONAL_TABLE[rel_pos],
            _ => panic!("Tried to get value of empty piece"),
        }
    }
}

impl Display for GameState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Turn: {}  Player: {}\n{}",
            self.turn_count, self.current_player, self.mailbox_repr
        )
    }
}

impl Debug for GameState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Turn: {}  Player: {}\n",
            self.turn_count, self.current_player
        )?;
        write!(
            f,
            "Whites: {:#?}\nBlacks: {:#?}\n",
            self.all_whites, self.all_blacks
        )?;
        write!(f, "{}", self.mailbox_repr)
    }
}

// Moving things
impl GameState {
    fn get_pieceboard(&mut self, p: Piece) -> &mut BitBoard {
        use Piece::*;
        match p {
            PawnWhite => &mut self.white_pieces.pawns,
            KnightWhite => &mut self.white_pieces.knights,
            BishopWhite => &mut self.white_pieces.bishops,
            RookWhite => &mut self.white_pieces.rooks,
            QueenWhite => &mut self.white_pieces.queens,
            KingWhite => &mut self.white_pieces.kings,
            PawnBlack => &mut self.black_pieces.pawns,
            KnightBlack => &mut self.black_pieces.knights,
            BishopBlack => &mut self.black_pieces.bishops,
            RookBlack => &mut self.black_pieces.rooks,
            QueenBlack => &mut self.black_pieces.queens,
            KingBlack => &mut self.black_pieces.kings,
            _ => panic!("Tried to query game for empty piece board"),
        }
    }
}

// -------------------------------------
// Moving impls
// ------------------------------------

impl GameState {
    /// Returns all possible moves from current game position.
    /// Moves are only possible due to piece movement rules, not necessarily
    /// legal (might leave king in check). If the king is capturable from
    /// this position, None will be returned (indicating that the last move
    /// must have been illegal).
    pub fn gen_moves(&self) -> Option<Vec<Move>> {
        let mut res = Vec::new();

        for (start_pos, piece) in self.mailbox_repr.into_iter() {
            if piece == Piece::Empty || piece.get_color() != self.current_player {
                continue;
            }

            let move_board = self.piece_move_board(start_pos, piece);

            if move_board.is_empty() {
                continue;
            }

            let enemy_occ = self.get_enemy_occ();
            // TODO: Can we have a more efficient method of scanning
            // positions?
            // Idea: Row wise scanning first?
            // Most inefficient parts of the code right now

            // At least one valid move can be made for this piece
            for end_pos in Position::all_positions() {
                if move_board.bit_set_at(end_pos.into()) {
                    let movetype = if enemy_occ.bit_set_at(end_pos) {
                        let captured_piece = self.mailbox_repr[end_pos];
                        if captured_piece.get_type() == PieceType::King {
                            return None;
                        }
                        MoveType::Capture(captured_piece)
                    } else {
                        MoveType::Standard
                    };
                    res.push(Move::new(start_pos, end_pos, piece, movetype));
                }
            }
            // TODO: Check for Castling and E.P.
        }
        Some(res)
    }

    /// Gets occupancy of the enemy of the current player
    /// (Black occupancy for White, White Occupancy for Black)
    fn get_enemy_occ(&self) -> BitBoard {
        match self.current_player {
            Color::White => self.all_blacks,
            Color::Black => self.all_whites,
            _ => panic!("Empty color at play"),
        }
    }

    // Makes a move on the board.
    pub fn make_move(&mut self, m: &Move) {
        // Making the move
        self.move_piece_bitboard(m.piece, m.start, m.end);
        self.mailbox_repr.make_move(m.start, m.end);

        // Updating our representation
        match m.kind {
            MoveType::Standard => match self.current_player {
                Color::White => self.recalculate_all_whites(),
                Color::Black => self.recalculate_all_blacks(),
                _ => panic!("Empty color at play"),
            },
            MoveType::Capture(captured) => {
                debug_assert!(captured.get_color() != self.current_player);
                self.capture_piece_bitboard(captured, m.end);

                self.recalculate_all_whites();
                self.recalculate_all_blacks();
            }
            _ => todo!(),
        }
        self.advance_turn();
    }

    pub fn undo_move(&mut self, m: &Move) {
        self.deadvance_turn();
        self.move_piece_bitboard(m.piece, m.end, m.start);
        self.mailbox_repr.make_move(m.end, m.start);
        match m.kind {
            MoveType::Standard => match self.current_player {
                Color::White => self.recalculate_all_whites(),
                Color::Black => self.recalculate_all_blacks(),
                _ => panic!("Empty color at play"),
            },
            MoveType::Capture(captured) => {
                self.uncapture_piece_bitboard(captured, m.end);

                self.recalculate_all_blacks();
                self.recalculate_all_whites();
                self.mailbox_repr.add(m.end, captured).expect(
                    "Unexpected execution flow in undo move:
                position already occupied in mailbox",
                );
            }
            _ => todo!(),
        };
    }

    /// Updates the piece bit board corresponding to the given piece by moving the entry
    /// at position start to position end. Not required to check for double occupancy or whether
    /// the piece was at the correct position to begin with.
    fn move_piece_bitboard(&mut self, piece: Piece, start: Position, end: Position) {
        let b = self.get_pieceboard(piece);
        debug_assert!(
            b.bit_set_at(start) && (!b.bit_set_at(end)),
            "\nMove illegal on board. {}: {} → {}\n{:?}",
            piece,
            start,
            end,
            b,
        );
        *b = b.make_move(start, end);
    }

    /// Updates the piece bit board corresponding to the given piece by deleting
    /// the bit set at position. Not required to check if the position was occupied in
    /// the first place.
    fn capture_piece_bitboard(&mut self, piece: Piece, pos: Position) {
        let b = self.get_pieceboard(piece);
        debug_assert!(
            b.bit_set_at(pos),
            "\nTarget piece not set. {}: captured at {}\n{:?}",
            piece,
            pos,
            b
        );
        *b = b.unset_bit_at(pos);
    }

    /// Reverse operation to capture_piece_bitboard
    fn uncapture_piece_bitboard(&mut self, piece: Piece, pos: Position) {
        let b = self.get_pieceboard(piece);
        debug_assert!(
            !b.bit_set_at(pos),
            "\nTarget piece not set. {}: captured at {}\n{:?}",
            piece,
            pos,
            b
        );
        *b = b.set_bit_at(pos);
    }

    fn advance_turn(&mut self) {
        self.turn_count += 1;
        self.flip_color();
    }

    fn deadvance_turn(&mut self) {
        self.turn_count -= 1;
        self.flip_color();
    }

    fn recalculate_all_whites(&mut self) {
        self.all_whites = self.white_pieces.combine();
    }

    fn recalculate_all_blacks(&mut self) {
        self.all_blacks = self.black_pieces.combine();
    }

    fn flip_color(&mut self) {
        self.current_player = self.current_player.opposite();
    }

    // Returns move board of the piece at pos. Undefined behaviour if an empty piece
    // is passed in. Makes no checks whether the piece is really at the given
    // position.
    fn piece_move_board(&self, pos: Position, piece: Piece) -> BitBoard {
        assert!(piece != Piece::Empty);

        let total_occ = self.all_whites | self.all_blacks;

        let attack_board = get_attack(pos, total_occ, piece);
        let color = piece.get_color();

        let same_color_occ = match color {
            Color::White => self.all_whites,
            Color::Black => self.all_blacks,
            Color::None => panic!("Tried to query empty piece"),
        };

        if piece == Piece::PawnWhite || piece == Piece::PawnBlack {
            if color == Color::White {
                (attack_board & self.all_blacks) | get_white_pawn_move(pos, total_occ)
            } else if color == Color::Black {
                (attack_board & self.all_whites) | get_black_pawn_move(pos, total_occ)
            } else {
                panic!("Tried to query empty piece")
            }
        } else {
            attack_board & (!same_color_occ)
        }
    }

    /// Attemps to find the current player move in all of the legal moves that the engine
    /// can find from the current position. Returns None if the move is not among
    /// the legal moves. Ensures that check is not violated.
    fn find_player_move(&self, mv: &PlayerMove) -> Option<Move> {
        let PlayerMove(start, end) = mv;
        let moves = self.gen_moves()?;
        for m in moves {
            if m.start == *start && m.end == *end {
                let mut state_copy = self.clone();
                state_copy.make_move(&m);
                if state_copy.gen_moves().is_some() {
                    return Some(m);
                } else {
                    // m was illegal
                    return None;
                }
            }
        }
        None
    }

    pub fn play_random_turn(&mut self) -> ChessResult<Move> {
        use rand::seq::SliceRandom;
        let rng = &mut rand::thread_rng();
        let moves = &self
            .gen_moves()
            .ok_or(ChessError::from("Previous move was illegal."))?;
        let mv = moves
            .choose(rng)
            .ok_or(ChessError::from("No playable moves left."))?;
        self.make_move(mv);
        Ok(mv.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitboard;

    // Needs ~ >4 MB stack size to run and also takes a bit long when in Debug mode,
    // so we turned it off here
    #[test]
    fn test_move_boards() {
        let game = GameState::standard_setup();
        assert_eq!(
            game.piece_move_board(57.into(), Piece::KnightWhite),
            bitboard!(40, 42)
        );
        assert_eq!(
            game.piece_move_board(56.into(), Piece::RookWhite),
            bitboard!()
        );
        assert_eq!(
            game.piece_move_board(56.into(), Piece::RookBlack),
            bitboard!(57, 48)
        );
        assert_eq!(
            game.piece_move_board(51.into(), Piece::PawnWhite),
            bitboard!(43, 35)
        );
        assert_eq!(
            game.piece_move_board(19.into(), Piece::PawnWhite),
            bitboard!(10, 12)
        );
        assert_eq!(
            game.piece_move_board(27.into(), Piece::PawnWhite),
            bitboard!(19)
        );
        assert_eq!(
            game.piece_move_board(51.into(), Piece::PawnWhite),
            bitboard!(43, 35)
        );
        assert_eq!(
            game.piece_move_board(11.into(), Piece::PawnBlack),
            bitboard!(19, 27)
        );
        assert_eq!(
            game.piece_move_board(43.into(), Piece::PawnBlack),
            bitboard!(50, 52)
        );
        assert_eq!(
            game.piece_move_board(35.into(), Piece::PawnBlack),
            bitboard!(43)
        );
    }

    #[test]
    fn test_standard_setup() {
        let g = GameState::standard_setup();
        assert_eq!(
            g.white_pieces.pawns,
            bitboard!(48, 49, 50, 51, 52, 53, 54, 55)
        );
        assert_eq!(
            g.black_pieces.pawns,
            bitboard!(8, 9, 10, 11, 12, 13, 14, 15)
        );
        assert_eq!(g.white_pieces.knights, bitboard!(57, 62));
        assert_eq!(g.black_pieces.knights, bitboard!(1, 6));
        assert_eq!(g.white_pieces.kings, bitboard!(60));
        assert_eq!(g.black_pieces.kings, bitboard!(4));
        assert_eq!(g.white_pieces.queens, bitboard!(59));
        assert_eq!(g.black_pieces.queens, bitboard!(3));
        assert_eq!(g.white_pieces.rooks, bitboard!(56, 63));
        assert_eq!(g.black_pieces.rooks, bitboard!(0, 7));
        assert_eq!(g.white_pieces.bishops, bitboard!(58, 61));
        assert_eq!(g.black_pieces.bishops, bitboard!(2, 5));

        println!("{}", g.all_whites);

        assert_eq!(g.current_player, Color::White);
        assert_eq!(g.turn_count, 0);
        assert_eq!(
            g.all_whites,
            bitboard!(48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63)
        );
        assert_eq!(
            g.all_blacks,
            bitboard!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
        );
    }

    #[test]
    fn test_simple_move() {
        let mut g = GameState::standard_setup();
        let prev_g = g.clone();
        g.player_move(&PlayerMove(57.into(), 42.into())).unwrap();
        assert_eq!(
            g.all_whites,
            bitboard!(48, 49, 50, 51, 52, 53, 54, 55, 56, 42, 58, 59, 60, 61, 62, 63)
        );
        assert_eq!(g.white_pieces.knights, bitboard!(62, 42));
        assert_eq!(g.black_pieces, prev_g.black_pieces);
        assert_eq!(g.current_player, Color::Black);
        assert_eq!(g.turn_count, 1);
        assert_eq!(g.white_pieces.pawns, prev_g.white_pieces.pawns);
        assert_eq!(g.white_pieces.kings, prev_g.white_pieces.kings);
        assert_eq!(g.white_pieces.queens, prev_g.white_pieces.queens);
        assert_eq!(g.white_pieces.rooks, prev_g.white_pieces.rooks);
        assert_eq!(g.white_pieces.bishops, prev_g.white_pieces.bishops);
    }

    #[test]
    fn test_simple_capture() {
        let mut g = GameState::standard_setup();
        let prev_g = g.clone();

        let mv = Move::new(
            57.into(),
            10.into(),
            Piece::KnightWhite,
            MoveType::Capture(Piece::PawnBlack),
        );
        g.make_move(&mv);

        assert_eq!(
            g.all_whites,
            bitboard!(48, 49, 50, 51, 52, 53, 54, 55, 56, 10, 58, 59, 60, 61, 62, 63)
        );
        assert_eq!(
            g.all_blacks,
            bitboard!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15)
        );
        assert_eq!(g.white_pieces.knights, bitboard!(62, 10));
        assert_eq!(g.black_pieces.pawns, bitboard!(8, 9, 11, 12, 13, 14, 15));

        assert_eq!(g.current_player, Color::Black);
        assert_eq!(g.turn_count, 1);

        assert_eq!(g.white_pieces.pawns, prev_g.white_pieces.pawns);
        assert_eq!(g.white_pieces.kings, prev_g.white_pieces.kings);
        assert_eq!(g.white_pieces.queens, prev_g.white_pieces.queens);
        assert_eq!(g.white_pieces.rooks, prev_g.white_pieces.rooks);
        assert_eq!(g.white_pieces.bishops, prev_g.white_pieces.bishops);

        assert_eq!(g.black_pieces.kings, prev_g.black_pieces.kings);
        assert_eq!(g.black_pieces.queens, prev_g.black_pieces.queens);
        assert_eq!(g.black_pieces.rooks, prev_g.black_pieces.rooks);
        assert_eq!(g.black_pieces.bishops, prev_g.black_pieces.bishops);
    }

    #[test]
    fn test_undo_capture() {
        let mut g = GameState::standard_setup();
        let prev_g = g.clone();
        let mv = Move::new(
            57.into(),
            10.into(),
            Piece::KnightWhite,
            MoveType::Capture(Piece::PawnBlack),
        );
        g.make_move(&mv);
        g.undo_move(&mv);
        assert_eq!(g, prev_g);
    }

    #[test]
    fn test_undo_standard_move() {
        let mut g = GameState::standard_setup();
        let prev_g = g.clone();
        let mv = Move::new(49.into(), 41.into(), Piece::PawnWhite, MoveType::Standard);
        g.make_move(&mv);
        g.undo_move(&mv);
        assert_eq!(g, prev_g);
    }

    #[test]
    fn test_make_undo_random_moves() {
        let mut g = GameState::standard_setup();

        for _ in 0..50 {
            let prev_g = g.clone();
            let mv = g.play_random_turn().unwrap();
            g.undo_move(&mv);
            assert_eq!(g, prev_g, "\nCouldn't undo move \n{:?}\n", mv);

            g.play_random_turn().unwrap();
        }
    }
}
