// GOALS:
// * Create Chess Engine:
//   * Move generator & validator
//   * Move heuristic
//   * Search strategy
// * Include tests
use crate::attacks::*;
use crate::boards::*;
use crate::chess_errors::*;
use crate::game::Game;
use crate::pieces::*;
use crate::positions::*;
use std::fmt::{self, Debug, Display};

// -------------------------------------
// BitBoardGame
// ------------------------------------

// Note on this struct: property access must be highly efficient,
// so we do not use a hashmap to access the BitBoards for the
// different pieces. This sadly makes some of the code a bit bloated.
#[derive(Clone)]
pub struct BitBoardGame {
    // The individual bit boards for the pieces
    white_pieces: PieceBitBoards,
    black_pieces: PieceBitBoards,
    // Bit board showing where all the white pieces are
    all_whites: BitBoard,
    all_blacks: BitBoard,
    occupancy: BitBoard,
    mailbox_repr: MailboxBoard,
    turn_count: u16,
    current_player: Color,
}

impl Game for BitBoardGame {
    // This could be reimplemented with specialized move functions for each piece later.
    fn player_move(&mut self, piece: Piece, start: Position, end: Position) -> ChessResult<()> {
        // TODO: Implement different kind of moves and validate moves
        if piece.get_color() != self.current_player {
            return Err("Wrong player color".into());
        }
        let m = self
            .find_player_move(start, end)
            .ok_or(ChessError::from("Illegal move"))?;
        self.make_move(&m);
        Ok(())
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

        BitBoardGame {
            all_whites: all_whites,
            all_blacks: all_blacks,
            white_pieces: whites,
            black_pieces: blacks,
            occupancy: all_whites | all_blacks,
            mailbox_repr: mailbox_repr,
            turn_count: 0,
            current_player: Color::White,
        }
    }
}

impl Display for BitBoardGame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Turn: {}  Player: {}\n{}",
            self.turn_count, self.current_player, self.mailbox_repr
        )
    }
}

impl Debug for BitBoardGame {
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

impl BitBoardGame {
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

// TODO: Undo pub again
#[derive(Debug, Clone)]
pub struct Move {
    start: Position,
    end: Position,
    piece: Piece,
    kind: MoveType,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.piece.algebraic(),
            self.start,
            if self.kind == MoveType::Capture {
                "x"
            } else {
                ""
            },
            self.end
        )
    }
}

impl Move {
    fn new(start: Position, end: Position, piece: Piece, kind: MoveType) -> Self {
        Move {
            start: start,
            end: end,
            piece: piece,
            kind: kind,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum MoveType {
    Standard,
    Capture,
    EnPassant,
    Castle,
    PawnTwostep,
}

// -------------------------------------
// Engine Parts
// ------------------------------------

impl BitBoardGame {
    /// Returns all possible moves from current game position.
    /// Moves are only possible due to piece movement rules, not necessarily
    /// legal (might leave king in check).
    pub fn gen_moves(&self) -> Vec<Move> {
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

            // At least one valid move can be made for this piece
            for end_pos in Position::all_positions() {
                if move_board.bit_set_at(end_pos.into()) {
                    let movetype = if enemy_occ.bit_set_at(end_pos) {
                        MoveType::Capture
                    } else {
                        MoveType::Standard
                    };
                    res.push(Move::new(start_pos, end_pos, piece, movetype));
                }
            }
            // TODO: Check for Castling and E.P.
        }
        res
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
    fn make_move(&mut self, m: &Move) {
        // Making the move
        self.move_piece_bitboard(m.piece, m.start, m.end);

        // Updating our representation
        match m.kind {
            MoveType::Standard => {
                self.mailbox_repr.make_move(m.start, m.end);
                match self.current_player {
                    Color::White => self.recalculate_all_whites(),
                    Color::Black => self.recalculate_all_blacks(),
                    _ => panic!("Empty color at play"),
                }
            }
            MoveType::Capture => {
                let captured_piece = self.mailbox_repr[m.end];
                debug_assert!(captured_piece.get_color() != self.current_player);
                self.capture_piece_bitboard(captured_piece, m.end);

                self.recalculate_all_whites();
                self.recalculate_all_blacks();
                self.mailbox_repr.make_move(m.start, m.end);
            }
            _ => todo!(),
        }
        self.advance_turn();
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
            b
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

    fn advance_turn(&mut self) {
        self.turn_count += 1;
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

        let attack_board = get_attack(pos, self.occupancy, piece);
        let color = piece.get_color();

        let same_color_occ = match color {
            Color::White => self.all_whites,
            Color::Black => self.all_blacks,
            Color::None => panic!("Tried to query empty piece"),
        };

        if piece == Piece::PawnWhite || piece == Piece::PawnBlack {
            if color == Color::White {
                (attack_board & self.all_blacks) | get_white_pawn_move(pos, self.occupancy)
            } else if color == Color::Black {
                (attack_board & self.all_whites) | get_black_pawn_move(pos, self.occupancy)
            } else {
                panic!("Tried to query empty piece")
            }
        } else {
            attack_board & (!same_color_occ)
        }
    }

    /// Attemps to find the current player move in all of the legal moves that the engine
    /// can find from the current position. Returns None if the move is not among
    /// the legal moves.
    fn find_player_move(&self, start: Position, end: Position) -> Option<Move> {
        let moves = self.gen_moves();
        for m in moves {
            if m.start == start && m.end == end {
                return Some(m);
            }
        }
        None
    }

    pub fn play_random_turn(&mut self) -> ChessResult<Move> {
        use rand::seq::SliceRandom;
        let rng = &mut rand::thread_rng();
        let moves = &self.gen_moves();
        let mv = moves
            .choose(rng)
            .ok_or(ChessError::from("No playable moves left"))?;
        self.make_move(mv);
        Ok(mv.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitboard;
    use crate::game::Game;

    // Needs ~ >4 MB stack size to run and also takes a bit long when in Debug mode,
    // so we turned it off here
    #[test]
    #[cfg(not(debug_assertions))]
    fn test_move_boards() {
        let game = BitBoardGame::standard_setup();
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
        let g = BitBoardGame::standard_setup();
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
        let mut g = BitBoardGame::standard_setup();
        let prev_g = g.clone();
        g.player_move(Piece::KnightWhite, 57.into(), 42.into())
            .unwrap();
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
        let mut g = BitBoardGame::standard_setup();
        let prev_g = g.clone();

        let mv = Move::new(57.into(), 10.into(), Piece::KnightWhite, MoveType::Capture);
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
}
