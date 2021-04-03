// GOALS:
// * Create Chess Engine:
//   * Move generator & validator
//   * Move heuristic
//   * Search strategy
// * Include tests
use crate::attacks::*;
use crate::boards::*;
use crate::game::BitBoardGame;
use crate::pieces::*;
use std::fmt;

#[derive(Debug, Clone)]
struct Move {
    start: Position,
    end: Position,
    piece: Piece,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}{}",
            self.piece,
            pos_to_algebraic(self.start).unwrap_or("Invalid".into()),
            pos_to_algebraic(self.start).unwrap_or("Invalid".into())
        )
    }
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
    /// Returns all possible moves from current game position.
    /// Moves are only possible due to piece movement rules, not necessarily
    /// legal (might leave king in check).
    fn gen_moves(&self) -> Vec<Move> {
        let mut res = Vec::new();
        let occupancy = self.all_whites | self.all_blacks;

        for (pos, piece) in self.mailbox_repr.into_iter().enumerate() {}
        res
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
            game.piece_move_board(57, Piece::KnightWhite),
            bitboard!(40, 42)
        );
        assert_eq!(game.piece_move_board(56, Piece::RookWhite), bitboard!());
        assert_eq!(
            game.piece_move_board(56, Piece::RookBlack),
            bitboard!(57, 48)
        );
        assert_eq!(
            game.piece_move_board(51, Piece::PawnWhite),
            bitboard!(43, 35)
        );
        assert_eq!(
            game.piece_move_board(19, Piece::PawnWhite),
            bitboard!(10, 12)
        );
        assert_eq!(game.piece_move_board(27, Piece::PawnWhite), bitboard!(19));
        assert_eq!(
            game.piece_move_board(51, Piece::PawnWhite),
            bitboard!(43, 35)
        );
        assert_eq!(
            game.piece_move_board(11, Piece::PawnBlack),
            bitboard!(19, 27)
        );
        assert_eq!(
            game.piece_move_board(43, Piece::PawnBlack),
            bitboard!(50, 52)
        );
        assert_eq!(game.piece_move_board(35, Piece::PawnBlack), bitboard!(43));
    }
}
