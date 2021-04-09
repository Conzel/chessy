use crate::boards::BitBoard;
use crate::pieces::*;
/// Describing the moves that can be done in a chessboard.
use crate::positions::*;
use std::fmt;

/// Move from ... to ...
pub struct PlayerMove(pub Position, pub Position);

impl From<Move> for PlayerMove {
    fn from(mv: Move) -> Self {
        PlayerMove(mv.start, mv.end)
    }
}

impl From<&Move> for PlayerMove {
    fn from(mv: &Move) -> Self {
        PlayerMove(mv.start, mv.end)
    }
}

impl fmt::Display for PlayerMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

#[derive(Debug, Clone)]
pub struct Move {
    pub start: Position,
    pub end: Position,
    pub piece: Piece,
    pub kind: MoveType,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let MoveType::Castle(side) = self.kind {
            match side {
                CastleSide::QueensideBlack | CastleSide::QueensideWhite => write!(f, "O-O-O"),
                CastleSide::KingsideBlack | CastleSide::KingsideWhite => write!(f, "O-O"),
            }
        } else {
            write!(
                f,
                "{}{}{}{}",
                self.piece.algebraic(),
                self.start,
                if let MoveType::Capture(_) = self.kind {
                    "x"
                } else {
                    ""
                },
                self.end
            )
        }
    }
}

impl Move {
    pub fn new(start: Position, end: Position, piece: Piece, kind: MoveType) -> Self {
        Move {
            start: start,
            end: end,
            piece: piece,
            kind: kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MoveType {
    Standard,
    Capture(Piece),
    EnPassant,
    Castle(CastleSide),
    PawnTwostep,
    Promotion(PieceType),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CastleSide {
    QueensideBlack,
    KingsideBlack,
    QueensideWhite,
    KingsideWhite,
}

impl CastleSide {
    pub fn get_kingside(c: Color) -> CastleSide {
        use CastleSide::*;

        match c {
            Color::White => KingsideWhite,
            Color::Black => KingsideBlack,
            _ => panic!("Tried to query castling of empty color"),
        }
    }

    pub fn get_queenside(c: Color) -> CastleSide {
        use CastleSide::*;

        match c {
            Color::White => QueensideWhite,
            Color::Black => QueensideBlack,
            _ => panic!("Tried to query castling of empty color"),
        }
    }

    /// Returns both the associated king move as standard move and
    /// the associated rook move
    pub fn associated_rook_move(&self) -> Move {
        use CastleSide::*;
        match self {
            QueensideBlack => Move::new(0.into(), 3.into(), Piece::RookBlack, MoveType::Standard),
            KingsideBlack => Move::new(7.into(), 5.into(), Piece::RookBlack, MoveType::Standard),
            QueensideWhite => Move::new(56.into(), 59.into(), Piece::RookWhite, MoveType::Standard),
            KingsideWhite => Move::new(63.into(), 61.into(), Piece::RookWhite, MoveType::Standard),
        }
    }

    // Returns the king move as a castling move (e.g. e8 g8 for black kingside)
    pub fn associated_castle_move(&self) -> Move {
        use CastleSide::*;

        match self {
            QueensideBlack => Move::new(
                4.into(),
                2.into(),
                Piece::KingBlack,
                MoveType::Castle(QueensideBlack),
            ),
            KingsideBlack => Move::new(
                4.into(),
                6.into(),
                Piece::KingBlack,
                MoveType::Castle(KingsideBlack),
            ),
            QueensideWhite => Move::new(
                60.into(),
                58.into(),
                Piece::KingWhite,
                MoveType::Castle(QueensideWhite),
            ),
            KingsideWhite => Move::new(
                60.into(),
                62.into(),
                Piece::KingWhite,
                MoveType::Castle(KingsideWhite),
            ),
        }
    }

    pub fn castle_boardmask(&self) -> BitBoard {
        use CastleSide::*;

        match self {
            QueensideBlack => 0x7000000000000000,
            KingsideBlack => 0x0600000000000000,
            QueensideWhite => 0x0000000000000070,
            KingsideWhite => 0x0000000000000006,
        }
        .into()
    }

    pub fn castle_checkmask(&self) -> BitBoard {
        use CastleSide::*;

        match self {
            QueensideBlack => 0x3800000000000000,
            KingsideBlack => 0x0e00000000000000,
            QueensideWhite => 0x0000000000000038,
            KingsideWhite => 0x000000000000000e,
        }
        .into()
    }

    pub fn castling_possible(&self, attacks: BitBoard, occs: BitBoard) -> bool {
        let not_through_check = (attacks & self.castle_checkmask()).is_empty();
        let not_blocked = (occs & self.castle_boardmask()).is_empty();
        not_through_check && not_blocked
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitboard;

    #[test]
    fn test_castling_possible() {
        use CastleSide::*;

        assert!(!KingsideWhite.castling_possible(bitboard!(62), bitboard!()));
        assert!(KingsideWhite.castling_possible(bitboard!(), bitboard!()));
        assert!(!KingsideWhite.castling_possible(bitboard!(), bitboard!(62)));
        assert!(KingsideWhite.castling_possible(bitboard!(), bitboard!(63)));
        assert!(KingsideWhite.castling_possible(bitboard!(12, 34, 27), bitboard!(23, 47, 28)));

        assert!(!QueensideWhite.castling_possible(bitboard!(58), bitboard!()));
        assert!(QueensideWhite.castling_possible(bitboard!(), bitboard!()));
        assert!(!QueensideWhite.castling_possible(bitboard!(), bitboard!(57)));
        assert!(QueensideWhite.castling_possible(bitboard!(), bitboard!(56)));
        assert!(QueensideWhite.castling_possible(bitboard!(12, 34, 27), bitboard!(23, 47, 28)));

        assert!(!KingsideBlack.castling_possible(bitboard!(6), bitboard!()));
        assert!(KingsideBlack.castling_possible(bitboard!(), bitboard!()));
        assert!(!KingsideBlack.castling_possible(bitboard!(), bitboard!(6)));
        assert!(KingsideBlack.castling_possible(bitboard!(), bitboard!(7)));
        assert!(KingsideBlack.castling_possible(bitboard!(12, 34, 27), bitboard!(23, 47, 28)));

        assert!(!QueensideBlack.castling_possible(bitboard!(2), bitboard!()));
        assert!(QueensideBlack.castling_possible(bitboard!(), bitboard!()));
        assert!(!QueensideBlack.castling_possible(bitboard!(), bitboard!(1)));
        assert!(QueensideBlack.castling_possible(bitboard!(), bitboard!(0)));
        assert!(QueensideBlack.castling_possible(bitboard!(12, 34, 27), bitboard!(23, 47, 28)));
    }
}
