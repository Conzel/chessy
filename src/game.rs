use crate::boards::*;
use crate::chess_errors::*;
use crate::pieces::*;
use std::fmt::{self, Display};

// ---------------------------------------------
// Game
// ---------------------------------------------

// Note on this struct: property access must be highly efficient,
// so we do not use a hashmap to access the BitBoards for the
// different pieces. This sadly makes some of the code a bit bloated.
pub struct BitBoardGame {
    // The individual bit boards for the pieces
    pub white_pieces: PieceBitBoards,
    pub black_pieces: PieceBitBoards,
    // Bit board showing where all the white pieces are
    pub all_whites: BitBoard,
    pub all_blacks: BitBoard,
    pub occupancy: BitBoard,
    pub mailbox_repr: MailboxBoard,
}

impl Game for BitBoardGame {
    // This could be reimplemeted with specialized move functions for each piece later.
    fn make_move(&mut self, piece: Piece, start: Position, end: Position) {
        let b = self.get_pieceboard(piece);
        *b = b.make_move(start, end);
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
        }
    }
}

impl Display for BitBoardGame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.mailbox_repr.fmt(f)
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

pub trait Game: Display {
    /// Returns a game with the chess pieces on the standard positions
    fn standard_setup() -> Self;
    /// Moves piece p from start to end. Checking for legality
    /// is implementation dependent. May panic on illegal moves.
    fn make_move(&mut self, p: Piece, start: Position, end: Position);
}
