use crate::boards::*;
use crate::chess_errors::*;
use crate::game_state::*;
use crate::moves::*;
use crate::pieces::*;
use crate::positions::*;

/// An agent is an object that can play chess by choosing moves appropriate to a
/// current game state.
pub trait Agent {
    fn play_move(&self, state: &GameState) -> PlayerMove;
}

pub struct Game<A1: Agent, A2: Agent> {
    white: A1,
    black: A2,
    state: GameState,
}

impl<A1: Agent, A2: Agent> Game<A1, A2> {
    pub fn new(white: A1, black: A2) -> Game<A1, A2> {
        Game {
            white: white,
            black: black,
            state: GameState::standard_setup(),
        }
    }

    pub fn play(&mut self) {
        let mut current = Color::White;
        loop {
            let outcome = self.make_halfturn(current);
            current = current.opposite();
            if outcome != GameOutcome::Running {
                println!("{}\nGame was finished: {}", self.state, outcome);
                return;
            }
        }
    }

    pub fn make_halfturn(&mut self, c: Color) -> GameOutcome {
        println!("{}", self.state);
        let mv = match c {
            Color::White => self.white.play_move(&self.state),
            Color::Black => self.black.play_move(&self.state),
            _ => panic!("Unexpected program flow: tried to play as empty color"),
        };
        println!("{}", mv);
        self.state
            .player_move(&mv)
            .expect(&format!("Illegal move tried: {}", mv));
        self.state.outcome()
    }
}
