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
        loop {
            println!("{}", self.state);
            let white_mv = self.white.play_move(&self.state);
            self.state.player_move(&white_mv);
            println!("{}", self.state);
            let black_mv = self.black.play_move(&self.state);
            self.state.player_move(&black_mv);
        }
    }
}
