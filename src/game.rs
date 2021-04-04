use crate::boards::*;
use crate::chess_errors::*;
use crate::game_state::*;
use crate::moves::*;
use crate::pieces::*;
use crate::positions::*;

/// An engine is an object that drives a GameState with the corresponding memory
/// (stack of past moves etc).
pub trait Engine {
    fn new(g: GameState) -> Self;
    fn next(&mut self, m: Move);
    fn undo(&mut self);
    fn legal_moves(&self) -> Vec<Move>;
}

/// An agent is an object that can play chess by choosing moves appropriate to a
/// current game state.
pub trait Agent {
    fn play_move(&self, state: &mut GameState);
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
            self.white.play_move(&mut self.state);
            println!("{}", self.state);
            self.black.play_move(&mut self.state);
        }
    }
}
