use crate::engine::*;
/// Differing kinds of agents that can play the game
use crate::game::Agent;
use crate::game_state::GameState;
use crate::moves::{Move, PlayerMove};
use crate::Position;
use std::io::{stdout, Write};
use text_io::read;

pub struct HumanAgent {}

impl HumanAgent {
    pub fn new() -> Self {
        HumanAgent {}
    }
}

impl Agent for HumanAgent {
    fn play_move(&self, g: &GameState) -> PlayerMove {
        println!("Your turn: ");
        print!("From: ");
        stdout().flush().unwrap();
        let from: Position = read!();
        print!("To: ");
        stdout().flush().unwrap();
        let to: Position = read!();
        PlayerMove(from, to)
    }
}

pub struct RandomAgent {}

impl RandomAgent {
    pub fn new() -> Self {
        RandomAgent {}
    }
}

impl Agent for RandomAgent {
    fn play_move(&self, state: &GameState) -> PlayerMove {
        state.clone().play_random_turn().unwrap().into()
    }
}

pub struct GreedyMaterialAgent {}

impl GreedyMaterialAgent {
    pub fn new() -> Self {
        GreedyMaterialAgent {}
    }
}

impl Agent for GreedyMaterialAgent {
    fn play_move(&self, state: &GameState) -> PlayerMove {
        use rand::seq::SliceRandom;

        let mut best_move = None;
        let mut best_material_gain: i16 = -255;
        let player_color = state.get_current_player();
        let mut moves = state.gen_moves();
        moves.shuffle(&mut rand::thread_rng());

        for mv in moves {
            let mut new_state = state.clone();
            new_state.make_move(&mv);
            let our_material = new_state.material_value(player_color);
            let enemy_material = new_state.material_value(player_color.opposite());
            let material_gain = our_material as i16 - enemy_material as i16;

            if material_gain > best_material_gain {
                best_move = Some(mv);
                best_material_gain = material_gain;
            }
        }

        best_move.expect("No moves left").into()
    }
}

pub struct SlowAgent<A: Agent> {
    inner: A,
    response_time_millis: u64,
}

impl<A: Agent> SlowAgent<A> {
    pub fn new(agent: A, response_time_millis: u64) -> Self {
        SlowAgent {
            inner: agent,
            response_time_millis: response_time_millis,
        }
    }
}

impl<A: Agent> Agent for SlowAgent<A> {
    fn play_move(&self, state: &GameState) -> PlayerMove {
        std::thread::sleep(std::time::Duration::from_millis(self.response_time_millis));
        self.inner.play_move(state)
    }
}

pub struct LookaheadHeuristicAgent<H: Fn(&GameState) -> i16> {
    heuristic: H,
    lookahead: u8,
}

impl<H: Fn(&GameState) -> i16> LookaheadHeuristicAgent<H> {
    pub fn new(heuristic: H, lookahead: u8) -> LookaheadHeuristicAgent<H> {
        debug_assert!(lookahead > 0);
        LookaheadHeuristicAgent {
            heuristic: heuristic,
            lookahead: lookahead,
        }
    }

    fn move_recursive(&self, e: &mut StandardEngine, n: u8) -> (Vec<Move>, i16) {
        if n == 0 {
            let h = &self.heuristic;
            let val = h(e.get_state());
            (e.get_moves(), val)
        } else {
            let mut best_val = i16::MIN;
            let mut best_moves = vec![];

            for mv in e.legal_moves() {
                e.next(mv);
                let (moves, val) = self.move_recursive(e, n - 1);
                if val > best_val {
                    best_val = val;
                    best_moves = moves;
                }
                e.undo().expect("Tried to undo empty engine");
            }

            (best_moves, best_val)
        }
    }
}

impl<H: Fn(&GameState) -> i16> Agent for LookaheadHeuristicAgent<H> {
    fn play_move(&self, state: &GameState) -> PlayerMove {
        let start_state = state.clone();
        let mut engine = StandardEngine::new(start_state);
        let (moves, _) = self.move_recursive(&mut engine, self.lookahead);
        (&moves[0]).into()
    }
}
