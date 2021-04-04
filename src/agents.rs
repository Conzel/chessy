/// Differing kinds of agents that can play the game
use crate::game::Agent;
use crate::game_state::GameState;
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
    fn play_move(&self, g: &mut GameState) {
        println!("Your turn: ");
        print!("From: ");
        stdout().flush().unwrap();
        let from: Position = read!();
        print!("To: ");
        stdout().flush().unwrap();
        let to: Position = read!();
        g.player_move(from, to).unwrap();
    }
}

pub struct RandomAgent {}

impl RandomAgent {
    pub fn new() -> Self {
        RandomAgent {}
    }
}

impl Agent for RandomAgent {
    fn play_move(&self, state: &mut GameState) {
        state.play_random_turn().unwrap();
    }
}

pub struct GreedyMaterialAgent {}

impl GreedyMaterialAgent {
    pub fn new() -> Self {
        GreedyMaterialAgent {}
    }
}

impl Agent for GreedyMaterialAgent {
    fn play_move(&self, state: &mut GameState) {
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

        state.make_move(&best_move.expect("No moves left"));
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
    fn play_move(&self, state: &mut GameState) {
        std::thread::sleep(std::time::Duration::from_millis(self.response_time_millis));
        self.inner.play_move(state)
    }
}
