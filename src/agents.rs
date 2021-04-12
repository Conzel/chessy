use crate::algorithms::AlphaBetaSearch;
/// Differing kinds of agents that can play the game
use crate::algorithms::Heuristics;
use crate::engine::*;
use crate::game::Agent;
use crate::game_state::GameState;
use crate::moves::*;
use crate::pieces::Color;
use crate::pieces::PieceType;
use crate::Position;
use std::io::{stdout, Write};
use text_io::try_read;

pub struct HumanAgent {}

impl HumanAgent {
    pub fn new() -> Self {
        HumanAgent {}
    }
}

impl Agent for HumanAgent {
    fn play_move(&mut self, g: &GameState) -> PlayerMove {
        loop {
            println!("Your turn: ");
            print!("From: ");
            stdout().flush().unwrap();
            let from: Result<Position, _> = try_read!();
            print!("To: ");
            stdout().flush().unwrap();
            let to: Result<Position, _> = try_read!();
            if from.is_ok() && to.is_ok() {
                let pm = PlayerMove(from.unwrap(), to.unwrap());
                if g.player_move_legal(&pm) {
                    return pm;
                }
            }
            println!("Illegal move.");
        }
    }
}

pub struct RandomAgent {}

impl RandomAgent {
    pub fn new() -> Self {
        RandomAgent {}
    }
}

impl Agent for RandomAgent {
    fn play_move(&mut self, state: &GameState) -> PlayerMove {
        state.clone().get_random_turn().unwrap().into()
    }
}

pub struct GreedyMaterialAgent {}

impl GreedyMaterialAgent {
    pub fn new() -> Self {
        GreedyMaterialAgent {}
    }
}

impl Agent for GreedyMaterialAgent {
    fn play_move(&mut self, state: &GameState) -> PlayerMove {
        use rand::seq::SliceRandom;

        let mut best_move = None;
        let mut best_material_gain: i16 = -255;
        let player_color = state.get_current_player();
        let mut moves = state.gen_moves().unwrap();
        moves.shuffle(&mut rand::thread_rng());

        for mv in moves {
            let mut new_state = state.clone();
            new_state.make_move(&mv);
            let val = new_state.material_value();

            let our_material = val.value(player_color);
            let enemy_material = val.value(player_color.opposite());
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
    fn play_move(&mut self, state: &GameState) -> PlayerMove {
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

            if let Some(moves) = e.gen_moves() {
                for mv in moves {
                    e.next(mv);
                    let (moves, val) = self.move_recursive(e, n - 1);
                    if val > best_val {
                        best_val = val;
                        best_moves = moves;
                    }
                    e.undo().expect("Tried to undo empty engine");
                }
            }

            (best_moves, best_val)
        }
    }
}

impl<H: Fn(&GameState) -> i16> Agent for LookaheadHeuristicAgent<H> {
    fn play_move(&mut self, state: &GameState) -> PlayerMove {
        let start_state = state.clone();
        let mut engine = StandardEngine::new(start_state);
        let (moves, _) = self.move_recursive(&mut engine, self.lookahead);
        (&moves[0]).into()
    }
}

pub struct AlphaBetaAgent<H: Heuristics> {
    depth: u8,
    alphabeta: AlphaBetaSearch<H>,
}

impl<H: Heuristics> AlphaBetaAgent<H> {
    pub fn new(depth: u8, heuristic: H) -> Self {
        AlphaBetaAgent {
            depth: depth,
            alphabeta: AlphaBetaSearch::new(heuristic),
        }
    }
}

pub struct PureMaterialHeuristic(pub Color);

impl Heuristics for PureMaterialHeuristic {
    fn score(&self, g: &GameState) -> i32 {
        let val = g.material_value();
        return val.value(self.0) as i32 - val.value(self.0.opposite()) as i32;
    }

    fn move_score(_: &Move) -> i32 {
        0
    }
}

pub struct MaterialAndPositionalHeuristic(pub Color);
impl Heuristics for MaterialAndPositionalHeuristic {
    fn score(&self, g: &GameState) -> i32 {
        let val = g.material_value() + g.positional_value();
        return val.value(self.0) as i32 - val.value(self.0.opposite()) as i32;
    }

    fn move_score(_: &Move) -> i32 {
        0
    }
}

pub struct MatPosPreorderHeuristic(pub Color);
impl Heuristics for MatPosPreorderHeuristic {
    fn score(&self, g: &GameState) -> i32 {
        let val = g.material_value() + g.positional_value();
        return val.value(self.0) as i32 - val.value(self.0.opposite()) as i32;
    }

    fn move_score(m: &Move) -> i32 {
        use MoveType::*;

        match m.kind {
            Castle(_) => 100,
            Capture(captured) => {
                piece_material_value(captured.get_type()) - piece_material_value(m.piece.get_type())
            }
            Promotion(_) => 900,
            PromotionCapture(_, captured) => piece_material_value(captured.get_type()) + 900,
            _ => 0,
        }
    }
}

fn piece_material_value(p: PieceType) -> i32 {
    use PieceType::*;

    match p {
        Pawn => 100,
        Queen => 900,
        King => 2000,
        Rook => 500,
        Bishop => 300,
        Knight => 300,
        _ => 0,
    }
}

impl<H: Heuristics> Agent for AlphaBetaAgent<H> {
    fn play_move(&mut self, state: &GameState) -> PlayerMove {
        let engine = StandardEngine::new(state.clone());
        self.alphabeta
            .search(&engine, self.depth.into())
            .unwrap()
            .into()
    }
}
