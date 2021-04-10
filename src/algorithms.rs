use crate::engine::StandardEngine;
use crate::game_state::*;
use crate::moves::{Move, MoveType};
use std::cmp::{max, min};

const QUIESCENCE_SAFETY_BOUND: i32 = 200;
const QUIESCENCE_MAX_DEPTH: u16 = 3;

// If we used the true min, then -1 * i32::MIN = i32::MIN, due to overflow
const ALMOST_MIN: i32 = i32::MIN + 1;
const ALMOST_MAX: i32 = i32::MAX - 1;

static mut num_nodes_visited: u64 = 0;
static mut num_quiescent_nodes: u64 = 0;

pub trait AlphaBetaSearch {
    fn score(&self, g: &GameState) -> i32;
    /// Gives every move a heuristical score. Higher => Better
    /// Used to determine move ordering and quiescence
    /// (moves with high values still existing indicate
    /// non-quiescent nodes)
    fn move_score(m: &Move) -> i32;

    fn move_cmp(lhs: &Move, rhs: &Move) -> std::cmp::Ordering {
        Self::move_score(rhs).cmp(&Self::move_score(lhs))
    }

    fn alphabeta(&self, engine: &StandardEngine, n: u16) -> Option<Move> {
        unsafe {
            num_nodes_visited = 0;
            num_quiescent_nodes = 0;
        }
        let mut best_val = ALMOST_MIN;
        let mut best_move = None;
        let mut alpha_ = ALMOST_MIN;
        let beta_ = ALMOST_MAX;

        let mut engine_copy = engine.clone();

        let mut moves = engine
            .gen_moves()
            .expect("Unexpected game state: Previous move was illegal");
        moves.sort_unstable_by(Self::move_cmp);

        for mv in moves {
            engine_copy.next(mv.clone());

            let move_val = -self.alphabeta_helper(&mut engine_copy, n - 1, -beta_, -alpha_, -1);

            if move_val > best_val {
                best_val = move_val;
                best_move = Some(mv.clone());
            }

            engine_copy.undo().unwrap();
            alpha_ = max(alpha_, best_val);
            if alpha_ >= beta_ {
                break;
            }
        }

        unsafe {
            println!(
                "Visited: {} quiescent: {}",
                num_nodes_visited, num_quiescent_nodes
            );
        }
        best_move
    }
    // Implementation of quiescence search
    // <https://www.chessprogramming.org/Quiescence_Search>
    fn quiesce(
        &self,
        engine: &mut StandardEngine,
        alpha: i32,
        beta: i32,
        sign: i32,
        n: u16,
    ) -> i32 {
        unsafe {
            num_quiescent_nodes += 1;
        }

        let stand_pat = sign * self.score(engine.get_state());
        if stand_pat >= beta {
            return beta;
        }
        let mut alpha_ = alpha;

        if alpha < stand_pat {
            alpha_ = stand_pat;
        }

        let moves = engine.gen_moves();

        // Check if previous move was illegal
        // Could also return an Option
        if (&moves).is_none() {
            return ALMOST_MAX;
        }

        let mut moves_reordered = moves.unwrap();

        // Check if no moves are left after this point
        if n == 0 || moves_reordered.is_empty() {
            return stand_pat;
        }

        moves_reordered.sort_unstable_by(Self::move_cmp);

        for m in moves_reordered {
            match m.kind {
                MoveType::Capture(_) | MoveType::PromotionCapture(_, _) => {
                    let score;
                    engine.next(m);
                    score = -self.quiesce(engine, -beta, -alpha_, -sign, n - 1);
                    engine.undo().unwrap();
                    if score > alpha_ {
                        alpha_ = score;
                    }
                    if score >= beta {
                        return score;
                    }
                }
                _ => (),
            }
        }
        return alpha_;
    }

    #[doc(hidden)]
    fn alphabeta_helper(
        &self,
        engine: &mut StandardEngine,
        n: u16,
        alpha: i32,
        beta: i32,
        sign: i32,
    ) -> i32 {
        unsafe {
            num_nodes_visited += 1;
        }
        let moves = engine.gen_moves();

        // Check if previous move was illegal
        // Could also return an Option
        if (&moves).is_none() {
            return ALMOST_MAX;
        }

        let mut moves_reordered = moves.unwrap();

        // Check if no moves are left after this point
        // if moves_reordered.is_empty() {
        //     return sign * self.score(engine.get_state());
        // }

        if n == 0 {
            // return sign * self.score(engine.get_state());
            return self.quiesce(engine, alpha, beta, sign, QUIESCENCE_MAX_DEPTH);
        }

        moves_reordered.sort_unstable_by(Self::move_cmp);

        // Main part of the algorithm
        // Negamax Implemenation
        // See <https://en.wikipedia.org/wiki/Negamax>

        let mut alpha_ = alpha;
        let mut val = ALMOST_MIN;

        // TODO: Use owned iters?
        for mv in moves_reordered {
            engine.next(mv.clone());
            val = max(
                val,
                -self.alphabeta_helper(engine, n - 1, -beta, -alpha_, -sign),
            );
            engine.undo().unwrap();
            alpha_ = max(alpha_, val);
            if alpha_ >= beta {
                break;
            }
        }
        return val;
    }
}
