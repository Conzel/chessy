use crate::engine::StandardEngine;
use crate::game_state::*;
use crate::moves::Move;
use std::cmp::{max, min};

pub trait AlphaBetaSearch {
    fn score(&self, g: &GameState) -> i32;
    // Move ordering heristic will be important!
    // fn heuristic_moves(m: &Move) -> u64;

    fn alphabeta(&self, engine: &StandardEngine, n: u16) -> Option<Move> {
        let mut best_val = i32::MIN;
        let mut best_move = None;
        let mut alpha_ = i32::MIN;
        let beta_ = i32::MAX;

        let mut engine_copy = engine.clone();

        // TODO: Handle this option correctly
        for mv in engine.gen_moves().expect(
            "Unexpected game state: 
                                            Previous move was illegal",
        ) {
            engine_copy.next(mv.clone());

            let move_val = self.alphabeta_helper(&mut engine_copy, n - 1, alpha_, beta_, false);

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
        best_move
    }

    #[doc(hidden)]
    fn alphabeta_helper(
        &self,
        engine: &mut StandardEngine,
        n: u16,
        alpha: i32,
        beta: i32,
        maximizing: bool,
    ) -> i32 {
        let moves = engine.gen_moves();

        // Check if previous move was illegal
        // Could also return an Option
        if (&moves).is_none() {
            return if maximizing { i32::MAX } else { i32::MIN };
        }

        // Check if no moves are left after this point
        if n == 0 || (&moves).as_ref().unwrap().is_empty() {
            return self.score(engine.get_state());
        }

        // Main part of the algorithm
        let mut alpha_ = alpha;
        let mut beta_ = beta;
        let mut val;
        if maximizing {
            val = i32::MIN;

            // TODO: Use owned iters?
            for mv in moves.unwrap() {
                engine.next(mv.clone());
                val = max(
                    val,
                    self.alphabeta_helper(engine, n - 1, alpha_, beta_, false),
                );
                engine.undo().unwrap();
                alpha_ = max(alpha_, val);
                if alpha_ >= beta_ {
                    break;
                }
            }
        } else {
            val = i32::MAX;

            for mv in moves.unwrap() {
                engine.next(mv.clone());
                val = min(
                    val,
                    self.alphabeta_helper(engine, n - 1, alpha_, beta_, true),
                );
                engine.undo().unwrap();
                beta_ = min(beta_, val);
                if alpha_ >= beta_ {
                    break;
                }
            }
        }
        return val;
    }
}
