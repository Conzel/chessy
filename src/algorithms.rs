use crate::engine::StandardEngine;
use crate::game_state::*;
use crate::moves::{Move, MoveType};
use std::cmp::{max, min};

const QUIESCENCE_SAFETY_BOUND: i32 = 200;
const ABSOLUTE_MAX_DEPTH: u16 = 20;

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
        let mut best_val = i32::MIN;
        let mut best_move = None;
        let mut alpha_ = i32::MIN;
        let beta_ = i32::MAX;

        let mut engine_copy = engine.clone();

        let mut moves = engine
            .gen_moves()
            .expect("Unexpected game state: Previous move was illegal");
        moves.sort_unstable_by(Self::move_cmp);

        for mv in moves {
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
    // Implementation of quiescence search
    // <https://www.chessprogramming.org/Quiescence_Search>
    fn quiesce(
        &self,
        engine: &mut StandardEngine,
        alpha: i32,
        beta: i32,
        maximizing: bool,
        n: u16,
    ) -> i32 {
        let stand_pat = self.score(engine.get_state());
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
            return if maximizing { i32::MAX } else { i32::MIN };
        }

        let mut moves_reordered = moves.unwrap();

        // Check if no moves are left after this point
        if n == ABSOLUTE_MAX_DEPTH || moves_reordered.is_empty() {
            return stand_pat;
        }

        moves_reordered.sort_unstable_by(Self::move_cmp);

        for m in moves_reordered {
            match m.kind {
                MoveType::Capture(_) | MoveType::PromotionCapture(_, _) => {
                    let score;
                    if maximizing {
                        engine.next(m);
                        score = self.quiesce(engine, alpha_, beta, !maximizing, n - 1);
                        engine.undo().unwrap();
                    } else {
                        engine.next(m);
                        score = self.quiesce(engine, alpha_, beta, !maximizing, n - 1);
                        engine.undo().unwrap();
                    }
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
        maximizing: bool,
    ) -> i32 {
        let moves = engine.gen_moves();

        // Check if previous move was illegal
        // Could also return an Option
        if (&moves).is_none() {
            return if maximizing { i32::MAX } else { i32::MIN };
        }

        let mut moves_reordered = moves.unwrap();

        // Check if no moves are left after this point
        if moves_reordered.is_empty() {
            return self.score(engine.get_state());
        }

        if n == 0 {
            return self.quiesce(engine, alpha, beta, maximizing, ABSOLUTE_MAX_DEPTH);
        }

        moves_reordered.sort_unstable_by(Self::move_cmp);

        // Main part of the algorithm
        let mut alpha_ = alpha;
        let mut beta_ = beta;
        let mut val;
        if maximizing {
            val = i32::MIN;

            // TODO: Use owned iters?
            for mv in moves_reordered {
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

            for mv in moves_reordered {
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
