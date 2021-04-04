use crate::BitBoard;
use rand::{thread_rng, Rng};
use std::fmt;

// For testing purposes: Easily creates a bitboard with multiple positions set.
// Not very efficient.
#[macro_export]
macro_rules! bitboard {
    ( $( $x:expr ),* ) => {
        {
            #[allow(unused)]
            let mut base = BitBoard::empty();
            $(
                base = base | BitBoard::singular(($x as u8).into());
            )*
            base
        }
    };
}

#[macro_export]
macro_rules! make_usize_wrapper {
    ($func_name:ident, $orig_f:expr) => {
        const fn $func_name(pos: usize) -> BitBoard {
            $orig_f(Position::const_new(pos as u8))
        }
    };
}

// Functions relevant for tests
#[cfg(test)]
pub fn id<T>(x: T) -> T {
    x
}

/// Tests if F and H are identical functions using generated values by G.
/// Tests 100 cycles.
#[cfg(test)]
pub fn is_id<T, F, G, H>(f: F, h: H, g: G)
where
    F: Fn(T) -> T,
    H: Fn(T) -> T,
    G: Fn() -> T,
    T: fmt::Debug + std::cmp::PartialEq<T> + Clone,
{
    for _ in 1..100 {
        let v = g();
        assert_eq!(f(v.clone()), h(v));
    }
}

#[cfg(test)]
pub fn random_board() -> BitBoard {
    let mut rng = thread_rng();
    BitBoard::new(rng.gen_range(std::u64::MIN..std::u64::MAX))
}

fn random_u64(r: &mut impl Rng) -> u64 {
    r.gen_range(std::u64::MIN..std::u64::MAX)
}

pub fn random_u64_few_bits(r: &mut impl Rng) -> u64 {
    random_u64(r) & random_u64(r) & random_u64(r)
}
