use rand::Rng;

// For testing purposes: Easily creates a bitboard with multiple positions set.
// Not very efficient.
#[macro_export]
macro_rules! bitboard {
    ( $( $x:expr ),* ) => {
        {
            let mut base = BitBoard::empty();
            $(
                base = base | BitBoard::singular($x);
            )*
            base
        }
    };
}

#[macro_export]
macro_rules! make_usize_wrapper {
    ($func_name:ident, $orig_f:expr) => {
        const fn $func_name(pos: usize) -> BitBoard {
            $orig_f(pos as u8)
        }
    };
}

fn random_u64(r: &mut impl Rng) -> u64 {
    r.gen_range(std::u64::MIN..std::u64::MAX)
}

pub fn random_u64_few_bits(r: &mut impl Rng) -> u64 {
    random_u64(r) & random_u64(r) & random_u64(r)
}
