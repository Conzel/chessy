use crate::boards::MailboxBoard;
use crate::game_state::GameState;
use crate::pieces::Piece;
use crate::utils::random_u64;
use lazy_static::lazy_static;
/// Table to store previously calculated board positions in.
/// Implemented using Zobrist hashing
/// Further reading:
///   - <https://en.wikipedia.org/wiki/Zobrist_hashing
///   - <https://en.wikipedia.org/wiki/Transposition_table>
use std::marker::PhantomData;

type ZobristHash = u64;
type ZobristPieceTable = [[ZobristHash; 12]; 64];
type ZobristCastlingTable = [ZobristHash; 16];

lazy_static! {
    static ref ZOBRIST_PIECE_TABLE: Box<ZobristPieceTable> = Box::new(zobrist_piece_table());
}

lazy_static! {
    static ref ZOBRIST_CASTLING_TABLE: Box<ZobristCastlingTable> =
        Box::new(zobrist_castling_table());
}

fn zobrist_piece_table() -> ZobristPieceTable {
    let r = &mut rand::thread_rng();
    let mut res: ZobristPieceTable = [[0; 12]; 64];
    for i in 0..12 {
        for j in 0..63 {
            res[i][j] = random_u64(r);
        }
    }
    res
}

fn zobrist_castling_table() -> ZobristCastlingTable {
    let r = &mut rand::thread_rng();
    let mut res: ZobristCastlingTable = [0; 16];
    for i in 0..15 {
        res[i] = random_u64(r);
    }
    res
}

pub trait ZobristHashable {
    fn zhash(&self) -> u64;
}

impl ZobristHashable for MailboxBoard {
    fn zhash(&self) -> u64 {
        let mut h = 0u64;
        for (pos, piece) in self.into_iter() {
            if piece != Piece::Empty {
                let bitstring: u64 = ZOBRIST_PIECE_TABLE[piece.as_index()][pos.as_index()];
                h = h ^ bitstring;
            }
        }
        h
    }
}

impl ZobristHashable for GameState {
    fn zhash(&self) -> u64 {
        self.mailbox().zhash() ^ ZOBRIST_CASTLING_TABLE[self.castling_bitflag() as usize]
    }
}

struct CacheInfo {
    full_hash: ZobristHash,
    prio: u8,
}

impl CacheInfo {
    fn new(full_hash: ZobristHash, prio: u8) -> CacheInfo {
        CacheInfo {
            full_hash: full_hash,
            prio: prio,
        }
    }
}

/// Struct for a Tranposition Table Cache.
/// N is the number of bits reserved for the Hash.
/// The table will take up
/// 2^B bits * (size of the the hashed item + cache info)
/// of memory.
/// Due to const generic expressions in Rust still being unstable, the user
/// themself has to ensure that, when instantiating this struct,
/// it holds that N = 2^B.
///
/// Also, ensure that the table does not crash memory.
/// Example sizes:
/// Caching a struct with size 32 bits + 32 bits of Info for given B:
///
/// 5:  (2^5 + 2^5) * 2^5  = 2^11  = 2kb  = 512 Byte
/// 10: (2^5 + 2^5) * 2^10 = 2^16  = 64kb = 8kB
/// 20: (2^5 + 2^5) * 2^10 = 2^26  = 64Mb = 8MB
///
/// etc.
///
/// As a side note, a transposition table underlies the assumption that collisions
/// in the full hash do not occur for performance reasons (as there is no way we can
/// check the full item for equality in a tight performance loop).
/// Collisions are very unlikely (as a 64 bit hash is used),
/// but prepare for the implications.
///
pub struct TranspositionCache<H: ZobristHashable, I, const N: usize, const B: u8> {
    cache: [Option<(I, CacheInfo)>; N],
    // Phantom data because the type of the hashed item is tightly associated
    // with the cache. We want to prevent different types of items being saved
    // alongside the cache.
    phantom: PhantomData<H>,
}

impl<H: ZobristHashable, I, const N: usize, const B: u8> TranspositionCache<H, I, N, B> {
    pub fn new() -> TranspositionCache<H, I, N, B> {
        TranspositionCache {
            cache: [Self::INIT; N],
            phantom: PhantomData,
        }
    }

    const hash_mask: u64 = 0xFFFFFFFFFFFFFFFF >> (64 - B);
    // Please don't ask me why we need this.
    // See:
    // <https://stackoverflow.com/questions/28656387/initialize-a-large-fixed-size-array-with-non-copy-types>
    const INIT: Option<(I, CacheInfo)> = None;

    fn trim_hash(h: ZobristHash) -> usize {
        (h & Self::hash_mask) as usize
    }

    /// Adds item to the transposition table. Items with
    /// higher priority will preferrably be kept in the table.
    pub fn add(&mut self, index: H, item: I, prio: u8) {
        let full_hash = index.zhash();
        let stored = &mut self.cache[Self::trim_hash(full_hash)];
        if let Some((_, info)) = stored {
            if info.prio > prio {
                // stored item is more valueable
                return;
            }
        }
        *stored = Some((item, CacheInfo::new(full_hash, prio)));
    }

    pub fn retrieve(&self, index: H) -> Option<&I> {
        let full_hash = index.zhash();
        let (i, c) = self.cache[Self::trim_hash(index.zhash())].as_ref()?;
        if full_hash != c.full_hash {
            None
        } else {
            Some(i)
        }
    }
}
