package gipfj;

//
// Optimization idea: as it stands, 30% of CPU
// is used by rankBoardOrg. By placing a single Integer field
// onto a board, one could incrementally rank it:
// 
// Each board change leads to average 4 cell changes. 
// Branching number of 40 and depth of 3, pure minimax
// leads to 40^3 nodes, 40^3 * hex4 rankings, old style.
// 
// Incremental mode leads to 40^3 * 4 rankings, but there 
// is a loss of flexibility ? Well, 1/9th the number
// of rankings needed...
//
// 30% * 8/9 = 27% faster, 0.63x time needed.
//
// But, a caveat: rankings "could" later
// depend on the number of GIPF pieces on the board,
// as well as number of pieces in own/opp's reserve
// can't change
//
// Update: Outdated? The faster ranking is now ~1% CPU; 
// empty?/casts outweigh it
//
//

//
// Hashing of board positions: it is natural to do this incrementally.
// How?
//
// PieceXLoc -> component; less than SIZE things hashed; introduction/removal
//                         of pieces could be a problem??; 
// LocXType -> component; total SIZE things hashed together
//
//
// For a transposition table, the GameState hash is (board hash ^ reserve hash)
//

//
// Additional note: if we ever want to use any degree of parallel
// mapping (threads on different CPUs?), then we will need to allocate
// a set of buffers for each thread...
//

@SuppressWarnings("unused")
public class Board {
    public static final int SIZE = IMath.hexNum(4);
    public final int[] data;

    private Board() {
        data = new int[SIZE];
        for (int i = 0; i < SIZE; i++) {
            data[i] = 0;
        }
    }

    public Board(int[] nd) {
        data = nd;
    }

    public static Board makeBoard() {
        return new Board();
    }

    public static int get(Board b, int loc) {
        return b.data[loc];
    }

    public static boolean equals(Board a, Board b) {
        boolean eq = true;
        int i;
        for (i = 0; i < SIZE; i++) {
            if (a.data[i] != b.data[i]) {
                eq = false;
            }
        }

        return eq;
    }

    public static Board change(Board b, int loc, int val) {
        // so what if we allocate a lot?
        int[] nd = new int[SIZE];
        System.arraycopy(b.data, 0, nd, 0, SIZE);
        nd[loc] = val;
        return new Board(nd);
    }

    public static int countItem(Board b, int item) {
        int c = 0;
        int i;
        for (i = 0; i < SIZE; i++) {
            if (b.data[i] == item)
                c++;
        }
        return c;
    }

    private static final byte[][][] hashArray = makeHashArray();

    private static byte[][][] makeHashArray() {
        // to make hashing as efficient as possible,
        // what should this array of piece-pos/pos-piece
        // combos look like?

        byte[][][] r = new byte[SIZE][][];
        return r;
    }

    /**
     * 
     * This is deliberately simple.
     * 
     */
    @Override
    public String toString() {
        return "[board]";
    }
}
