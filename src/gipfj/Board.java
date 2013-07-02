package gipfj;

import java.util.Random;

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

public class Board {
    public static final int SIZE = IMath.hexNum(4);
    public final byte[] data;
    public final int hashCode;

    private Board() {
        data = new byte[SIZE];
        for (int i = 0; i < SIZE; i++) {
            data[i] = 0;
        }
        hashCode = recalcHashCode();
    }

    public Board(byte[] nd, int hash) {
        data = nd;
        hashCode = hash;
    }

    private int recalcHashCode() {
        int hc = hashArray[0][data[0] + 2];
        for (int k = 1; k < SIZE; k++) {
            hc ^= hashArray[k][data[k] + 2];
        }
        return hc;
    }

    public static final int[][] hashArray = makeHashArray();

    private static int[][] makeHashArray() {
        Random rand = new Random(-6786871167745675445L);

        int[][] r = new int[SIZE][5];
        for (int z = 0; z < SIZE; z++) {
            r[z][0] = rand.nextInt();
            r[z][1] = rand.nextInt();
            r[z][2] = rand.nextInt();
            r[z][3] = rand.nextInt();
            r[z][4] = rand.nextInt();
        }

        return r;
    }

    @Override
    public int hashCode() {
        return hashCode;
    }

    /**
     * 
     * This is deliberately simple.
     * 
     */
    @Override
    public String toString() {
        return "[board " + Integer.toHexString(hashCode) + "]";
    }

    // CLOJURE ACCESS

    public static Board makeBoard() {
        return new Board();
    }

    public static int get(Board b, int loc) {
        return b.data[loc];
    }

    public static Board change(Board b, long location, long val) {
        int loc = (int) location;
        // so what if we allocate a lot?
        byte[] nd = new byte[SIZE];
        System.arraycopy(b.data, 0, nd, 0, SIZE);
        int nhash = b.hashCode ^ hashArray[loc][(int) (val + 2)]
                ^ hashArray[loc][nd[loc] + 2];
        nd[loc] = (byte) val;
        return new Board(nd, nhash);
    }

    public static int countItem(Board b, long item) {
        int c = 0;
        int i;
        for (i = 0; i < SIZE; i++) {
            if (b.data[i] == item)
                c++;
        }
        return c;
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

    /**
     * Don't use this often.
     * 
     * @return
     */
    public String verboseToString() {
        String q = "[board";

        for (byte b : data) {
            q += String.format(" %2d", b);
        }

        q += "]";
        return q;
    }
}
