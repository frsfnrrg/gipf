package gipfj;

//
// Optimization idea: as it stands, 30% of CPU
// is used by rankBoardOrg. By placing a single long field
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

//
// Additional note: if we ever want to use any degree of parallel
// mapping (threads on different CPUs?), then we will need to allocate
// a set of buffers for each thread...
//

public class Board {
    public static final int SIZE = (int) MathUtil.hexNum(4);
    public final long[] data;

    private Board() {
        data = new long[SIZE];
        for (int i = 0; i < SIZE; i++) {
            data[i] = 0;
        }
    }

    public Board(long[] nd) {
        data = nd;
    }

    public static Board makeBoard() {
        return new Board();
    }

    public static long get(Board b, long loc) {
        return b.data[(int) loc];
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

    public static Board change(Board b, long loc, long val) {
        // so what if we allocate a lot?
        long[] nd = new long[SIZE];
        System.arraycopy(b.data, 0, nd, 0, SIZE);
        nd[(int) loc] = val;
        return new Board(nd);
    }

    public static long countItem(Board b, long item) {
        long c = 0;
        int i;
        for (i = 0; i < SIZE; i++) {
            if (b.data[i] == item)
                c++;
        }
        return c;
    }
}
