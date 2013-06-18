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

public class Board {
    private static final int SIZE = (int) MathUtil.hexNum(4);
    private final long[] data;

    private Board() {
        data = new long[SIZE];
        for (int i = 0; i < SIZE; i++) {
            data[i] = 0;
        }
    }

    private Board(long[] nd) {
        data = nd;
    }

    private long[] cloneData() {
        long[] r = new long[SIZE];
        System.arraycopy(data, 0, r, 0, SIZE);
        return r;
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

    private static long[] radiusBoard = MathUtil.getHexFloorArray(SIZE);

    // These are the weights ... that would, eventually, need
    // to be genetically tuned....

    public static long valueCell(long value, long player, long radius) {
        long q = player * value;
        if (q > 0) {
            if (q == 1) {
                return 15 * (3 - radius);
            } else {
                return 40 * (3 - radius);
            }
        } else if (q < 0) {
            if (q == -1) {
                return -10 * (3 - radius);
            } else {
                return -50 * (3 - radius);
            }
        } else {
            return 0;
        }
    }

    public static long valueLineCell(long value, long player, long radius) {
        long q = player * value;
        if (q > 0) {
            if (q == 1) {
                return -3 * (4 - radius);
            } else {
                // you will not take your own GIPF piece
                return 50 * (4 - radius);
            }
        } else if (q < 0) {
            if (q == -1) {
                return 100 * (4 - radius);
            } else {
                return 500 * (4 - radius);
            }
        } else {
            return 0;
        }
    }

    // the thing that was 84.6% cpu...
    public static long rankBoardOrg(Board b, long player) {
        long r = 0;
        for (int i = 0; i < SIZE; i++) {
            r += valueCell(b.data[i], player, radiusBoard[i]);
        }
        return r;
    }

    public static boolean lineFull(Board b, long start, long delta) {
        long le = Geometry.lend(start, delta);

        while (start != le) {
            if (b.data[(int) start] == 0) {
                return false;
            }
            start = Geometry.padd(start, delta);
        }
        if (b.data[(int) start] == 0) {
            return false;
        }
        return true;
    }

    public static boolean lineFull(Board b, Line l) {
        long q = Line.getStart(l);
        long delta = Line.getDelta(l);
        return lineFull(b, q, delta);
    }

    private static Line[] listOfLines = getLoL();
    private static int[][] listOfLinePoints = getLolP(listOfLines);

    private static Line[] getLoL() {
        Line[] lol = new Line[21];

        // why I love macros/regexps!
        lol[0] = Line.makeSignedLine(0, 25, 1);
        lol[2] = Line.makeSignedLine(2, 26, 1);
        lol[3] = Line.makeSignedLine(3, 27, 1);
        lol[4] = Line.makeSignedLine(4, 28, 1);
        lol[5] = Line.makeSignedLine(5, 29, 1);
        lol[6] = Line.makeSignedLine(6, 30, 1);
        lol[7] = Line.makeSignedLine(7, 31, 1);
        lol[8] = Line.makeSignedLine(8, 22, 6);
        lol[9] = Line.makeSignedLine(9, 23, 6);
        lol[10] = Line.makeSignedLine(10, 24, 6);
        lol[11] = Line.makeSignedLine(11, 25, 6);
        lol[12] = Line.makeSignedLine(12, 26, 6);
        lol[13] = Line.makeSignedLine(13, 27, 6);
        lol[14] = Line.makeSignedLine(14, 28, 6);
        lol[15] = Line.makeSignedLine(15, 19, 5);
        lol[16] = Line.makeSignedLine(16, 20, 5);
        lol[17] = Line.makeSignedLine(17, 21, 5);
        lol[18] = Line.makeSignedLine(18, 22, 5);
        lol[19] = Line.makeSignedLine(19, 23, 5);
        lol[20] = Line.makeSignedLine(20, 24, 5);
        lol[1] = Line.makeSignedLine(1, 25, 5);

        return lol;
    }

    private static int[][] getLolP(Line[] lol) {
        int[][] foo = new int[21][];
        // no lines longer than this...
        int[] buffer = new int[8];

        int x = 0;
        for (Line l : lol) {
            long s = Line.getStart(l);
            long d = Line.getDelta(l);

            long e = Geometry.lend(s, d);
            int length = (int) Geometry.pdistance(e, s);

            for (int k = 0; k <= length; k++) {
                buffer[k] = (int) s;

                s = Geometry.padd(s, d);
            }

            int[] fin = new int[length + 1];
            System.arraycopy(buffer, 0, fin, 0, length + 1);
            foo[x] = fin;

            x++;
        }

        return foo;
    }

    private static Line[] gblres = new Line[21];

    public static Line[] getBoardLines(Board b) {
        int found = 0;

        for (int xx = 0; xx < 21; xx++) {
            int[] l = listOfLinePoints[xx];
            long sign = 0;
            int count = 1;
            int len = l.length;
            for (int kk = 0; kk < len; kk++) {
                long v = b.data[l[kk]];
                if (v == 0 || (v * sign <= 0)) {
                    count = 1;
                    sign = v;
                } else {
                    count++;
                }

                if (count == 4) {
                    gblres[found] = Line.sign(listOfLines[xx], sign);
                    found++;
                    break;
                }
            }
        }

        Line[] foo = new Line[found];
        System.arraycopy(gblres, 0, foo, 0, found);

        return foo;
    }

    public static long rankBoardLines(Board b, long player) {
        long r = 0;

        for (int xx = 0; xx < 21; xx++) {
            int[] l = listOfLinePoints[xx];

            long sign = 0;
            long fix = 0;
            int count = 1;
            int len = l.length;
            for (int kk = 0; kk < len; kk++) {
                long v = b.data[l[kk]];
                if (v == 0 || (sign * v <= 0)) {
                    count = 1;
                    sign = v;
                } else {
                    count++;
                }

                if (count == 4) {
                    fix = sign;
                    break;
                }
            }

            // most will not pass this test
            if (fix != 0) {
                r += rankLine(b, l, fix);
            }
        }

        return r;
    }

    private static long rankLine(Board b, int[] l, long player) {
        long r = 0;

        for (int xx = 0; xx < l.length; xx++) {
            int loc = l[xx];
            r += valueLineCell(b.data[loc], player, radiusBoard[loc]);
        }

        return r;
    }

    public static long rankLine(Board b, Line l, long player) {
        long q = Line.getStart(l);
        long delta = Line.getDelta(l);
        long le = Geometry.lend(q, delta);

        long r = 0;

        while (q != le) {
            int i = (int) q;
            r += valueLineCell(b.data[i], player, radiusBoard[i]);
            q = Geometry.padd(q, delta);

            if (q == le) {
                break;
            }
        }

        return r;
    }

    public static String toString(Board b) {
        return "<board ...>";
    }

    private static Line[] buff = new Line[21];

    public static Line[] filterLines(Line[] ls, long player) {
        int yy = 0;
        for (Line l : ls) {
            if (Line.getSig(l) == player) {
                buff[yy] = l;
                yy++;
            }
        }

        Line[] foo = new Line[yy];
        System.arraycopy(buff, 0, foo, 0, yy);

        return foo;
    }

    public static GameState[] getLineTakingResults(GameState g, long player) {
        // returns a sequence of stuff like this:
        // ( [[[prot] take1 take2 ...] ...)
        // but typically just:
        //
        // ( [[[prot] take 1 take 2..]]
        //
        // i.e., the cheapo take-first way

        GameState[] results = new GameState[1];

        GameState curr = g;

        Line[] found;
        while ((found = filterLines(getBoardLines(curr.b), player)).length > 0) {
            curr = simpleLineEmpty(curr, found[0], player);
        }

        results[0] = curr;

        return results;
    }

    /*
     * Takes a well adjusted (start radius 3) line as `shove`
     */
    public static GameState placeAndShove(GameState foo, long player, Line shove) {
        long[] cdata = foo.b.cloneData();
        // yay! I can mutate!

        long q = Line.getStart(shove);
        long d = Line.getDelta(shove);

        long last = player;
        while (true) {
            int i = (int) q;
            long v = cdata[i];

            cdata[i] = last;

            // should occur before radius == 4
            if (v == 0) {
                break;
            }

            last = v;

            q = Geometry.padd(q, d);
        }

        return new GameState(new Board(cdata), Reserves.decReserves(foo.r,
                player));
    }

    private static GameState simpleLineEmpty(GameState curr, Line found,
            long player) {
        // basically, over the line we do .... something.

        long[] cdata = curr.b.cloneData();
        Reserves rr = curr.r;

        // I could, theoretically, have a lookup table
        // of qq[hex4][hex1] -> int[up to 7].
        // Why? because there is a _lot_
        // of inefficiency with repeated padds & lends
        // best is one lookup on a large table, then
        // almost no calcs - 1/5th the cost

        long q = Line.getStart(found);
        long d = Line.getDelta(found);
        long le = Geometry.lend(q, d);

        while (true) {
            int i = (int) q;
            long v = (int) (player * cdata[i]);

            if (v < 0) {
                cdata[i] = 0;
            } else if (v == 1) {
                cdata[i] = 0;
                rr = Reserves.incReserves(rr, player);
            }

            if (q == le) {
                break;
            }

            q = Geometry.padd(q, d);
        }

        return new GameState(new Board(cdata), rr);
    }

    private final static Line[] lb = new Line[42];

    public static Line[] getOpenMoves(Board b) {
        // these are set 1 back; basically the list of lines
        // and its mirrors

        int i = 0;
        for (Line l : listOfLines) {
            long s = Line.getStart(l);
            long d = Line.getDelta(l);
            if (!lineFull(b, s, d)) {
                lb[i] = l;
                i++;
                lb[i] = Line.makeLine(Geometry.lend(s, d), Geometry.pnegate(d));
                i++;
            }
        }

        Line[] foo = new Line[i];
        System.arraycopy(lb, 0, foo, 0, i);

        return foo;
    }

    private static final GameState[] bgk = new GameState[42];

    private static GameState[] getMoveMakingResults(GameState gs, long player) {

        // FORTRAN IN ANY LANGUAGE!

        int mm = 0;

        // we iterate over LOL. forwards AND backwards

        long[] orig = gs.b.data;
        Reserves decced = Reserves.decReserves(gs.r, player);

        for (int[] n : listOfLinePoints) {
            // question: iterate twice, or allocate and discard?
            // answer: iterate 1 1/2 times!

            boolean skip = true;
            for (int i = 0; i < n.length; i++) {
                if (orig[n[i]] == 0) {
                    skip = false;
                    break;
                }
            }

            if (skip) {
                continue;
            }

            long[] up = orig.clone();
            long[] down = orig.clone();

            long last = player;
            for (int j = 0; j < n.length; j++) {
                int ind = n[j];
                long v = up[ind];
                up[ind] = last;
                if (v == 0) {
                    break;
                }
                last = v;
            }

            for (int j = n.length - 1; j >= 0; j--) {
                int ind = n[j];
                long v = down[ind];
                down[ind] = last;
                if (v == 0) {
                    break;
                }
                last = v;
            }

            bgk[mm] = new GameState(new Board(up), decced);
            mm++;
            bgk[mm] = new GameState(new Board(down), decced);
            mm++;
        }

        GameState[] foo = new GameState[mm];
        System.arraycopy(bgk, 0, foo, 0, mm);
        return foo;
    }

    // these limit must be increased once
    // we start branching on the taking of pieces.
    private static final GameState[] mpm1 = new GameState[3];
    private static final GameState[] mpm2 = new GameState[120];
    private static final GameState[] mpm3 = new GameState[360];

    public static GameState[] listPossibleBoards(Board b, Reserves r,
            long player) {

        int i = 0, j = 0, k = 0;

        for (GameState q : getLineTakingResults(new GameState(b, r), player)) {
            mpm1[i] = q;
            i++;
        }

        for (int x = 0; x < i; x++) {
            for (GameState q : getMoveMakingResults(mpm1[x], player)) {
                mpm2[j] = q;
                j++;
            }
        }

        // java style map. I want a macro for this.
        for (int y = 0; y < j; y++) {
            for (GameState q : getLineTakingResults(mpm2[y], player)) {
                mpm3[k] = q;
                k++;
            }
        }

        GameState[] foo = new GameState[k];
        System.arraycopy(mpm3, 0, foo, 0, k);

        return foo;
    }
}
