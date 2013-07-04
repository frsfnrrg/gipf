package gipfj;

/**
 * 
 * This is a simple history heuristic table.
 * 
 * Each move is assigned a number 0:42 (array style).
 */
public class HistoryTable {
    public static final Line[] listOfPushes = doubleAndMirror(GameCalc.listOfLines);
    public static final int[][] listOfPushPoints = doubleAndMirror(GameCalc.listOfLinePoints);
    public static final int MOVES = 42;

    /**
     * If this is increased, we may need to switch to longs.
     */
    public static final int MAX_DEPTH = 24;

    private static Line[] doubleAndMirror(Line[] lines) {
        Line[] r = new Line[lines.length * 2];
        for (int i = 0; i < lines.length; i++) {
            r[2 * i] = Line.sign(lines[i], 2 * i);
            Line rv = lines[i];
            int nend = Geometry.lend(Line.getStart(rv), Line.getDelta(rv));
            int antidelt = Geometry.pnegate(Line.getDelta(rv));
            r[2 * i + 1] = Line.makeSignedLine(2 * i + 1, nend, antidelt);
        }
        return r;
    }

    private static int[][] doubleAndMirror(int[][] lps) {
        int[][] r = new int[lps.length * 2][];
        for (int i = 0; i < lps.length; i++) {
            r[2 * i] = lps[i];

            r[2 * i + 1] = reverse(lps[i]);
        }
        return r;
    }

    private static int[] reverse(int[] is) {
        int[] cl = new int[is.length];
        for (int i = 0, j = is.length - 1; i < is.length; i++, j--) {
            cl[i] = is[j];
        }
        return cl;
    }

    private final Rk[] otable;
    private Rk best;

    /**
     * Only 42 moves exist: we can burn memory!!
     * 
     * @param depth
     */
    public HistoryTable(int depth) {
        otable = new Rk[MOVES];
        clear();
    }

    private class Rk {
        public int rank;
        public Rk next;
        public Rk prev;
        public int pos;

        // next is greater; prev is lesser.
        public Rk(int rank, int pos, Rk next, Rk prev) {
            this.rank = rank;
            this.next = next;
            this.prev = prev;
            this.pos = pos;
        }
    }

    /**
     * Returns an ordering for the moves at a depth based on the history
     * heuristic
     * 
     * @param depth
     * @return
     */
    public int[] getMoveOrdering() {
        int[] moves = OrderingPool.OPOOL.get();
        Rk c = best;
        for (int i = 0; i < MOVES; i++) {
            moves[i] = c.pos;
            c = c.prev;
        }

        return moves;
    }

    /**
     * A sufficient move is the best move ranked among the children of a node,
     * at end of evaluation or at time of cutoff.
     * 
     * Weighting is greater the less the depth is. 2 ** (k - u)
     * 
     * @param depth
     * @param move
     */
    public void addSufficientMove(int depth, int move) {
        int weight = 1 << depth;
        Rk o = otable[move];
        o.rank += weight;

        int r = o.rank;

        // already the best
        if (o.next == null) {
            return;
        }

        Rk c = o;
        if (c.next.rank < r) {
            boolean end = false;
            do {
                c = c.next;

                if (c.next == null) {
                    end = true;
                    break;
                }

            } while (c.next.rank < r);

            // seam up start
            if (o.prev == null) {
                o.next.prev = null;
            } else {
                o.prev.next = o.next;
                o.next.prev = o.prev;
            }

            // insert later on
            if (end) {
                o.next = null;
                o.prev = c;

                c.next = o;

                best = o;
            } else {
                Rk e = c.next;

                e.prev = o;
                o.next = e;

                o.prev = c;
                c.next = o;
            }
        }

    }

    public void clear() {
        for (int i = 0; i < MOVES; i++) {
            otable[i] = new Rk(0, i, null, null);
        }

        otable[1].prev = otable[0];
        for (int i = 1; i < MOVES - 1; i++) {
            otable[i - 1].next = otable[i];
            otable[i + 1].prev = otable[i];
        }
        otable[MOVES - 2].next = otable[MOVES - 1];

        best = otable[MOVES - 1];
    }

    /**
     * Do a postmortem analysis of the move.
     */
    public void analyze() {
        System.out
                .println("** History table analysis results: sorted by move.");
        int maxwidth = 2;
        for (Rk i : otable) {
            maxwidth = Math.max(maxwidth, (int) Math.ceil(Math.log10(i.rank)));
        }
        String fstring = String.format("%%%dd ", maxwidth);

        System.out.print("** ");
        for (int i = 0; i < MOVES; i++) {
            System.out.format(fstring, i);
        }
        System.out.println();

        System.out.print("** ");
        for (int i = 0; i < MOVES; i++) {
            System.out.format(fstring, otable[i].rank);
        }
        System.out.println();

        System.out
                .println("** History table analysis results: sorted by rank.");
        System.out.print("** ");
        Rk b = best;
        for (int i = 0; i < MOVES; i++) {
            System.out.format(fstring, b.pos);
            b = b.prev;
        }
        System.out.println();

        System.out.print("** ");
        Rk c = best;
        for (int i = 0; i < MOVES; i++) {
            System.out.format(fstring, c.rank);
            c = c.prev;
        }
        System.out.println();

    }

    public static void hclear(HistoryTable t) {
        t.clear();
    }

    public static HistoryTable hmake() {
        return new HistoryTable(MAX_DEPTH);
    }

    public static void hadd(HistoryTable t, long depth, long move) {
        t.addSufficientMove((int) depth, (int) move);
    }

    public static int[] hordering(HistoryTable t) {
        return t.getMoveOrdering();
    }

    public static void hanalyze(HistoryTable t) {
        t.analyze();
    }

    public static Line moveToLine(long move) {
        return listOfPushes[(int) move];
    }
}
