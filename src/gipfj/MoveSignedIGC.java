package gipfj;

import java.util.Iterator;

/**
 * Clone of IncrementalGameCalc; signs its moves, uses a move ordering.
 * 
 * @author msto
 * 
 */
public class MoveSignedIGC implements Iterator<MoveSignedGS> {

    private final int[] order;
    protected final int[][] g1 = new int[3][];
    protected final MoveSignedGS[] g2 = new MoveSignedGS[10];
    private final MoveSignedGS[] g3 = new MoveSignedGS[3];
    protected int plo;
    private int g3_pos;
    private int g3_end;
    protected int g1_pos;
    private int g1_end;
    protected int player;
    private boolean ready;
    protected Reserves deccedReserves;
    protected int origHash;

    public MoveSignedIGC(GameState g, long p, int[] ordering) {
        this.player = (int) p;
        order = ordering;

        g1_pos = 0;
        g1_end = 0;
        for (GameState q : GameCalc.getLineTakingResults(g, player)) {
            if (!Reserves.losingReserve(g.r, player)) {
                g1[g1_end] = q.b.data.clone();
                g1_end++;
            }
        }

        if (g1_end == 0) {
            ready = false;
            return;
        }

        origHash = g.b.hashCode;
        deccedReserves = g.r.applyDelta(player, -1, 1, 0);

        plo = 0;
        boostG3();
    }

    private void boostG3() {
        MoveSignedGS f = getNextMoveResult();
        if (f == null) {
            ready = false;
        } else {
            ready = true;
            g3_pos = 0;
            g3_end = 0;
            for (GameState q : GameCalc.getLineTakingResults(f, player)) {
                g3[g3_end] = (MoveSignedGS) q;
                g3_end++;
            }
        }
    }

    @Override
    public MoveSignedGS next() {

        MoveSignedGS result = g3[g3_pos];

        g3_pos++;

        if (g3_pos == g3_end) {
            boostG3();
        }

        return result;
    }

    @Override
    public boolean hasNext() {
        return ready;
    }

    protected MoveSignedGS getNextMoveResult() {
        // works off the g1 buffer; writes two at a time into g2

        int[] orig = g1[g1_pos];
        // now, we analyze a line...

        // n will be set anyway
        int[] n = null;
        while (plo < 42) {
            n = HistoryTable.listOfPushPoints[order[plo]];

            // System.out.format("%s %d %d\n", n.toString(), plo, order[plo]);

            boolean skip = true;
            for (int i = 0; i < n.length; i++) {
                if (orig[n[i]] == 0) {
                    skip = false;
                    break;
                }
            }

            if (skip) {
                plo++;
                continue;
            }

            break;
        }

        if (plo == 42) {
            return null;
        }

        int[] r = new int[Board.SIZE];
        System.arraycopy(orig, 0, r, 0, Board.SIZE);
        int hcr = origHash;

        int last = player;
        for (int j = 0; j < n.length; j++) {
            int ind = n[j];
            int v = r[ind];
            hcr ^= Board.hashArray[ind][v + 2] ^ Board.hashArray[ind][last + 2];
            r[ind] = last;
            if (v == 0) {
                break;
            }
            last = v;
        }

        MoveSignedGS result = new MoveSignedGS(new Board(r, hcr),
                deccedReserves, order[plo]);
        plo++;
        return result;
    }

    /**
     * Won't do.
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

}