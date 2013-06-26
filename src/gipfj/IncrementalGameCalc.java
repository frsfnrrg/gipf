package gipfj;

import java.util.Iterator;

public class IncrementalGameCalc implements Iterator<GameState> {

    protected final int[][] g1 = new int[3][];
    protected final GameState[] g2 = new GameState[10];
    private final GameState[] g3 = new GameState[3];
    protected int plo;
    private int g3_pos;
    private int g3_end;
    protected int g2_pos;
    protected int g1_pos;
    private int g1_end;
    protected int player;
    private boolean ready;
    protected Reserves deccedReserves;
    protected int origHash;

    public IncrementalGameCalc(GameState g, long p) {
        this.player = (int) p;
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
        g2_pos = 2;
        boostG3();
    }

    // this function does work beforehand
    @Override
    public GameState next() {

        GameState result = g3[g3_pos];

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

    @Override
    public void remove() {
        // don't bother
    }

    private void boostG3() {
        GameState f = getNextMoveResult();
        if (f == null) {
            ready = false;
        } else {
            ready = true;
            g3_pos = 0;
            g3_end = 0;
            for (GameState q : GameCalc.getLineTakingResults(f, player)) {
                g3[g3_end] = q;
                g3_end++;
            }
        }
    }

    // this function works by need;
    protected GameState getNextMoveResult() {
        // works off the g1 buffer; writes two at a time into g2
        if (g2_pos == 2) {
            g2_pos = 0;

            int[] orig = g1[g1_pos];
            // now, we analyze a line...

            // n will be set anyway
            int[] n = null;
            while (plo < 21) {
                n = GameCalc.listOfLinePoints[plo];

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

            // nothing more to deliver
            if (plo == 21) {
                // technically, we should loop around and
                // try from the next g1 data set.
                // But _that_ hinges on a comprehensive
                // list generator... Fail loud!
                return null;
            }
            // we don't want to use the same thing twice
            plo++;

            int[] up = new int[Board.SIZE];
            System.arraycopy(orig, 0, up, 0, Board.SIZE);
            int[] down = new int[Board.SIZE];
            System.arraycopy(orig, 0, down, 0, Board.SIZE);
            int hcu = origHash;
            int hcd = origHash;

            int last = player;
            for (int j = 0; j < n.length; j++) {
                int ind = n[j];
                int v = up[ind];
                hcu ^= Board.hashArray[ind][v + 2]
                        ^ Board.hashArray[ind][last + 2];
                up[ind] = last;
                if (v == 0) {
                    break;
                }
                last = v;
            }

            last = player;
            for (int j = n.length - 1; j >= 0; j--) {
                int ind = n[j];
                int v = down[ind];
                hcd ^= Board.hashArray[ind][v + 2]
                        ^ Board.hashArray[ind][last + 2];
                down[ind] = last;
                if (v == 0) {
                    break;
                }
                last = v;
            }

            g2[0] = new GameState(new Board(up, hcu), deccedReserves);
            g2[1] = new GameState(new Board(down, hcd), deccedReserves);
        }

        GameState result = g2[g2_pos];
        g2_pos++;
        return result;
    }
}
