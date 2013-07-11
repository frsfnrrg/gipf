package gipfj;

import java.util.Iterator;

/**
 * We can afford to use some memory on this - the real cost is in the items it
 * creates.
 * 
 */
public class MoveSignedIGC implements Iterator<GameState> {

    private final int[] order;
    private final Board[] g1 = new Board[3];
    private final Reserves[] g1r1 = new Reserves[3];
    private final Reserves[] g1r2 = new Reserves[3];
    private GameState[] g3;// actually MSGS
    private int plo;
    private int g3_pos;
    private int g3_end;
    private int g1_pos;
    private int g1_end;
    private byte player;
    private boolean ready;
    private int origHash;
    private boolean gipfs;
    private boolean oppg;
    private int glvl;
    private ThreadBuffer buf;

    public MoveSignedIGC(ThreadBuffer b, GameState g, long p, int[] ordering) {
        buf = b;
        this.player = (byte) p;
        order = ordering;

        g1_pos = 0;
        g1_end = 0;
        for (GameState q : GameCalc.primedLineRemoval(buf, g, player)) {
            if (!q.losingGameState(player)) {
                g1[g1_end] = q.b;
                g1r1[g1_end] = q.r.applyDelta(player, -1, 1, 0);
                if (player > 0) {
                    if (q.r.p1 >= 2) {
                        g1r2[g1_end] = q.r.applyDelta(player, -2, 0, 1);
                    }
                } else {
                    if (q.r.p2 >= 2) {
                        g1r2[g1_end] = q.r.applyDelta(player, -2, 0, 1);
                    }
                }
                g1_end++;
            }
        }

        if (player > 0) {
            gipfs = g.gphase1;
            oppg = g.gphase2;
        } else {
            gipfs = g.gphase2;
            oppg = g.gphase1;
        }

        if (g1_end == 0) {
            ready = false;
            return;
        }

        origHash = g.b.hashCode;

        plo = 0;
        glvl = 1;
        boostG3();
    }

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

    private void boostG3() {
        // works off the g1 buffer; writes two at a time into g2

        int[] pd = null;
        while (plo < 42) {
            pd = Const.listOfPushPoints[order[plo]];
            byte[] orig = g1[g1_pos].data;

            boolean select = false;

            if (glvl == 2 && g1r2[g1_pos] == null) {
                select = false;
            } else {
                for (int i = 0; i < pd.length; i++) {
                    if (orig[pd[i]] == 0) {
                        select = true;
                        break;
                    }
                }
            }

            if (select) {
                break;
            }

            glvl++;
            if (!gipfs || glvl == 3) {
                glvl = 1;
                g1_pos++;
                if (g1_pos == g1_end) {
                    g1_pos = 0;
                    plo++;
                }
            }
        }

        if (plo == 42) {
            ready = false;
            return;
        }

        // we want to ensure resource sharing among reserves
        byte[] source = g1[g1_pos].data;
        Reserves ruk;
        if (glvl == 1) {
            ruk = g1r1[g1_pos];
        } else {
            ruk = g1r2[g1_pos];
        }

        byte[] r = new byte[Board.SIZE];
        System.arraycopy(source, 0, r, 0, Board.SIZE);
        int hcr = origHash;

        byte last = player;
        for (int j = 0; j < pd.length; j++) {
            int ind = pd[j];
            byte v = r[ind];
            hcr ^= Board.hashArray[ind][v + 2] ^ Board.hashArray[ind][last + 2];
            r[ind] = last;
            if (v == 0) {
                break;
            }
            last = v;
        }

        GameState result;
        if (player > 0) {
            result = new GameState(new Board(r, hcr), ruk, glvl == 2, oppg,
                    (byte) order[plo]);
        } else {
            result = new GameState(new Board(r, hcr), ruk, oppg, glvl == 2,
                    (byte) order[plo]);
        }
        plo++;

        ready = true;
        g3_pos = 0;
        GameCalc.primeListsOfLines(buf, result, player);
        g3 = GameCalc.primedLineRemoval(buf, result, player);
        g3_end = g3.length;
    }

    /**
     * Won't do.
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

    // CLJR INTEROP

    private static final int[] DEFAULT_MOVE_ORDER = gdfmo();

    private static int[] gdfmo() {
        int[] q = new int[42];
        for (int i = 0; i < 42; i++) {
            q[i] = i;
        }
        return q;
    }

    /**
     * WARNING: not to be used by multiple threads - it accesses one buffer.
     * 
     * @param g
     * @param player
     * @return
     */
    public static MoveSignedIGC makeIncrementalGameCalc(ThreadBuffer b,
            GameState g, long player) {
        return new MoveSignedIGC(b, g, player, DEFAULT_MOVE_ORDER);
    }

    public static MoveSignedIGC makeMSIGC(ThreadBuffer b, GameState g,
            long player, int[] ordering) {
        return new MoveSignedIGC(b, g, player, ordering);
    }

    /**
     * Deprecated unless found useful
     * 
     * @param b
     * @param g
     * @param player
     * @return
     */
    public static MoveSignedIGC makeRandomMoveGenerator(ThreadBuffer b,
            GameState g, long player) {
        for (int i = 0; i < Const.MOVES; i++) {
            b.ordbuf[i] = i;
        }

        int[] ordering = b.OPOOL.get();
        for (int i = 0, cap = Const.MOVES; i < Const.MOVES; i++, cap--) {
            int ind = b.nextRandomInt(cap);
            ordering[i] = b.ordbuf[ind];
            b.ordbuf[cap - 1] = b.ordbuf[ind];
        }

        return new MoveSignedIGC(b, g, player, ordering);
    }

    /**
     * Recycles the resources from the MSIGC; specifically, the ordering int[]
     * array. Do NOT dispose an unordered move generator - it may ruin the
     * default move ordering. Then again, so what if that ordering changes every
     * now and then??
     * 
     * @param k
     */
    public static void dispose(MoveSignedIGC k) {
        k.buf.OPOOL.dispose(k.order);
    }

}
