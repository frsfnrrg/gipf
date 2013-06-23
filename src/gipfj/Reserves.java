package gipfj;

import java.util.Random;

/*
 * Gipf count: how many are on the board
 * Piece count: how many are in reserve.
 * 
 * What about: piece-on-field count??.
 * not important
 *  
 */
public class Reserves {
    public final int p1;
    public final int p2;
    public final int o1;
    public final int o2;
    public final int g1;
    public final int g2;
    public final int hashCode;

    public final static int MAX_CAPACITY = 18;

    /**
     * 
     * @param p1
     *            Pieces in reserve
     * @param p2
     * @param o1
     *            Pieces on board
     * @param o2
     * @param g1
     *            GIPF pieces on board
     * @param g2
     */
    public Reserves(int p1, int p2, int o1, int o2, int g1, int g2) {
        this.p1 = p1;
        this.p2 = p2;
        this.g1 = g1;
        this.g2 = g2;
        this.o1 = o1;
        this.o2 = o2;
        hashCode = calcHashCode(p1, p2, g1, g2, o1, o2);
    }

    public Reserves(int p1, int p2, int o1, int o2, int g1, int g2, int hc) {
        this.p1 = p1;
        this.p2 = p2;
        this.g1 = g1;
        this.g2 = g2;
        this.o1 = o1;
        this.o2 = o2;
        hashCode = hc;
    }

    private final static int[][] hashArray = makeHashArray();

    private int calcHashCode(int p1, int p2, int g1, int g2, int o1, int o2) {

        // debug on crash
        // System.out.format("%d, %d, %d, %d, %d, %d\n", p1, p2, o1, o2, g1,
        // g2);

        int r = hashArray[0][p1];
        r ^= hashArray[1][p2];
        r ^= hashArray[2][o1];
        r ^= hashArray[3][o2];
        r ^= hashArray[4][g1];
        r ^= hashArray[5][g2];

        return r;
    }

    private static int[][] makeHashArray() {
        int[][] a = new int[6][MAX_CAPACITY];
        Random rng = new Random(7987897987987897L);
        for (int z = 0; z < a.length; z++) {
            for (int q = 0; q < a[z].length; q++) {
                a[z][q] = rng.nextInt();
            }
        }
        return a;
    }

    @Override
    public String toString() {
        return String.format("reserves r %d %d : p %d %d : g %d %d", p1, p2,
                o1, o2, g1, g2);
    }

    // Statics...

    public static Reserves makeReserves(long p, long g) {
        return new Reserves((int) p, (int) p, 0, 0, (int) g, (int) g);
    }

    public static long getReserves(Reserves r, long player) {
        if (player > 0) {
            return r.p1;
        } else {
            return r.p2;
        }
    }

    public static long getPieces(Reserves r, long player) {
        if (player > 0) {
            return r.o1;
        } else {
            return r.o2;
        }
    }

    public static long getTotalPieces(Reserves r, long player) {
        if (player > 0) {
            return r.o1 + r.p1;
        } else {
            return r.o1 + r.p1;
        }
    }

    public static long getGipfs(Reserves r, long player) {
        if (player > 0) {
            return r.g1;
        } else {
            return r.g2;
        }
    }

    public static boolean losingReserve(Reserves r, int player) {
        if (player > 0) {
            return (r.p1 <= 0) || (r.g1 <= 0);
        } else {
            return (r.p2 <= 0) || (r.g2 <= 0);
        }
    }

    /**
     * Are these reserves equal?
     * 
     * @param a
     * @param b
     * @return
     */
    public static boolean equiv(Reserves a, Reserves b) {
        return (a == b)
                || ((a.p1 == b.p1) && (a.p2 == b.p2) && (a.g1 == b.g1)
                        && (a.g2 == a.g2) && (a.o1 == b.o1) && (a.o2 == a.o2));
    }

    /**
     * Were any pieces taken between these these two moves? (do they have the
     * same number of total pieces, and same number of GIPFs on field?)
     * 
     * @param a
     * @param b
     * @return
     */
    public static boolean wasTaken(Reserves a, Reserves b) {
        return (a.g1 == b.g1) && (a.g2 == b.g2)
                && ((a.o1 + a.p1) == (b.o1 + b.p1));
    }

    public static Reserves change(Reserves r, int player,
            int delta_reserve_pieces, int delta_board_pieces,
            int delta_board_gipfs) {
        return r.applyDelta(player, delta_reserve_pieces, delta_board_pieces,
                delta_board_gipfs);
    }

    /**
     * Change reserve amounts
     * 
     * @param in
     * @param player
     * @param del_reserve_pieces
     *            Change in pieces in reserve
     * @param del_board_pieces
     *            Change in pieces on board
     * @param del_board_gipfs
     *            Change in GIPF pieces on board
     * @return
     */
    public Reserves applyDelta(int player, int delta_reserve_pieces,
            int delta_board_pieces, int delta_board_gipfs) {
        if (player > 0) {
            return new Reserves(p1 + delta_reserve_pieces, p2, o1
                    + delta_board_pieces, o2, g1 + delta_board_gipfs, g2);
        } else {
            return new Reserves(p1, p2 + delta_reserve_pieces, o1, o2
                    + delta_board_pieces, g1, g2 + delta_board_gipfs);
        }
    }

    @Override
    public int hashCode() {
        return hashCode;
    }

    /**
     * Same as equiv
     * 
     * @param a
     * @param b
     * @return
     */
    public static boolean equalNHC(Reserves a, Reserves b) {
        return (a == b)
                || ((a.p1 == b.p1) && (a.p2 == b.p2) && (a.g1 == b.g1)
                        && (a.g2 == a.g2) && (a.o1 == b.o1) && (a.o2 == a.o2));
    }
}
