package gipfj;

/*
 * Gipf count: how many are on the board
 * Piece count: how many are in reserve.
 * 
 * What about: piece-on-field count??.
 * not important
 *  
 */
public class Reserves {
    private final long p1;
    private final long p2;
    private final long o1;
    private final long o2;
    private final long g1;
    private final long g2;

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
    public Reserves(long p1, long p2, long o1, long o2, long g1, long g2) {
        this.p1 = p1;
        this.p2 = p2;
        this.g1 = g1;
        this.g2 = g2;
        this.o1 = o1;
        this.o2 = o2;
    }

    @Override
    public String toString() {
        return String.format("reserves %d %d : %d %d", p1, p2, g1, g2);
    }

    // Statics...

    public static Reserves makeReserves(long p, long g) {
        return new Reserves(p, p, 0, 0, g, g);
    }

    public static Reserves incReserves(Reserves r, long player) {
        if (player > 0) {
            return new Reserves(r.p1 + 1, r.p2, r.o1, r.o2, r.g1, r.g2);
        } else {
            return new Reserves(r.p1, r.p2 + 1, r.o1, r.o2, r.g1, r.g2);
        }
    }

    public static Reserves decReserves(Reserves r, long player) {
        if (player > 0) {
            return new Reserves(r.p1 - 1, r.p2, r.o1, r.o2, r.g1, r.g2);
        } else {
            return new Reserves(r.p1, r.p2 - 1, r.o1, r.o2, r.g1, r.g2);
        }
    }

    public static long getReserves(Reserves r, long player) {
        if (player > 0) {
            return r.p1;
        } else {
            return r.p2;
        }
    }

    public static Reserves decGipfs(Reserves r, long player) {
        if (player > 0) {
            return new Reserves(r.p1, r.p2, r.o1, r.o2, r.g1 - 1, r.g2);
        } else {
            return new Reserves(r.p1, r.p2, r.o1, r.o2, r.g1, r.g2 - 1);
        }
    }

    public static Reserves incGipfs(Reserves r, long player) {
        if (player > 0) {
            return new Reserves(r.p1, r.p2, r.o1, r.o2, r.g1 + 1, r.g2);
        } else {
            return new Reserves(r.p1, r.p2, r.o1, r.o2, r.g1, r.g2 + 1);
        }
    }

    public static long getGipfs(Reserves r, long player) {
        if (player > 0) {
            return r.g1;
        } else {
            return r.g2;
        }
    }

    public static boolean losingReserve(Reserves r, long player) {
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
}
