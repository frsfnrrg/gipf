package gipfj;

public class Compression {

    public static final int DEG = 2;
    public static final int FLUSH_EXP = 8 * DEG;
    public static final int FLUSH_THR = 1 << FLUSH_EXP;// 16
    public static final int WORKING_EXP = FLUSH_EXP + 8;// 24 - 7 bits free
    public static final int WORKING_THR = 1 << WORKING_EXP;
    public static final int LEN = 7;

    public static final int[] RPROF = { 5, Reserves.MAX_GIPFS_ON_BOARD,
            Reserves.MAX_GIPFS_ON_BOARD, Reserves.MAX_CAPACITY,
            Reserves.MAX_CAPACITY, Reserves.MAX_CAPACITY };

    public static final int BYTES = 14; // DEG * LEN should be > BYTES

    public static Entry compressSlower(ThreadBuffer buf, GameState g,
            long player) {

        Reserves rrr = g.r;
        int[] A = buf.A;
        for (int i = 0; i < LEN; i++) {
            A[i] = 0;
        }

        byte[] temp = buf.data;
        byte[] bdata = g.b.data;
        int[] r = buf.r;
        r[0] = rrr.g1;
        r[1] = rrr.g2;
        r[2] = rrr.o1;
        r[3] = rrr.o2;
        r[4] = rrr.p1;
        r[5] = rrr.p2;

        // compression phase

        long q_a;
        if (player > 0) {
            q_a = 1;
        } else {
            q_a = 2;
        }
        for (int k = 0; k < 24; k++) {
            q_a *= 5;
            q_a += bdata[k];
        }

        long q_b = 0;
        for (int k = 25; k < Board.SIZE; k++) {
            q_b *= 5;
            q_b += bdata[k];
        }

        for (int k = 0; k < 6; k++) {
            q_b *= RPROF[k];
            q_b *= r[k];
        }

        // packing phase

        A[0] = (int) q_a;
        A[1] = (int) ((q_a - A[0]) / (1 << 32));
        A[2] = (int) q_b;
        A[3] = (int) ((q_b - A[2]) / (1 << 32));
        A[4] = 0;
        A[5] = 0;
        A[6] = 0;
        // flush - less tolerance
        for (int i = 0; i < LEN; i++) {
            if (A[i] > FLUSH_THR) {
                A[i + 1] += A[i] >>> FLUSH_EXP;
                A[i] &= FLUSH_EXP;
            } else {
                break;
            }
        }

        // transfer stage
        int k = 0;
        int j = 0;
        for (int i = 0; i < 14; i++) {
            temp[i] = (byte) A[k];
            A[k] >>= 8;

            j++;
            if (j == DEG) {
                j = 0;
                k++;
            }
        }
        Entry e = buf.EPOOL.getEntry(temp, g.b.hashCode ^ rrr.hashCode
                ^ (int) player);
        return e;
    }

    /**
     * Optimization target! - especially the overflow - get n ints; multiply
     * with overflow; reduce!
     * 
     * @param buf
     * @param g
     * @param player
     * @return
     */
    public static Entry compress(ThreadBuffer buf, GameState g, long player) {
        // 105.76 bits; we can pack 75 more states in.
        byte[] bdata = g.b.data;
        Reserves rrr = g.r;
        int[] A = buf.A;
        for (int i = 0; i < LEN; i++) {
            A[i] = 0;
        }
        byte[] temp = buf.data;
        int[] r = buf.r;
        r[0] = rrr.g1;
        r[1] = rrr.g2;
        r[2] = rrr.o1;
        r[3] = rrr.o2;
        r[4] = rrr.p1;
        r[5] = rrr.p2;

        if (player > 0) {
            A[0] = 1;
        } else {
            A[0] = 2;
        }
        for (int i = 1; i < LEN; i++) {
            A[i] = 0;
        }

        for (int v : bdata) {
            A[0] *= 5;
            A[0] += v;

            for (int i = 0; i < LEN; i++) {
                if (A[i] > WORKING_THR) {
                    A[i + 1] += A[i] >>> FLUSH_EXP;
                    A[i] &= FLUSH_EXP;
                } else {
                    break;
                }
            }
        }

        for (int k = 0; k < 6; k++) {
            A[0] *= RPROF[k];
            A[0] += r[k];

            for (int i = 0; i < LEN; i++) {
                if (A[i] > WORKING_THR) {
                    A[i + 1] += A[i] >>> FLUSH_EXP;
                    A[i] &= FLUSH_EXP;
                } else {
                    break;
                }
            }
        }

        // flush - less tolerance
        for (int i = 0; i < LEN; i++) {
            if (A[i] > FLUSH_THR) {
                A[i + 1] += A[i] >>> FLUSH_EXP;
                A[i] &= FLUSH_EXP;
            } else {
                break;
            }
        }

        // transfer stage
        int k = 0;
        int j = 0;
        for (int i = 0; i < 14; i++) {
            temp[i] = (byte) A[k];
            A[k] >>>= 8;

            j++;
            if (j == DEG) {
                j = 0;
                k++;
            }
        }
        Entry e = buf.EPOOL.getEntry(temp, g.b.hashCode ^ rrr.hashCode
                ^ (int) player);
        return e;
    }
}
