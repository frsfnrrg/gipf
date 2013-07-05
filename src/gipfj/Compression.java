package gipfj;

public class Compression {

    public static final int DEG = 3;
    public static final int EXP = 8 * DEG;
    public static final int THR = 1 << EXP;
    public static final int LEN = 5;

    public static final int[] RPROF = { 5, Reserves.MAX_GIPFS_ON_BOARD,
            Reserves.MAX_GIPFS_ON_BOARD, Reserves.MAX_CAPACITY,
            Reserves.MAX_CAPACITY, Reserves.MAX_CAPACITY };

    public static final int BYTES = 14;

    public static Entry compress(ThreadBuffer buf, GameState g, long player) {
        // 105.76 bits; we can pack 75 more states in.
        byte[] bdata = g.b.data;
        Reserves r = g.r;
        int[] A = buf.A;
        byte[] temp = buf.data;

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
                if (A[i] > THR) {
                    A[i + 1] += A[i] >> EXP;
                    A[i] &= THR;
                } else {
                    break;
                }
            }
        }
        // hmm, maybe I should have used a static buffer??

        // This code is manually loop unrolled, to avoid the int allocation.
        // @formatter:off
        //  static int[] RPROF = { 5, Reserves.MAX_GIPFS_ON_BOARD,
        //     Reserves.MAX_GIPFS_ON_BOARD, Reserves.MAX_CAPACITY,
        //     Reserves.MAX_CAPACITY, Reserves.MAX_CAPACITY };
        //  int[] rdata = { r.g1, r.g2, r.o1, r.o2, r.p1, r.p2 };
        //
        //  for (int k = 0; k < 6; k++) {
        //      A[0] *= RPROF[k];
        //      A[0] += rdata[k];
        //
        //      for (int i = 0; i < LEN; i++) {
        //          if (A[i] > THR) {
        //              A[i + 1] += A[i] >> EXP;
        //              A[i] &= THR;
        //          } else {
        //              break;
        //          }
        //      }
        //  }
        // @formatter:on

        // MANUAL LOOP UNROLL START

        A[0] *= 5;
        A[0] += r.g1;
        for (int i = 0; i < LEN; i++) {
            if (A[i] > THR) {
                A[i + 1] += A[i] >> EXP;
                A[i] &= THR;
            } else {
                break;
            }
        }
        A[0] *= Reserves.MAX_GIPFS_ON_BOARD;
        A[0] += r.g2;
        for (int i = 0; i < LEN; i++) {
            if (A[i] > THR) {
                A[i + 1] += A[i] >> EXP;
                A[i] &= THR;
            } else {
                break;
            }
        }
        A[0] *= Reserves.MAX_GIPFS_ON_BOARD;
        A[0] += r.o1;
        for (int i = 0; i < LEN; i++) {
            if (A[i] > THR) {
                A[i + 1] += A[i] >> EXP;
                A[i] &= THR;
            } else {
                break;
            }
        }
        A[0] *= Reserves.MAX_CAPACITY;
        A[0] += r.o2;
        for (int i = 0; i < LEN; i++) {
            if (A[i] > THR) {
                A[i + 1] += A[i] >> EXP;
                A[i] &= THR;
            } else {
                break;
            }
        }
        A[0] *= Reserves.MAX_CAPACITY;
        A[0] += r.p1;
        for (int i = 0; i < LEN; i++) {
            if (A[i] > THR) {
                A[i + 1] += A[i] >> EXP;
                A[i] &= THR;
            } else {
                break;
            }
        }
        A[0] *= Reserves.MAX_CAPACITY;
        A[0] += r.p2;
        for (int i = 0; i < LEN; i++) {
            if (A[i] > THR) {
                A[i + 1] += A[i] >> EXP;
                A[i] &= THR;
            } else {
                break;
            }
        }

        // MANUAL LOOP UNROLL OVER

        int k = 0;
        int j = 0;
        for (int i = 0; i < 14; i++) {
            temp[i] = (byte) A[k];
            A[k] >>= 8;

            j++;
            if (j == 3) {
                j = 0;
                k++;
            }
        }
        Entry e = buf.EPOOL.getEntry(temp, g.b.hashCode ^ r.hashCode
                ^ (int) player);
        // Entry e = new Entry(data, g.b.hashCode ^ r.hashCode ^ (int) player);

        return e;
    }
}
