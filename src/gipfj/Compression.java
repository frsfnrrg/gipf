package gipfj;

public class Compression {

    public static final int DEG = 3;
    public static final int EXP = 8 * DEG;
    public static final int THR = 1 << EXP;
    public static final int LEN = 5;
    public static final int[] RPROF = { 5, Reserves.MAX_GIPFS_ON_BOARD,
            Reserves.MAX_GIPFS_ON_BOARD, Reserves.MAX_CAPACITY,
            Reserves.MAX_CAPACITY, Reserves.MAX_CAPACITY };

    public static byte[] compress(Board b, Reserves r, long player) {
        byte[] data = new byte[14];
        // 105.76 bits; we can pack 75 more states in.
        byte[] bdata = b.data;
        int[] rdata = { r.g1, r.g2, r.o1, r.o2, r.p1, r.p2 };

        int A[] = new int[LEN]; // 120 ; 5 * 24
        if (player > 0) {
            A[0] = 1;
        } else {
            A[0] = 2;
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

        for (int k = 0; k < 6; k++) {
            A[0] *= RPROF[k];
            A[0] += rdata[k];

            for (int i = 0; i < LEN; i++) {
                if (A[i] > THR) {
                    A[i + 1] += A[i] >> EXP;
                    A[i] &= THR;
                } else {
                    break;
                }
            }
        }

        int k = 0;
        int j = 0;
        for (int i = 0; i < 14; i++) {
            data[i] = (byte) A[k];
            A[k] >>= 8;

            j++;
            if (j == 3) {
                j = 0;
                k++;
            }
        }

        return data;
    }

    public static class CGS {
        public byte[] d;
        public int hc;

        public CGS(byte[] d, int hc) {
            this.d = d;
            this.hc = hc;
        }

        @Override
        public int hashCode() {
            return hc;
        }
    }

    public static CGS compressgs(GameState g, long player) {
        return new CGS(compress(g.b, g.r, player), g.b.hashCode ^ g.r.hashCode
                ^ (int) player);
    }
}
