package gipfj;

public class CompressedSGS implements Compressed {
    private final int hc;
    private final byte[] data;

    // overhead; 4 bytes !
    public CompressedSGS(GameState g, long player) {
        // our goal: to shrink this data as small as possible...,
        // while keeping all detail

        // data:
        // board: 37*5 bits : 185
        // player: 1 bit : 186
        // reserves: 5,5,4,5,5,4 : 28 bits; 214
        // 214/8 = 26.75 - 2 bits free!

        // but.. reserves: log2(18*18*9*18*18*9) = 23.019550008653873
        // 24 bits;
        // 186 + 24 = 210; 210 % 8 = 2 ;-(
        // 4 bytes hash

        // aside:
        // "optimal" board, for all possible states: log2(5^37) - 86 bits
        // so 110 -> 14 bytes; as byte[], 12 + 24 = 36 bytes, maybe 40 to pad it
        // mod 8

        // total 27+4+4(class overhead) + 12 (byte[] overhead)
        // 47 bytes. As pure byte[], we have 43 bytes... But it is trivial to
        // change over.

        int[] bd = g.b.data;
        data = new byte[27];
        for (int i = 0; i < Board.SIZE; i++) {
            int pos = i * 5 + bd[i] + 2;// range 0 -> 184
            // set pos to 1
            int seg = pos / 8;
            data[seg] ^= 1 << (pos - seg);
        }
        if (player > 0) {
            data[23] ^= 1 << 1; // bit at 185
        }
        // reserves: bits 186 -> 215

        // each of the fields in the reserve is given 5 bits, to fill things out
        Reserves r = g.r;
        int[] rq = { r.g1, r.g2, r.o1, r.o2, r.p1, r.p2 };
        for (int i = 0; i < 6; i++) {
            int start = 186 + i * 5;
            for (int k = 0; k < 5; k++) {
                boolean tp = (0 == (rq[i] & (1 << k)) >> k);
                int indx = start + k;
                int seg = indx / 8;
                if (tp) {
                    data[seg] ^= 1 << (indx - seg);
                }
            }
        }

        hc = g.b.hashCode ^ g.r.hashCode ^ (int) player;
    }

    public static CompressedSGS compress(GameState g, long player) {
        return new CompressedSGS(g, player);
    }

    public boolean equals(CompressedSGS other) {
        byte[] opp = other.data;
        for (int i = 0; i < data.length; i++) {
            if (opp[i] != data[i]) {
                return false;
            }
        }
        return true;
    }

    @Override
    public byte[] getData() {
        return data;
    }

    @Override
    public int hashCode() {
        return hc;
    }

}
