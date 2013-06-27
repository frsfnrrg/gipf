package gipfj;

public class CompressedSGS implements Compressed {
    private final int hc;
    private byte[] data;
    private final boolean p;
    private final GameState g;

    public CompressedSGS(GameState g, long player) {
        this.p = (player > 0);
        this.g = g;

        data = null;

        hc = g.b.hashCode ^ g.r.hashCode ^ (int) player;
    }

    public static CompressedSGS compress(GameState g, long player) {
        return new CompressedSGS(g, player);
    }

    public boolean equals(CompressedSGS other) {
        byte[] opp = other.data;
        for (int i = 4; i < data.length; i++) {
            if (opp[i] != data[i]) {
                return false;
            }
        }
        return true;
    }

    @Override
    public byte[] getData() {
        if (data == null) {
            // Theoretically, one could get almost perfect compression
            // via 1:1. Perfect only recognizes possible states ;-)

            // FIRST 4 BYTES ARE RESERVED FOR TRANSP TABLE USE

            byte[] bd = g.b.data;
            data = new byte[4 + 27];
            for (int i = 0; i < Board.SIZE; i++) {
                int pos = i * 5 + bd[i] + 2;// range 0 -> 184
                int seg = pos / 8;
                data[4 + seg] ^= 1 << (pos - seg);
            }
            if (p) {
                data[4 + 23] ^= 1 << 1; // bit at 185
            }
            // reserves: bits 186 -> 215

            // each of the fields in the reserve is given 5 bits, to fill things
            // out
            Reserves r = g.r;
            int[] rq = { r.g1, r.g2, r.o1, r.o2, r.p1, r.p2 };
            for (int i = 0; i < 6; i++) {
                int start = 186 + i * 5;
                for (int k = 0; k < 5; k++) {
                    boolean tp = (0 == (rq[i] & (1 << k)) >> k);
                    int indx = start + k;
                    int seg = indx / 8;
                    if (tp) {
                        data[4 + seg] ^= 1 << (indx - seg);
                    }
                }
            }
        }
        return data;
    }

    @Override
    public int hashCode() {
        return hc;
    }

}
