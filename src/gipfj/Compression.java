package gipfj;

/**
 * 
 * Note: only the board values, reserve counts, and player need be stored.
 * 
 * This compression is currently the largest bottleneck;
 * 
 * Alternate decompositions:
 * 
 * <pre>
 * 
 * long: 5 ** 27 * 2; 5 ** 10 * 19 ** 2
 * 
 * </pre>
 */
public class Compression {
    // criterium benchmark (pure server VM):
    // Evaluation count : 481888080 in 60 samples of 8031468 calls.
    public static Ident compress(ThreadBuffer buf, GameState g, long player) {

        Reserves rrr = g.r;

        byte[] bdata = g.b.data;

        int x, y;
        int b = 0;

        for (int i = 0; i < 10; i++) {
            b *= 5;
            b += bdata[i] + 2;
        }
        b *= 19;
        b += rrr.p1;
        b *= 19;
        b += rrr.p2;

        if (player > 0) {
            x = 1;
        } else {
            x = 0;
        }
        for (int i = 10; i < 23; i++) {
            x *= 5;
            x += bdata[i] + 2;
        }
        y = 0;
        for (int i = 23; i < 36; i++) {
            y *= 5;
            y += bdata[i] + 2;
        }

        long a = ((long) x) * ((long) y) * 5 + bdata[36] + 2;

        // end phase
        return new Ident(a, b, g.b.hashCode ^ rrr.hashCode ^ (int) player);
    }
}
