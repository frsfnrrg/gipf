package gipfj;

public class Geometry {

    private static final int JUMP_1D = (int) MathUtil.hexNum(9);
    private static final int JUMP_2D = (int) MathUtil.hexNum(7);
    private static final int RAD_1 = (int) MathUtil.hexNum(2);

    // core point actions
    private static long[][] pmult;
    private static long[][] pdiv;
    private static long[][] padd;
    private static long[][] psub;
    private static long[] pneg;
    private static long[] prad;
    private static long[] pdiv4;
    private static long[] protp60;
    private static long[] protm60;
    // line actions
    private static long[][] lend;

    private Geometry() {
        // Nono call this...
    }

    // Optimized long-point section: all public

    public static void loadTables() {
        System.out.println("Loading geometry jump tables");
        long startTime = System.nanoTime();

        // first args do not need be this large...
        pmult = new long[JUMP_2D][JUMP_2D];
        pdiv = new long[JUMP_2D][JUMP_2D];

        padd = new long[JUMP_2D][JUMP_2D];
        psub = new long[JUMP_2D][JUMP_2D];

        lend = new long[JUMP_2D][RAD_1]; // second arg is a delta

        pneg = new long[JUMP_1D];
        prad = new long[JUMP_1D];
        pdiv4 = new long[JUMP_1D];
        protp60 = new long[JUMP_1D];
        protm60 = new long[JUMP_1D];

        int i, j, k;
        UVPoint pi, pj, pk;
        for (i = 0; i < JUMP_2D; i++) {
            pi = convertLongToUVPoint(i);
            for (k = 0; k < JUMP_2D; k++) {
                pk = convertLongToUVPoint(k);

                pmult[i][k] = convertUVPointToLong(mult(i, pk));
                if (i != 0) {
                    pdiv[i][k] = convertUVPointToLong(div(i, pk));
                }

                padd[i][k] = convertUVPointToLong(add(pi, pk));
                psub[i][k] = convertUVPointToLong(subtract(pi, pk));

            }
            for (j = 1; j < RAD_1; j++) {
                // WARNING: if j is 0, lineEnd never returns (infinite-loops)
                pj = convertLongToUVPoint(j);
                lend[i][j] = convertUVPointToLong(lineEnd(pi, pj));
            }
        }

        for (i = 0; i < JUMP_1D; i++) {
            pi = convertLongToUVPoint(i);
            pneg[i] = convertUVPointToLong(negate(pi));
            prad[i] = radius(pi);
            pdiv4[i] = convertUVPointToLong(div4(pi));
            protp60[i] = convertUVPointToLong(rotp60(pi));
            protm60[i] = convertUVPointToLong(rotm60(pi));
        }

        long endTime = System.nanoTime();

        System.out.format("Table loading took: %f msecs.\n",
                1e-9 * (endTime - startTime));
    }

    public static boolean equals(long a, long b) {
        return a == b;
    }

    public static long distance(long a, long b) {
        return radius(subtract(a, b));
    }

    public static long multiply(long a, long b) {
        return pmult[(int) a][(int) b];
    }

    public static long divide(long a, long b) {
        return pdiv[(int) a][(int) b];
    }

    public static long add(long a, long b) {
        return padd[(int) a][(int) b];
    }

    public static long subtract(long a, long b) {
        return psub[(int) a][(int) b];
    }

    public static long negate(long a) {
        return pneg[(int) a];
    }

    public static long radius(long a) {
        return prad[(int) a];
    }

    public static long divide4(long a) {
        return pdiv4[(int) a];
    }

    public static long lend(long start, long delta) {
        return lend[(int) start][(int) delta];
    }

    public static long rotm60(long i) {
        return protm60[(int) i];
    }

    public static long rotp60(long i) {
        return protp60[(int) i];
    }

    /**
     * Create a point.
     * 
     * @param u
     *            U-Coordinate
     * @param v
     *            V-Coordinate
     * @param w
     *            W-Coordinate
     * @return efficient representation
     */
    public static long makePt(long u, long v, long w) {
        return convertUVPointToLong(makeUVPoint(u, v, w));
    }

    public static String toString(long i) {
        UVPoint conv = convertLongToUVPoint(i);
        return "<" + String.valueOf(conv.u) + "," + String.valueOf(conv.v)
                + ">";
    }

    // how do you integrate this as a clojure record?
    static public class XYPoint {
        final public double x;
        final public double y;

        public XYPoint(double x, double y) {
            this.x = x;
            this.y = y;
        }

        public double getX() {
            return x;
        }

        public double getY() {
            return y;
        }
    }

    private static XYPoint xyu = new XYPoint(0, -1);
    private static XYPoint xyv = new XYPoint(Math.sqrt(3) / 2, -.5);

    @SuppressWarnings("unused")
    private static XYPoint xyw = new XYPoint(Math.sqrt(3) / 2, .5);

    public static XYPoint convertPtToXY(long in) {
        UVPoint i = convertLongToUVPoint(in);
        return add(multiply(i.u, xyu), multiply(i.v, xyv));
    }

    public static long convertXYToPt(XYPoint in) {
        // yeah. efficiency. this isn't called 2^28 times...
        long u = Math.round(in.x / 2 - in.y);
        long w = Math.round(in.x / (2 * Math.sqrt(3)));
        return convertUVPointToLong(makeUVPoint(u, 0, w));
    }

    public static XYPoint add(XYPoint a, XYPoint b) {
        return new XYPoint(a.x + b.x, a.y + b.y);
    }

    public static XYPoint subtract(XYPoint a, XYPoint b) {
        return new XYPoint(a.x - b.x, a.y - b.y);
    }

    public static XYPoint multiply(double factor, XYPoint b) {
        return new XYPoint(factor * b.x, factor * b.y);
    }

    public static XYPoint divide(double factor, XYPoint b) {
        return new XYPoint(b.x / factor, b.y / factor);
    }

    // UV Section; all private

    static private class UVPoint {
        final public long u;
        final public long v;

        public UVPoint(long u, long v) {
            this.u = u;
            this.v = v;
        }
    }

    private static UVPoint lineEnd(UVPoint start, UVPoint delta) {
        UVPoint q = start;
        int i = 0;
        // safety is a great thing!
        while (radius(q) < 4 && i < 50) {
            q = add(q, delta);
            i++;
        }

        if (i == 50) {
            System.out.println("lineEnd failed to terminate");
        }

        return q;
    }

    private static UVPoint makeUVPoint(long u, long v, long w) {
        return new UVPoint(u - w, v + w);
    }

    private static long convertUVPointToLong(UVPoint in) {
        long u = in.u;
        long v = in.v;

        if (u == 0 && u == v)
            return 0;

        else if (v == 0 && u > 0)
            return cvluvph(u, 0, 0);
        else if (u == -v && u > 0)
            return cvluvph(u, 1, 0);
        else if (u == 0 && v < 0)
            return cvluvph(-v, 2, 0);
        else if (v == 0 && u < 0)
            return cvluvph(-u, 3, 0);
        else if (u == -v && u < 0)
            return cvluvph(v, 4, 0);
        else if (u == 0 && v > 0)
            return cvluvph(v, 5, 0);

        else if (u > -v && -v > 0)
            return cvluvph(u, 0, -v);
        else if (-v > u && u > 0)
            return cvluvph(-v, 1, -v - u);
        else if (u < 0 && v < 0)
            return cvluvph(-u - v, 2, -u);
        else if (-u > v && v > 0)
            return cvluvph(-u, 3, v);
        else if (v > -u && v > 0)
            return cvluvph(v, 4, v + u);
        else if (u > 0 && v > 0)
            return cvluvph(u + v, 5, u);

        return -1; // failure
    }

    private static long cvluvph(long layer, long segment, long prog) {
        return MathUtil.hexNum(layer) + layer * segment + prog;
    }

    private static UVPoint convertLongToUVPoint(long in) {
        if (in == 0)
            return new UVPoint(0, 0);

        long layer = MathUtil.reverseHexFloor(in);
        long ring = in - MathUtil.hexNum(layer);
        long prog = ring % layer;
        int segment = (int) ((ring - prog) / layer);

        switch (segment) {
        case 0:
            return new UVPoint(layer, -prog);
        case 1:
            return new UVPoint(layer - prog, -layer);
        case 2:
            return new UVPoint(-prog, prog - layer);
        case 3:
            return new UVPoint(-layer, prog);
        case 4:
            return new UVPoint(prog - layer, layer);
        case 5:
            return new UVPoint(prog, layer - prog);
        }

        return null;
    }

    @SuppressWarnings("unused")
    private static long distance(UVPoint a, UVPoint b) {
        return radius(subtract(a, b));
    }

    private static long radius(UVPoint a) {
        // So, I heard you like case work...
        long u = a.u;
        long v = a.v;
        if (u == v && u == 0)
            return 0;

        else if (v == 0 && u > 0)
            return u;
        else if (u == -v && u > 0)
            return u;
        else if (u == 0 && v < 0)
            return -v;
        else if (v == 0 && u < 0)
            return -u;
        else if (u == -v && u < 0)
            return v;
        else if (u == 0 && v > 0)
            return v;

        else if (u > -v && -v > 0)
            return u;
        else if (-v > u && u > 0)
            return -v;
        else if (u < 0 && v < 0)
            return -u - v;
        else if (-u > v && v > 0)
            return -u;
        else if (v > -u && -u > 0)
            return v;
        else if (u > 0 && v > 0)
            return u + v;

        System.out.println("***Warning***: Radius func failed");
        // failure mode.
        return -1;
    }

    @SuppressWarnings("unused")
    private static boolean equals(UVPoint a, UVPoint b) {
        return (a.u == b.u) && (a.v == b.v);
    }

    private static UVPoint add(UVPoint a, UVPoint b) {
        return new UVPoint(a.u + b.u, a.v + b.v);
    }

    private static UVPoint subtract(UVPoint a, UVPoint b) {
        return new UVPoint(a.u + b.u, a.v + b.v);
    }

    private static UVPoint negate(UVPoint a) {
        return new UVPoint(-a.u, -a.v);
    }

    private static UVPoint div(long factor, UVPoint a) {
        return new UVPoint(a.u / factor, a.v / factor);
    }

    private static UVPoint mult(long factor, UVPoint a) {
        return new UVPoint(a.u * factor, a.v * factor);
    }

    private static UVPoint div4(UVPoint a) {
        return new UVPoint(a.u / 4, a.v / 4);
    }

    private static UVPoint rotp60(UVPoint a) {
        return makeUVPoint(a.v, 0, -a.u);
    }

    private static UVPoint rotm60(UVPoint a) {
        return makeUVPoint(0, a.u, a.v);
    }
}
