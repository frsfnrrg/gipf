package gipfj;

public class Geometry {

    private static final int JUMP_1D = IMath.hexNum(10);
    private static final int JUMP_2D = IMath.hexNum(7);
    private static final int RAD_1 = IMath.hexNum(2);

    // core point actions
    private static int[][] pmult;
    private static int[][] pdiv;
    private static int[][] padd;
    private static int[][] psub;
    private static int[] pneg;
    private static int[] prad;
    private static int[] pdiv4;
    private static int[] protp60;
    private static int[] protm60;
    // line actions
    private static int[][] lend;

    private static final Geometry GEOOBJ = new Geometry();

    private Geometry() {
        loadTables();
        // Nono call this...
    }

    public static Geometry getMe() {
        // Why? I don't know...
        // if you really, really want a useless obj..
        return GEOOBJ;
    }

    // Optimized int-point section: all public

    private static void loadTables() {
        System.out.println("Loading geometry jump tables");
        long startTime = System.nanoTime();

        // first args do not need be this large...
        pmult = new int[JUMP_2D][JUMP_2D];
        pdiv = new int[JUMP_2D][JUMP_2D];

        padd = new int[JUMP_2D][JUMP_2D];
        psub = new int[JUMP_2D][JUMP_2D];

        lend = new int[JUMP_2D][RAD_1]; // second arg is a delta

        pneg = new int[JUMP_1D];
        prad = new int[JUMP_1D];
        pdiv4 = new int[JUMP_1D];
        protp60 = new int[JUMP_1D];
        protm60 = new int[JUMP_1D];

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
                1e-6 * (endTime - startTime));
    }

    public static boolean pequals(int a, int b) {
        return a == b;
    }

    public static int pdistance(int a, int b) {
        return pradius(psubtract(a, b));
    }

    public static int pmultiply(int a, int b) {
        return pmult[a][b];
    }

    public static int pdivide(int a, int b) {
        return pdiv[a][b];
    }

    public static int padd(int a, int b) {
        return padd[a][b];
    }

    public static int psubtract(int a, int b) {
        return psub[a][b];
    }

    public static int pnegate(int a) {
        return pneg[a];
    }

    public static int pradius(int a) {
        return prad[a];
    }

    public static int pdivide4(int a) {
        return pdiv4[a];
    }

    public static int lend(int start, int delta) {
        return lend[start][delta];
    }

    public static int protm60(int i) {
        return protm60[i];
    }

    public static int protp60(int i) {
        return protp60[i];
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
    public static int pmakePt(int u, int v, int w) {
        return convertUVPointToLong(makeUVPoint(u, v, w));
    }

    public static String toString(int i) {
        UVPoint conv = convertLongToUVPoint(i);
        return "<" + String.valueOf(conv.u) + "," + String.valueOf(conv.v)
                + ">";
    }

    // how do you integrate this as a clojure record?

    private static XYPoint xyu = new XYPoint(0, -1);
    private static XYPoint xyv = new XYPoint(Math.sqrt(3) / 2, -.5);

    @SuppressWarnings("unused")
    private static XYPoint xyw = new XYPoint(Math.sqrt(3) / 2, .5);

    public static XYPoint convertPtToXY(int in) {
        UVPoint i = convertLongToUVPoint(in);
        return add(multiply(i.u, xyu), multiply(i.v, xyv));
    }

    public static int convertXYToPt(XYPoint in) {
        // okay, this version retains some uglyness

        // alas, this version is kinda jumpy. Why?

        int u = (int) -Math.round(in.x / 2 + in.y);
        int v = (int) Math.round(in.x * 2 / Math.sqrt(3));

        return convertUVPointToLong(new UVPoint(u, v));
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
        final public int u;
        final public int v;

        public UVPoint(int u, int v) {
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

        return subtract(q, delta);
    }

    private static UVPoint makeUVPoint(int u, int v, int w) {
        return new UVPoint(u - w, v + w);
    }

    private static int convertUVPointToLong(UVPoint in) {
        int u = in.u;
        int v = in.v;

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
        else if (v > -u && -u > 0)
            return cvluvph(v, 4, v + u);
        else if (u > 0 && v > 0)
            return cvluvph(u + v, 5, u);

        return -1; // failure
    }

    private static int cvluvph(int layer, int segment, int prog) {
        return IMath.hexNum(layer) + layer * segment + prog;
    }

    private static UVPoint convertLongToUVPoint(int in) {
        if (in == 0)
            return new UVPoint(0, 0);

        int layer = IMath.reverseHexFloor(in);
        int ring = in - IMath.hexNum(layer);
        int prog = ring % layer;
        int segment = (ring - prog) / layer;

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
    private static int distance(UVPoint a, UVPoint b) {
        return radius(subtract(a, b));
    }

    private static int radius(UVPoint a) {
        // So, I heard you like case work...
        int u = a.u;
        int v = a.v;
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
        return new UVPoint(a.u - b.u, a.v - b.v);
    }

    private static UVPoint negate(UVPoint a) {
        return new UVPoint(-a.u, -a.v);
    }

    private static UVPoint div(int factor, UVPoint a) {
        return new UVPoint(a.u / factor, a.v / factor);
    }

    private static UVPoint mult(int factor, UVPoint a) {
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
