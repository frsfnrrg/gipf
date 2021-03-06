package gipfj;

/**
 * 
 */
public class Const {
    private Const() {
    }

    private static Line[] doubleAndMirror(Line[] lines) {
        Line[] r = new Line[lines.length * 2];
        for (int i = 0; i < lines.length; i++) {
            r[2 * i] = Line.sign(lines[i], 2 * i);
            Line rv = lines[i];
            int nend = Geometry.lend(Line.getStart(rv), Line.getDelta(rv));
            int antidelt = Geometry.pnegate(Line.getDelta(rv));
            r[2 * i + 1] = Line.makeSignedLine(2 * i + 1, nend, antidelt);
        }
        return r;
    }

    private static int[][] doubleAndMirror(int[][] lps) {
        int[][] r = new int[lps.length * 2][];
        for (int i = 0; i < lps.length; i++) {
            r[2 * i] = lps[i];

            r[2 * i + 1] = reverse(lps[i]);
        }
        return r;
    }

    private static int[] reverse(int[] is) {
        int[] cl = new int[is.length];
        for (int i = 0, j = is.length - 1; i < is.length; i++, j--) {
            cl[i] = is[j];
        }
        return cl;
    }

    public static int pushToLine(int push) {
        return push >> 1;
    }

    public static final Line[] listOfLines = getLoL();
    public static final int[][] listOfLinePoints = getLolP(listOfLines);
    public static final Line[] listOfPushes = doubleAndMirror(listOfLines);
    public static final int[][] listOfPushPoints = doubleAndMirror(listOfLinePoints);
    public static final int MOVES = 42;

    public static final int[][][] listOfSplitLinePoints = splitk(listOfLinePoints);

    private static Line[] getLoL() {
        Line[] lol = new Line[21];

        lol[0] = Line.makeSignedLine(0, 25, 1);
        lol[2] = Line.makeSignedLine(2, 26, 1);
        lol[3] = Line.makeSignedLine(3, 27, 1);
        lol[4] = Line.makeSignedLine(4, 28, 1);
        lol[5] = Line.makeSignedLine(5, 29, 1);
        lol[6] = Line.makeSignedLine(6, 30, 1);
        lol[7] = Line.makeSignedLine(7, 31, 1);
        lol[8] = Line.makeSignedLine(8, 22, 6);
        lol[9] = Line.makeSignedLine(9, 23, 6);
        lol[10] = Line.makeSignedLine(10, 24, 6);
        lol[11] = Line.makeSignedLine(11, 25, 6);
        lol[12] = Line.makeSignedLine(12, 26, 6);
        lol[13] = Line.makeSignedLine(13, 27, 6);
        lol[14] = Line.makeSignedLine(14, 28, 6);
        lol[15] = Line.makeSignedLine(15, 19, 5);
        lol[16] = Line.makeSignedLine(16, 20, 5);
        lol[17] = Line.makeSignedLine(17, 21, 5);
        lol[18] = Line.makeSignedLine(18, 22, 5);
        lol[19] = Line.makeSignedLine(19, 23, 5);
        lol[20] = Line.makeSignedLine(20, 24, 5);
        lol[1] = Line.makeSignedLine(1, 25, 5);

        return lol;
    }

    private static int[][][] splitk(int[][] in) {
        int[][][] r = new int[in.length][][];
        for (int i = 0; i < in.length; i++) {
            int[] f = in[i];
            int fl = f.length / 2;
            int fr = f.length - fl;
            int[][] b = new int[2][];
            b[0] = new int[fl];
            b[1] = new int[fr];
            for (int k = 0; k < fl; k++) {
                b[0][k] = f[fl - k - 1];
            }
            for (int k = fl; k < f.length; k++) {
                b[1][k - fl] = f[k];
            }

            r[i] = b;
        }

        return r;
    }

    private static int[][] getLolP(Line[] lol) {
        int[][] foo = new int[21][];
        // no lines longer than this...
        int[] buffer = new int[8];

        int x = 0;
        for (Line l : lol) {
            int s = Line.getStart(l);
            int d = Line.getDelta(l);

            int e = Geometry.lend(s, d);
            int length = Geometry.pdistance(e, s);

            for (int k = 0; k <= length; k++) {
                buffer[k] = s;

                s = Geometry.padd(s, d);
            }

            int[] fin = new int[length + 1];
            System.arraycopy(buffer, 0, fin, 0, length + 1);
            foo[x] = fin;

            x++;
        }

        return foo;
    }

    /**
     * Indexing scheme: [move][idx][dir] -> [arm]
     * 
     * dir=0,1 and dir=2,3 are opposite.
     */
    public static final Butterfly[][] butterflies = getButterFlyList();

    public static class Butterfly {
        public int[] xp;
        public int[] yp;
        public int[] ym;
        public int[] xm;
        public final int x;
        public final int y;
        public final int v;

        public Butterfly(int v, int x, int y, int[] xp, int[] yp, int[] xm,
                int[] ym) {
            this.xp = xp;
            this.yp = yp;
            this.xm = xm;
            this.ym = ym;
            this.x = x;
            this.y = y;
            this.v = v;
        }

        private void set(int idx, int[] val) {
            switch (idx) {
            case 0:
                this.xp = val;
            case 1:
                this.xm = val;
            case 2:
                this.yp = val;
            case 3:
                this.ym = val;
            }
        }
    }

    private static Butterfly[][] getButterFlyList() {
        Butterfly[][] bfl = new Butterfly[MOVES][];

        int[] armbuffer = new int[7];
        // in the last array, 0-1 are a p-m pair, as are 2-3

        for (int i = 0; i < MOVES; i++) {
            int[] pts = listOfPushPoints[i];
            int delta = Line.getDelta(listOfPushes[i]);

            // xdp, xdm oppose, as do ydp, ydm
            int xdp, xdm, ydp, ydm;
            xdm = Geometry.protm60(delta);
            ydm = Geometry.protm60(xdm);
            ydp = Geometry.protp60(delta);
            xdp = Geometry.protp60(ydp);
            int[] dlts = { xdp, xdm, ydp, ydm };

            Butterfly[] mptl = new Butterfly[pts.length];
            bfl[i] = mptl;
            for (int k = 0; k < pts.length; k++) {
                Butterfly juju = new Butterfly(pts[k], Line.lineToLint(Line
                        .makeLine(pts[k], dlts[0])), Line.lineToLint(Line
                        .makeLine(pts[k], dlts[2])), null, null, null, null);
                mptl[k] = juju;
                for (int j = 0; j < 4; j++) {
                    int d = dlts[j];
                    int nx = Geometry.padd(pts[k], d);
                    if (Geometry.pradius(nx) > 3) {
                        continue;
                    }
                    armbuffer[0] = nx;
                    int armlen = 1;
                    while (true) {
                        nx = Geometry.padd(nx, d);
                        if (Geometry.pradius(nx) > 3) {
                            break;
                        }

                        armbuffer[armlen] = nx;
                        armlen++;
                    }

                    int[] ne = new int[armlen];
                    System.arraycopy(armbuffer, 0, ne, 0, armlen);
                    juju.set(j, ne);
                }

            }
        }

        // System.out.println(Arrays.deepToString(bfl));

        return bfl;
    }
}
