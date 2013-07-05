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

    public static final Line[] listOfLines = getLoL();
    public static final int[][] listOfLinePoints = getLolP(listOfLines);
    public static final Line[] listOfPushes = doubleAndMirror(listOfLines);
    public static final int[][] listOfPushPoints = doubleAndMirror(listOfLinePoints);
    public static final int MOVES = 42;

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

}
