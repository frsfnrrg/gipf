package gipfj;

/**
 * TODO: use: Line, & PUSH; both integral.
 * 
 * Hide everything, use lookups for line-end etc.
 * 
 * The current Line situation is ugly. - best to hide it all
 */
public class Line {
    public static Line advanceLine(Line q) {
        return new Line(q.sig, Geometry.padd(q.start, q.delta), q.delta);
    }

    public static boolean equals(Line a, Line b) {
        // order is proportional to possibilities
        return (a.start == b.start && a.delta == b.delta && a.sig == b.sig);
    }

    public static int getDelta(Line q) {
        return q.delta;
    }

    public static int getSig(Line q) {
        return q.sig;
    }

    public static int getStart(Line q) {
        return q.start;
    }

    public static Line makeLine(int start, int delta) {
        return new Line(0, start, delta);
    }

    public static Line makeSignedLine(int sig, int start, int delta) {
        return new Line(sig, start, delta);
    }

    public static boolean onLine(int loc, Line q) {
        int delta = Geometry.psubtract(loc, q.start);
        int dist = Geometry.pradius(delta);
        int approx = Geometry.pmultiply(dist, q.delta);
        return (delta == approx || delta == Geometry.pnegate(approx));
    }

    public static boolean same(Line a, Line b) {
        // order is proportional to possibilities
        return (a.start == b.start && a.delta == b.delta);
    }

    public static Line sign(Line l, int sig) {
        return new Line(sig, l.start, l.delta);
    }

    private final int delta;

    private final int sig;

    private final int start;

    private Line(int sig, int start, int delta) {
        this.start = start;
        this.delta = delta;
        this.sig = sig;
    }

    @Override
    public String toString() {
        return String.format("%d %d - %d", this.start, this.delta, this.sig);
        // return String.format("<Line G: %d S: %s D: %s>", this.sig,
        // Geometry.toString(this.start), Geometry.toString(this.delta));
    }

    public boolean equiv(Line line) {
        if (delta == line.delta) {
            return Line.onLine(line.start, this);
        } else if (Geometry.pnegate(delta) == line.delta) {
            return Line.onLine(line.start, this);
        } else {
            return false;
        }
    }

    /**
     * Warning: expensive.
     * 
     * Convert a line to its (faster, simpler) integral representation.
     * 
     * @param l
     * @return
     */
    public static int lineToLint(Line l) {
        for (int i = 0; i < Const.listOfLines.length; i++) {
            if (l.equiv(Const.listOfLines[i])) {
                return i;
            }
        }
        System.out.format("lnf %s\n", l.toString());
        return -9999;
    }

    public static Line lintToLine(int l) {
        return Const.listOfLines[l];
    }
}
