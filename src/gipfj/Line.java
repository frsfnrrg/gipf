package gipfj;

// what clojure interface controls rendering?
public class Line {
    private final long start;
    private final long delta;
    private final long sig;

    private Line(long sig, long start, long delta) {
        this.start = start;
        this.delta = delta;
        this.sig = sig;
    }

    @Override
    public String toString() {
        return String.format("%d %d", this.start, this.delta);
        // return String.format("<Line G: %d S: %s D: %s>", this.sig,
        // Geometry.toString(this.start), Geometry.toString(this.delta));
    }

    public static Line makeLine(long start, long delta) {
        return new Line(0, start, delta);
    }

    public static Line makeSignedLine(long sig, long start, long delta) {
        return new Line(sig, start, delta);
    }

    public static Line advanceLine(Line q) {
        return new Line(q.sig, Geometry.padd(q.start, q.delta), q.delta);
    }

    public static long getSig(Line q) {
        return q.sig;
    }

    public static long getStart(Line q) {
        return q.start;
    }

    public static long getDelta(Line q) {
        return q.delta;
    }

    public static boolean onLine(long loc, Line q) {
        long delta = Geometry.psubtract(loc, q.start);
        long dist = Geometry.pradius(delta);
        long approx = Geometry.pmultiply(dist, q.delta);
        return (delta == approx || delta == Geometry.pnegate(approx));
    }

    public static boolean equals(Line a, Line b) {
        // order is proportional to possibilities
        return (a.start == b.start && a.delta == b.delta && a.sig == b.sig);
    }

    public static Line sign(Line l, long sig) {
        return new Line(sig, l.start, l.delta);
    }
}
