package gipfj;

public class Reserves {
    private final long p1;
    private final long p2;

    public Reserves(long p1, long p2) {
        this.p1 = p1;
        this.p2 = p2;
    }

    public static Reserves makeReserves(long p) {
        return new Reserves(p, p);
    }

    public static Reserves incReserves(Reserves r, long player) {
        if (player > 0) {
            return new Reserves(r.p1 + 1, r.p2);
        } else {
            return new Reserves(r.p1, r.p2 + 1);
        }
    }

    public static Reserves decReserves(Reserves r, long player) {
        if (player > 0) {
            return new Reserves(r.p1 - 1, r.p2);
        } else {
            return new Reserves(r.p1, r.p2 - 1);
        }
    }

    public static long getReserves(Reserves r, long player) {
        if (player > 0) {
            return r.p1;
        } else {
            return r.p2;
        }
    }
}
