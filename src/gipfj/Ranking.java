package gipfj;

/**
 * A repository of goodness; static ranking acceleration functions...
 * 
 * 
 * 
 */
public class Ranking {
    private static final int[][] gf1Central = GeneralizedPointWeighting
            .radiusWeights(3, 2, -2, -3, 5, 2, 1, 0);
    public static final Long NEG_INF = -1000000L;
    public static final int NEG_INF_I = -1000000;
    public static final Long POS_INF = 1000000L;
    public static final int POS_INF_I = 1000000;

    /**
     * Returns a number twixt 1000 and -1000;
     * 
     * good and bad must both be non-negative
     * 
     * @param good
     * @param bad
     * @return
     */
    public static long balanceNormalize(long plus_good, long plus_bad) {
        return POS_INF * (plus_good - plus_bad) / (plus_good + plus_bad);
    }

    public static long gf1Rank(GameState g, long player) {
        int plusp = 20;
        int minusp = 20;

        Reserves r = g.r;

        plusp += 20 * (18 - r.p1 - r.o1);
        minusp += 20 * (18 - r.p2 - r.o2);

        plusp += r.p1;
        minusp += r.p2;

        if (r.g1 == 1) {
            plusp -= 5;
        }
        if (r.g2 == 1) {
            minusp -= 5;
        }

        // rw is in favor of plusp
        long rw = GeneralizedPointWeighting.calcVal(g.b, 1, gf1Central);

        // gf1 algorithm used
        // plusp += 1/2 * rw
        // minusp -= 1/2 * rw
        //
        // with weights of 1.5 and 1 on gipf and normal;
        // I just algebraically simplified in the end.

        // ........... +=rw/2, m-= rw/2
        // (p - m) / (p + m) -----> (p - m + rw) / (p + m)

        if (player > 0) {
            return POS_INF * (plusp - minusp + rw) / (plusp + minusp);
        } else {
            return POS_INF * (minusp - plusp - rw) / (plusp + minusp);
        }
    }

    public static long linearScale(long val, long mini, long maxi, long mino,
            long maxo) {
        return (val - mini) / (maxi - mini) * (maxo - mino) + mino;
    }

    public static long reserveCubicDiff(Reserves r, long player, long gipfs,
            long pieces, long res) {
        if (player > 0) {
            return gipfs * (r.g1 * r.g1 * r.g1 - r.g2 * r.g2 * r.g2) + pieces
                    * (r.o1 * r.o1 * r.o1 - r.o2 * r.o2 * r.o2) + res
                    * (r.p1 * r.p1 * r.p1 - r.p2 * r.p2 * r.p2);
        } else {
            return gipfs * (r.g2 * r.g2 * r.g2 - r.g1 * r.g1 * r.g1) + pieces
                    * (r.o2 * r.o2 * r.o2 - r.o1 * r.o1 * r.o1) + res
                    * (r.p2 * r.p2 * r.p2 - r.p1 * r.p1 * r.p1);
        }
    }

    public static long reserveLinearDiff(Reserves r, long player, long gipfs,
            long pieces, long res) {
        if (player > 0) {
            return gipfs * (r.g1 - r.g2) + pieces * (r.o1 - r.o2) + res
                    * (r.p1 - r.p2);
        } else {
            return gipfs * (r.g2 - r.g1) + pieces * (r.o2 - r.o1) + res
                    * (r.p2 - r.p1);
        }
    }

    public static long reserveQuadDiff(Reserves r, long player, long gipfs,
            long pieces, long res) {
        if (player > 0) {
            return gipfs * (r.g1 * r.g1 - r.g2 * r.g2) + pieces
                    * (r.o1 * r.o1 - r.o2 * r.o2) + res
                    * (r.p1 * r.p1 - r.p2 * r.p2);
        } else {
            return gipfs * (r.g2 * r.g2 - r.g1 * r.g1) + pieces
                    * (r.o2 * r.o2 - r.o1 * r.o1) + res
                    * (r.p2 * r.p2 - r.p1 * r.p1);
        }
    }

    public static long weightedAdd(long va, long ca, long vb, long cb) {
        return va * ca + vb * cb;
    }

    public static long weightedAdd(long va, long ca, long vb, long cb, long vc,
            long cc) {
        return va * ca + vb * cb + vc * cc;
    }

    public static long weightedAdd(long va, long ca, long vb, long cb, long vc,
            long cc, long vd, long cd) {
        return va * ca + vb * cb + vc * cc + vd * cd;
    }
}
