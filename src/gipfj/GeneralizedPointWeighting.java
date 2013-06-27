package gipfj;

/**
 * @author msto
 * 
 */
public class GeneralizedPointWeighting {
    private GeneralizedPointWeighting() {
    }

    //
    // The optimal? (is this really fastest?) weighted point-system:
    // User sends in two long[SIZE][5] with weights;
    // GCalc just sums up

    private static final int[] ar1 = { 1, 2, 3, 4, 5, 6 };
    private static final int[] ar2 = { 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
            18 };
    private static final int[] ar3 = { 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
            29, 30, 31, 32, 33, 34, 35, 36 };

    /**
     * radiusWeights
     * 
     * @param pg
     *            Good GIPF
     * @param pp
     *            Good piece
     * @param ng
     *            Evil piece
     * @param np
     *            Evil GIPF
     * @param r0
     *            Radius 0 Weight
     * @param r1
     *            Radius 1 Weight
     * @param r2
     *            Radius 2 Weight
     * @param r3
     *            Radius 3 Weight
     * @return Magic (weight array)
     */
    public static int[][] radiusWeights(int pg, int pp, int ng, int np, int r0,
            int r1, int r2, int r3) {
        int[][] rel = new int[Board.SIZE][5];
        int[] crossv = { ng, np, 0, pp, pg };

        for (int q = 0; q < 5; q++) {
            rel[0][q] = crossv[q] * r0;

            for (int p : ar1) {
                rel[p][q] = crossv[q] * r1;
            }
            for (int p : ar2) {
                rel[p][q] = crossv[q] * r2;
            }
            for (int p : ar3) {
                rel[p][q] = crossv[q] * r3;
            }
        }
        return rel;
    }

    private static final int[] ext = { 8, 10, 12, 14, 16, 18, 20, 21, 23, 24,
            26, 27, 29, 30, 32, 33, 35, 36 };
    private static final int[] diag = { 1, 2, 3, 4, 5, 6, 7, 9, 11, 13, 15, 17,
            19, 22, 25, 28, 31, 34 };

    /**
     * 
     * 
     * @param pg
     *            Good GIPF
     * @param pp
     *            Good piece
     * @param ng
     *            Evil piece
     * @param np
     *            Evil GIPF
     * @param co
     *            Center Weight *
     * @param cd
     *            Diagonal Weight
     * @param ce
     *            Non-diagonal Weight
     * @return Magic (weight array)
     */
    public static int[][] diagWeights(int pg, int pp, int ng, int np, int co,
            int cd, int ce) {
        int[][] rel = new int[Board.SIZE][5];
        int[] crossv = { ng, np, 0, pp, pg };

        for (int q = 0; q < 5; q++) {
            rel[0][q] = crossv[q] * co;

            for (int p : diag) {
                rel[p][q] = crossv[q] * cd;
            }
            for (int p : ext) {
                rel[p][q] = crossv[q] * ce;
            }
        }
        return rel;
    }

    private final static int[] astar2 = { 7, 9, 11, 13, 15, 17, };
    private final static int[] amid2 = { 8, 10, 12, 14, 16, 18 };
    private final static int[] astar3 = { 19, 22, 25, 28, 31, 34 };
    private final static int[] amid3 = { 20, 21, 23, 24, 26, 27, 29, 30, 32,
            33, 35, 36 };

    /**
     * 
     * Rank each position on the board with a weight.
     * 
     * Pass these into totalControl for use with calcVal;
     * 
     */
    public static int[] totalControlSubLevel(int center, int r1, int star2,
            int mid2, int star3, int mid3) {
        int[] r = new int[Board.SIZE];
        r[0] = center;
        for (int p : ar1) {
            r[p] = r1;
        }
        for (int p : astar2) {
            r[p] = star2;
        }
        for (int p : amid2) {
            r[p] = mid2;
        }
        for (int p : astar3) {
            r[p] = star3;
        }
        for (int p : amid3) {
            r[p] = mid3;
        }

        return r;
    }

    /**
     * 
     * Given four layers, as made from totalControlSubLevel, Generate a
     * comprehensive weighting array. This is for crazy freaks.
     * 
     * @param pg
     * @param pp
     * @param ng
     * @param np
     * @return
     */
    public static int[][] totalControl(int[] pg, int[] pp, int[] ng, int[] np) {
        int[] nulllayer = new int[Board.SIZE];
        for (int q = 0; q < Board.SIZE; q++) {
            nulllayer[q] = 0;
        }

        int[][] rr = { ng, np, nulllayer, pp, pg };
        return rr;
    }

    /**
     * Does a weighted linear merge of the two arrays. Does not modify
     * arguments.
     * 
     * @param alpha
     * @param at
     * @param beta
     * @param bt
     * @return
     */
    public static int[][] mergeWeights(int[][] alpha, int at, int[][] beta,
            int bt) {
        int[][] rel = new int[Board.SIZE][5];
        for (int i = 0; i < Board.SIZE; i++) {
            for (int j = 0; j < 5; j++) {
                rel[i][j] = at * alpha[i][j] + bt * beta[i][j];
            }
        }
        return rel;
    }

    // is this faster?
    /**
     * Given a weighted array, a player-perspective, and a Board, rank that
     * board. Just read the source, will you?
     * 
     * We pass in long for optimization
     * 
     * <pre>
     * public static long calcVal(Board b, long player, int[][] weights) {
     *      int r = 0;
     *      int[] d = b.data;
     *      for (int i = 0; i &lt; Board.SIZE; i++) {
     *          r += weights[i][d[i] * player + 2];
     *      }
     *      return r;
     * </pre>
     * 
     * @param b
     * @param player
     * @param weights
     * @return
     */
    public static long calcVal(Board b, long player, int[][] weights) {
        int r = 0;
        byte[] d = b.data;
        for (int i = 0; i < Board.SIZE; i++) {
            r += weights[i][d[i] * (int) player + 2];
        }
        return r;
    }
}
