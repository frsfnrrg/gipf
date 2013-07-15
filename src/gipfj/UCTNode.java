package gipfj;

public class UCTNode {
    public static float UCT_CONST = 1.0f;

    public static void setUCTLevel(double f) {
        UCT_CONST = (float) f;
    }

    public final byte move;
    private int good_wins;
    private int visits;
    // another option is to use chaining, but it isn't worth it for large
    // numbers of children.
    private UCTNode[] children;
    private int kcount;

    public UCTNode(GameState g) {
        this.move = g.move;
        kcount = 0;
        visits = 0;
        good_wins = 0;
        children = null;
    }

    public void setFinalWonLost(int good, int winner) {
        visits = Integer.MAX_VALUE;
        if (good == winner) {
            good_wins = Integer.MAX_VALUE;
        } else {
            good_wins = 0;
        }
    }

    public boolean terminal() {
        return visits == Integer.MAX_VALUE;
    }

    public void addChild(UCTNode child) {
        if (children == null) {
            children = new UCTNode[21];
        } else if (children.length == kcount) {
            UCTNode[] t = children;
            children = new UCTNode[kcount * 2];
            System.arraycopy(t, 0, children, 0, kcount);
        }

        children[kcount] = child;
        kcount++;
    }

    private static float rankNode(int parentvisits, UCTNode child) {
        return (((float) child.good_wins / (float) child.visits) + UCT_CONST
                * (float) Math
                        .sqrt(Math.log(parentvisits) / (5 * child.visits)));
    }

    public void postWin(int good, int winner) {
        if (good == winner) {
            good_wins++;
        }
        visits++;
    }

    /**
     * assumes there are children; searches through everything
     * 
     * @return
     */
    public UCTNode selectNextChild() {
        UCTNode q = children[0];
        float best = rankNode(visits, q);
        for (int j = 1; j < kcount; j++) {
            UCTNode r = children[j];

            float nv;
            if (r.visits > 0) {
                nv = rankNode(visits, r);
            } else {
                nv = 10000.0f;
            }
            if (nv > best) {
                best = nv;
                q = r;
            }
        }

        return q;
    }

    public static UCTNode uctselect(UCTNode u) {
        return u.selectNextChild();
    }

    public static void uctpost(UCTNode u, long good, long win) {
        u.postWin((int) good, (int) win);
    }

    public static long uctmove(UCTNode u) {
        return u.move;
    }

    public static void uctterminate(UCTNode u, long good, long win) {
        u.setFinalWonLost((int) good, (int) win);
    }

    public static void uctgrow(UCTNode u, UCTNode n) {
        u.addChild(n);
    }

    public static long uctrank(UCTNode u, long good, long player) {
        float r;
        if (good == player) {
            r = (float) u.good_wins / u.visits;
        } else {
            r = 1.0f - (float) u.good_wins / u.visits;
        }

        long f = (long) (r * Ranking.POS_INF_I * 2 - Ranking.POS_INF_I);

        return f;
    }

    public static boolean uctfinal(UCTNode u) {
        return u.terminal();
    }

    public static boolean uctchilded(UCTNode u) {
        return u.kcount > 0;
    }

    public static boolean uctunvisited(UCTNode u) {
        return u.visits == 0;
    }
}
