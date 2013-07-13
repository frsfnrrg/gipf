package gipfj;

import gipfj.Const.Butterfly;

/**
 * BIG FAT NOTE: technically, the line _removal_ algorithm is wrong; the pattern
 * of removal does not extend over a clear spot: XXXXO.O removes 4 X's and 1 O
 * 
 * 
 */
public class GameCalc {
    private static int CENTER = 0;
    private static int[] CENTER_ARM_1 = { 1, 7, 19 };
    private static int[] CENTER_ARM_2 = { 2, 9, 22 };
    private static int[] CENTER_ARM_3 = { 3, 11, 25 };
    private static int[] CENTER_ARM_4 = { 4, 13, 28 };
    private static int[] CENTER_ARM_5 = { 5, 15, 31 };
    private static int[] CENTER_ARM_6 = { 6, 17, 34 };
    private static int CENTER_MOD_1 = 4;
    private static int CENTER_MOD_2 = 18;
    private static int CENTER_MOD_3 = 11;
    private static int[] RING_A = { 1, 2, 3, 4, 5, 6 };
    private static int[][] RING_A_MINUS = { { 2, 10, 24 }, { 3, 12, 27 },
            { 4, 14, 30 }, { 5, 16, 33 }, { 6, 18, 36 }, { 1, 8, 21 } };
    private static int[][] RING_A_PLUS = { { 18, 25 }, { 8, 20 }, { 10, 23 },
            { 12, 26 }, { 14, 29 }, { 16, 32 } };
    private static int[] RING_A_REGISTRY = { 10, 3, 19, 12, 5, 17 };
    private static int[] RING_B = { 8, 10, 12, 14, 16, 18 };
    private static int[][] RING_B_MINUS = { { 9, 23 }, { 11, 26 }, { 13, 29 },
            { 15, 32 }, { 17, 35 }, { 7, 20 } };
    private static int[][] RING_B_PLUS = { { 7, 36 }, { 9, 21 }, { 11, 24 },
            { 13, 27 }, { 15, 30 }, { 17, 33 } };
    private static int[] RING_B_REGISTRY = { 9, 2, 20, 13, 6, 16 };
    private static int[][] RING_C_EXT = { { 22, 21, 20, 19 },
            { 25, 24, 23, 22 }, { 28, 27, 26, 25 }, { 31, 30, 29, 28 },
            { 34, 33, 32, 31 }, { 19, 36, 35, 34 } };
    private static int[] RING_C_REGISTRY = { 8, 0, 1, 14, 7, 15 };

    /**
     * Helper method - the compiler will inline this.
     * 
     * @param data
     * @return
     */
    private static int getArmLength(byte[] data, int[] arm, byte player) {
        int k = 0;
        int length = arm.length;
        // (int1 ^ int2) >> 31 == 0 is faster?
        while (k < length && data[arm[k]] * player > 0) {
            k++;
        }
        return k;
    }

    private static int getArmLengthN(byte[] data, int[] arm, byte player) {
        if (arm == null) {
            return 0;
        } else {
            return getArmLength(data, arm, player);
        }
    }

    /**
     * Not for multithreading.
     * 
     * @param b
     * @param player
     * @return
     */
    public static Line[] getBoardLines(Board b, long player) {
        int[] ls = getFilteredBoardLines(ThreadBuffer.DEFAULT, b, (int) player);
        if (ls == null) {
            return new Line[0];
        }

        Line[] foo = new Line[ls.length];
        for (int c = 0; c < ls.length; c++) {
            foo[c] = Line.sign(Const.listOfLines[ls[c]], (int) player);
        }
        return foo;
    }

    /**
     * Null return is to save allocation in the default state;
     * 
     * @param buf
     * @param b
     * @param player
     * @return
     */
    private static int[] getFilteredBoardLines(ThreadBuffer buf, Board b,
            int player) {
        byte pb = (byte) player;
        byte[] data = b.data;
        int m = 0;
        int[] lbuf = buf.linebuf;

        // plan of attack
        //
        // Look at key points - if they are not of the player, then
        // there can be no line through them.. If yes, check the neighbors...
        //

        //
        // Pick a point- follow an arm, count how far it goes;
        // follow the opposite arm - count how far it goes;
        // add, if it works, increment m & enjoy!
        //

        if (data[CENTER] * pb > 0) {
            int m1p = getArmLength(data, CENTER_ARM_1, pb);
            int m1m = getArmLength(data, CENTER_ARM_4, pb);
            if (m1p + m1m >= 3) {
                lbuf[m] = CENTER_MOD_1;
                m++;
            }

            int m2p = getArmLength(data, CENTER_ARM_2, pb);
            int m2m = getArmLength(data, CENTER_ARM_5, pb);
            if (m2p + m2m >= 3) {
                lbuf[m] = CENTER_MOD_2;
                m++;
            }

            int m3p = getArmLength(data, CENTER_ARM_3, pb);
            int m3m = getArmLength(data, CENTER_ARM_6, pb);
            if (m3p + m3m >= 3) {
                lbuf[m] = CENTER_MOD_3;
                m++;
            }
        }

        for (int u = 0; u < 6; u++) {
            if (data[RING_A[u]] * pb > 0) {
                int minus = getArmLength(data, RING_A_MINUS[u], pb);
                if (minus >= 1) {
                    int plus = getArmLength(data, RING_A_PLUS[u], pb);
                    if (plus + minus >= 3) {
                        lbuf[m] = RING_A_REGISTRY[u];
                        m++;
                    }
                }
            }

            if (data[RING_B[u]] * pb > 0) {
                int minus = getArmLength(data, RING_B_MINUS[u], pb);
                if (minus >= 1) {
                    int plus = getArmLength(data, RING_B_PLUS[u], pb);
                    if (plus + minus >= 3) {
                        lbuf[m] = RING_B_REGISTRY[u];
                        m++;
                    }
                }
            }

            if (getArmLength(data, RING_C_EXT[u], pb) == 4) {
                lbuf[m] = RING_C_REGISTRY[u];
                m++;
            }
        }

        if (m == 0) {
            return null;
        }

        int[] foo = new int[m];
        System.arraycopy(buf.linebuf, 0, foo, 0, m);
        return foo;
    }

    public static int[] getImpactedCells(Board b, int loc, int delta) {
        byte[] d = b.data;
        int m = 0;
        int[] gicbuf = ThreadBuffer.DEFAULT.gicbuf;
        while (true) {
            int ind = loc;
            gicbuf[m] = loc;
            m++;
            if (d[ind] == 0) {
                break;
            }

            loc = Geometry.padd(loc, delta);
        }

        int[] res = new int[m];
        System.arraycopy(gicbuf, 0, res, 0, m);
        return res;
    }

    /**
     * WARNING: does not take lines into account
     * 
     * @param buf
     * @param g
     * @param ppp
     * @return
     */
    public static GameState getRandomProgression(ThreadBuffer buf, GameState g,
            long ppp) {
        int player = (int) ppp;

        int[] ord = buf.ordbuf;

        for (int i = 0; i < Const.MOVES; i++) {
            ord[i] = i;
        }

        int cx = 0;

        boolean mxv = g.getPhase(player);

        GameState[] options = primedLineRemoval(buf, g, player);

        byte[] data;

        while (cx < Const.MOVES) {
            int rr = cx + buf.nextRandomInt(Const.MOVES - cx);
            int tmp = ord[cx];
            ord[cx] = ord[rr];
            ord[rr] = tmp;

            int[] pp = Const.listOfPushPoints[ord[cx]];

            for (GameState bob : options) {
                data = bob.b.data;

                boolean good = false;
                for (int i = 0; i < pp.length; i++) {
                    if (data[pp[i]] == 0) {
                        good = true;
                        break;
                    }
                }

                cx++;

                if (!good) {
                    continue;
                }

                // final stage

                boolean gp1 = bob.gphase1;
                boolean gp2 = bob.gphase2;
                byte last;
                Reserves fnl;
                // 2/3rds preference toward gipfs..
                int rem = bob.r.numReserves(player);
                if (mxv) {
                    if (buf.nextRandomInt(3) != 0 && rem >= 2) {
                        last = (byte) (player * 2);
                        fnl = bob.r.applyDelta(player, -2, 0, 1);
                    } else if (rem >= 1) {
                        if (player > 0) {
                            gp1 = false;
                        } else {
                            gp2 = false;
                        }
                        last = (byte) player;
                        fnl = bob.r.applyDelta(player, -1, 1, 0);
                    } else {
                        continue;
                    }
                } else if (rem >= 1) {
                    last = (byte) player;
                    fnl = bob.r.applyDelta(player, -1, 1, 0);
                } else {
                    continue;
                }

                byte[] r = new byte[Board.SIZE];
                System.arraycopy(data, 0, r, 0, Board.SIZE);
                int hcr = bob.b.hashCode;

                for (int j = 0; j < pp.length; j++) {
                    int ind = pp[j];
                    byte v = r[ind];
                    hcr ^= Board.hashArray[ind][v + 2]
                            ^ Board.hashArray[ind][last + 2];
                    r[ind] = last;
                    if (v == 0) {
                        break;
                    }
                    last = v;
                }

                GameState woah = new GameState(new Board(data, hcr), fnl, gp1,
                        gp2, (byte) ord[cx - 1]);
                primeListsOfLines(buf, woah, player);
                return primedLineRemoval(buf, woah, player)[0];
            }
        }
        return null;
    }

    /**
     * NOTE: noncompliant with the game rules (removes all items in line)
     * 
     * @param curr
     * @param found
     * @param player
     * @return
     */
    private static GameState lineEmpty(GameState curr, int found, int player) {
        byte[] cdata = new byte[Board.SIZE];
        System.arraycopy(curr.b.data, 0, cdata, 0, Board.SIZE);
        int hc = curr.b.hashCode;
        int[] res = curr.r.toArray();
        for (int q : Const.listOfLinePoints[found]) {
            int v = player * cdata[q];
            // System.out.format("v: %d p:%d d:%d d:%d q:%d %s\n", v, player,
            // cdata[q], d, q, rr.toString());

            // / System.out.format("%d %d, %d %d, %d %d\n", rr.p1, rr.p2, rr.o1,
            // rr.o2, rr.g1, rr.g2);

            if (v < 0) {
                cdata[q] = 0;
                hc ^= Board.hashArray[q][2] ^ Board.hashArray[q][v + 2];
                if (v == -2) {
                    Reserves.mutateArray(res, -player, 0, 0, -1);
                } else {
                    Reserves.mutateArray(res, -player, 0, -1, 0);
                }
            } else if (v == 1) {
                hc ^= Board.hashArray[q][2] ^ Board.hashArray[q][3];
                cdata[q] = 0;
                Reserves.mutateArray(res, player, 1, -1, 0);
            }
        }

        // .change preserves superclass metadata
        return curr.change(new Board(cdata, hc), new Reserves(res),
                curr.gphase1, curr.gphase2, curr.move);
    }

    public static boolean lineFull(Board b, int start, int delta) {
        int le = Geometry.lend(start, delta);

        while (start != le) {
            if (b.data[start] == 0) {
                return false;
            }
            start = Geometry.padd(start, delta);
        }
        if (b.data[start] == 0) {
            return false;
        }
        return true;
    }

    public static boolean lineFull(Board b, Line l) {
        int q = Line.getStart(l);
        int delta = Line.getDelta(l);
        return lineFull(b, q, delta);
    }

    /*
     * Takes a well adjusted (start radius 3) line as `shove`
     * 
     * THIS SHOULD ONLY BE CALLED EXTERNALLY. It sets the GameState gphase.
     */
    public static GameState placeAndShove(GameState foo, long pieceval,
            Line shove) {
        byte[] cdata = new byte[Board.SIZE];
        System.arraycopy(foo.b.data, 0, cdata, 0, Board.SIZE);
        // yay! I can mutate!

        int q = Line.getStart(shove);
        int d = Line.getDelta(shove);

        int hc = foo.b.hashCode;

        byte last = (byte) pieceval;
        while (true) {
            byte v = cdata[q];
            hc ^= Board.hashArray[q][v + 2] ^ Board.hashArray[q][last + 2];
            cdata[q] = last;

            // should occur before radius == 4
            if (v == 0) {
                break;
            }

            last = v;

            q = Geometry.padd(q, d);
        }

        if (pieceval > 0) {
            if (foo.gphase1 && pieceval == 2) {
                return foo.change(new Board(cdata, hc),
                        foo.r.applyDelta((int) pieceval, -2, 0, 1), true,
                        foo.gphase2, (byte) -1);
            } else {
                return foo.change(new Board(cdata, hc),
                        foo.r.applyDelta((int) pieceval, -1, 1, 0), false,
                        foo.gphase2, (byte) -1);
            }
        } else {
            if (foo.gphase2 && pieceval == -2) {
                return foo.change(new Board(cdata, hc),
                        foo.r.applyDelta((int) pieceval, -2, 0, 1),
                        foo.gphase1, true, (byte) -1);
            } else {
                return foo.change(new Board(cdata, hc),
                        foo.r.applyDelta((int) pieceval, -1, 1, 0),
                        foo.gphase1, false, (byte) -1);
            }
        }
    }

    /**
     * To be called only on already "primed" GameStates.
     * 
     * @param buf
     * @param g
     * @param player
     * @return
     */
    public static GameState[] primedLineRemoval(ThreadBuffer buf, GameState g,
            int player) {

        GameState[] ee = new GameState[1];
        GameState curr = g;

        int[] tried = buf.tried;
        int tco = 0;

        int[] ml, ol;
        if (player > 0) {
            ml = g.plus_lines;
            ol = g.minus_lines;
            boolean found = true;
            while (ml != null && found) {
                found = false;
                for (int chx : ml) {
                    boolean good = true;
                    for (int i = 0; i < tco; i++) {
                        if (chx == tried[i]) {
                            good = false;
                            break;
                        }

                    }

                    if (good) {
                        found = true;
                        curr = lineEmpty(curr, chx, player);
                        ml = srlol(buf, curr, ml, (byte) 1);
                        ol = srlol(buf, curr, ol, (byte) -1);
                        tried[tco] = chx;
                        tco++;
                        break;
                    }
                }
            }
            curr.plus_lines = ml;
            curr.minus_lines = ol;
        } else {
            ol = g.plus_lines;
            ml = g.minus_lines;
            boolean found = true;
            while (ml != null && found) {
                found = false;
                for (int chx : ml) {
                    boolean good = true;
                    for (int i = 0; i < tco; i++) {
                        if (chx == tried[i]) {
                            good = false;
                            break;
                        }
                    }

                    if (good) {
                        found = true;
                        curr = lineEmpty(curr, chx, player);
                        ml = srlol(buf, curr, ml, (byte) -1);
                        ol = srlol(buf, curr, ol, (byte) 1);
                        tried[tco] = chx;
                        tco++;
                        break;
                    }
                }
            }
            curr.plus_lines = ol;
            curr.minus_lines = ml;
        }

        ee[0] = curr;
        return ee;
    }

    /**
     * Creates a list of lines that result from a move.
     * 
     * This must be done after a move has been made and signed.
     * 
     * @param buf
     * @param g
     * @param player
     */
    public static void primeListsOfLines(ThreadBuffer buf, GameState g,
            int player) {
        //
        // Logically, this function is awesome. However, it seems to be overly
        // slow
        // to to array access whatever.
        //
        //
        // safety
        if (g.move == -1) {
            g.minus_lines = getFilteredBoardLines(buf, g.b, -1);
            g.plus_lines = getFilteredBoardLines(buf, g.b, 1);
            return;
        }

        // QUESTION: What about the 4-gipf case?? Do we never keep it?

        if (g.minus_lines != null || g.plus_lines != null) {
            System.out.println("RETAINED");
        }

        // buffers
        int[] pbuf = buf.pbuf;
        int pc = 0;
        int[] mbuf = buf.mbuf;
        int mc = 0;

        // do a change analysis:
        byte[] data = g.b.data;

        Butterfly[] wings = Const.butterflies[g.move];
        Butterfly curb = wings[0];
        Butterfly nxtb;
        byte nxt;
        byte cur = data[curb.v]; // which is either a GIPF or not.
        for (int i = 1; i < wings.length; i++) {
            nxtb = wings[i];
            nxt = data[nxtb.v];

            if (nxt * cur <= 0) {
                // do butterfly search - spread out.
                int xp = getArmLengthN(data, curb.xp, cur);
                int xm = getArmLengthN(data, curb.xm, cur);
                if (xp + xm >= 3) {
                    int line = curb.x;
                    // System.out.format("XLine: %d\n", line);
                    if (cur > 0) {
                        pbuf[pc] = line;
                        pc++;
                    } else {
                        mbuf[mc] = line;
                        mc++;
                    }
                }

                // what about the case where only one succeeds?
                int yp = getArmLengthN(data, curb.yp, cur);
                int ym = getArmLengthN(data, curb.ym, cur);
                if (yp + ym >= 3) {
                    int line = curb.y;
                    // System.out.format("YLine: %d\n", line);
                    if (cur > 0) {
                        pbuf[pc] = line;
                        pc++;
                    } else {
                        mbuf[mc] = line;
                        mc++;
                    }
                }
            }

            if (nxt == 0) {
                break;
            }

            curb = nxtb;
            cur = nxt;
        }

        byte pb = (byte) player;
        int selfrow = 1;
        // 1st one is already pb. Inefficient?
        for (Butterfly fu : wings) {
            selfrow++;
            if (data[fu.v] * pb <= 0) {
                break;
            }
        }

        if (selfrow >= 4) {
            // System.out.format("Selfer %d %d\n", player,g.move);
            if (player > 0) {
                pbuf[pc] = Const.pushToLine(g.move);
                pc++;
            } else {
                mbuf[mc] = Const.pushToLine(g.move);
                mc++;
            }
        }

        if (pc == 0) {
            g.plus_lines = null;
        } else {
            // System.out.format("PLUS %d %d\n", player, pc);
            g.plus_lines = new int[pc];
            System.arraycopy(pbuf, 0, g.plus_lines, 0, pc);
        }

        if (mc == 0) {
            g.minus_lines = null;
        } else {
            // System.out.format("MINUS %d %d\n", player, mc);
            g.minus_lines = new int[mc];
            System.arraycopy(mbuf, 0, g.minus_lines, 0, mc);
        }
    }

    private static int[] srlol(ThreadBuffer buf, GameState g, int[] lp, byte p) {
        if (lp == null) {
            return null;
        }

        int[] bf = buf.pbuf;
        int pc = 0;

        byte[] data = g.b.data;
        for (int q : lp) {

            int sign = p;
            int count = 1;
            int[] line = Const.listOfLinePoints[q];
            // count longest continuous set
            for (int kk = 0; kk < line.length; kk++) {
                int v = data[line[kk]];
                if (v == 0 || (v * sign <= 0)) {
                    count = 1;
                    sign = v;
                } else {
                    count++;
                }
            }

            // sign _must_ be p anyway
            if (count == 4 && sign * p > 0) {
                bf[pc] = q;
                pc++;
            }
        }

        if (pc == 0) {
            return null;
        }

        int[] rq = new int[pc];
        System.arraycopy(bf, 0, rq, 0, pc);
        return rq;
    }

    public static String toString(Board b) {
        return "<board ...>";
    }
}
