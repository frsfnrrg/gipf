package gipfj;

// LONGTERM: create instances of this for passable
// use inside threads... That way allocation, buffers 
// still are minimized, and we do not have interference
// or synchonization issues.

public class GameCalc {

    // These are the weights ... that would, eventually, need
    // to be genetically tuned....

    private static int VC_M_PIECE = 15;
    private static int VC_M_GIPF = 40;
    private static int VC_O_PIECE = -10;
    private static int VC_O_GIPF = -50;
    private static int VC_RADIUS_FALLOFF = 3;

    public static void setValueCellConstants(int mp, int mg, int op, int og,
            int rf) {
        VC_M_PIECE = mp;
        VC_M_GIPF = mg;
        VC_O_PIECE = op;
        VC_O_GIPF = og;
        VC_RADIUS_FALLOFF = rf;
    }

    private static int[] radiusBoard = IMath.getHexFloorArray(Board.SIZE);

    public static int valueCell(int value, int player, int radius) {
        int q = player * value;
        if (q > 0) {
            if (q == 1) {
                return VC_M_PIECE * (VC_RADIUS_FALLOFF - radius);
            } else {
                return VC_M_GIPF * (VC_RADIUS_FALLOFF - radius);
            }
        } else if (q < 0) {
            if (q == -1) {
                return VC_O_PIECE * (VC_RADIUS_FALLOFF - radius);
            } else {
                return VC_O_GIPF * (VC_RADIUS_FALLOFF - radius);
            }
        } else {
            return 0;
        }
    }

    private static int VL_M_PIECE = -3;
    private static int VL_M_GIPF = 50;
    private static int VL_O_PIECE = 100;
    private static int VL_O_GIPF = 500;
    private static int VL_RADIUS_FALLOFF = 4;

    public static void setValueLineCellConstants(int mp, int mg, int op,
            int og, int rf) {
        VL_M_PIECE = mp;
        VL_M_GIPF = mg;
        VL_O_PIECE = op;
        VL_O_GIPF = og;
        VL_RADIUS_FALLOFF = rf;
    }

    public static int valueLineCell(int value, int player, int radius) {
        int q = player * value;
        if (q > 0) {
            if (q == 1) {
                return VL_M_PIECE * (VL_RADIUS_FALLOFF - radius);
            } else {
                // you will not take your own GIPF piece
                return VL_M_GIPF * (VL_RADIUS_FALLOFF - radius);
            }
        } else if (q < 0) {
            if (q == -1) {
                return VL_O_PIECE * (VL_RADIUS_FALLOFF - radius);
            } else {
                return VL_O_GIPF * (VL_RADIUS_FALLOFF - radius);
            }
        } else {
            return 0;
        }
    }

    // the thing that was 84.6% cpu...
    public static int rankBoardOrg(GameState g, int player) {
        int[] d = g.b.data;
        int r = 0;
        for (int i = 0; i < Board.SIZE; i++) {
            r += valueCell(d[i], player, radiusBoard[i]);
        }
        return r;
    }

    private static final int[] rbo1 = { 1, 2, 3, 4, 5, 6 };
    private static final int[] rbo2 = { 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
            17, 18 };
    private static final int[] rbo3 = { 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
            29, 30, 31, 32, 33, 34, 35, 36 };

    // might be a bit faster than rankBoardOrg1.
    public static int rankBoardOrg2(GameState g, int player) {
        int[] d = g.b.data;
        int r = valueCell(d[0], player, 0);
        for (int p : rbo3) {
            r += valueCell(d[p], player, 3);
        }
        for (int p : rbo2) {
            r += valueCell(d[p], player, 2);
        }
        for (int p : rbo1) {
            r += valueCell(d[p], player, 1);
        }
        return r;
    }

    private static final int[] hex3 = { 19, 22, 25, 28, 31, 34 };
    private static final int[] hex2 = { 7, 9, 11, 13, 15, 17 };
    private static final int[] hex1 = { 1, 2, 3, 4, 5, 6 };

    public static int rankBoardDiagonals(GameState g, int player) {
        int[] d = g.b.data;
        int r = valueDiagCell(d[0], player, 0);
        for (int p : hex3) {
            r += valueCell(d[p], player, 3);
        }
        for (int p : hex2) {
            r += valueCell(d[p], player, 2);
        }
        for (int p : hex1) {
            r += valueCell(d[p], player, 1);
        }
        return r;
    }

    // copy paste code is eeeevil..

    private static int VD_M_PIECE = -3;
    private static int VD_M_GIPF = 50;
    private static int VD_O_PIECE = 100;
    private static int VD_O_GIPF = 500;
    private static int VD_RADIUS_FALLOFF = 4;

    public static void setValueDiagCellConstants(int mp, int mg, int op,
            int og, int rf) {
        VD_M_PIECE = mp;
        VD_M_GIPF = mg;
        VD_O_PIECE = op;
        VD_O_GIPF = og;
        VD_RADIUS_FALLOFF = rf;
    }

    private static int valueDiagCell(int value, int player, int radius) {
        int q = player * value;
        if (q > 0) {
            if (q == 1) {
                return VD_M_PIECE * (VD_RADIUS_FALLOFF - radius);
            } else {
                // you will not take your own GIPF piece
                return VD_M_GIPF * (VD_RADIUS_FALLOFF - radius);
            }
        } else if (q < 0) {
            if (q == -1) {
                return VD_O_PIECE * (VD_RADIUS_FALLOFF - radius);
            } else {
                return VD_O_GIPF * (VD_RADIUS_FALLOFF - radius);
            }
        } else {
            return 0;
        }
    }

    public static int rankBoardCenter(GameState g, int player) {
        int[] d = g.b.data;
        int r = valueCell(0, player, 0);
        for (int p : rbo1) {
            r += valueCell(d[p], player, 1);
        }
        return r;
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

    public static final Line[] listOfLines = getLoL();
    public static final int[][] listOfLinePoints = getLolP(listOfLines);

    private static Line[] getLoL() {
        Line[] lol = new Line[21];

        // why I love macros/regexps!
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

    private static Line[] gblres = new Line[21];

    public static Line[] getBoardLines(Board b) {
        int found = 0;

        for (int xx = 0; xx < 21; xx++) {
            int[] l = listOfLinePoints[xx];
            int sign = 0;
            int count = 1;
            int len = l.length;
            for (int kk = 0; kk < len; kk++) {
                int v = b.data[l[kk]];
                if (v == 0 || (v * sign <= 0)) {
                    count = 1;
                    sign = v;
                } else {
                    count++;
                }

                if (count == 4) {
                    gblres[found] = Line.sign(listOfLines[xx], sign);
                    found++;
                    break;
                }
            }
        }

        Line[] foo = new Line[found];
        System.arraycopy(gblres, 0, foo, 0, found);

        return foo;
    }

    public static int rankBoardLines(GameState g, int player) {
        int r = 0;
        int[] d = g.b.data;

        for (int xx = 0; xx < 21; xx++) {
            int[] l = listOfLinePoints[xx];

            int sign = 0;
            int fix = 0;
            int count = 1;
            int len = l.length;
            for (int kk = 0; kk < len; kk++) {
                int v = d[l[kk]];
                if (v == 0 || (sign * v <= 0)) {
                    count = 1;
                    sign = v;
                } else {
                    count++;
                }

                if (count == 4) {
                    fix = sign;
                    break;
                }
            }

            // most will not pass this test
            if (fix != 0) {
                r += rankLine(g.b, l, fix);
            }
        }

        return r;
    }

    private static int rankLine(Board b, int[] l, int player) {
        int r = 0;

        for (int xx = 0; xx < l.length; xx++) {
            int loc = l[xx];
            r += valueLineCell(b.data[loc], player, radiusBoard[loc]);
        }

        return r;
    }

    public static int rankLine(Board b, Line l, int player) {
        int q = Line.getStart(l);
        int delta = Line.getDelta(l);
        int le = Geometry.lend(q, delta);

        int r = 0;

        while (q != le) {
            int i = q;
            r += valueLineCell(b.data[i], player, radiusBoard[i]);
            q = Geometry.padd(q, delta);

            if (q == le) {
                break;
            }
        }

        return r;
    }

    public static String toString(Board b) {
        return "<board ...>";
    }

    private static Line[] buff = new Line[21];

    public static Line[] filterLines(Line[] ls, int player) {
        int yy = 0;
        for (Line l : ls) {
            if (Line.getSig(l) == player) {
                buff[yy] = l;
                yy++;
            }
        }

        Line[] foo = new Line[yy];
        System.arraycopy(buff, 0, foo, 0, yy);

        return foo;
    }

    public static GameState[] getLineTakingResults(GameState g, int player) {
        // returns a sequence of stuff like this:
        // ( [[[prot] take1 take2 ...] ...)
        // but typically just:
        //
        // ( [[[prot] take 1 take 2..]]
        //
        // i.e., the cheapo take-first way

        GameState[] results = new GameState[1];

        GameState curr = g;

        Line[] found;
        while ((found = filterLines(getBoardLines(curr.b), player)).length > 0) {
            curr = simpleLineEmpty(curr, found[0], player);
        }

        results[0] = curr;

        return results;
    }

    private static int[] gicbuf = new int[7];

    public static int[] getImpactedCells(Board b, int loc, int delta) {
        int[] d = b.data;
        int m = 0;
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

    /*
     * Takes a well adjusted (start radius 3) line as `shove`
     */
    public static GameState placeAndShove(GameState foo, int player, Line shove) {
        int[] cdata = foo.b.data.clone();
        // yay! I can mutate!

        int q = Line.getStart(shove);
        int d = Line.getDelta(shove);

        int last = player;
        while (true) {
            int i = q;
            int v = cdata[i];

            cdata[i] = last;

            // should occur before radius == 4
            if (v == 0) {
                break;
            }

            last = v;

            q = Geometry.padd(q, d);
        }

        return new GameState(new Board(cdata), Reserves.decReserves(foo.r,
                player));
    }

    private static GameState simpleLineEmpty(GameState curr, Line found,
            int player) {
        // basically, over the line we do .... something.

        int[] cdata = curr.b.data.clone();
        Reserves rr = curr.r;

        // I could, theoretically, have a lookup table
        // of qq[hex4][hex1] -> int[up to 7].
        // Why? because there is a _lot_
        // of inefficiency with repeated padds & lends
        // best is one lookup on a large table, then
        // almost no calcs - 1/5th the cost

        int q = Line.getStart(found);
        int d = Line.getDelta(found);
        int le = Geometry.lend(q, d);

        while (true) {
            int i = q;
            int v = player * cdata[i];

            if (v < 0) {
                cdata[i] = 0;
                if (v == -2) {
                    rr = Reserves.decGipfs(rr, -player);
                }
            } else if (v == 1) {
                cdata[i] = 0;
                rr = Reserves.incReserves(rr, player);
            }

            if (q == le) {
                break;
            }

            q = Geometry.padd(q, d);
        }

        return new GameState(new Board(cdata), rr);
    }

    private final static Line[] lb = new Line[42];

    public static Line[] getOpenMoves(Board b) {
        // these are set 1 back; basically the list of lines
        // and its mirrors

        int i = 0;
        for (Line l : listOfLines) {
            int s = Line.getStart(l);
            int d = Line.getDelta(l);
            if (!lineFull(b, s, d)) {
                lb[i] = l;
                i++;
                lb[i] = Line.makeLine(Geometry.lend(s, d), Geometry.pnegate(d));
                i++;
            }
        }

        Line[] foo = new Line[i];
        System.arraycopy(lb, 0, foo, 0, i);

        return foo;
    }

    private static final GameState[] bgk = new GameState[42];

    private static GameState[] getMoveMakingResults(GameState gs, int player) {

        // FORTRAN IN ANY LANGUAGE!

        int mm = 0;

        // we iterate over LOL. forwards AND backwards

        int[] orig = gs.b.data;
        Reserves decced = Reserves.decReserves(gs.r, player);

        for (int[] n : listOfLinePoints) {
            // question: iterate twice, or allocate and discard?
            // answer: iterate 1 1/2 times!

            boolean skip = true;
            for (int i = 0; i < n.length; i++) {
                if (orig[n[i]] == 0) {
                    skip = false;
                    break;
                }
            }

            if (skip) {
                continue;
            }

            int[] up = orig.clone();
            int[] down = orig.clone();

            int last = player;
            for (int j = 0; j < n.length; j++) {
                int ind = n[j];
                int v = up[ind];
                up[ind] = last;
                if (v == 0) {
                    break;
                }
                last = v;
            }

            for (int j = n.length - 1; j >= 0; j--) {
                int ind = n[j];
                int v = down[ind];
                down[ind] = last;
                if (v == 0) {
                    break;
                }
                last = v;
            }

            bgk[mm] = new GameState(new Board(up), decced);
            mm++;
            bgk[mm] = new GameState(new Board(down), decced);
            mm++;
        }

        GameState[] foo = new GameState[mm];
        System.arraycopy(bgk, 0, foo, 0, mm);
        return foo;
    }

    // these limit must be increased once
    // we start branching on the taking of pieces.
    private static final GameState[] mpm1 = new GameState[3];
    private static final GameState[] mpm2 = new GameState[120];
    private static final GameState[] mpm3 = new GameState[360];

    public static GameState[] listPossibleBoards(GameState g, int player) {

        int i = 0, j = 0, k = 0;

        for (GameState q : getLineTakingResults(g, player)) {
            mpm1[i] = q;
            i++;
        }

        for (int x = 0; x < i; x++) {
            if (Reserves.losingReserve(mpm1[x].r, player)) {
                continue;
            }

            for (GameState q : getMoveMakingResults(mpm1[x], player)) {
                mpm2[j] = q;
                j++;
            }
        }

        // java style map. I want a macro for this.
        for (int y = 0; y < j; y++) {
            for (GameState q : getLineTakingResults(mpm2[y], player)) {
                mpm3[k] = q;
                k++;
            }
        }

        GameState[] foo = new GameState[k];
        System.arraycopy(mpm3, 0, foo, 0, k);

        return foo;
    }

}
