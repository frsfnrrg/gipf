package gipfj;

public class Board {
    private static final int SIZE = (int) MathUtil.hexNum(4);
    private final long[] data;

    private Board() {
        data = new long[SIZE];
        for (int i = 0; i < SIZE; i++) {
            data[i] = 0;
        }
    }

    private Board(long[] nd) {
        data = nd;
    }

    public static Board makeBoard() {
        return new Board();
    }

    public static long get(Board b, long loc) {
        return b.data[(int) loc];
    }

    public static boolean equals(Board a, Board b) {
        boolean eq = true;
        int i;
        for (i = 0; i < SIZE; i++) {
            if (a.data[i] != b.data[i]) {
                eq = false;
            }
        }

        return eq;
    }

    public static Board change(Board b, long loc, long val) {
        // so what if we allocate a lot?
        long[] nd = new long[SIZE];
        System.arraycopy(b.data, 0, nd, 0, SIZE);
        nd[(int) loc] = val;
        return new Board(nd);
    }

    public static long countItem(Board b, long item) {
        long c = 0;
        int i;
        for (i = 0; i < SIZE; i++) {
            if (b.data[i] == item)
                c++;
        }
        return c;
    }

    private static long[] radiusBoard = MathUtil.getHexFloorArray(SIZE);

    // These are the weights ... that would, eventually, need
    // to be genetically tuned....

    public static long valueCell(Board b, long player, long pos) {
        int i = (int) pos;
        long q = (player * b.data[i]);
        if (q > 0) {
            if (q == 1) {
                return 15 * (3 - radiusBoard[i]);
            } else {
                return 40 * (3 - radiusBoard[i]);
            }
        } else if (q < 0) {
            if (q == -1) {
                return -10 * (3 - radiusBoard[i]);
            } else {
                return -50 * (3 - radiusBoard[i]);
            }
        } else {
            return 0;
        }
    }

    public static long valueLineCell(Board b, long player, long pos) {
        int i = (int) pos;
        long q = (player * b.data[i]);
        if (q > 0) {
            if (q == 1) {
                return -3 * (4 - radiusBoard[i]);
            } else {
                return 0;
            }
        } else if (q < 0) {
            if (q == -1) {
                return 100 * (4 - radiusBoard[i]);
            } else {
                return 500 * (4 - radiusBoard[i]);
            }
        } else {
            return 0;
        }
    }

    // the thing that was 84.6% cpu...
    public static long rankBoardOrg(Board b, long player) {
        long r = 0;
        for (int i = 0; i < SIZE; i++) {
            r += valueCell(b, player, i);
        }
        return r;
    }

    public static boolean lineFull(Board b, long start, long delta) {
        long le = Geometry.lend(start, delta);

        while (start != le) {
            if (b.data[(int) start] == 0) {
                return false;
            }
            start = Geometry.padd(start, delta);
        }
        if (b.data[(int) start] == 0) {
            return false;
        }
        return true;
    }

    public static boolean lineFull(Board b, Line l) {
        long q = Line.getStart(l);
        long delta = Line.getDelta(l);
        return lineFull(b, q, delta);
    }

    private static Line[] listOfLines = getLoL();

    private static Line[] getLoL() {
        Line[] lol = new Line[21];

        // why I love macros/regexps!
        lol[0] = Line.makeLine(25, 1);
        lol[2] = Line.makeLine(26, 1);
        lol[3] = Line.makeLine(27, 1);
        lol[4] = Line.makeLine(28, 1);
        lol[5] = Line.makeLine(29, 1);
        lol[6] = Line.makeLine(30, 1);
        lol[7] = Line.makeLine(31, 1);
        lol[8] = Line.makeLine(22, 6);
        lol[9] = Line.makeLine(23, 6);
        lol[10] = Line.makeLine(24, 6);
        lol[11] = Line.makeLine(25, 6);
        lol[12] = Line.makeLine(26, 6);
        lol[13] = Line.makeLine(27, 6);
        lol[14] = Line.makeLine(28, 6);
        lol[15] = Line.makeLine(19, 5);
        lol[16] = Line.makeLine(20, 5);
        lol[17] = Line.makeLine(21, 5);
        lol[18] = Line.makeLine(22, 5);
        lol[19] = Line.makeLine(23, 5);
        lol[20] = Line.makeLine(24, 5);
        lol[1] = Line.makeLine(25, 5);

        return lol;
    }

    private static Line[] gblres = new Line[21];

    // ooh, how I love macrooooos
    public static Line[] getBoardLines(Board b) {
        int found = 0;

        for (Line l : listOfLines) {
            long s = Line.getStart(l);
            long d = Line.getDelta(l);
            long le = Geometry.lend(s, d);

            long sign = 0;
            int count = 1;
            while (s != le) {
                long v = b.data[(int) s];
                if (v == 0 || (v * sign <= 0)) {
                    count = 1;
                    sign = v;
                } else {
                    count++;
                }

                if (count == 4) {
                    gblres[found] = Line.sign(l, sign);
                    found++;
                    break;
                }

                s = Geometry.padd(s, d);
            }

            if (s == le) {
                long v = b.data[(int) s];
                if (!(v == 0 || (v * sign <= 0))) {
                    count++;
                }

                if (count == 4) {
                    gblres[found] = Line.sign(l, sign);
                    found++;
                }
            }

        }

        Line[] foo = new Line[found];
        System.arraycopy(gblres, 0, foo, 0, found);

        return foo;
    }

    public static long rankBoardLines(Board b, long player) {
        long r = 0;

        for (Line l : listOfLines) {
            long s = Line.getStart(l);
            long d = Line.getDelta(l);
            long le = Geometry.lend(s, d);

            long sign = 0;
            long fix = 0;
            int count = 1;
            while (s != le) {
                long v = b.data[(int) s];
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

                s = Geometry.padd(s, d);
            }

            if (s == le) {
                long v = b.data[(int) s];
                if (!(v == 0 || (v * sign <= 0))) {
                    count++;
                }

                if (count == 4) {
                    fix = sign;
                }
            }

            // most will not pass this test
            if (fix != 0) {
                r += rankLine(b, l, fix);
            }
        }

        return r;
    }

    public static long rankLine(Board b, Line l, long player) {
        long q = Line.getStart(l);
        long delta = Line.getDelta(l);
        long le = Geometry.lend(q, delta);

        long r = 0;

        while (q != le) {
            r += valueLineCell(b, player, q);
            q = Geometry.padd(q, delta);
        }
        r += valueLineCell(b, player, q);

        return r;
    }

    public static String toString(Board b) {
        return "<board ...>";
    }

}
