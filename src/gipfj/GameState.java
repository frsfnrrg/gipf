package gipfj;

/**
 * ?? Make an interface, to avoid nested-constructor overhead
 * 
 */
public class GameState {
    public final Board b;
    public final Reserves r;
    public final boolean gphase1; // positive
    public final boolean gphase2; // negative

    /**
     * A number from 0 to 83; 0-41 corresponds to a placed piece, 42-83 to a
     * placed GIPF potential
     */
    public final byte move;

    // these are deliberately not final
    public int[] plus_lines;
    public int[] minus_lines;

    public GameState(Board b, Reserves r, boolean gphase1, boolean gphase2,
            byte move) {
        this.b = b;
        this.r = r;
        this.gphase1 = gphase1;
        this.gphase2 = gphase2;
        this.move = move;
        plus_lines = null;
        minus_lines = null;
    }

    /**
     * Clojure use only.
     * 
     * @param b
     * @param r
     * @param gipfp
     * @param gipfm
     * @return
     */
    public static GameState makeGameState(Board b, Reserves r, Boolean gipfp,
            Boolean gipfm) {
        GameState g = new GameState(b, r, gipfp, gipfm, (byte) -1);
        // ensure that all GameStates in the system are well cared for.
        // so what is we duplicate effort at the beginning??
        GameCalc.primeListsOfLines(ThreadBuffer.DEFAULT, g, 0);
        return g;
    }

    public static Board getBoard(GameState g) {
        return g.b;
    }

    public static Reserves getReserves(GameState g) {
        return g.r;
    }

    public static boolean isGipfing(GameState g, long player) {
        if (player > 0) {
            return g.gphase1;
        } else {
            return g.gphase2;
        }
    }

    /**
     * Returns true iff the current player can place a GIPF piece.
     * 
     * @param player
     * @return
     */
    public boolean getPhase(int player) {
        if (player > 0) {
            return gphase1;
        } else {
            return gphase2;
        }
    }

    public GameState endGipf(int player) {
        if (player > 0) {
            return new GameState(b, r, false, gphase2, move);
        } else {
            return new GameState(b, r, gphase1, false, move);
        }
    }

    @Override
    public int hashCode() {
        // we deliberately ignore phasing
        return b.hashCode() ^ r.hashCode();
    }

    @Override
    public String toString() {
        return "{" + b.toString() + " " + r.toString() + "}";
    }

    public static long getMove(GameState g) {
        return g.move;
    }

    /**
     * 
     * @param board
     * @param rr
     * @return
     */
    public GameState change(Board board, Reserves rr, boolean gphase1,
            boolean gphase2, byte move) {
        return new GameState(board, rr, gphase1, gphase2, move);
    }

    public boolean losingGameState(int player) {
        if (player > 0) {
            return r.losingReserve(player, gphase1);
        } else {
            return r.losingReserve(player, gphase2);
        }
    }

    public static GameState changeBR(GameState in, Board nb, Reserves nr) {
        return new GameState(nb, nr, in.gphase1, in.gphase2, in.move);
    }

    // todo: make losingGameState =eqv= .r.losingReserve(p, .getPhase(p))
}
