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

    public GameState(Board b, Reserves r, boolean gphase1, boolean gphase2) {
        this.b = b;
        this.r = r;
        this.gphase1 = gphase1;
        this.gphase2 = gphase2;
    }

    public static GameState makeGameState(Board b, Reserves r, Boolean gipfp,
            Boolean gipfm) {
        return new GameState(b, r, gipfp, gipfm);
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

    public boolean getPhase(int player) {
        if (player > 0) {
            return gphase1;
        } else {
            return gphase2;
        }
    }

    public GameState endGipf(int player) {
        if (player > 0) {
            return new GameState(b, r, false, gphase2);
        } else {
            return new GameState(b, r, gphase1, false);
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

    /**
     * To be overridden by subclasses of GameState, so key data is not lost.
     * 
     * @param board
     * @param rr
     * @return
     */
    public GameState change(Board board, Reserves rr, boolean gphase1,
            boolean gphase2) {
        return new GameState(board, rr, gphase1, gphase2);
    }

    public boolean losingGameState(int player) {
        if (player > 0) {
            return r.losingReserve(player, gphase1);
        } else {
            return r.losingReserve(player, gphase2);
        }
    }

    public static GameState changeBR(GameState in, Board nb, Reserves nr) {
        return new GameState(nb, nr, in.gphase1, in.gphase2);
    }

    // todo: make losingGameState =eqv= .r.losingReserve(p, .getPhase(p))
}
