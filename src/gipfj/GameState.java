package gipfj;

/**
 * 
 * TODO: eventually, once we have stuff like MTD-f/killer/transposition tables
 * 
 * Add support for tournament style, by adding two "mode" fields to the
 * GameState, i.e, if said player can place GIPFs. We do not need that in
 * reserves or Board; though board *could* use a hash field
 * 
 * sorted out,
 * 
 * TODO: maybe even short term:
 * 
 * I never see a GameState without the player who was responsible for it; It
 * would be much easier to wrap that in as well...
 * 
 * However, rank-board is a counter example - that player was not responsible.
 * Whatever. Less work is better than more work.
 * 
 * 
 * @author msto
 * 
 */
public class GameState {
    public final Board b;
    public final Reserves r;
    public final boolean gphase;

    public GameState(Board b, Reserves r, boolean gphase) {
        this.b = b;
        this.r = r;
        this.gphase = gphase;
    }

    public static GameState makeGameState(Board b, Reserves r, Boolean gipf) {
        return new GameState(b, r, gipf);
    }

    public static Board getBoard(GameState g) {
        return g.b;
    }

    public static Reserves getReserves(GameState g) {
        return g.r;
    }

    public static boolean isGipfing(GameState g) {
        return g.gphase;
    }

    @Override
    public int hashCode() {
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
    public GameState change(Board board, Reserves rr, boolean gphase) {
        return new GameState(board, rr, gphase);
    }
}
