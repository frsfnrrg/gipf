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

    public GameState(Board b, Reserves r) {
        this.b = b;
        this.r = r;
    }

    public static GameState makeGameState(Board b, Reserves r) {
        return new GameState(b, r);
    }

    public static Board getBoard(GameState g) {
        return g.b;
    }

    public static Reserves getReserves(GameState g) {
        return g.r;
    }

    @Override
    public int hashCode() {
        return b.hashCode() ^ r.hashCode();
    }

    @Override
    public String toString() {
        return "{" + b.toString() + " " + r.toString() + "}";
    }
}
