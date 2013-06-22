package gipfj;

/**
 * 
 * TODO: eventually, once we have stuff like MTD-f/killer/transposition tables
 * 
 * Add support for tournament style, by adding two "mode" fields to the
 * GameState, i.e, if said player can place Gipfs. We do not need that in
 * reserves or Board; though board *could* use a hash field
 * 
 * 
 * sorted out,
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
    public String toString() {
        return "{" + b.toString() + " " + r.toString() + "}";
    }
}
