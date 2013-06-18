package gipfj;

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
}
