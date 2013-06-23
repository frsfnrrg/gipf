package gipfj;

/**
 * 
 * This class will be indexed over in a HashMap
 * 
 * @author msto
 * 
 */
public class SignedGameState extends GameState {
    final int player;

    public SignedGameState(Board b, Reserves r, long player) {
        super(b, r);
        this.player = (int) player;
    }

    public static SignedGameState makeSignedGameState(GameState g, long player) {
        return new SignedGameState(g.b, g.r, player);
    }

    @Override
    public int hashCode() {
        return b.hashCode() ^ r.hashCode() ^ player;
    }

    @Override
    public boolean equals(Object q) {
        SignedGameState f = (SignedGameState) q;

        return (player == f.player) && Board.equalsNHC(b, f.b)
                && Reserves.equalNHC(r, f.r);
    }

    @Override
    public String toString() {
        return "[SGS " + Integer.toString(player) + " " + b.toString() + " "
                + r.toString() + "]";
    }
}
