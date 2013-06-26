package gipfj;

public class MoveSignedGS extends GameState {
    final int m;

    public MoveSignedGS(Board b, Reserves r, int move) {
        super(b, r);
        m = move;
    }

    @Override
    public MoveSignedGS change(Board board, Reserves rr) {
        return new MoveSignedGS(board, rr, m);
    }

    public static long getMove(MoveSignedGS g) {
        return g.m;
    }

}
