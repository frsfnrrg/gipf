package gipfj;

public class MoveSignedGS extends GameState {
    final byte m;

    public MoveSignedGS(Board b, Reserves r, int move) {
        super(b, r);
        m = (byte) move;
    }

    @Override
    public MoveSignedGS change(Board board, Reserves rr) {
        return new MoveSignedGS(board, rr, m);
    }

    public static long getMove(MoveSignedGS g) {
        return g.m;
    }

}
