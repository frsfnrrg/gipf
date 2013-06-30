package gipfj;

public class MoveSignedGS extends GameState {
    final byte m;

    public MoveSignedGS(Board b, Reserves r, boolean gs, int move) {
        super(b, r, gs);
        m = (byte) move;
    }

    @Override
    public MoveSignedGS change(Board board, Reserves rr, boolean gphase) {
        return new MoveSignedGS(board, rr, gphase, m);
    }

    public static long getMove(MoveSignedGS g) {
        return g.m;
    }
}
