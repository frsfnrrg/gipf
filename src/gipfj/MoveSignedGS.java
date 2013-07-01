package gipfj;

public class MoveSignedGS extends GameState {
    final byte m;

    public MoveSignedGS(Board b, Reserves r, boolean gs1, boolean gs2, int move) {
        super(b, r, gs1, gs2);
        m = (byte) move;
    }

    @Override
    public MoveSignedGS change(Board board, Reserves rr, boolean gphase1,
            boolean gphase2) {
        return new MoveSignedGS(board, rr, gphase1, gphase2, m);
    }

    public static long getMove(MoveSignedGS g) {
        return g.m;
    }
}
