package gipfj;

public class IncrementalGameCalc {

    private static long ilpbI;
    private static long ilpbJ;
    private static long ilpbK;

    private static GameState[] ilpb1 = new GameState[3];
    private static GameState[] ilpb2 = new GameState[120];
    private static GameState[] ilpb3 = new GameState[360];

    public static void setupILPB() {

    }

    // this is necessary if we want high performance, less allocation, etc
    // when discarding nodes. Laziness is our friend...
    public static GameState incrementallyListPossibleBoards(GameState g,
            long player) {
        return null;
    }
}
