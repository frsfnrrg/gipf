package gipfj;

public class IDRNode {
    private final Object children;
    private final byte player;
    private final int rank;
    private final GameState gamestate;

    private IDRNode(GameState g, long p, long r, Object c) {
        children = c;
        player = (byte) p;
        rank = (int) r;
        gamestate = g;
    }

    public static IDRNode makeIDRNode(GameState g, long p, long r) {
        return new IDRNode(g, p, r, null);
    }

    public static IDRNode updateIDRNode(IDRNode d, long r, Object c) {
        return new IDRNode(d.gamestate, d.player, r, c);
    }

    public static GameState getGameState(IDRNode i) {
        return i.gamestate;
    }

    public static long getPlayer(IDRNode i) {
        return i.player;
    }

    public static long getRank(IDRNode i) {
        return i.rank;
    }

    public static Object getChildren(IDRNode i) {
        return i.children;
    }
}
