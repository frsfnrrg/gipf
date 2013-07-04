package gipfj;

/**
 * Alas, we can't chain items... Well, we "could" create a generic pool, with
 * PoolHolders to hold the items; the PoolHolders must be instantiated, so we
 * shall have a PoolHolderPool, and maybe even a PoolHolderPoolHolder. Nah, just
 * use an array for those.
 * 
 */
class OrderingPool {
    public final static OrderingPool OPOOL = new OrderingPool();

    private int[][] cache;
    private int maxind;

    public OrderingPool() {
        cache = new int[1 << 16][];
        maxind = 0;
    }

    public int[] get() {
        if (maxind == 0) {
            return new int[HistoryTable.MOVES];
        }
        maxind--;
        int[] r = cache[maxind];
        cache[maxind] = null;
        return r;
    }

    public void dispose(int[] foo) {
        cache[maxind] = foo;
        maxind++;
        if (maxind == cache.length) {
            int sz = cache.length;
            int[][] old = cache;
            cache = new int[sz * 2][];
            System.arraycopy(old, 0, cache, 0, sz);
        }
    }

    /**
     * Reduces the cache size
     */
    public void flush() {
        cache = new int[1 << 16][];
        maxind = 0;
    }
}
