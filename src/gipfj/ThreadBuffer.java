package gipfj;

/**
 * This object is to be passed along the functions calling the thread; It holds
 * all the needed buffers to accelerate calculations, as well as its own object
 * pools.
 * 
 */
public class ThreadBuffer {
    public final OrderingPool OPOOL;
    public final EntryPool EPOOL;

    /**
     * Which lines have been attempted??
     */
    public final int[] tried;
    /**
     * List of found lines
     */
    public final int[] linebuf;
    /**
     * Compression - Temporary
     */
    public final byte[] data;
    /**
     * Compression - Large numbers
     */
    public final int[] A;

    public final int id;

    public ThreadBuffer(int id) {
        this.id = id;
        OPOOL = new OrderingPool();
        EPOOL = new EntryPool();
        tried = new int[21];
        linebuf = new int[21];
        data = new byte[Compression.BYTES];
        A = new int[Compression.LEN];
    }

    public void analyze() {
        System.out.println("XX Thread local buffer analysis:");
        System.out
                .format("XX Ordering pool: disposed: %d; recieved %d; delta %d; size: %d\n",
                        OPOOL.disposed, OPOOL.delivered, OPOOL.disposed
                                - OrderingPool.OPOOL.delivered, OPOOL.maxind);
        System.out.format("XX Entrypool size: %d; returned: %d; nsize: %d\n",
                EPOOL.size, EPOOL.returned, EPOOL.getSize());
    }

    // Solely for use outside of the ai search proper
    public static final ThreadBuffer DEFAULT = new ThreadBuffer(-1);

    public static ThreadBuffer create(Long id) {
        return new ThreadBuffer((int) (long) id);
    }

    public static void recycleEntry(ThreadBuffer b, Entry e) {
        b.EPOOL.returnEntry(e);
    }
}
