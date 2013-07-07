package gipfj;

/**
 * This object is to be passed along the functions calling the thread; It holds
 * all the needed buffers to accelerate calculations, as well as its own object
 * pools.
 * 
 */
public class ThreadBuffer {
    public final OrderingPool OPOOL;

    /**
     * Which lines have been attempted??
     */
    public final int[] tried;
    /**
     * List of found lines
     */
    public final int[] linebuf;

    public final int id;

    public ThreadBuffer(int id) {
        this.id = id;
        OPOOL = new OrderingPool();
        tried = new int[21];
        linebuf = new int[21];
    }

    public void analyze() {
        System.out.println("XX Thread local buffer analysis:");
        System.out
                .format("XX Ordering pool: disposed: %d; recieved %d; delta %d; size: %d\n",
                        OPOOL.disposed, OPOOL.delivered, OPOOL.disposed
                                - OrderingPool.OPOOL.delivered, OPOOL.maxind);
    }

    // Solely for use outside of the ai search proper
    public static final ThreadBuffer DEFAULT = new ThreadBuffer(-1);

    public static ThreadBuffer create(Long id) {
        return new ThreadBuffer((int) (long) id);
    }
}
