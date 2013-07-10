package gipfj;

import java.util.Random;

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

    public final int[] ordbuf;

    public final int id;

    public final int[] pbuf;

    public final int[] mbuf;

    public ThreadBuffer(int id) {
        this.id = id;
        OPOOL = new OrderingPool();
        tried = new int[21];
        linebuf = new int[21];
        ordbuf = new int[Const.MOVES];
        seed = new Random().nextInt();
        pbuf = new int[21];
        mbuf = new int[21];
    }

    public void analyze() {
        System.out.println("XX Thread local buffer analysis:");
        System.out
                .format("XX Ordering pool: disposed: %d; recieved %d; delta %d; size: %d\n",
                        OPOOL.disposed, OPOOL.delivered, OPOOL.disposed
                                - OrderingPool.OPOOL.delivered, OPOOL.maxind);
    }

    private long seed;

    // MODULUS_I is to be used for integer random calculation - warning - this
    // _may_ be problematic. log2(I) <= 15.5; log2(L) <= 31.5
    public static final int MODULUS_I = 41989;
    public static final long MODULUS_L = 1696286587; // 41183 * 41189

    /**
     * Goals: to be fast, uniformly distributed.. TODO: distribution test
     * (generate 10 * 9 primes at each normal max)
     * 
     * @param max
     * @return
     */
    public int nextRandomInt(int max) {
        long m2 = MODULUS_L - 2;
        long cap = m2 - (m2 % max) + 2;

        do {
            seed = (seed * seed) % MODULUS_L;
        } while (seed > cap);

        long dqt = (seed - 2) % max;

        return (int) dqt;
    }

    // Solely for use outside of the ai search proper
    public static final ThreadBuffer DEFAULT = new ThreadBuffer(-1);

    public static ThreadBuffer create(Long id) {
        return new ThreadBuffer((int) (long) id);
    }
}
