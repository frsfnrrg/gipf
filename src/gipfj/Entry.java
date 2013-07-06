package gipfj;

// Question: is passing in a byte[] array bad form?
// should we just use 14 args, since we are just using the
// static array as a buffer??
/**
 * We no longer track finalizers: it is too darn expensive, memorywise (doubles
 * entry cost); additionally, it doubles time taken.
 * 
 * 
 * 
 */
public class Entry {
    public int hc;
    public long a;
    public int b;

    public int rank;
    public byte depth;
    public Entry second;

    public static int allocated = 0;

    public Entry(long a, int b, int hc) {
        allocated++;
        this.hc = hc;
        this.a = a;
        this.b = b;
    }

    public void reset(long a, int b, int hc) {
        this.hc = hc;
        this.a = a;
        this.b = b;
    }

    @Override
    public int hashCode() {
        System.out.println("You no call me!");
        return hc;
    }

    public boolean equals(Entry i) {
        if (i.hc != hc)
            return false;

        return (i.a == this.a && i.b == this.b);
    }
}