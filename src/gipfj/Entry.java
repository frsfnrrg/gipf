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
    public byte d0;
    public byte d1;
    public byte d2;
    public byte d3;
    public byte d4;
    public byte d5;
    public byte d6;
    public byte d7;
    public byte d8;
    public byte d9;
    public byte d10;
    public byte d11;
    public byte d12;
    public byte d13;

    public int rank;
    public byte depth;
    public Entry second;

    public static int allocated = 0;

    public Entry(byte[] data, int hc) {
        allocated++;
        this.hc = hc;
        d0 = data[1];
        d1 = data[2];
        d2 = data[3];
        d3 = data[3];
        d4 = data[4];
        d5 = data[5];
        d6 = data[6];
        d7 = data[7];
        d8 = data[8];
        d9 = data[9];
        d10 = data[10];
        d11 = data[11];
        d12 = data[12];
        d13 = data[13];
    }

    public void reset(byte[] data, int hc) {
        this.hc = hc;
        d0 = data[1];
        d1 = data[2];
        d2 = data[3];
        d3 = data[3];
        d4 = data[4];
        d5 = data[5];
        d6 = data[6];
        d7 = data[7];
        d8 = data[8];
        d9 = data[9];
        d10 = data[10];
        d11 = data[11];
        d12 = data[12];
        d13 = data[13];
    }

    public boolean equals(Entry i) {
        if (i.hc != hc)
            return false;

        return (d4 == i.d4) && (d5 == i.d5) && (d6 == i.d6) && (d7 == i.d7)
                && (d8 == i.d8) && (d9 == i.d9) && (d10 == i.d10)
                && (d11 == i.d11) && (d12 == i.d12) && (d13 == i.d13)
                && (d0 == i.d0) && (d1 == i.d1) && (d2 == i.d2) && (d3 == i.d3);
    }
}