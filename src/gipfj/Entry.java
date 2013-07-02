package gipfj;

public class Entry {
    public final int hc;
    public final byte d0;
    public final byte d1;
    public final byte d2;
    public final byte d3;
    public final byte d4;
    public final byte d5;
    public final byte d6;
    public final byte d7;
    public final byte d8;
    public final byte d9;
    public final byte d10;
    public final byte d11;
    public final byte d12;
    public final byte d13;

    public int rank;
    public byte depth;
    public Entry second;

    public Entry(byte[] data, int hc) {
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