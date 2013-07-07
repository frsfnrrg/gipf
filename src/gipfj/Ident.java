package gipfj;

public class Ident {
    public long a;
    public int b;
    public int hc;

    public Ident(long a, int b, int hc) {
        this.a = a;
        this.b = b;
        this.hc = hc;
    }

    @Override
    public int hashCode() {
        System.out.println("You no call me!");
        return 0;
    }
}
