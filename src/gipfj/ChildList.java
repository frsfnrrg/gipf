package gipfj;

import java.util.Iterator;

public class ChildList {
    private static class Node {
        public final int rank;
        public final Object data;
        public Node next;
        public Node prev;

        public Node(int rank, Object data) {
            this.rank = rank;
            this.data = data;
            next = null;
            prev = null;
        }
    }

    private Node mid;
    private Node top;
    private Node bottom;
    private int length;

    public ChildList() {
        mid = null;
        top = null;
        bottom = null;
        length = 0;
    }

    /**
     * Currently the largest bottleneck
     * 
     * @param n
     * @param rank
     */
    public void add(Object n, int rank) {
        Node thingy = new Node(rank, n);
        length++;

        if (mid == null) {
            mid = thingy;
            return;
        }

        Node bef, aft;
        if (rank > mid.rank) {
            if (top == null) {
                top = thingy;

                mid.next = top;
                top.prev = mid;
                return;
            }
            if (rank == top.rank) {
                top.next = thingy;
                thingy.prev = top;
                top = thingy;
                return;
            }
            bef = top.prev;
            while (bef.rank > rank) {
                bef = bef.prev;
            }
            aft = bef.next;
            // rank <= mid.rank
        } else if (bottom == null) {
            thingy.next = mid;
            mid.prev = thingy;
            bottom = thingy;
            return;
        } else if (bottom.rank == rank) {
            bottom.prev = thingy;
            thingy.next = bottom;
            bottom = thingy;
            return;
        } else if (rank == mid.rank) {
            bef = mid.prev;
            aft = mid;
        } else { // rank < mid.rank
            aft = bottom.next;
            while (aft.rank < rank) {
                aft = aft.next;
            }

            bef = aft.prev;
        }

        thingy.prev = bef;
        bef.next = thingy;
        thingy.next = aft;
        aft.prev = thingy;
    }

    private static Iterator<Object> emptyIterator = new Iterator<Object>() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public Object next() {
            return null;
        }

        @Override
        public void remove() {
        }
    };

    /**
     * Returns an iterator over the elements of this. No addition should be
     * attempted after this. How to save memory.. (12 + 4*k) vs. (20 + 20*k)
     * 
     * @return
     */
    public static Iterator<Object> clpack(ChildList t, boolean downward) {
        // t.mid _must_ exist (length >= 1)
        final int length = t.length;
        // TODO: maybe, recycle these - how? by length?
        final Object[] foo = new Object[length];
        Node q;
        if (length == 0) {
            return emptyIterator;
        } else if (downward) {
            int i = 0;
            if (t.top == null) {
                q = t.mid;
            } else {
                q = t.top;
            }
            while (q != null) {
                foo[i] = q.data;
                i++;
                q = q.prev;
            }
        } else {
            int j = 0;
            if (t.bottom == null) {
                q = t.mid;
            } else {
                q = t.bottom;
            }
            while (q != null) {
                foo[j] = q.data;
                j++;
                q = q.next;
            }
        }

        return new Iterator<Object>() {
            private int index = 0;

            @Override
            public boolean hasNext() {
                return (index != length);
            }

            @Override
            public Object next() {
                Object k = foo[index];
                index++;
                return k;
            }

            @Override
            public void remove() {
            }
        };
    }

    /**
     * static constructor
     * 
     * @return
     */
    public static ChildList clmake() {
        return new ChildList();
    }

    /**
     * static add - to avoid reflection
     * 
     * @return
     */
    public static void cladd(ChildList a, Object q, long r) {
        a.add(q, (int) r);
    }

    public static String tsr(Node q) {
        if (q == null)
            return "null";
        else
            return Integer.toHexString(q.hashCode());

    }
}
