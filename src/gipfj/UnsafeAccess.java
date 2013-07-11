package gipfj;

import java.lang.reflect.Field;

import sun.misc.Unsafe;

/**
 * To avoid repeating reflection ugliness.
 * 
 */
public class UnsafeAccess {
    private static final Unsafe paul = getTheUnsafe();

    private static Unsafe getTheUnsafe() {
        try {
            Field singleoneInstanceField = Unsafe.class
                    .getDeclaredField("theUnsafe");
            singleoneInstanceField.setAccessible(true);
            return (Unsafe) singleoneInstanceField.get(null);

        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (SecurityException e) {
            e.printStackTrace();
        } catch (NoSuchFieldException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }

        return null;
    }

    public static Unsafe getUnsafe() {
        return paul;
    }
}
