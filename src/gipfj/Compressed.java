package gipfj;

/**
 * 
 * One additional, implicit requirement. The first 4 fields of the byte[] must
 * not be filled with data - these are for ranking info.
 * 
 * 
 * @author msto
 * 
 */
public interface Compressed {
    public byte[] getData();
}
