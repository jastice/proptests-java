package props;


public class GCD {

    /** Greatest common divisor function.
     * Implement it and test against the specification.
     * Is the implementation correct? Is the specification?
     */
    static long gcd(long a, long b) {
        if (b==0) return a;
        else return gcd(b, a%b);
    }
}
