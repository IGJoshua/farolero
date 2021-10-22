package farolero.signal;

/**
 * Class used for control flow operators for farolero.
 *
 * This class extends {@link java.lang.Error} rather than
 * {@link java.lang.Throwable} or {@link java.lang.Exception} in order to
 * prevent most Java code from catching it. Some code may unfortunately catch
 * {@link java.lang.Throwable}, which may interfere. In most cases though, these
 * cases are most often in framework-style applications, oftentimes at
 * top-level, meaning most times signals will not be raised past that boundary.
 */
public class Signal extends Error {
    /**
     * An object naming where this signal should jump to.
     */
    public final Object target;

    /**
     * An object containing the arguments to the function handling the signal.
     */
    public final Object args;

    public Signal(Object target, Object args) {
        this.target = target;
        this.args = args;
    }

    /**
     * Filling in the stacktrace is relatively slow during the construction of
     * exceptions. This method is overriden in order to prevent that runtime
     * cost.
     */
    @Override
    public Throwable fillInStackTrace() {
        return this;
    }

}
