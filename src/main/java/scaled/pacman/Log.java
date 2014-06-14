//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

/** Ye olde log facade. */
public abstract class Log {

  /** Records {@code msg} to the log. */
  public abstract void log (String msg);

  /** Records {@code msg} and {@code error} to the log. */
  public abstract void log (String msg, Throwable error);

  /**
   * Records {@code msg} plus {@code [key=value, ...]} to the log. If the last {@code keyVals}
   * argument is a lone exception, its stack trace will be logged.
   */
  public void log (String msg, String key, Object value, Object... keyVals) {
    StringBuilder sb = new StringBuilder(msg);
    sb.append(" [").append(key).append("=").append(value);
    int ii = 0; for (int ll = keyVals.length-1; ii < ll; ii += 2) {
      sb.append(", ").append(keyVals[ii]).append("=").append(keyVals[ii+1]);
    }
    sb.append("]");
    if (ii >= keyVals.length || !(keyVals[ii] instanceof Throwable)) log(sb.toString());
    else log(sb.toString(), (Throwable)keyVals[ii]);
  }
}
