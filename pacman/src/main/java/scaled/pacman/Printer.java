//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.pacman;

import java.io.PrintStream;

public class Printer {

  public Printer (PrintStream out) {
    this(out, "");
  }

  public Printer (PrintStream out, String indent) {
    _out = out;
    _indent = indent;
  }

  public Printer nest (String indent) {
    return new Printer(_out, _indent+indent);
  }

  public void println (Object value) {
    _out.println(value);
  }

  public void printCols (Iterable<String[]> cols, String onEmpty) {
    printCols(cols, onEmpty, 2);
  }

  public void printCols (Iterable<String[]> cols, String onEmpty, int gap) {
    if (!cols.iterator().hasNext()) {
      println(onEmpty);
      return;
    }

    // compute the maximum column width
    int[] wids = null;
    for (String[] col : cols) {
      if (wids == null) wids = new int[col.length];
      for (int ii = 0; ii < col.length; ii++) wids[ii] = Math.max(wids[ii], col[ii].length());
    }

    // now print out the columns, with appropriate gap
    for (String[] col : cols) {
      for (int ii = 0; ii < col.length; ii++) {
        _out.print(col[ii]);
        if (ii < col.length-1) pad(wids[ii]-col[ii].length()+gap);
      }
      _out.println();
    }
  }

  private void pad (int count) {
    for (int ii = 0; ii < count; ii++) _out.print(" ");
  }

  private final PrintStream _out;
  private final String _indent;
}
