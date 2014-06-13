//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Exposes the contents of the packges directory project via a useful API.
 */
public class PackageDirectory {

  public static class Entry {
    public final Source source;
    public final String name;
    public final String descrip;

    public Entry (Source source, String name, String descrip) {
      this.source = source;
      this.name = name;
      this.descrip = descrip;
    }
  }

  public List<Entry> entries = new ArrayList<>();
  public Map<String,Entry> byName = new HashMap<>();

  public void init (Log log, Path root) {
    Path pfile = root.resolve("packages");
    try {
      for (String line : Files.readAllLines(pfile)) {
        String[] bits = line.split(" ", 3);
        if (bits.length != 3) {
          log.log("Invalid directory entry: " + line);
          continue;
        }
        try {
          Entry e = new Entry(Source.parse(bits[1]), bits[0], bits[2]);
          entries.add(e);
          byName.put(e.name, e);
        } catch (URISyntaxException e) {
          log.log("Invalid source:", "entry", line, "error", e);
          // skip it!
        }
      }
    } catch (IOException e) {
      log.log("Error reading package directory", "file", pfile, e);
    }
  }
}
