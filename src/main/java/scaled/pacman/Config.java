//
// Scaled Package Manager - builds and installs Scaled packages
// http://github.com/scaled/scaled-pacman/blob/master/LICENSE

package scaled.pacman;

import java.util.*;

public class Config {

  // defines simply parsing and accumulation scheme for package config files
  public static abstract class Parser<T> {
    public abstract T parse (String text) throws Exception;
    public Optional<T> zero () {
      return Optional.empty();
    }
    // by default we disallow accumulation; a key can only be bound once
    public T accum (String key, T oval, T nval) {
      throw new IllegalStateException(
        String.format("'%s' already defined:\n  old: '%s'\n  new: '%s'", key, oval, nval));
    }
  }

  public static class DependP extends Parser<List<Depend>> {
    public final Depend.Scope scope;
    public DependP (Depend.Scope scope) {
      this.scope = scope;
    }
    public List<Depend> parse (String text) throws Exception {
      try {
        return new ArrayList<>(Collections.singletonList(Depend.parse(text, scope)));
      } catch (Exception e) {
        throw new IllegalArgumentException(
          "Failed to parse depend '" + text + "': " + e.getMessage());
      }
    }
    public Optional<List<Depend>> zero () {
      return Optional.of(new ArrayList<>());
    }
    public List<Depend> accum (String key, List<Depend> have, List<Depend> next) {
      have.addAll(next);
      return have;
    }
  }

  public static Parser<String> StringP = new Parser<String>() {
    public String parse (String text) { return text; }
  };

  public static Parser<Source> SourceP = new Parser<Source>() {
    public Source parse (String text) throws Exception {
      return Source.parse(text);
    }
  };

  public Config (Iterable<String> lines) {
    for (String line : lines) {
      line = trim(line);
      if (line.length() == 0) continue;
      String[] parts = line.split(":", 2);
      if (parts.length != 2) {
        _errors.add("Invalid: " + line);
        continue;
      }
      String key = parts[0].trim();
      List<String> klines = _data.get(key);
      if (klines == null) _data.put(key, klines = new ArrayList<>());
      klines.add(parts[1].trim());
    }
  }

  public <T> T resolve (String key, Parser<T> parser) {
    List<String> errors = new ArrayList<>();
    Optional<T> res = parser.zero();
    List<String> vals = _data.remove(key);
    if (vals != null) {
      for (String val : vals) {
        try {
          T nval = parser.parse(val);
          res = Optional.of(res.isPresent() ? parser.accum(key, res.get(), nval) : nval);
        } catch (Exception e) {
          errors.add(e.getMessage());
        }
      }
    }
    _errors.addAll(errors);
    if (res.isPresent()) return res.get();
    throw new IllegalArgumentException(String.format(
      "Missing or invalid binding for '%s' [data=%s, errors=%s]", key, vals, errors));
  }

  public List<String> finish () {
    for (Map.Entry<String,List<String>> e : _data.entrySet()) {
      _errors.add(String.format("Unknown binding: '%s' %s", e.getKey(), e.getValue()));
    }
    return _errors;
  }

  private String trim (String line) {
    int hidx = line.indexOf('#');
    return (hidx == -1) ? line.trim() : line.substring(0, hidx).trim();
  }

  private final List<String> _errors = new ArrayList<>();
  private final Map<String,List<String>> _data = new HashMap<>();
}
