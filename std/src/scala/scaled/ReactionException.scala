//
// Reactual - an FRP-ish library for Scala
// Copyright (c) 2013, Michael Bayne - All rights reserved.
// http://github.com/samskivert/reactual/blob/master/LICENSE

package scaled

import java.io.{PrintStream, PrintWriter}

/** Communicates failures from one or more reactors. */
class ReactionException extends RuntimeException {

  override def getMessage = {
    val buf = new StringBuilder
    val sup = getSuppressed
    for (failure <- sup) {
      if (buf.length > 0) buf.append(", ")
      buf.append(failure.getClass.getName).append(": ").append(failure.getMessage)
    }
    s"${sup.length} failures: $buf"
  }

  override def fillInStackTrace () :Throwable = this // no stack trace here
}
