//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a class as implementing a major mode.
 *
 * <p>Note: modes <em>must</em> be implemented by classes named {@code FooMode}. The @Major
 * annotation is only sought on classes matching that name pattern.</p>
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Major {

    /**
     * The name of this mode. Displayed to the user. This is generally a simple single lowercase
     * word. Note that modes exist in a global namespace, so pick something unique. Examples:
     * {@code whitespace}, {@code scala}, {@code foo-bar}.
     */
    String name ();

    /**
     * A description of this mode's functionality.
     */
    String desc ();

    /**
     * Defines tags that describe the environment provided by this mode. These are used to
     * automatically activate minor modes appropriate to a major mode. There are some "predfined"
     * tags which a mode is encouraged to use if they are appropriate:
     * <ul>
     * <li> {@code text} - for modes which edit plain text (not code)
     * <li> {@code code} - for modes which edit program code.
     * <li> {@code project} - for modes which edit files that are in a project (a directory
     * structure containing many related files). Adding this tag will activate the project minor
     * mode.
     * </ul>
     *
     * <p> A mode should define any general tags appropriate to itself as well as one or more
     * specific tags. Here are some examples to give you an idea:
     * <ul>
     * <li> java-mode might declare {@code code, project, java}.
     * <li> scala-mode might declare {@code code, project, scala}.
     * <li> html-mode might declare {@code code, project, text, html}.
     * <li> text-mode declares only {@code text}.
     * </ul>
     */
    String[] tags () default {};

    /**
     * Defines a set of regular expressions that match the names of files handled by this mode. This
     * is used to automatically activate this mode when a file with a matching name is edited. These
     * should not be overly general, lest a mode usurp other modes inadvertently. Most commonly, one
     * will specify the suffix to match: {@code ".*\\.scala"}, {@code ".*\\.java"}, but some modes
     * may need to match exact file names: {@code "Makefile"}.
     */
    String[] pats () default {};

    /**
     * Defines the "interpreters" handled by this mode. This is used to automatically activate this
     * mode when a file with a matching "interpreter" is edited. An interpreter is specified in a
     * shebang clause at the top of a file. Some examples:
     * <ul>
     * <li>#!/usr/bin/perl
     * <li>#!/bin/sh
     * <li>#!/usr/bin/env ruby
     * </ul>
     * Before interpreter detection is performed, the line is split on spaces and slashes, and the
     * resulting tokens are matched against the interpreters declared by modes. So a mode should
     * simply specify simple single strings here, like {@code ruby}, {@code perl}, {@code sh},
     * {@code bash}, etc.
     */
    String[] ints () default {};
}
