# Rule deduplication

*2026-07-27*

Here's some pseudocode for some rules that lead to an article being rendered:

```hcl
rule "article-adjacency" {
  depends = ["article:*"]

  build = ...
  # * Sort articles chronologically and create/update
  # * For each `article:{name}` in the sorted list, create/update `adjacency:{name}`,
  #   containing previous/next entries based on `article:{name}`'s adjacent entries in the sorted list.
  #   * Only update entries whose content has changed to prevent redundant rebuilds.
}

rule "article-render" {
  depends = [
    # The system's base URL
    "config:base-url",

    # Template into which the article will be substituted
    "template:article.html.temple",

    # The article to be rendered
    #
    # The shared {name} component means this rule depends on a matching article-adjacency pair
    "article:{name}",

    # "previous" and "next" articles for the current article
    "adjacency:{name}"
  ]

  build = ...
    # * Parse, type check the template
    # * Render the article to HTML, extract and type check metadata
    # * Substitute article HTML into template using article contents, article metadata, and adjacency info
    # * Store rendered HTML as a new `html` resource
}
```

This rule handles changes to *any* of its dependencies.
If only `article:x` changes, then the rule files with the current values of `config:base-url`, `template:article.html.temple`, `article:x` and `adjacency:x`.
But if `config:base-url` or `template:article.html.temple` changes,
then the rule fire with the current value of `config:base-url` and `template:article.html.temple` for *every* pair `article:{name}` & `adjacency:{name}`.

## Issue

Currently when I upload an article, it produces a trace like this:

```
created article:3
* updated adjacency:2 (article:3 updated)
* updated adjacency:3 (article:3 updated)
* updated html:article-3 (article:3 updated)
* updated html:article-2 (adjacency:2 updated)
* updated html:article-3 (adjacency:3 updated)
```

Note that `html:article-3` is updated twice, first because `article:3` was updated, and second because `adjacency:3` was updated. The correct trace should be:

```
created article:3
* updated adjacency:2 (article:3 updated)
* updated adjacency:3 (article:3 updated)
* updated html:article-3 (article:3 updated, adjacency:3 updated)
* updated html:article-2 (adjacency:2 updated)
```

`html:article-3` being updated only once, noting that the rule fired because *two* of its dependencies changed.

`html:article-3` is updated twice because the build system maintains a queue of resource IDs to match against rules, and processes these resource IDs serially.
So the `article-render` rule first fires for the creation of `article:3`.
But the creation of `article:3` also leads to the creation of `adjacency:3` via another rule, and `adjacency:3` is put onto the queue.
Then when it's time for `adjacency:3` to be processed, the `article-render` rule fires a second time with `article:3`.

A rule like `article-render` should be deferred until all rules that *could* produce inputs to it have completed.
If rules also declared their outputs, then we'd be able to see a directed graph between rules.
Then, since `article-adjacency` produces an output that's depended on by `article-render`, `article-adjacency` will be run until completion before `article-render` is ever called.
