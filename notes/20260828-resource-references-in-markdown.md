# Resource references in Markdown

*2026-08-28*

I wanted to write a checked "table of contents" in a `page` resource:

````
```toml+blog-metadata
title = "About Me"
url = "/about"
description = "A little bit about me."
```

<h1 class="centered">About Me</h1>

<div id="toc">
<h3>Links</h3>
<ul>
<li><a href="{{resource.page.cv.metadata.url}}">CV</a>
<li><a href="{{resource.page.talks.metadata.url}}">Talks</a>
<li><a href="https://github.com/LightAndLight">GitHub</a>
<li><a href="https://soundcloud.com/LightAndLight">SoundCloud</a>
</ul>
</div>

Regular Markdown paragraphs...
````

This doesn't work in general because some of my articles contain backslashes,
e.g. in Haskell code examples: `\n -> n + 1`. I think it'd be annoying to
remember that an article is actually a template and to escape the right
characters, so I need a different way to add resource references.

It probably has to reuse some kind of Markdown syntax so that I can rewrite the
references using Pandoc. If did that then I'd have to rewrite the above example:

```
<div id="toc">

### Links

* [CV](some reference to resource.page.cv.metadata.url)
* [Talks](some reference to resource.page.talks.metadata.url)
* [GitHub](https://github.com/LightAndLight)
* [SoundCloud](https://soundcloud.com/LightAndLight)

</div>
```

Is that an acceptable tradeoff? We'll see.

I could use the
[wikilinks Commonmark extension](https://hackage-content.haskell.org/package/commonmark-extensions-0.2.7.1/docs/Commonmark-Extensions-Wikilinks.html)
for resource references:

```
<div id="toc">

### Links

* [[CV|resource.page.cv.metadata.url]]
* [[Talks|resource.page.talks.metadata.url]]
* [GitHub](https://github.com/LightAndLight)
* [SoundCloud](https://soundcloud.com/LightAndLight)

</div>
```

The Commonmark parser adds a `wikilink` class to the generated link, which
would let me know which links to process.
