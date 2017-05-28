# Introduction

This module provides a filter for [pandoc](https://www.pandoc.org) to support certain
elements from [vimwiki](https://vimwiki.github.io)'s markup. Due to the
limitations of pandoc filters not all elements that go beyond markdown are
supported.

In particular, the following elements will converted:

* ticked and unticked boxes to unicode characters
* transclusions using double curly brackets
* linking to external wiki pages using double square brackets, including
  anchors

# Example

## vimwiki markup and HTML conversion
The following wiki source code

```
* [ ] Not done.
* [.] Parially done
    * [ ] Subitem 1 [[sub1]]
    * [X] Subitem 2 [[sub2]]
    * [ ] Last subitem
* [o] Half done
    * [ ] Subitem 1 [[sub1]]
    * [X] Subitem 2
    * **[X] Last subitem**
* [O] Almost done
    * [ ] Subitem 1 [[sub1]]
    * [X] Subitem 2
    * [X] Subitem 3
    * [X] Last subitem

{{sub3}}
```

is converted to

* ☐ Not done.
* ▄⃞▄ Parially done
    * ☐ Subitem 1 [sub1](#sub1)
    * ☑ Subitem 2 [sub2](#sub2)
    * ☐ Last subitem
* ▆⃞▆ Half done
    * ☐ Subitem 1 [sub1](#sub1)
    * ☑ Subitem 2
    * **☑ Last subitem**
* ▇⃞▇ Almost done
    * ☐ Subitem 1 [sub1](#sub1)
    * ☑ Subitem 2
    * ☑ Subitem 3
    * ☑ Last subitem

This is the content of `sub3`.

## Explanation and Limits
The wiki pages `sub1` and `sub2` have been left out, but would appear at the
end of the document. The content of `sub3` would appear where the transclusion
was found in the source. Transclusion of the same file multiple times make the
content of the file show up multiple times.

However, linking to the same file multiple times (with or without anchors)
only includes the file once.

There are some limits how the additional items can be used:

* Check boxes can be placed at the beginning of any plain or formatted text,
  e.g. the beginning of an item in any list (ordered or unordered).

  If you really a check box in the middle of text, you have set a format, e.g.
  bold. Changing this checkbox is not supported by vimwiki and must be done
  manually.
* Transclusions must be placed in top-level paragraphs. This has a technical
  reason [^1] and a use-case reason [^2].
* Links can be placed anywhere. You can format them.
* Neither links nor transclusions must contain spaces. This is a limit of the
  current implementation of this filter [^3], not of pandoc. Pull requests are
  welcome.
* Transcluded meta information (author, title, date) will be taken from the
  first place they are found. Partial information will be mixed according to
  the same rule: If you set the title in the top-level document to `Title 1` and transclude
  a title (e.g. `Title 2`) and author, the final document will have `Title 1`
  and the author.
* Linked and transcluded files will be searched with the following list of
  extensions: `.md`, `.markdown`, `.wiki`, `.mdwiki`. The first file to be
  found will be read. There is no check for another match. `.mdwiki` can be
  used to distinguish between *regular* markdown and *wikified* markdown using
  `vimwiki-register-extension`. See vimwiki's help for that.

# What's missing / next steps

tl;dr: Not much / bug fixing.

The filter implements most of my needs. It's sufficient to bridge the time
until Pandoc adds [native support for vimwiki](https://github.com/jgm/pandoc/issues/863).

If you find any bugs, please create an issue on github or submit a pull
request.

If you have better unicode art to indicate the *partially done* check
boxes, create an issue or submit a PR.


# Footnotes

[^1]: Files are composed of `Block`s, a paragraph is a `Block` and
  therefore a file can replace a paragraph. Other things (e.g. bold sections)
  are not blocks and cannot be replaced by a `Block`.

[^2]: You need to transclude other wikis on non-top level positions for two
  reasons: you want to refer to the same text twice (which can be faked with
  links) or you want to use the text as a template (which makes little sense
  as neither vimwiki nor pandoc have a mechanism to provide parameters for
  these templates; would be a cool feature, though).

[^3]: ~~I can't be bothered to implement it.~~ I don't need it right now.
