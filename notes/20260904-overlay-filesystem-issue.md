# Overlay filesystem issue

*2026-09-04*

State machine tests have pointed out a design flaw in my overlay filesystem.

It consists of 4 directories: `base`, `create`, `update`, and `delete`. `base`
is a read-only file system, and `create`, `update`, `delete` contain changes
to that system.

Here's some problem code:

```
createDir("a")
commit()
createDir("a/b")
removeDir("a/b")
commit()
```

Given my current design, this is equivalent to:

```
createDir("a")
commit()
createDir("a")
commit()
```

`a/` exists in the base file system, so creating `a/b` is permitted. `a/b` is
created, then `a/b` is removed, which leaves `a/` in the `create` filesystem.
We can't introduce a rule to remove empty directories from the `create` filesystem,
because then the following would run incorrectly:

```
createDir("a")
createDir("a/b")
removeDir("a/b")
commit()
```

The above code adds `a/` to the `create` filesystem, then adds `b/` within `a/`,
then takes that back, leaving `a/`. But since `a/` is now empty, the cleanup logic
would have it removed, and so `a/` would not be created on commit.

I think the root issue is that there's no way to distinguish between a created
`b/` within an existing `a/`, and a created `b/` within a created `a/`.
