# Publishing and unpublishing

*2026-09-30*

I'm working on private drafts for articles/notes and pages and I've just found
an interesting behaviour that I missed. I created a page with `published = true`.
Naturally that lead to the creation of a public route, and the page was visible
at its URL. Then I edited the page and set `published = false`. I expected the
page to be no longer visible at its URL, but it still was. The route resource
was correctly updated, so the change *was* applied when I restarted the server.

The problem is that when the routing table is updated in memory, the new private
route is added but the old public route isn't removed. An update to a route
resource implies a deletion of the old route in addition to the insertion of
the new route.

I'm fixing this by having the route old route removed when route changes are
committed. This isn't safe for concurrent edits, but the rest of the transaction
system isn't either, so it's okay for now.
