# Batch updates

*2026-08-20*

There are two templates on the server: `page.html.temple` and `page-list.html.temple`.
`page-list.html.temple` extends (calls) `page.html.temple`. I want to add a new parameter
to `page.html.temple` and have `page-list.html.temple` pass that parameter.

The server also has a rule that uses `page-list.html.temple`; whenever that template or
its dependencies (of which `page.html.temple` is one) change, the rule will run.

If I try to add the parameter to `page.html.temple` then the rule fires, attempting to
instantiate `page-list.html.temple`, which causes an error because the parameter I
introduced has not been satisfied. If I try to preemptively pass the parameter from
`page-list.html.temple` to `page.html.temple` then the rule also fires, and the error
says that `page-list.html.temple` provided an unknown argument to `page.html.temple`.
I need to be able to change both templates before any rule fires.

I'm thinking about using transactions to support this. Currently changes within a
transaction trigger rules immediately. I could add a "defer" flag to transaction
creation that causes rules to be run with all changes when it's time to commit
the transaction.
