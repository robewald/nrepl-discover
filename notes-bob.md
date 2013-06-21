# Notes on technomacy's proposal #

* Today depends on nrepl vanilla, these changes would need to go into
  tools.nrepl? How does that affect other nrepl clients.

* New language, scoping might help. Should we do this in edn?
  overlay - as a position and a set of tags, whether these tags are
  displayed as colors, fringe symbols etc. is up to the client.
  returns change semantics depending on the operation.
	
* boilerplate: (t/send transport (m/response-for msg....)
  Looks like this can be factored out...

* Prefix magic in `nrepl-discover-command-for`, difficult to discover. I think the
  `:name` of the `:nrepl/op` determines the full name or the prefix could be a
  parameter to discover.

* How do we make sure that nrepl and emacs agree about the location of
  a file to be able to use positions.

* position, a s/character/line offset?

	
