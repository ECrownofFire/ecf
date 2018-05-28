# Erlang Creative Forum
## Simple BEAM-powered forum software

### About

Erlang Creative Forum (ECF) is intended to be a very simple forum host, similar
to SMF or vBulletin or the like, but very lightweight and fast. It's still very
early in development so it's missing a lot of features, and I would (currently)
strongly recommend against using it. Feel free to take a look around the code
though!

At the bare minimum, ECF still needs the following:
* Documentation, and LOTS of it!
* Permissions system, to restrict actions to administrators and the like
* Subforum creation (currently you must manually call `ecf_forum:new_forum/4`)
* Administration functionality (e.g. deleting threads/posts from other users)
* Email registration
* CAPTCHAs on registration and login
* Rate-limiting on creating threads and posts
* Lots and lots of CSS and HTML magic to make it look decent!
* Logging of access and account creation and such
* Some code should probably be cleaned up eventually

The following may happen at some point, but I'm unsure about them:
* Markdown for posts
* Using Argon2 for password hashing instead of PBKDF2
* Possibly an expansion upon the current "template-ish" system, maybe a future
switch to ErlyDTL for something a bit easier

### Installation
Currently there is no real installation process. However, testing ECF is very
simple since it uses [erlang.mk](https://erlang.mk). Simple `make run` and it
will drop you into an Erlang REPL. For the first time you start it up,
`ecf_db:install().` followed by `mnesia:start().` to create and setup the
database.

### Configuration
TODO: Describe all configuration options. Most of them are just simple messages
in case you want to customize them, but a few are actually important.

