# Erlang Creative Forum
## Simple BEAM-powered forum software

### About

Erlang Creative Forum (ECF) is intended to be a very simple forum host, similar
to SMF or vBulletin or the like, but very lightweight and fast. It's still very
early in development so it's missing a lot of features, and I would (currently)
strongly recommend against using it. Feel free to take a look around the code
though!

At the bare minimum, ECF still needs the following:
* Documentation
* Lots and lots of CSS and HTML magic to make it look decent!
* Pagination of threads and posts (former can probably just use `lists:sublist`,
but the latter should probably use something more performant)
* Administration functionality
    * Subforum creation (currently you must manually call `ecf_forum:new_forum/4`)
    * Deleting threads/posts
    * Banning accounts
* Permissions system
    * Locking threads/forums
    * User groups
    * Restricting viewing forums
    * Restricting creating threads in forums
    * Restricting posting in threads/forums
* Actual security
    * HTTPS
    * Rate limiting for creating threads and posting
    * Email on registration
    * CAPTCHAs on registration and login
    * Logging of access and account creation and such

To actually be useable, I'd like to add the following:
* More admin actions
    * Banning people for a given length of time
* Listing all users, sorting by stuff
* Searching

The following may happen at some point, but I'm unsure about them:
* Markdown for posts
* Using Argon2 for password hashing instead of PBKDF2
* Possibly an expansion upon the current "template-ish" system, maybe a future
switch to ErlyDTL or something else

### Requirements
Erlang/OTP 20


### Installation
Currently there is no real installation process. However, testing ECF is very
simple since it uses [erlang.mk](https://erlang.mk). Simple `make run` and it
will start it listening on port 8080 and drop you into an Erlang REPL. The first
time you start it up, be sure to use `ecf_db:install().` followed by
`mnesia:start().` to create and setup the database.


### Configuration
TODO: Describe all configuration options. Most of them are just simple messages
in case you want to customize them, but a few are actually important.

