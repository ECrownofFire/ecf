# Erlang Creative Forum
## Simple BEAM-powered forum software

### About

Erlang Creative Forum (ECF) is intended to be a very simple forum host, similar
to SMF or vBulletin or the like, but very lightweight and fast. It's still very
early in development so it's missing a lot of features, and I would (currently)
recommend against using it. Feel free to take a look around the code though!

At the bare minimum, ECF still needs the following:
* Documentation
* Tests
* Lots and lots of CSS/HTML/JS magic to make it look decent!
* Terms of use and privacy policy?
* Administration frontend
    * Subforum reordering
    * Deleting posts
    * Banning accounts
    * Editing permissions
    * Groups
        * Creation
        * Editing
        * Adding/removing members
* Security
    * HTTPS (works fine behind nginx reverse proxy though)
    * Email on registration (need `gen_smtp`)
    * Logging stuff (most likely through lager)

The templating system is currently very inefficient, going over the entire
document multiple times, once for each replacement variable. It would be better
to scan for the next `{{`, look for what's between it and `}}`, and then replace
and move on to the next set.

A rework of the current templating system for conditional inclusion would be
very useful. I don't think a full templating engine (e.g. ErlyDTL) is necessary,
but it would reduce the number of round-trips to the server as well as reduce
necessary JS on the client if conditions (mainly on permissions) were added.

It would also probably be nice if including other files were possible, because
the current system where the templates are split up between multiple files is
kinda awkward.

So that ECF is reasonably useable, I'd like to add the following as well:
* Editing posts (and displaying time/user)
* Messaging (needs backend)
* Pagination of threads and posts (former can probably just use
`lists:sublist/3`, but the latter should probably select in Mnesia itself)
* Banning people for a given length of time (needs backend)
* Listing all users, sorting by various fields (partially done)
* List all posts by a user
* Searching (needs backend)

The following may happen at some point, but I'm unsure about them:
* Markdown for posts (could be done with clientside JS)
* Using Argon2 for password hashing instead of PBKDF2

And these would be nice to eventually get:
* Events calendar
* Announcements
* Track read/unread posts
* Replying to posts

### Requirements
Erlang/OTP 20


### Installation
Currently there is no real installation process. However, testing ECF is very
simple since it uses [erlang.mk](https://erlang.mk). Simple `make run` and it
will start it listening on port 8080 and drop you into an Erlang REPL. The first
time you start it up, be sure to use `ecf_db:install().` to create and setup the
database. After that if you change the backing records you will need to either
delete and recreate the Mnesia database, or call `mnesia:transform_table/3`.

You will need to set the following app env variables in rel/private.config:
* `captcha_key`: reCAPTCHA site key
* `captcha_secret`: reCAPTCHA site secret

### Configuration
I'd recommend leaving sys.config alone and editing private.config with all of
your needed configuration.

TODO: Describe all configuration options. Most of them are just simple messages
in case you want to customize them, but a few are actually important.

