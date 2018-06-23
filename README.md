# Erlang Creative Forum
## Simple BEAM-powered forum software

### About

Erlang Creative Forum (ECF) is intended to be a very simple forum host, similar
to SMF or vBulletin or the like, but very lightweight and fast. It's still very
early in development so it's missing a lot of features, and I would (currently)
recommend against using it. Feel free to take a look around the code though!

Just keep in mind that this is a big giant mess of testing random junk so
everything is super inconsistent and weird :P

At the bare minimum, ECF still needs the following:
* Documentation
* Tests
* Lots and lots of CSS/HTML/JS magic to make it look decent!
* Terms of use and privacy policy?
* Limit lengths on various things
* Administration frontend
    * Subforum reordering
    * Banning accounts
    * Editing permissions
* Security
    * HTTPS (works fine behind nginx reverse proxy though)
    * Email on registration (need `gen_smtp`)
    * Logging stuff (most likely through lager)

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

