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
* Tests
* Lots and lots of CSS/HTML/JS magic to make it look decent!
* Terms of use and privacy policy?
* Administration frontend
    * Subforum creation
    * Deleting threads/posts
    * Banning accounts
    * Editing permissions
* Security
    * HTTPS
    * Rate limiting for creating threads and posting
    * Email on registration
    * Logging most stuff

So that ECF is reasonably useable, I'd like to add the following as well:
* Editing posts
* Messaging
* Pagination of threads and posts (former can probably just use `lists:sublist/3`,
but the latter should probably use something more performant)
* Banning people for a given length of time (needs backend)
* Listing all users, sorting by various fields
* List all posts by a user
* Searching

The following may happen at some point, but I'm unsure about them:
* Markdown for posts
* Using Argon2 for password hashing instead of PBKDF2
* Possibly an expansion upon the current "template-ish" system, maybe a future
switch to ErlyDTL or something else, but the current method works well enough

And these would be nice to eventually get:
* Events calendar
* Announcements
* Track read/unread posts
* Replying directly to posts

### Requirements
Erlang/OTP 20


### Installation
Currently there is no real installation process. However, testing ECF is very
simple since it uses [erlang.mk](https://erlang.mk). Simple `make run` and it
will start it listening on port 8080 and drop you into an Erlang REPL. The first
time you start it up, be sure to use `ecf_db:install().` followed by
`mnesia:start().` to create and setup the database.

You will need to set the following app env variables as well:
* `captcha_key`: reCAPTCHA site key
* `captcha_secret`: reCAPTCHA site secret

### Configuration
I'd recommend setting all the relevant config options with an overlay file (see
[the Erlang documentation](http://erlang.org/doc/man/config.html) on how to do
that).

TODO: Describe all configuration options. Most of them are just simple messages
in case you want to customize them, but a few are actually important.

