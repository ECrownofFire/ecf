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
    * Banning accounts
    * Editing permissions on forums, threads, and groups
* Security
    * HTTPS (works fine behind nginx reverse proxy though)
    * Logging stuff (most likely through lager)

So that ECF is reasonably useable, I'd like to add the following as well:
* Displaying time/user that edited each post
* Messaging (needs backend)
* Banning people for a given length of time (needs backend)
* Listing all users, sorting by various fields (partially done)
* List all posts by a user
* Searching (needs backend)
* Refreshing sessions
* Pinning threads

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
simple since it uses [erlang.mk](https://erlang.mk). Simply `make run` and it
will start it listening on port 8080 and drop you into an Erlang REPL. The first
time you start it up, be sure to use `ecf_db:install().` to create and setup the
database. After that if you change the backing records you will need to either
delete and recreate the Mnesia database, or call `mnesia:transform_table/3`.

### Configuration
I'd recommend leaving sys.config alone and editing private.config with all of
your needed configuration.

TODO: Describe all configuration options.

You will need to set the following app env variables:
* `captcha_key`: reCAPTCHA site key
* `captcha_secret`: reCAPTCHA site secret
* `host`: The FQDN that the forum is hosted on
* `base_url`: The "base" url of the forum, use a leading slash but no trailing
* `email_addr`: The address to send email as
* `email_relay`: The host to use as a relay
* `email_host`: The host to EHLO as when sending email
* `forum_name`: The name of your forum

