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
    * Logging stuff (most likely through lager)
    * Deny non-HTTPS traffic

So that ECF is reasonably useable, I'd like to add the following as well:
* Track read/unread posts (needs backend)
* Banning people for a given length of time
* Listing all users, sorting by various fields (partially done, needs sorting)
* Searching (needs backend)
    * List all posts by a user
* Refreshing sessions
* Pinning threads

The following may happen at some point, but I'm unsure about them:
* Markdown for posts (could be done with clientside JS)

And these would be nice to eventually get:
* Events calendar
* Announcements
* Replying to posts
* Email notifications for posts/replies/threads/forums/PMs

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
* `http`: `false` or a port number.
* `https`: `false` or `{Port, CertFile, KeyFile}`.

So when setting up ECF behind an HTTPS-serving reverse proxy, you'll want to set
`http` to the local port (such as 8080, the default), and `https` to `false`.

If you want ECF to server HTTPS traffic directly, set `http` to `false` and then
`https` to the required things. Note that they must be strings and not binaries.
The PEM file pointed at `CertFile` may also contain the private key. If that's
the case, then `KeyFile` should be `false`.

Please note that ECF does *not* support using HTTP unless behind a reverse proxy
that serves HTTPS. This will not change.

Also note that ECF does not currently support using a HTTPS-serving proxy that
in turn connects to ECF using HTTPS. It should be supported in the future
though.

## License and Third Party Software
ECF is under the AGPL 3.0, you may find a copy of it in LICENSE.

ECF makes use of the following third-party software:

* [iso8601](https://github.com/erlsci/iso8601), MIT license.
* [Jiffy](https://github.com/davisp/jiffy), MIT License.
* [Cowboy](https://github.com/ninenines/cowboy), ISC License.
* [gen_smtp](https://github.com/Vagabond/gen_smtp), BSD 2-Clause License.
* [ErlyDTL](https://github.com/erlydtl/erlydtl), MIT License.
* [parse_trans](https://github.com/uwiger/parse_trans), Apache License 2.0.
* [enacl](https://github.com/jlouis/enacl), MIT License.
* [libsodium](https://github.com/jedisct1/libsodium), ISC License.

