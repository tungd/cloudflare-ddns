# Changelog for cloudflare-ddns

## Unreleased changes

## v0.2.0.0

- Add support for updating a list of zones, instead of only one.

  The config format now has to be a list instead of just a record. This is a
  breaking change, please update your configuration file accordingly.

- Add support for using Authorization token, instead of API token.

  You can now specify API key using the `auth` key, instead of `email` + `apikey`.

- Add support for resolving external IP address, not only local ones.

  The `interface` key now accept a `public` keyword, that would resolve to your
  external IP address instead. The IP address is resolved using the
  `resolver1.opendns.com` DNS server from OpenDNS.

## v0.1.0.0

Initial release
