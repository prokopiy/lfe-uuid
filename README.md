# lfe-uuid

[![Build Status][travis badge]][travis]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tag][github tag badge]][github tag]
[![Downloads][hex downloads]][hex package]

[![Project Logo][logo]][logo-large]

*An LFE library*

##### Table of Contents

* [About](#about-)
* [Build](#build-)
* [Start the Project REPL](#start-the-repl-)
* [Tests](#tests-)
* [Usage](#usage-)
* [License](#license-)

## About [&#x219F;](#table-of-contents)

This module implements UUID v4 as of RFC 4122

UUID v1 return a UUID generated by a timestamp and node id.
UUID v3 return a UUID generated using MD5 and a given name within a namespace.
UUID v4 return a UUID generated by a (pseudo) random number generator.
UUID v5 return a UUID generated using SHA1 and a given name within a namespace.

Source tracking available at

    http://github.com/avtobiff/erlang-uuid

## Build [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe compile
```

# Start the Project REPL [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe repl
```

# Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe test
```

## Usage [&#x219F;](#table-of-contents)

TBD

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2020, Prokopiy N. Stelmash <prokopiy@inbox.ru>.

<!-- Named page links below: /-->

[logo]: https://avatars1.githubusercontent.com/u/3434967?s=250
[logo-large]: https://avatars1.githubusercontent.com/u/3434967
[github]: https://github.com/ORG/lfe-uuid
[gitlab]: https://gitlab.com/ORG/lfe-uuid
[travis]: https://travis-ci.org/ORG/lfe-uuid
[travis badge]: https://img.shields.io/travis/ORG/lfe-uuid.svg
[gh-actions-badge]: https://github.com/ORG/lfe-uuid/workflows/Go/badge.svg
[gh-actions]: https://github.com/ORG/lfe-uuid/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.3.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[version]: https://github.com/ORG/lfe-uuid/blob/master/.travis.yml
[github tags]: https://github.com/ORG/lfe-uuid/tags
[github tags badge]: https://img.shields.io/github/tag/ORG/lfe-uuid.svg
[github downloads]: https://img.shields.io/github/downloads/ORG/lfe-uuid/total.svg
[hex badge]: https://img.shields.io/hexpm/v/lfe-uuid.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/lfe-uuid
[hex downloads]: https://img.shields.io/hexpm/dt/lfe-uuid.svg