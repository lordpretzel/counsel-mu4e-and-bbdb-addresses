[![License: GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
<!-- [![GitHub release](https://img.shields.io/github/release/lordpretzel/counsel-mu4e-and-bbdb-addresses.svg?maxAge=86400)](https://github.com/lordpretzel/counsel-mu4e-and-bbdb-addresses/releases) -->
<!-- [![MELPA Stable](http://stable.melpa.org/packages/counsel-mu4e-and-bbdb-addresses-badge.svg)](http://stable.melpa.org/#/counsel-mu4e-and-bbdb-addresses) -->
<!-- [![MELPA](http://melpa.org/packages/counsel-mu4e-and-bbdb-addresses-badge.svg)](http://melpa.org/#/counsel-mu4e-and-bbdb-addresses) -->
[![Build Status](https://secure.travis-ci.org/lordpretzel/counsel-mu4e-and-bbdb-addresses.png)](http://travis-ci.org/lordpretzel/counsel-mu4e-and-bbdb-addresses)


# counsel-mu4e-and-bbdb-addresses

Small library for adding and removing advice to functions.

## Installation

<!-- ### MELPA -->

<!-- Symbol’s value as variable is void: $1 is available from MELPA (both -->
<!-- [stable](http://stable.melpa.org/#/counsel-mu4e-and-bbdb-addresses) and -->
<!-- [unstable](http://melpa.org/#/counsel-mu4e-and-bbdb-addresses)).  Assuming your -->
<!-- ((melpa . https://melpa.org/packages/) (gnu . http://elpa.gnu.org/packages/) (org . http://orgmode.org/elpa/)) lists MELPA, just type -->

<!-- ~~~sh -->
<!-- M-x package-install RET counsel-mu4e-and-bbdb-addresses RET -->
<!-- ~~~ -->

<!-- to install it. -->

### Quelpa

Using [use-package](https://github.com/jwiegley/use-package) with [quelpa](https://github.com/quelpa/quelpa).

~~~elisp
(use-package
:quelpa ((counsel-mu4e-and-bbdb-addresses
:fetcher github
:repo "lordpretzel/counsel-mu4e-and-bbdb-addresses")
:upgrade t)
)
~~~

### straight

Using [use-package](https://github.com/jwiegley/use-package) with [straight.el](https://github.com/raxod502/straight.el)

~~~elisp
(use-package counsel-mu4e-and-bbdb-addresses
:straight (counsel-mu4e-and-bbdb-addresses :type git :host github :repo "lordpretzel/counsel-mu4e-and-bbdb-addresses")
~~~

### Source

Alternatively, install from source. First, clone the source code:

~~~sh
cd MY-PATH
git clone https://github.com/lordpretzel/counsel-mu4e-and-bbdb-addresses.git
~~~

Now, from Emacs execute:

~~~
M-x package-install-file RET MY-PATH/counsel-mu4e-and-bbdb-addresses
~~~

Alternatively to the second step, add this to your Symbol’s value as variable is void: \.emacs file:

~~~elisp
(add-to-list 'load-path "MY-PATH/counsel-mu4e-and-bbdb-addresses")
(require 'counsel-mu4e-and-bbdb-addresses)
~~~
