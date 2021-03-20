# unclechu-i3-status

My own status line generator for [i3 window manager][i3wm].

It also closely integrated with my own software such as
[xlib-keys-hack][xlib-keys-hack].
It gets state of indicators using specific [D-Bus][dbus] interface,
but any other application could implement such interface instead of
[xlib-keys-hack][xlib-keys-hack].

## Usage

### With NixOS

Clone this repo, `cd` to it and run:

``` sh
nix-shell --run unclechu-i3-status
```

See also [shell.nix]’s arguments for available options.
For instance if you want to enter a `nix-shell` with `cabal` available:

``` sh
nix-shell --arg withCabal true
```

#### NixOS

You can add this application into your NixOS `configuration.nix` like this:

``` nix
{ pkgs, ... }:
let
  unclechu-i3-status-src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "unclechu-i3-status";
    rev = "ffffffffffffffffffffffffffffffffffffffff"; # Git commit hash
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };

  unclechu-i3-status = pkgs.callPackage unclechu-i3-status-src {};
in
{
  environment.systemPackages = [ unclechu-i3-status ];
}
```

#### Use it with [i3wm]

Since you probably would need it to use with i3wm only then it makes sense to
make this dependency be specifically addressed to [i3wm] (in your
`/etc/nixos/configuration.nix`):

``` nix
{ pkgs, ... }:
let
  unclechu-i3-status-src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "unclechu-i3-status";
    rev = "ffffffffffffffffffffffffffffffffffffffff"; # Git commit hash
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };

  unclechu-i3-status = pkgs.callPackage unclechu-i3-status-src {};
in
{
  services.xserver.windowManager.i3.extraPackages = [
    pkgs.i3status
    unclechu-i3-status
  ];
}
```

### With Stack

``` sh
stack build
stack install
```

At this point make sure you have `~/.local/bin` in `PATH` environment variable
when [i3][i3wm] starts. You could do so by adding this to your `~/.profile`:

``` sh
export PATH="$HOME/.local/bin:$PATH"
```

But `~/.profile` must be evaluated at X11 session initialization step in your
X11 config, for example it could be:

``` sh
grep -F '.profile' /etc/X11/xinit/xinitrc-common
## [ -r $HOME/.profile ] && . $HOME/.profile
```

Then, when you make sure `unclechu-i3-status` executable is available when
[i3][i3wm] starts, you could add this to `~/.config/i3/config`:

``` conf
bar {
  status_command unclechu-i3-status
}
```

## Author

Viacheslav Lotsmanov

## License

[GPLv3](LICENSE)

[i3wm]: https://i3wm.org/
[xlib-keys-hack]: https://github.com/unclechu/xlib-keys-hack
[dbus]: https://www.freedesktop.org/wiki/Software/dbus/

[shell.nix]: shell.nix
