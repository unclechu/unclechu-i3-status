# unclechu-i3-status

My own status line generator for [i3 window manager][i3wm].

It also closely integrated with my own software such as
[xlib-keys-hack][xlib-keys-hack].
It gets states of indicators using specific [D-Bus][dbus] interface,
but any other application could implement such interface instead of
[xlib-keys-hack][xlib-keys-hack] so it isn't hardly attached.

# Usage

```bash
stack build
stack install
```

At this point make sure you have `~/.local/bin` in `PATH` environment variable
when [i3][i3wm] starts. You could do so by adding this to your `~/.profile`:

```sh
export PATH="$HOME/.local/bin:$PATH"
```

But `~/.profile` must be evaluated at X11 session initialization step in your
X11 config, for example it could be:

```bash
grep -F '.profile' /etc/X11/xinit/xinitrc-common
# [ -r $HOME/.profile ] && . $HOME/.profile
```

Then, when you make sure `unclechu-i3-status` executable is available when
[i3][i3wm] starts, you could add this to `~/.config/i3/config`:

```conf
bar {
  status_command unclechu-i3-status
}
```

# Author

Viacheslav Lotsmanov

# License

[GPLv3](LICENSE)

[i3wm]: https://i3wm.org/
[xlib-keys-hack]: https://github.com/unclechu/xlib-keys-hack
[dbus]: https://www.freedesktop.org/wiki/Software/dbus/
