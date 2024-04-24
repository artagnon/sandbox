# chwall

A simple Android application written in [Mirah](http://mirah.org).
Fires up the gallery application so you can choose an image to set as
the wallpaper.  It mainly serves to document how to use the Android
API in Mirah.

## Requirements

A phone running JellyBean (4.2).  Failing that, you can change the
target_version in the Rakefile.

## Building

You will require OpenJDK 6 or 7 installed via your distribution's
package manager.

First, get JRuby 1.6.8 (I can't recommend
[rbenv](https://github.com/sstephenson/rbenv) enough):

    $ rbenv install jruby-1.6.8

Next, get rake and mirah:

    $ gem install rake
    $ gem install mirah

Next, get pindah:

    $ git clone git://github.com/mirah/pindah
    $ cd pindah
    $ gem build pindah.gemspec
    $ gem install pindah-0.1.3.dev.gem

(a `gem install pindah` would install an outdated broken version).

You need the Android SDK with platform-tools and API version 4.2
installed.  Connect your phone via your USB cable and run this in the
chwall directory:

    $ rake debug install

You'll now find a "Chwall" application installed on your phone.
