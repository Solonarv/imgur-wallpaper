# Imgur Wallpaper
Use images from imgur as your wallpaper. Tested on an unregistered Windows 7 installation, but it should work on Windows XP and above.

## How to install

You must have `pycurl`, `urlparse` and `imgurpython` installed.

Then, simply clone this repository wherever you want. No installation required.

## How to use

From the command line:

    python wallpaper.py some-file.txt

to extract image URLs from `some-file.txt`, or just

    python wallpaper.py
    
to reuse the image URLs from the last invocation.

The script will then randomly select an image from that list and set it as the desktop background.
It will repeat this every 5 minutes by default. The delay can be adjusted by editing the config file.

## Config format

The config file is located at `%APPDATA%\imgur-wallpaper\config.txt` and uses a simple key-value format. Lines beginning with `#` are ignored.

    # The delay between background changes, in seconds.
    delay=300