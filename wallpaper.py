from imgurpython    import *
from urllib.parse   import urlparse
from sys            import argv
from collections    import defaultdict
from time           import sleep
from random         import choice
from subprocess     import call

import os.path
import os
import sys
import platform
import pycurl

system = None
is_64_bit = False # Assume 32-bit by default

if sys.platform == "win32":
    system = "win32"

if platform.machine().endswith("64"): # As per http://stackoverflow.com/a/12578715, should work for most platforms
    is_64_bit = True
    
if system in ["win32"]:
    import ctypes

client_id = 'bbda1ffa8243b11'
client_secret = '084a967901a372ad0e13f315110a3ccc7a41fcc2'

baseurl = 'http://i.imgur.com/'

root = os.path.join(os.environ['APPDATA'], 'imgur-wallpaper')
imglist = os.path.join(root, 'cached-urls.txt')
cfg = os.path.join(root, 'config.txt')
imgcache = os.path.join(root, 'imgcache')
albumcache = os.path.join(root, 'albumcache')

class Config(object):
    default_delay = 300
    def __init__(self, cfg_file = None):
        self.delay = Config.default_delay
        if cfg_file is not None and os.path.exists(cfg_file):
            with open(cfg_file) as fh:
                for line in fh:
                    line = line.strip()
                    if line[0] == '#':
                        continue
                    parts = line.split('=', 1)
                    var = parts[0].strip()
                    val = parts[1].strip()
                    if var == "delay":
                        self.delay = int(val)
                    

def main():
    client = ImgurClient(client_id, client_secret)
    
    config = Config(cfg)

    if not os.path.exists(imgcache):
        os.makedirs(imgcache)
    if not os.path.exists(albumcache):
        os.makedirs(albumcache)
    if not os.path.exists(cfg):
        with open(cfg, 'w') as f:
            f.write('')
    
    if len(argv)<2 or argv[1] == '':
        print("No URL file specified. Using cached image list.")
        images = get_cached()
    else:
        print("Reading imgur URLs from %s" % argv[1])
        images = get_images(argv[1], client)
        cache_imgs(images)
    
    while True:
        current_img = choice(images)
        set_bg(current_img)
        sleep(config.delay)

def expand(t, client):
        ty, pt = t
        print("Expanding", t)
        if ty == 'r':
            return [baseurl + pt]
        elif ty == 'i':
            return [client.get_image(pt).link]
        elif ty == 'g':
            gal = client.gallery_item(pt)
            if gal.is_album:
                return expand(('a', gal.id), client)
            else:
                return [gal.link]
        elif ty == 'a':
            alpath = os.path.join(albumcache, pt + '.txt')
            if os.path.exists(alpath):
                print("Using cached album at %s" % alpath)
                with open(alpath) as f:
                    return [l.strip() for l in f]
            else:
                al = client.get_album(pt)
                imgs = [i['link'] for i in al.images]
                with open(alpath, 'w') as f:
                    f.writelines('\n'.join(imgs))
                return imgs

def get_images(fname, client):
    print("Reading URLs from %s" % fname)
    targets = []
    with open(fname) as file:
        targets = [canonicalize(url.strip()) for url in file]
        targets = [t for t in targets if t is not None]
        print(targets)
    
    if not targets:
        print("No valid URLs found. Exiting.")
        exit(1)
    images = set()
    
    for t in targets:
        for i in expand(t, client):
            images.add(i)
    
    return tuple(images)

def canonicalize(url):
    path = urlparse(url).path
    if len(path) == 0:
        return None
    if path.startswith('i.'):
        path = path[2:]
    if path.startswith('www.'):
        path = path[4:]
    if path.startswith("imgur.com"):
        path = path[9:]
    if path[0] == '/':
        path = path[1:]
    if path.startswith('a/'):
        return ('a', path[2:])
    elif path.startswith('gallery/'):
        return ('g', path[8:])
    elif '.' in path:
        return ('r', path)
    else:
        return ('i', path)

def cache_imgs(images):
    with open(imglist, 'w') as f:
        f.writelines('\n'.join(images))

def get_cached():
    with open(imglist) as f:
        return [l.strip() for l in f]

def set_bg(img):
    img_name = img.split('/')[-1]
    img_path = os.path.join(imgcache, img_name)
    if not os.path.exists(img_path):
        print("downloading %s" % img)
        with open(img_path, 'wb') as f:
            c = pycurl.Curl()
            c.setopt(c.URL, img)
            c.setopt(c.WRITEDATA, f)
            c.perform()
            c.close()
    if system == "win32":
        setbg_win32(img_path)
    else:
        print("Your OS ({}) isn't supported yet or there was an error.".format(sys.platform))
        exit(1)
    

def setbg_win32(img_path):
    user32 = ctypes.windll.user32
    spi = user32.SystemParametersInfoW if is_64_bit else user32.SystemParametersInfoA
    # Newer Windows versions don't need BMP conversion anymore, but it doesn't really hurt either
    bmp_path = os.path.join(imgcache, "_background.bmp")
    # Remove the old file first, to make sure conversion doesn't trip up
    if os.path.exists(bmp_path):
        os.remove(bmp_path)
    # Call out to ffmpeg for conversion
    print("converting {} to BMP format".format(img_path))
    call(['ffmpeg', '-v', 'quiet', '-i', img_path, bmp_path])
    SPI_SETDESKWALLPAPER = 20
    print("setting background to {}".format(bmp_path))
    spi(SPI_SETDESKWALLPAPER, 0, bmp_path , 3)
    
if __name__ == '__main__':
    main()