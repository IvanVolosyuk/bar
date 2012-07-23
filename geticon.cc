#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <string>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <png.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

Display *dpy = NULL;
using namespace std;
Atom iconAtom;

void die(const string& msg) {
  fprintf(stderr, "%s\n", msg.c_str());
  exit(1);
}

void die(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
}

void log(const string& msg) {
  fprintf(stderr, "%s\n", msg.c_str());
}

string getName(Window win) {
  Atom propType;
  int format;
  unsigned long nitems_return;
  unsigned long bytes_after_return;
  unsigned char *prop_return;

  int res = XGetWindowProperty(dpy, win, XA_WM_NAME, 0, 4096, false, AnyPropertyType, &propType,
		  &format, &nitems_return, &bytes_after_return, &prop_return);

  if (res != Success) {
     fprintf(stderr, "Name: ???\n");
     return "";
  }

  unsigned char *name = (unsigned char*) malloc(nitems_return + 1);
  memcpy(name, prop_return, nitems_return);
  name[nitems_return] = 0;
  string name2 = (char*) name;
  fprintf(stderr, "Name: %s\n", name);
  XFree(prop_return);
  free(name);
  return name2;
}


/* A coloured pixel. */

typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
    uint8_t alpha;
} pixel_t;

/* A picture. */
    
typedef struct  {
    pixel_t *pixels;
    size_t width;
    size_t height;
} bitmap_t;
    
/* Given "bitmap", this returns the pixel of bitmap at the point 
   ("x", "y"). */

static pixel_t * pixel_at (bitmap_t * bitmap, int x, int y)
{
    return bitmap->pixels + (bitmap->width * y + x) * 2;
}
    
/* Write "bitmap" to a PNG file specified by "path"; returns 0 on
   success, non-zero on error. */

static int save_png_to_file (bitmap_t *bitmap, const char *path)
{
    FILE * fp;
    png_structp png_ptr = NULL;
    png_infop info_ptr = NULL;
    size_t x, y;
    png_byte ** row_pointers = NULL;
    /* "status" contains the return value of this function. At first
       it is set to a value which means 'failure'. When the routine
       has finished its work, it is set to a value which means
       'success'. */
    int status = -1;
    /* The following number is set by trial and error only. I cannot
       see where it it is documented in the libpng manual.
    */
    int pixel_size = 4;
    int depth = 8;
    
    fp = fopen (path, "wb");
    if (! fp) {
        goto fopen_failed;
    }

    png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (png_ptr == NULL) {
        goto png_create_write_struct_failed;
    }
    
    info_ptr = png_create_info_struct (png_ptr);
    if (info_ptr == NULL) {
        goto png_create_info_struct_failed;
    }
    
    /* Set up error handling. */

    if (setjmp (png_jmpbuf (png_ptr))) {
        goto png_failure;
    }
    
    /* Set image attributes. */

    png_set_IHDR (png_ptr,
                  info_ptr,
                  bitmap->width,
                  bitmap->height,
                  depth,
                  PNG_COLOR_TYPE_RGB_ALPHA,
                  PNG_INTERLACE_NONE,
                  PNG_COMPRESSION_TYPE_DEFAULT,
                  PNG_FILTER_TYPE_DEFAULT);
    
    /* Initialize rows of PNG. */

    row_pointers = (png_byte**) png_malloc (png_ptr, bitmap->height * sizeof (png_byte *));
    for (y = 0; y < bitmap->height; ++y) {
        png_byte *row = (png_byte*)
            png_malloc (png_ptr, sizeof (uint8_t) * bitmap->width * pixel_size);
        row_pointers[y] = row;
        for (x = 0; x < bitmap->width; ++x) {
            pixel_t * pixel = pixel_at (bitmap, x, y);
            *row++ = pixel->red;
            *row++ = pixel->green;
            *row++ = pixel->blue;
            *row++ = pixel->alpha;
        }
    }
    
    /* Write the image data to "fp". */

    png_init_io (png_ptr, fp);
    png_set_rows (png_ptr, info_ptr, row_pointers);
    png_write_png (png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);

    /* The routine has successfully written the file, so we set
       "status" to a value which indicates success. */

    status = 0;
    
    for (y = 0; y < bitmap->height; y++) {
        png_free (png_ptr, row_pointers[y]);
    }
    png_free (png_ptr, row_pointers);
    
 png_failure:
 png_create_info_struct_failed:
    png_destroy_write_struct (&png_ptr, &info_ptr);
 png_create_write_struct_failed:
    fclose (fp);
 fopen_failed:
    return status;
}

void queryWindow(int i, Window win) {
  Atom propType;
  int format;
  unsigned long nitems_return;
  unsigned long bytes_after_return;
  unsigned char *prop_return;

  int res = XGetWindowProperty(dpy, win, iconAtom, 0, 1000000, false, AnyPropertyType, &propType,
		  &format, &nitems_return, &bytes_after_return, &prop_return); // FIXME: cleanup
  fprintf(stderr, "res = %d\n", res);
  fprintf(stderr, "format = %d\n", format);
  fprintf(stderr, "nitems_return = %ld\n", nitems_return);
  fprintf(stderr, "bytes_after_return = %ld\n", bytes_after_return);
  string name = getName(win);

  if (format != 32) {
    ("format = %d\n", format);
  }

  int *pixels = (int*)prop_return;
  size_t n = (size_t) nitems_return;

  for (int j = 0; n > 0; j++) {
    if (n < 4) {
      die("n == %d\n", n);
    }

    fprintf(stderr, "%d %d %d %d %d %d %d \n",
        pixels[0],
        pixels[1],
        pixels[2],
        pixels[3],
        pixels[4],
        pixels[5],
        pixels[6],
        pixels[7]);


    bitmap_t icon;
    icon.width = (size_t) pixels[0];
    icon.height = (size_t) pixels[2];
    icon.pixels = (pixel_t*)(pixels + 4);
    fprintf(stderr, "Size: %ldx%ld\n", icon.width, icon.height);
    size_t npixels = icon.width * icon.height;
    if (npixels > n) {
      fprintf(stderr, "image pixels = %ld, remaining pixels = %ld\n", npixels, n);
    }
    if (npixels / icon.width != icon.height) {
      die("broken image");
    }
    char filename[16];
    snprintf(filename, 32, "icon%x_%d.png\0", (int)win, j);
    save_png_to_file (&icon, filename);

    n -= 2 + npixels;
    pixels += 4 + npixels * 2;
  }
}


void dumpIcon(Window win) {
  Atom propType;
  int format;
  unsigned long nitems_return;
  unsigned long bytes_after_return;
  unsigned char *prop_return;

  int res = XGetWindowProperty(dpy, win, iconAtom, 0, 1000000, false, AnyPropertyType, &propType,
		  &format, &nitems_return, &bytes_after_return, &prop_return);
  fprintf(stderr, "res = %d\n", res);
  fprintf(stderr, "format = %d\n", format);
  fprintf(stderr, "nitems_return = %ld\n", nitems_return);
  fprintf(stderr, "bytes_after_return = %ld\n", bytes_after_return);
  string name = getName(win);

  if (format != 32) {
    die("format = %d\n", format);
  }

  int *pixels = (int*)prop_return;
  size_t n = (size_t) nitems_return;

  bitmap_t match;
  match.width = 0;
  match.height = 0;
  match.pixels = NULL;

  for (int j = 0; n > 0; j++) {
    if (n < 4) {
      die("n == %d\n", n);
    }

    bitmap_t icon;
    icon.width = (size_t) pixels[0];
    icon.height = (size_t) pixels[2];
    icon.pixels = (pixel_t*)(pixels + 4);
    fprintf(stderr, "Size: %ldx%ld\n", icon.width, icon.height);
    size_t npixels = icon.width * icon.height;
    if (npixels > n) {
      fprintf(stderr, "image pixels = %ld, remaining pixels = %ld\n", npixels, n);
    }
    if (npixels / icon.width != icon.height) {
      die("broken image");
    }
    n -= 2 + npixels;
    pixels += 4 + npixels * 2;

    if ((icon.width > 24) == (icon.width < match.width)) {
      match = icon;
    }
  }

  for (int y = 0; y < match.height; ++y) {
    for (int x = 0; x < match.width; ++x) {
      pixel_t *pixel = pixel_at (&match, x, y);
      printf("%02x%02x%02x%02x", pixel->red, pixel->green, pixel->blue, pixel->alpha);
    }
  }
}


int main(int argc, char **argv) {
  dpy = XOpenDisplay(":0");
  if (dpy == NULL) {
    die("Can't open display");
  }
  log("Display opened");

  iconAtom = XInternAtom(dpy, "_NET_WM_ICON", false);

  if (argc > 1) {
    int winid = atoi(argv[1]);
    fprintf(stderr, "Window id = 0x%x\n", winid);

    dumpIcon((Window)winid);
    return 0;
  }

  Window w = DefaultRootWindow(dpy);
  Window root_return;
  Window parent;
  Window *children;
  unsigned int nchildren;

  Status status = XQueryTree(dpy, w, &root_return, &parent, &children, &nchildren);
  
  fprintf(stderr, "Num children: %d\n", nchildren);
  for (int i = 0; i < nchildren; i++) {
    fprintf(stderr, "** Name: %s\n", getName(children[i]).c_str());
    queryWindow(i, children[i]);
  }

  XFree(children);
}


