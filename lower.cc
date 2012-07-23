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

int main(int argc, char **argv) {
  dpy = XOpenDisplay(":0");
  if (dpy == NULL) {
    die("Can't open display");
  }
  log("Display opened");

  if (argc <= 1) {
     fprintf(stderr, "Need window id or window title to lower");
     exit(1);
  }


  char *end;
  int winid = (int) strtol(argv[1], &end, 10);
  if (end[0] == 0) {
    fprintf(stderr, "Window id = 0x%x\n", winid);
    XLowerWindow(dpy, (Window)winid);
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
    if (getName(children[i]) == argv[1]) {
      fprintf(stderr, "Window id = 0x%x\n", (int)children[i]);
      XLowerWindow(dpy, children[i]);
      return 0;
    }
  }

  XFree(children);
}


