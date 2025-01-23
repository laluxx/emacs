/* ctrl-quit-module.c */
#include <emacs-module.h>
#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#include <pthread.h>
#include <stdbool.h>
#include <unistd.h> 

int plugin_is_GPL_compatible;
static pthread_t x_thread;
static bool running = false;
static pthread_mutex_t env_mutex = PTHREAD_MUTEX_INITIALIZER;
static struct emacs_runtime *global_ert;

/* Helper function to safely call Emacs functions */
static void
call_emacs_function(const char *func_name)
{
    pthread_mutex_lock(&env_mutex);
    emacs_env *env = global_ert->get_environment(global_ert);
    
    emacs_value Qfuncall = env->intern(env, "funcall");
    emacs_value Qfunc = env->intern(env, func_name);
    emacs_value args[] = { Qfunc };
    
    env->funcall(env, Qfuncall, 1, args);
    pthread_mutex_unlock(&env_mutex);
}

static void *
x_event_loop(void *arg)
{
    (void)arg;
    Display *dpy = XOpenDisplay(NULL);
    if (!dpy) return NULL;
    
    Window root = DefaultRootWindow(dpy);
    XGrabKey(dpy, XKeysymToKeycode(dpy, XK_Control_L), 
             AnyModifier, root, True, GrabModeAsync, GrabModeAsync);
    XGrabKey(dpy, XKeysymToKeycode(dpy, XK_Control_R),
             AnyModifier, root, True, GrabModeAsync, GrabModeAsync);

    XEvent ev;
    while (running) {
        if (XPending(dpy)) {
            XNextEvent(dpy, &ev);
            if (ev.type == KeyRelease) {
                KeySym ks = XkbKeycodeToKeysym(dpy, ev.xkey.keycode, 0, 0);
                if (ks == XK_Control_L || ks == XK_Control_R) {
                    call_emacs_function("ctrl-quit-handler");
                }
            }
        }
        usleep(10000);  // Sleep for 10ms to reduce CPU usage
    }

    XUngrabKey(dpy, AnyKey, AnyModifier, root);
    XCloseDisplay(dpy);
    return NULL;
}

static emacs_value
Fctrl_quit_init_module(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    (void)nargs; (void)args; (void)data;
    
    if (!running) {
        running = true;
        if (pthread_create(&x_thread, NULL, x_event_loop, NULL) != 0) {
            running = false;
            return env->intern(env, "nil");
        }
    }
    return env->intern(env, "t");
}

static emacs_value
Fctrl_quit_cleanup_module(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    (void)nargs; (void)args; (void)data;
    
    if (running) {
        running = false;
        pthread_join(x_thread, NULL);
    }
    return env->intern(env, "t");
}

static void
provide(emacs_env *env, const char *feature)
{
    emacs_value Qfeat = env->intern(env, feature);
    emacs_value Qprovide = env->intern(env, "provide");
    emacs_value args[] = { Qfeat };
    env->funcall(env, Qprovide, 1, args);
}

static void
bind_function(emacs_env *env, const char *name, emacs_value (*fun)(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data))
{
    emacs_value Qfset = env->intern(env, "fset");
    emacs_value Qsym = env->intern(env, name);
    emacs_value Qfun = env->make_function(env, 0, 0, fun, NULL, NULL);
    
    emacs_value args[] = { Qsym, Qfun };
    env->funcall(env, Qfset, 2, args);
}

int
emacs_module_init(struct emacs_runtime *ert)
{
    emacs_env *env = ert->get_environment(ert);
    global_ert = ert;

    bind_function(env, "ctrl-quit-init-module", Fctrl_quit_init_module);
    bind_function(env, "ctrl-quit-cleanup-module", Fctrl_quit_cleanup_module);
    provide(env, "ctrl-quit-module");

    return 0;
}
