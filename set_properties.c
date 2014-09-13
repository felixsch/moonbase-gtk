/* This function is based on Jan Vornbergs great taffybar implementation
 * and was released under a BSD-3 style licence.
 *
 * checkout the original source at: https://github.com/travitch/taffybar/blob/master/src/gdk_property_change_wrapper.c
 */

#include <gtk/gtk.h>
#include <gdk/gdk.h>


void set_properties( GtkWindow *window, long *data ) {
    GdkWindow *gdkWindow = gtk_widget_get_window(GTK_WIDGET(window));
    gdk_property_change( gdkWindow,
                         gdk_atom_intern( "_NET_WM_STRUT_PARTIAL" , FALSE ),
                         gdk_atom_intern( "CARDINAL" , FALSE ),
                         32,
                         GDK_PROP_MODE_REPLACE,
                         (unsigned char *)data,
                         12);
}
