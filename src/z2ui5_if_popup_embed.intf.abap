"! Embedded mode of a popup app: the app renders INTO a page of a host app
"! (a view cluster, a tab of a page) instead of opening as a popup.
"!
"! The host creates the app (factory( ) or CREATE OBJECT), sets
"! mo_parent_view before every call of main( ) and calls main( ) with its own
"! client in each roundtrip:
"!   embed->mo_parent_view = page.
"!   CAST z2ui5_if_app( embed )->main( client ).
"!   IF embed->mv_view_display = abap_true.
"!     CLEAR embed->mv_view_display.
"!     client->view_display( view->stringify( ) ).
"!   ENDIF.
"! Embedded, the app has no OK/Cancel buttons, never calls popup_display( )
"! and never leaves itself with nav_app_leave( ); it handles its first call
"! itself (check_on_init( ) is the host's). Without mo_parent_view the app
"! is the popup it always was.
INTERFACE z2ui5_if_popup_embed PUBLIC.

  "! Set by the host before main( ): the node the app appends its content to
  "! - normally the sap.m.Page of the host view. Unbound: popup mode.
  DATA mo_parent_view  TYPE REF TO z2ui5_cl_ui5_view_builder.
  "! Set by the app: the host must display its view again, the app's content
  "! changed. The host clears it after reading it.
  DATA mv_view_display TYPE abap_bool.

ENDINTERFACE.
