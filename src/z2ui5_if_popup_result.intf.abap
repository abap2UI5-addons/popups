"! The result of a popup - read the same way for every popup of this
"! repository. Back in the calling app:
"!   DATA(popup) = CAST z2ui5_if_popup_result( client->get_app_prev( ) ).
"!   IF popup->check_confirmed( ) = abap_true.
"!     DATA(text) = popup->value_string( ).
"!     DATA(range) = CAST z2ui5_cl_popup_context=>ty_t_range( popup->value( ) ).
"!     LOOP AT range->* INTO DATA(row).
"! Each popup class says above its INTERFACES z2ui5_if_popup_result what
"! value( ) references and what value_string( ) returns.
INTERFACE z2ui5_if_popup_result PUBLIC.

  "! abap_true when the user confirmed the popup - OK, Save, a chosen row,
  "! an option, a transport; abap_false when the user cancelled it or closed
  "! it without a choice. A popup that only shows something has nothing to
  "! decline and returns abap_true.
  METHODS check_confirmed
    RETURNING
      VALUE(result) TYPE abap_bool.

  "! The value of the popup: a reference to the popup's own attribute that
  "! holds it (the entered text, the chosen row, the ranges, ...), typed as
  "! documented in the popup class. Unbound for a popup without a value.
  METHODS value
    RETURNING
      VALUE(result) TYPE REF TO data.

  "! The value as text, for a popup with a single value - the entered text,
  "! the chosen key, the transport request, ...; empty for a popup whose
  "! value is a table or a structure.
  METHODS value_string
    RETURNING
      VALUE(result) TYPE string.

ENDINTERFACE.
