"! A sap.m.MessagePopover for any messages: BAPIRET2 tables, sy, exceptions,
"! T100 and every other format z2ui5_cl_popup_messages takes. The popover
"! opens by a control of the calling app's view (i_open_by_id) and filters by
"! message type with a count per type, as MessagePopover does. Closing it -
"! a click outside or Esc - returns to the calling app, with i_event_close
"! when given.
"!
"! An app that keeps the popover in its own view (to toggle it from a button,
"! say) takes the parts instead: msg_get_t( ) converts the messages into the
"! item table it binds, count_get( ) gives the counts and the worst type for
"! the button, popover_xml( ) builds the popover over that binding.
CLASS z2ui5_cl_popup_msgpopover DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    " check_confirmed( ) - always abap_true, the popover only shows messages;
    " value( ) - REF TO ty_t_msg, the messages shown (mt_msg);
    " value_string( ) - empty
    INTERFACES z2ui5_if_popup_result.

    TYPES:
      "! one item of the popover - the components the MessageItem binds
      BEGIN OF ty_s_msg,
        "! Error, Warning, Success or Information (sap.ui.core.MessageType)
        type        TYPE string,
        title       TYPE string,
        "! message class and number, empty without a message class
        subtitle    TYPE string,
        description TYPE string,
        "! the type again, as the groupName of the item
        group       TYPE string,
        id          TYPE string,
        number      TYPE string,
      END OF ty_s_msg.
    TYPES ty_t_msg TYPE STANDARD TABLE OF ty_s_msg WITH DEFAULT KEY.

    TYPES:
      "! the messages per type, and the worst type of them with its icon -
      "! for the button that opens the popover
      BEGIN OF ty_s_count,
        error       TYPE i,
        warning     TYPE i,
        success     TYPE i,
        information TYPE i,
        total       TYPE i,
        "! the worst type - Error before Warning before Success before
        "! Information; empty without messages
        type        TYPE string,
        icon        TYPE string,
      END OF ty_s_count.

    CONSTANTS:
      "! sap.m.VerticalPlacementType - where the popover opens
      BEGIN OF cs_placement,
        vertical TYPE string VALUE `Vertical`,
        top      TYPE string VALUE `Top`,
        bottom   TYPE string VALUE `Bottom`,
      END OF cs_placement.

    DATA mt_msg TYPE ty_t_msg.

    "! @parameter i_messages | BAPIRET2/BAPIRETTAB, sy, an exception, T100,
    "!                         a string table - as z2ui5_cl_popup_messages
    "! @parameter i_open_by_id | the id of the control in the calling app's
    "!                         view the popover opens by - usually the button
    "!                         whose press event called it
    "! @parameter i_placement | a value of cs_placement
    "! @parameter i_event_close | raised in the calling app when the popover
    "!                         closes; without it the calling app is simply
    "!                         returned to (check_on_navigated)
    CLASS-METHODS factory
      IMPORTING
        i_messages      TYPE any
        i_open_by_id    TYPE clike
        i_placement     TYPE clike DEFAULT cs_placement-vertical
        i_event_close   TYPE clike OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_popup_msgpopover.

    "! the messages as the items of the popover, in the order given
    CLASS-METHODS msg_get_t
      IMPORTING
        i_messages    TYPE any
      RETURNING
        VALUE(result) TYPE ty_t_msg.

    CLASS-METHODS count_get
      IMPORTING
        it_msg        TYPE ty_t_msg
      RETURNING
        VALUE(result) TYPE ty_s_count.

    "! The popover as XML (a core:FragmentDefinition) for
    "! client->popover_display( ), for an app that shows it in its own view.
    "! @parameter i_items | the binding of the app's item table (ty_t_msg):
    "!                      client->_bind( mt_msg )
    "! @parameter i_event_close | the afterClose event: client->_event( ... )
    "! @parameter i_placement | a value of cs_placement
    CLASS-METHODS popover_xml
      IMPORTING
        i_items       TYPE clike
        i_event_close TYPE clike OPTIONAL
        i_placement   TYPE clike DEFAULT cs_placement-vertical
      RETURNING
        VALUE(result) TYPE string.

    "! the counts of the messages of the popover
    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_count.

  PROTECTED SECTION.
    DATA client      TYPE REF TO z2ui5_if_client.
    DATA open_by_id  TYPE string.
    DATA placement   TYPE string.
    DATA event_close TYPE string.

    METHODS view_display.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_msgpopover IMPLEMENTATION.

  METHOD factory.

    CREATE OBJECT r_result.
    r_result->mt_msg      = msg_get_t( i_messages ).
    r_result->open_by_id  = i_open_by_id.
    r_result->placement   = i_placement.
    r_result->event_close = i_event_close.

  ENDMETHOD.

  METHOD msg_get_t.

    DATA ls_msg TYPE ty_s_msg.

    DATA temp28 TYPE z2ui5_cl_popup_context=>ty_t_msg.
    DATA temp4 LIKE LINE OF temp28.
    DATA lr_row LIKE REF TO temp4.
    temp28 = z2ui5_cl_popup_context=>msg_get_t( i_messages ).


    LOOP AT temp28 REFERENCE INTO lr_row.

      CLEAR ls_msg.
      ls_msg-type        = z2ui5_cl_popup_context=>ui5_get_msg_type( lr_row->type ).
      ls_msg-title       = lr_row->text.
      ls_msg-description = lr_row->text.
      ls_msg-group       = ls_msg-type.
      ls_msg-id          = lr_row->id.
      ls_msg-number      = lr_row->no.
      IF lr_row->id IS NOT INITIAL.
        ls_msg-subtitle = |{ lr_row->id } { lr_row->no }|.
      ENDIF.
      INSERT ls_msg INTO TABLE result.

    ENDLOOP.

  ENDMETHOD.

  METHOD count_get.

    DATA temp29 LIKE LINE OF it_msg.
    DATA lr_msg LIKE REF TO temp29.
    LOOP AT it_msg REFERENCE INTO lr_msg.
      CASE lr_msg->type.
        WHEN z2ui5_cl_popup_context=>cs_ui5_msg_type-e.
          result-error = result-error + 1.
        WHEN z2ui5_cl_popup_context=>cs_ui5_msg_type-w.
          result-warning = result-warning + 1.
        WHEN z2ui5_cl_popup_context=>cs_ui5_msg_type-s.
          result-success = result-success + 1.
        WHEN OTHERS.
          result-information = result-information + 1.
      ENDCASE.
    ENDLOOP.

    result-total = lines( it_msg ).

    IF result-error > 0.
      result-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-e.
      result-icon = `sap-icon://message-error`.
    ELSEIF result-warning > 0.
      result-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-w.
      result-icon = `sap-icon://message-warning`.
    ELSEIF result-success > 0.
      result-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-s.
      result-icon = `sap-icon://message-success`.
    ELSEIF result-information > 0.
      result-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-i.
      result-icon = `sap-icon://message-information`.
    ENDIF.

  ENDMETHOD.

  METHOD result.

    result = count_get( mt_msg ).

  ENDMETHOD.

  METHOD popover_xml.

    DATA popover TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp30 TYPE string.
    popover = z2ui5_cl_ui5_view_builder=>factory(
                        )->ele( n = `FragmentDefinition` ns = `core`
                        )->a( n = `xmlns` v = `sap.m`
                        )->a( n = `xmlns:core` v = `sap.ui.core` ).

    " the caller's own binding and event, built with _bind( ) and _event( )
    " abap2ui5lint-disable-next-line unescaped-text-in-attribute -- a binding path from client->_bind( )
    popover = popover->ele( `MessagePopover` )->a( n = `items` v = i_items ).

    IF i_placement = cs_placement-top.
      temp30 = `Top`.
    ELSEIF i_placement = cs_placement-bottom.
      temp30 = `Bottom`.
    ELSE.
      temp30 = `Vertical`.
    ENDIF.
    popover->a( n = `placement` v = temp30 ).
    popover->a( n = `initiallyExpanded` b = abap_true ).
    IF i_event_close IS NOT INITIAL.
      " abap2ui5lint-disable-next-line unescaped-text-in-attribute -- an event from client->_event( )
      popover->a( n = `afterClose` v = i_event_close ).
    ENDIF.

    popover->ele( `MessageItem`
        )->a( n = `type` v = `{TYPE}`
        )->a( n = `title` v = `{TITLE}`
        )->a( n = `subtitle` v = `{SUBTITLE}`
        )->a( n = `description` v = `{DESCRIPTION}`
        )->a( n = `groupName` v = `{GROUP}` ).

    result = popover->stringify( ).

  ENDMETHOD.

  METHOD view_display.

    DATA popover TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp31 TYPE string.
    popover = z2ui5_cl_ui5_view_builder=>factory(
                        )->ele( n = `FragmentDefinition` ns = `core`
                        )->a( n = `xmlns` v = `sap.m`
                        )->a( n = `xmlns:core` v = `sap.ui.core` ).


    IF placement = cs_placement-top.
      temp31 = `Top`.
    ELSEIF placement = cs_placement-bottom.
      temp31 = `Bottom`.
    ELSE.
      temp31 = `Vertical`.
    ENDIF.
    popover->ele( `MessagePopover`
        )->a( n = `items` v = client->_bind( mt_msg )
        )->a( n = `placement` v = temp31
        )->a( n = `initiallyExpanded` b = abap_true
        )->a( n = `afterClose` v = client->_event( `MSGPOPOVER_CLOSE` )
        )->ele( `MessageItem`
        )->a( n = `type` v = `{TYPE}`
        )->a( n = `title` v = `{TITLE}`
        )->a( n = `subtitle` v = `{SUBTITLE}`
        )->a( n = `description` v = `{DESCRIPTION}`
        )->a( n = `groupName` v = `{GROUP}` ).

    client->popover_display( xml   = popover->stringify( )
                             by_id = open_by_id ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      view_display( ).
      RETURN.
    ELSEIF client->check_on_navigated( ) IS NOT INITIAL.
      view_display( ).
    ENDIF.

    IF client->check_on_event( `MSGPOPOVER_CLOSE` ) IS NOT INITIAL.
      client->popover_destroy( ).
      IF event_close IS INITIAL.
        client->nav_app_leave( ).
      ELSE.
        client->nav_app_leave( event = event_close ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~check_confirmed.

    " shows something only - nothing to decline
    result = abap_true.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value.

    GET REFERENCE OF mt_msg INTO result.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value_string.

    " no single value - empty
    CLEAR result.

  ENDMETHOD.

ENDCLASS.
