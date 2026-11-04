CLASS z2ui5_cl_popup_messages DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_msg,
        type       TYPE string,
        id         TYPE string,
        title      TYPE string,
        subtitle   TYPE string,
        number     TYPE string,
        message    TYPE string,
        message_v1 TYPE string,
        message_v2 TYPE string,
        message_v3 TYPE string,
        message_v4 TYPE string,
        group      TYPE string,
      END OF ty_s_msg.
    TYPES ty_t_msg TYPE STANDARD TABLE OF ty_s_msg WITH DEFAULT KEY.

    DATA mt_msg TYPE ty_t_msg.

    CLASS-METHODS factory
      IMPORTING
        i_messages      TYPE any
        i_title         TYPE string DEFAULT `abap2UI5 - Message Popup`
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_popup_messages.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    DATA title  TYPE string.

    METHODS view_display.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_popup_messages IMPLEMENTATION.

  METHOD factory.
    DATA temp16 TYPE z2ui5_cl_popup_context=>ty_t_msg.
    DATA temp2 LIKE LINE OF temp16.
    DATA lr_row LIKE REF TO temp2.
      DATA temp17 TYPE ty_s_msg.

    CREATE OBJECT r_result.

    temp16 = z2ui5_cl_popup_context=>msg_get_t( i_messages ).


    LOOP AT temp16 REFERENCE INTO lr_row.

      CLEAR temp17.
      temp17-type = z2ui5_cl_popup_context=>ui5_get_msg_type( lr_row->type ).
      temp17-title = lr_row->text.
      temp17-subtitle = |{ lr_row->id } { lr_row->no }|.
      INSERT temp17 INTO TABLE r_result->mt_msg.
    ENDLOOP.

    r_result->title = i_title.

  ENDMETHOD.

  METHOD view_display.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core` ).
    popup = popup->ele( `Dialog`
                )->a( n = `title` t = title
                )->a( n = `contentHeight` v = `50%`
                )->a( n = `contentWidth` v = `50%`
                )->a( n = `verticalScrolling` b = abap_false
                )->a( n = `afterClose` v = client->_event( `BUTTON_CONTINUE` ) ).

    popup->ele( `MessageView`
        )->a( n = `items` v = client->_bind( mt_msg )
        )->ele( `MessageItem`
        )->a( n = `type` v = `{TYPE}`
        )->a( n = `title` v = `{TITLE}`
        )->a( n = `subtitle` v = `{SUBTITLE}` ).

    popup->ele( `buttons`
        )->tag( `Button`
        )->a( n = `text` v = `Continue`
        )->a( n = `press` v = client->_event( `BUTTON_CONTINUE` )
        )->a( n = `type` v = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      view_display( ).
      RETURN.
    ELSEIF client->check_on_navigated( ) IS NOT INITIAL.
      view_display( ).
    ENDIF.

    IF client->check_on_event( `BUTTON_CONTINUE` ) IS NOT INITIAL.
      client->popup_destroy( ).
      client->nav_app_leave( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
