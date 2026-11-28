CLASS z2ui5_cl_popup_show_tr DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    " check_confirmed( ) - a transport request chosen, or No Transport
    " (ms_transport-locl); abap_false when closed or Select without a row;
    " value( ) - REF TO z2ui5_cl_popup_context=>ty_s_transport
    " (ms_transport); value_string( ) - the transport request, empty for
    " No Transport
    INTERFACES z2ui5_if_popup_result.

    DATA client       TYPE REF TO z2ui5_if_client.
    DATA ms_transport TYPE z2ui5_cl_popup_context=>ty_s_transport.

    "! Record the keys of the rows in ir_data on the transport request of
    "! is_transport (nothing for a local change, is_transport-locl - and no
    "! commit either, whatever iv_commit says). Raises z2ui5_cx_popup_error
    "! when the request refuses the entries, without a ROLLBACK WORK in
    "! either mode - see bus_tr_add( ) and the README, "Commits".
    "! @parameter iv_commit | abap_true (the default) commits the entries at
    "!                       once - and whatever else the caller has open;
    "!                       abap_false leaves the COMMIT WORK to the
    "!                       caller - a caller that writes the rows itself
    "!                       commits both together, or rolls both back
    CLASS-METHODS add_data_to_tranport
      IMPORTING
        ir_data      TYPE REF TO data
        iv_tabname   TYPE string
        is_transport TYPE z2ui5_cl_popup_context=>ty_s_transport
        iv_commit    TYPE abap_bool DEFAULT abap_true.

    TYPES temp1_25b75d9fca TYPE STANDARD TABLE OF z2ui5_cl_popup_context=>ty_s_transport WITH DEFAULT KEY.
DATA mt_data TYPE temp1_25b75d9fca.

    CLASS-METHODS factory
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_popup_show_tr.

  PROTECTED SECTION.
    "! the requests bus_tr_read( ) offered - the server's own copy of
    "! mt_data: mt_data is bound to the browser, which can rewrite any of its
    "! cells, so SELECT takes only the position of the selected row from it
    DATA mt_data_read TYPE z2ui5_cl_popup_context=>ty_t_data.

    METHODS on_init.
    METHODS render_view.
    METHODS on_event.

ENDCLASS.


CLASS z2ui5_cl_popup_show_tr IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      on_init( ).
      render_view( ).
    ELSEIF client->check_on_navigated( ) IS NOT INITIAL.
      render_view( ).
    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_init.

    mt_data      = z2ui5_cl_popup_context=>bus_tr_read( ).
    mt_data_read = mt_data.

  ENDMETHOD.

  METHOD render_view.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core` ).

    popup->ele( `Dialog`
        )->a( n = `contentWidth` v = `40%`
        )->a( n = `afterClose` v = client->_event( 'CLOSE' )
        )->a( n = `title` v = z2ui5_cl_popup_context=>rtti_get_data_element_texts( `SRET_TRORD`  )-long
        )->ele( `Table`
        )->a( n = `mode` v = `SingleSelectLeft`
        )->a( n = `items` v = client->_bind( mt_data )
        )->ele( `columns`
        )->ele( `Column`
        )->tag( `Text`
        )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_texts( `SRET_TRORD`  )-short
        )->end(
        )->ele( `Column`
        )->tag( `Text`
        )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_texts( `CC_TEXT`  )-short
        )->end(
        )->end(
        )->ele( `items`
        )->ele( `ColumnListItem`
        )->a( n = `selected` v = `{SELKZ}`
        )->ele( `cells`
        )->tag( `Text`
        )->a( n = `text` v = `{TRANSPORT}`
        )->tag( `Text`
        )->a( n = `text` v = `{SHORT_DESCRIPTION}`
        )->end(
        )->end(
        )->end(
        )->end(
        )->ele( `buttons`
        )->tag( `Button`
        )->a( n = `text` v = `No Transport`
        )->a( n = `press` v = client->_event( 'LOCL' )
        )->a( n = `type` v = `Default`
        )->tag( `Button`
        )->a( n = `text` v = `Select`
        )->a( n = `press` v = client->_event( 'SELECT' )
        )->a( n = `type` v = `Emphasized` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.
        DATA lv_index TYPE i.
        DATA temp3 LIKE sy-subrc.
          FIELD-SYMBOLS <line> TYPE z2ui5_cl_popup_context=>ty_s_transport.
    CASE client->get( )-event.

      WHEN `CLOSE`.

        CLEAR ms_transport.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `SELECT`.

        " the request and task come from what bus_tr_read( ) offered, not
        " from the bound row: a crafted request could otherwise name any
        " task, and add_data_to_tranport( ) records on it


        READ TABLE mt_data WITH KEY selkz = abap_true TRANSPORTING NO FIELDS.
        temp3 = sy-tabix.
        lv_index = temp3.
        IF lv_index > 0.

          READ TABLE mt_data_read INDEX lv_index ASSIGNING <line>.
          IF <line> IS ASSIGNED.
            ms_transport = <line>.
            ms_transport-selkz = abap_true.
          ENDIF.
        ENDIF.

        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN 'LOCL'.

        ms_transport-locl = abap_true.

        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.

  METHOD factory.
    CREATE OBJECT result.
  ENDMETHOD.

  METHOD add_data_to_tranport.

    IF is_transport-locl = abap_true.
      RETURN.
    ENDIF.

    z2ui5_cl_popup_context=>bus_tr_add( ir_data      = ir_data
                                        iv_tabname   = iv_tabname
                                        is_transport = is_transport
                                        iv_commit    = iv_commit ).

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~check_confirmed.

    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( ms_transport IS NOT INITIAL ).
    result = temp1.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value.

    GET REFERENCE OF ms_transport INTO result.

  ENDMETHOD.

  METHOD z2ui5_if_popup_result~value_string.

    result = ms_transport-transport.

  ENDMETHOD.

ENDCLASS.
