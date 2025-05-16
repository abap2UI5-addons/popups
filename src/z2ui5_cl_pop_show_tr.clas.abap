CLASS z2ui5_cl_pop_show_tr DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA client       TYPE REF TO z2ui5_if_client.
    DATA ms_transport TYPE z2ui5_cl_util=>ty_s_transport.

    CLASS-METHODS add_data_to_tranport
      IMPORTING
        ir_data      TYPE REF TO data
        iv_tabname   TYPE string
        is_transport TYPE z2ui5_cl_util=>ty_s_transport
      EXCEPTIONS
        ob_check_obj_error.

    CLASS-DATA mt_data TYPE STANDARD TABLE OF z2ui5_cl_util=>ty_s_transport WITH EMPTY KEY.

    CLASS-METHODS factory
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_pop_show_tr.


  PROTECTED SECTION.

    METHODS on_init.
    METHODS render_view.
    METHODS on_event.

ENDCLASS.


CLASS z2ui5_cl_pop_show_tr IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      on_init( ).
      render_view( ).
    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_init.

    mt_data = z2ui5_cl_util_abap=>bus_tr_read( ).

  ENDMETHOD.



  METHOD render_view.
    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    popup->dialog( contentwidth = '40%'
                   afterclose   = client->_event( 'CLOSE' )

                   title = z2ui5_cl_util=>rtti_get_data_element_texts( `SRET_TRORD`  )-long
    )->table( mode  = 'SingleSelectLeft'
              items = client->_bind_edit( mt_data )
        )->columns(
            )->column( )->text( z2ui5_cl_util=>rtti_get_data_element_texts( `SRET_TRORD`  )-short )->get_parent(
            )->column( )->text( z2ui5_cl_util=>rtti_get_data_element_texts( `CC_TEXT`  )-short )->get_parent(
                )->get_parent(
        )->items(
            )->column_list_item( selected = '{SELKZ}'
                )->cells(
                    )->text( '{TRANSPORT}'
                    )->text( '{SHORT_DESCRIPTION}'
    )->get_parent( )->get_parent( )->get_parent( )->get_parent(
    )->buttons( )->button( text  = 'Select'
                           press = client->_event( 'TRANSPORT_SELECT' )
                           type  = 'Emphasized' ).

    client->popup_display( popup->stringify( ) ).
  ENDMETHOD.

  METHOD on_event.
    CASE client->get( )-event.

      WHEN `CLOSE`.

        CLEAR ms_transport.
        client->popup_destroy( ).
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `TRANSPORT_SELECT`.

        READ TABLE mt_data INTO DATA(line) WITH KEY selkz = abap_true.
        IF sy-subrc = 0.
          ms_transport = line.
        ENDIF.

        client->popup_destroy( ).
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.

  METHOD factory.
    result = NEW #( ).
  ENDMETHOD.

  METHOD add_data_to_tranport.

    z2ui5_cl_util=>bus_tr_add( ir_data      = ir_data
                             iv_tabname   = iv_tabname
                             is_transport = is_transport ).


  ENDMETHOD.


ENDCLASS.
