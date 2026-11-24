CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_action TYPE REF TO z2ui5_cl_ui5_action.
    DATA mi_client TYPE REF TO z2ui5_if_client.

    METHODS client_create
      IMPORTING
        io_app TYPE REF TO z2ui5_if_app.

    METHODS roundtrip_event
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string.

    "! the popover the roundtrip queued: the XML and the anchor
    METHODS popover_displayed
      EXPORTING
        ev_xml     TYPE string
        ev_open_by TYPE string.

    METHODS popover_destroyed
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS msg_get_t_texts        FOR TESTING RAISING cx_static_check.
    METHODS msg_get_t_exception    FOR TESTING RAISING cx_static_check.
    METHODS count_get_worst_type   FOR TESTING RAISING cx_static_check.
    METHODS count_get_empty        FOR TESTING RAISING cx_static_check.
    METHODS popover_xml_bindings   FOR TESTING RAISING cx_static_check.
    METHODS init_displays_popover  FOR TESTING RAISING cx_static_check.
    METHODS close_leaves           FOR TESTING RAISING cx_static_check.
    METHODS close_raises_event     FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD client_create.

    mo_action = NEW #( NEW z2ui5_cl_ui5_handler( `` ) ).
    mo_action->mo_app->mo_app = io_app.
    mi_client = NEW z2ui5_cl_ui5_client( mo_action ).

  ENDMETHOD.

  METHOD roundtrip_event.

    client_create( io_app ).
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    io_app->main( mi_client ).

  ENDMETHOD.

  METHOD popover_displayed.

    CLEAR: ev_xml, ev_open_by.
    LOOP AT mo_action->ms_next-t_action_front INTO DATA(ls_action)
         WHERE slot = z2ui5_if_client=>cs_view-popover AND method = `display`.
      ev_xml = ls_action-xml.
      IF ls_action-options IS BOUND.
        ev_open_by = ls_action-options->get_string( `/openById` ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD popover_destroyed.

    result = xsdbool( line_exists( mo_action->ms_next-t_action_front[ slot   = z2ui5_if_client=>cs_view-popover
                                                                      method = `destroy` ] ) ).

  ENDMETHOD.

  METHOD msg_get_t_texts.

    DATA(lt_msg) = z2ui5_cl_popup_msgpopover=>msg_get_t( VALUE string_table( ( `first message` )
                                                                             ( `second message` ) ) ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_msg ) ).
    cl_abap_unit_assert=>assert_equals( exp = `first message`
                                        act = lt_msg[ 1 ]-title ).
    cl_abap_unit_assert=>assert_equals( exp = `first message`
                                        act = lt_msg[ 1 ]-description ).
    cl_abap_unit_assert=>assert_equals( exp = lt_msg[ 1 ]-type
                                        act = lt_msg[ 1 ]-group ).
    cl_abap_unit_assert=>assert_initial( lt_msg[ 1 ]-subtitle ).

  ENDMETHOD.

  METHOD msg_get_t_exception.

    DATA(lx) = NEW z2ui5_cx_popup_error( val = `went wrong` ).

    DATA(lt_msg) = z2ui5_cl_popup_msgpopover=>msg_get_t( lx ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_msg ) ).
    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_context=>cs_ui5_msg_type-e
                                        act = lt_msg[ 1 ]-type ).
    cl_abap_unit_assert=>assert_true( xsdbool( lt_msg[ 1 ]-title CS `went wrong` ) ).

  ENDMETHOD.

  METHOD count_get_worst_type.

    DATA(lt_msg) = VALUE z2ui5_cl_popup_msgpopover=>ty_t_msg(
                       ( type = z2ui5_cl_popup_context=>cs_ui5_msg_type-s )
                       ( type = z2ui5_cl_popup_context=>cs_ui5_msg_type-w )
                       ( type = z2ui5_cl_popup_context=>cs_ui5_msg_type-e )
                       ( type = z2ui5_cl_popup_context=>cs_ui5_msg_type-e )
                       ( type = z2ui5_cl_popup_context=>cs_ui5_msg_type-i ) ).

    DATA(ls_count) = z2ui5_cl_popup_msgpopover=>count_get( lt_msg ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = ls_count-error ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = ls_count-warning ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = ls_count-success ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = ls_count-information ).
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = ls_count-total ).
    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_context=>cs_ui5_msg_type-e
                                        act = ls_count-type ).
    cl_abap_unit_assert=>assert_equals( exp = `sap-icon://message-error`
                                        act = ls_count-icon ).

    DELETE lt_msg WHERE type = z2ui5_cl_popup_context=>cs_ui5_msg_type-e.
    ls_count = z2ui5_cl_popup_msgpopover=>count_get( lt_msg ).
    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_context=>cs_ui5_msg_type-w
                                        act = ls_count-type ).

  ENDMETHOD.

  METHOD count_get_empty.

    DATA lt_msg TYPE z2ui5_cl_popup_msgpopover=>ty_t_msg.

    DATA(ls_count) = z2ui5_cl_popup_msgpopover=>count_get( lt_msg ).

    cl_abap_unit_assert=>assert_initial( ls_count ).

  ENDMETHOD.

  METHOD popover_xml_bindings.

    DATA(lv_xml) = z2ui5_cl_popup_msgpopover=>popover_xml( i_items       = `{/MT_MSG}`
                                                           i_event_close = `.eB(['CLOSE'])`
                                                           i_placement   = z2ui5_cl_popup_msgpopover=>cs_placement-top ).

    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `MessagePopover` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `{/MT_MSG}` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `afterClose` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `placement="Top"` ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `{DESCRIPTION}` ) ).

    lv_xml = z2ui5_cl_popup_msgpopover=>popover_xml( `{/MT_MSG}` ).
    cl_abap_unit_assert=>assert_false( xsdbool( lv_xml CS `afterClose` ) ).

  ENDMETHOD.

  METHOD init_displays_popover.

    DATA lv_xml     TYPE string.
    DATA lv_open_by TYPE string.

    DATA(lo_pop) = z2ui5_cl_popup_msgpopover=>factory(
                       i_messages   = VALUE string_table( ( `first message` ) )
                       i_open_by_id = `btnMessages` ).
    client_create( lo_pop ).

    lo_pop->z2ui5_if_app~main( mi_client ).

    popover_displayed( IMPORTING ev_xml     = lv_xml
                                 ev_open_by = lv_open_by ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `MessagePopover` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `btnMessages`
                                        act = lv_open_by ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lo_pop->result( )-total ).

  ENDMETHOD.

  METHOD close_leaves.

    DATA(lo_pop) = z2ui5_cl_popup_msgpopover=>factory( i_messages   = VALUE string_table( ( `msg` ) )
                                                       i_open_by_id = `btn` ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `MSGPOPOVER_CLOSE` ).

    cl_abap_unit_assert=>assert_true( popover_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD close_raises_event.

    DATA(lo_pop) = z2ui5_cl_popup_msgpopover=>factory( i_messages    = VALUE string_table( ( `msg` ) )
                                                       i_open_by_id  = `btn`
                                                       i_event_close = `MESSAGES_CLOSED` ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `MSGPOPOVER_CLOSE` ).

    cl_abap_unit_assert=>assert_true( popover_destroyed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `MESSAGES_CLOSED`
                                        act = mo_action->ms_next-next_event ).

  ENDMETHOD.

ENDCLASS.
