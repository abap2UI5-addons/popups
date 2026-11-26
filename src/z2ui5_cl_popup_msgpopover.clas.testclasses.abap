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

    DATA temp1 TYPE REF TO z2ui5_cl_ui5_handler.
    CREATE OBJECT temp1 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp1.
    mo_action->mo_app->mo_app = io_app.
    CREATE OBJECT mi_client TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.

  ENDMETHOD.

  METHOD roundtrip_event.

    client_create( io_app ).
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    io_app->main( mi_client ).

  ENDMETHOD.

  METHOD popover_displayed.
    DATA ls_action LIKE LINE OF mo_action->ms_next-t_action_front.

    CLEAR: ev_xml, ev_open_by.

    LOOP AT mo_action->ms_next-t_action_front INTO ls_action
         WHERE slot = z2ui5_if_client=>cs_view-popover AND method = `display`.
      ev_xml = ls_action-xml.
      IF ls_action-options IS BOUND.
        ev_open_by = ls_action-options->get_string( `/openById` ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD popover_destroyed.

    DATA temp1 LIKE sy-subrc.
    DATA temp2 TYPE xsdboolean.
    READ TABLE mo_action->ms_next-t_action_front WITH KEY slot = z2ui5_if_client=>cs_view-popover method = `destroy` TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.

    temp2 = boolc( temp1 = 0 ).
    result = temp2.

  ENDMETHOD.

  METHOD msg_get_t_texts.

    DATA temp2 TYPE string_table.
    DATA lt_msg TYPE z2ui5_cl_popup_msgpopover=>ty_t_msg.
    FIELD-SYMBOLS <temp4> LIKE LINE OF lt_msg.
    DATA temp5 LIKE sy-tabix.
    FIELD-SYMBOLS <temp6> LIKE LINE OF lt_msg.
    DATA temp7 LIKE sy-tabix.
    FIELD-SYMBOLS <temp8> LIKE LINE OF lt_msg.
    DATA temp9 LIKE sy-tabix.
    FIELD-SYMBOLS <temp2> LIKE LINE OF lt_msg.
    DATA temp3 LIKE sy-tabix.
    FIELD-SYMBOLS <temp10> LIKE LINE OF lt_msg.
    DATA temp11 LIKE sy-tabix.
    CLEAR temp2.
    INSERT `first message` INTO TABLE temp2.
    INSERT `second message` INTO TABLE temp2.

    lt_msg = z2ui5_cl_popup_msgpopover=>msg_get_t( temp2 ).

    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_msg ) ).


    temp5 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp4>.
    sy-tabix = temp5.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `first message`
                                        act = <temp4>-title ).


    temp7 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp6>.
    sy-tabix = temp7.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = `first message`
                                        act = <temp6>-description ).


    temp9 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp8>.
    sy-tabix = temp9.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.


    temp3 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp2>.
    sy-tabix = temp3.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = <temp8>-type
                                        act = <temp2>-group ).


    temp11 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp10>.
    sy-tabix = temp11.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( <temp10>-subtitle ).

  ENDMETHOD.

  METHOD msg_get_t_exception.

    DATA lx TYPE REF TO z2ui5_cx_popup_error.
    DATA lt_msg TYPE z2ui5_cl_popup_msgpopover=>ty_t_msg.
    FIELD-SYMBOLS <temp12> LIKE LINE OF lt_msg.
    DATA temp13 LIKE sy-tabix.
    FIELD-SYMBOLS <temp14> LIKE LINE OF lt_msg.
    DATA temp15 LIKE sy-tabix.
    DATA temp3 TYPE xsdboolean.
    CREATE OBJECT lx TYPE z2ui5_cx_popup_error EXPORTING val = `went wrong`.


    lt_msg = z2ui5_cl_popup_msgpopover=>msg_get_t( lx ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_msg ) ).


    temp13 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp12>.
    sy-tabix = temp13.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( exp = z2ui5_cl_popup_context=>cs_ui5_msg_type-e
                                        act = <temp12>-type ).


    temp15 = sy-tabix.
    READ TABLE lt_msg INDEX 1 ASSIGNING <temp14>.
    sy-tabix = temp15.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.

    temp3 = boolc( <temp14>-title CS `went wrong` ).
    cl_abap_unit_assert=>assert_true( temp3 ).

  ENDMETHOD.

  METHOD count_get_worst_type.

    DATA temp16 TYPE z2ui5_cl_popup_msgpopover=>ty_t_msg.
    DATA temp17 LIKE LINE OF temp16.
    DATA lt_msg LIKE temp16.
    DATA ls_count TYPE z2ui5_cl_popup_msgpopover=>ty_s_count.
    CLEAR temp16.

    temp17-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-s.
    INSERT temp17 INTO TABLE temp16.
    temp17-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-w.
    INSERT temp17 INTO TABLE temp16.
    temp17-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-e.
    INSERT temp17 INTO TABLE temp16.
    temp17-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-e.
    INSERT temp17 INTO TABLE temp16.
    temp17-type = z2ui5_cl_popup_context=>cs_ui5_msg_type-i.
    INSERT temp17 INTO TABLE temp16.

    lt_msg = temp16.


    ls_count = z2ui5_cl_popup_msgpopover=>count_get( lt_msg ).

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

    DATA ls_count TYPE z2ui5_cl_popup_msgpopover=>ty_s_count.
    ls_count = z2ui5_cl_popup_msgpopover=>count_get( lt_msg ).

    cl_abap_unit_assert=>assert_initial( ls_count ).

  ENDMETHOD.

  METHOD popover_xml_bindings.

    DATA lv_xml TYPE string.
    DATA temp4 TYPE xsdboolean.
    DATA temp5 TYPE xsdboolean.
    DATA temp6 TYPE xsdboolean.
    DATA temp7 TYPE xsdboolean.
    DATA temp8 TYPE xsdboolean.
    DATA temp9 TYPE xsdboolean.
    lv_xml = z2ui5_cl_popup_msgpopover=>popover_xml( i_items       = `{/MT_MSG}`
                                                           i_event_close = `.eB(['CLOSE'])`
                                                           i_placement   = z2ui5_cl_popup_msgpopover=>cs_placement-top ).


    temp4 = boolc( lv_xml CS `MessagePopover` ).
    cl_abap_unit_assert=>assert_true( temp4 ).

    temp5 = boolc( lv_xml CS `{/MT_MSG}` ).
    cl_abap_unit_assert=>assert_true( temp5 ).

    temp6 = boolc( lv_xml CS `afterClose` ).
    cl_abap_unit_assert=>assert_true( temp6 ).

    temp7 = boolc( lv_xml CS `placement="Top"` ).
    cl_abap_unit_assert=>assert_true( temp7 ).

    temp8 = boolc( lv_xml CS `{DESCRIPTION}` ).
    cl_abap_unit_assert=>assert_true( temp8 ).

    lv_xml = z2ui5_cl_popup_msgpopover=>popover_xml( `{/MT_MSG}` ).

    temp9 = boolc( lv_xml CS `afterClose` ).
    cl_abap_unit_assert=>assert_false( temp9 ).

  ENDMETHOD.

  METHOD init_displays_popover.

    DATA lv_xml     TYPE string.
    DATA lv_open_by TYPE string.

    DATA temp18 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_msgpopover.
    DATA temp10 TYPE xsdboolean.
    CLEAR temp18.
    INSERT `first message` INTO TABLE temp18.

    lo_pop = z2ui5_cl_popup_msgpopover=>factory(
                       i_messages   = temp18
                       i_open_by_id = `btnMessages` ).
    client_create( lo_pop ).

    lo_pop->z2ui5_if_app~main( mi_client ).

    popover_displayed( IMPORTING ev_xml     = lv_xml
                                 ev_open_by = lv_open_by ).

    temp10 = boolc( lv_xml CS `MessagePopover` ).
    cl_abap_unit_assert=>assert_true( temp10 ).
    cl_abap_unit_assert=>assert_equals( exp = `btnMessages`
                                        act = lv_open_by ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lo_pop->result( )-total ).

  ENDMETHOD.

  METHOD close_leaves.

    DATA temp20 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_msgpopover.
    CLEAR temp20.
    INSERT `msg` INTO TABLE temp20.

    lo_pop = z2ui5_cl_popup_msgpopover=>factory( i_messages   = temp20
                                                       i_open_by_id = `btn` ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `MSGPOPOVER_CLOSE` ).

    cl_abap_unit_assert=>assert_true( popover_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD close_raises_event.

    DATA temp22 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_msgpopover.
    CLEAR temp22.
    INSERT `msg` INTO TABLE temp22.

    lo_pop = z2ui5_cl_popup_msgpopover=>factory( i_messages    = temp22
                                                       i_open_by_id  = `btn`
                                                       i_event_close = `MESSAGES_CLOSED` ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `MSGPOPOVER_CLOSE` ).

    cl_abap_unit_assert=>assert_true( popover_destroyed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `MESSAGES_CLOSED`
                                        act = mo_action->ms_next-next_event ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_popup_result DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_closed_is_confirmed FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_popup_result IMPLEMENTATION.

  METHOD test_closed_is_confirmed.

    DATA temp24 TYPE string_table.
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_msgpopover.
    DATA temp26 TYPE REF TO z2ui5_if_popup_result.
    DATA li_result LIKE temp26.
    DATA temp27 TYPE REF TO z2ui5_cl_popup_msgpopover=>ty_t_msg.
    DATA lr_msg LIKE temp27.
    CLEAR temp24.
    INSERT `msg` INTO TABLE temp24.

    lo_pop = z2ui5_cl_popup_msgpopover=>factory( i_messages   = temp24
                                                       i_open_by_id = `btn` ).

    temp26 ?= lo_pop.

    li_result = temp26.

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).

    temp27 ?= li_result->value( ).

    lr_msg = temp27.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lr_msg->* ) ).
    cl_abap_unit_assert=>assert_initial( li_result->value_string( ) ).

  ENDMETHOD.

ENDCLASS.
