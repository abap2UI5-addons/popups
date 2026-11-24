CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_factory          FOR TESTING RAISING cx_static_check.
    METHODS test_factory_defaults FOR TESTING RAISING cx_static_check.
    METHODS test_result_initial   FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD test_factory.
    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory(
      iv_image       = `data:image/png;base64,AAAA`
      iv_title       = `Edit`
      iv_save_text   = `Done`
      iv_cancel_text = `Abort` ).

    cl_abap_unit_assert=>assert_bound( lo_pop ).
  ENDMETHOD.

  METHOD test_factory_defaults.
    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory( `test_img` ).

    cl_abap_unit_assert=>assert_bound( lo_pop ).
  ENDMETHOD.

  METHOD test_result_initial.
    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory( `test_img` ).
    DATA(ls_result) = lo_pop->result( ).

    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = ls_result-check_confirmed ).
    cl_abap_unit_assert=>assert_equals( exp = `test_img`
                                        act = ls_result-image ).
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_test_roundtrip DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_action TYPE REF TO z2ui5_cl_ui5_action.
    DATA mi_client TYPE REF TO z2ui5_if_client.

    " what the roundtrip queued for the frontend, read off the response
    METHODS popup_xml
      RETURNING
        VALUE(result) TYPE string.
    METHODS popup_destroyed
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS client_create
      IMPORTING
        io_app TYPE REF TO z2ui5_if_app.

    METHODS test_init_displays_popup FOR TESTING RAISING cx_static_check.
    METHODS test_save                FOR TESTING RAISING cx_static_check.
    METHODS test_cancel              FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test_roundtrip IMPLEMENTATION.

  METHOD client_create.

    mo_action = NEW #( NEW z2ui5_cl_ui5_handler( `` ) ).
    mo_action->mo_app->mo_app = io_app.
    mi_client = NEW z2ui5_cl_ui5_client( mo_action ).

  ENDMETHOD.

  METHOD test_init_displays_popup.

    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory( iv_image = `data:image/png;base64,OLD`
                                                       iv_title = `Image Title` ).
    client_create( lo_pop ).

    lo_pop->z2ui5_if_app~main( mi_client ).

    DATA(lv_xml) = popup_xml( ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_xml CS `Image Title` ) ).

  ENDMETHOD.

  METHOD test_save.

    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory( `data:image/png;base64,OLD` ).
    client_create( lo_pop ).
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = `SAVE`.
    mo_action->ms_actual-t_event_arg = VALUE #( ( `data:image/png;base64,NEW` ) ).

    lo_pop->z2ui5_if_app~main( mi_client ).

    cl_abap_unit_assert=>assert_true( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_equals( exp = `data:image/png;base64,NEW`
                                        act = lo_pop->result( )-image ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).

  ENDMETHOD.

  METHOD test_cancel.

    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory( `data:image/png;base64,OLD` ).
    client_create( lo_pop ).
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = `CANCEL`.

    lo_pop->z2ui5_if_app~main( mi_client ).

    cl_abap_unit_assert=>assert_false( lo_pop->result( )-check_confirmed ).
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD popup_xml.

    LOOP AT mo_action->ms_next-t_action_front INTO DATA(ls_action)
         WHERE slot = z2ui5_if_client=>cs_view-popup AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_destroyed.

    result = xsdbool( line_exists( mo_action->ms_next-t_action_front[ slot   = z2ui5_if_client=>cs_view-popup
                                                                      method = `destroy` ] ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_popup_result DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_action TYPE REF TO z2ui5_cl_ui5_action.

    METHODS roundtrip
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string
        it_arg   TYPE string_table OPTIONAL.

    METHODS test_initial_not_confirmed FOR TESTING RAISING cx_static_check.
    METHODS test_confirmed             FOR TESTING RAISING cx_static_check.
    METHODS test_cancelled             FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_popup_result IMPLEMENTATION.

  METHOD roundtrip.

    " a roundtrip with an event, after the first one - as the button press
    " of the user
    mo_action = NEW #( NEW z2ui5_cl_ui5_handler( `` ) ).
    mo_action->mo_app->mo_app = io_app.
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    mo_action->ms_actual-t_event_arg = it_arg.
    io_app->main( NEW z2ui5_cl_ui5_client( mo_action ) ).

  ENDMETHOD.

  METHOD test_initial_not_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory( `data:image/png;base64,OLD` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

  METHOD test_confirmed.

    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory( `data:image/png;base64,OLD` ).
    roundtrip( io_app   = lo_pop
               iv_event = `SAVE`
               it_arg   = VALUE #( ( `data:image/png;base64,NEW` ) ) ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_true( li_result->check_confirmed( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `data:image/png;base64,NEW`
                                        act = li_result->value_string( ) ).
    DATA(lr_value) = CAST string( li_result->value( ) ).
    cl_abap_unit_assert=>assert_equals( exp = `data:image/png;base64,NEW`
                                        act = lr_value->* ).

  ENDMETHOD.

  METHOD test_cancelled.

    DATA(lo_pop) = z2ui5_cl_popup_image_edit=>factory( `data:image/png;base64,OLD` ).
    roundtrip( io_app   = lo_pop
               iv_event = `CANCEL` ).
    DATA(li_result) = CAST z2ui5_if_popup_result( lo_pop ).

    cl_abap_unit_assert=>assert_false( li_result->check_confirmed( ) ).

  ENDMETHOD.

ENDCLASS.
