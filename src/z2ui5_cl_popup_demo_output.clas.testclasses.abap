
CLASS ltcl_output_stub DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS ltcl_output_stub IMPLEMENTATION.

  METHOD get.

    result = `<p><span class="heading1">Hello cl_demo_output</span></p>`.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS test_factory          FOR TESTING RAISING cx_static_check.
    METHODS test_factory_custom   FOR TESTING RAISING cx_static_check.
    METHODS test_factory_as_page  FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_factory.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_demo_output.
    DATA temp1 TYPE REF TO ltcl_output_stub.
    CREATE OBJECT temp1 TYPE ltcl_output_stub.
    lo_pop = z2ui5_cl_popup_demo_output=>factory( temp1 ).
    cl_abap_unit_assert=>assert_bound( lo_pop ).

  ENDMETHOD.

  METHOD test_factory_custom.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_demo_output.
    DATA temp2 TYPE REF TO ltcl_output_stub.
    CREATE OBJECT temp2 TYPE ltcl_output_stub.
    lo_pop = z2ui5_cl_popup_demo_output=>factory(
      i_output      = temp2
      i_title       = `My Output`
      i_icon        = `sap-icon://hint`
      i_button_text = `Close`
      i_stretch     = abap_true ).

    cl_abap_unit_assert=>assert_bound( lo_pop ).

  ENDMETHOD.

  METHOD test_factory_as_page.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_demo_output.
    DATA temp3 TYPE REF TO ltcl_output_stub.
    CREATE OBJECT temp3 TYPE ltcl_output_stub.
    lo_pop = z2ui5_cl_popup_demo_output=>factory(
      i_output  = temp3
      i_as_page = abap_true ).

    cl_abap_unit_assert=>assert_bound( lo_pop ).

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
    METHODS view_xml
      RETURNING
        VALUE(result) TYPE string.

    METHODS client_create
      IMPORTING
        io_app TYPE REF TO z2ui5_if_app.

    METHODS roundtrip_event
      IMPORTING
        io_app   TYPE REF TO z2ui5_if_app
        iv_event TYPE string.

    METHODS test_init_displays_popup FOR TESTING RAISING cx_static_check.
    METHODS test_toggle_fullscreen   FOR TESTING RAISING cx_static_check.
    METHODS test_confirm_closes      FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test_roundtrip IMPLEMENTATION.

  METHOD client_create.

    DATA temp4 TYPE REF TO z2ui5_cl_ui5_handler.
    CREATE OBJECT temp4 TYPE z2ui5_cl_ui5_handler EXPORTING VAL = ``.
    CREATE OBJECT mo_action EXPORTING VAL = temp4.
    mo_action->mo_app->mo_app = io_app.
    CREATE OBJECT mi_client TYPE z2ui5_cl_ui5_client EXPORTING ACTION = mo_action.

  ENDMETHOD.

  METHOD roundtrip_event.

    client_create( io_app ).
    mo_action->mo_app->mv_check_initialized = abap_true.
    mo_action->ms_actual-event = iv_event.
    io_app->main( mi_client ).

  ENDMETHOD.

  METHOD test_init_displays_popup.

    " any object without a GET method works - the HTML extraction is guarded
    DATA lo_pop TYPE REF TO z2ui5_cl_popup_demo_output.
    DATA temp5 TYPE REF TO z2ui5_cx_popup_error.
    DATA temp1 TYPE xsdboolean.
    CREATE OBJECT temp5 TYPE z2ui5_cx_popup_error EXPORTING VAL = `x`.
    lo_pop = z2ui5_cl_popup_demo_output=>factory( i_output = temp5
                                                      i_title  = `Demo Title` ).
    client_create( lo_pop ).

    lo_pop->z2ui5_if_app~main( mi_client ).


    temp1 = boolc( popup_xml( ) CS `Demo Title` ).
    cl_abap_unit_assert=>assert_true( temp1 ).

  ENDMETHOD.

  METHOD test_toggle_fullscreen.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_demo_output.
    DATA temp6 TYPE REF TO z2ui5_cx_popup_error.
    CREATE OBJECT temp6 TYPE z2ui5_cx_popup_error EXPORTING VAL = `x`.
    lo_pop = z2ui5_cl_popup_demo_output=>factory( temp6 ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `TOGGLE_FULLSCREEN` ).

    " popup is destroyed and re-rendered as a full page
    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_not_initial( view_xml( ) ).

  ENDMETHOD.

  METHOD test_confirm_closes.

    DATA lo_pop TYPE REF TO z2ui5_cl_popup_demo_output.
    DATA temp7 TYPE REF TO z2ui5_cx_popup_error.
    CREATE OBJECT temp7 TYPE z2ui5_cx_popup_error EXPORTING VAL = `x`.
    lo_pop = z2ui5_cl_popup_demo_output=>factory( temp7 ).
    roundtrip_event( io_app   = lo_pop
                     iv_event = `BUTTON_CONFIRM` ).

    cl_abap_unit_assert=>assert_true( popup_destroyed( ) ).
    cl_abap_unit_assert=>assert_bound( mo_action->ms_next-o_app_leave ).

  ENDMETHOD.

  METHOD popup_xml.

    DATA ls_action LIKE LINE OF mo_action->ms_next-t_action_front.
    LOOP AT mo_action->ms_next-t_action_front INTO ls_action
         WHERE slot = z2ui5_if_client=>cs_view-popup AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_destroyed.

    DATA temp1 LIKE sy-subrc.
    DATA temp2 TYPE xsdboolean.
    READ TABLE mo_action->ms_next-t_action_front WITH KEY slot = z2ui5_if_client=>cs_view-popup method = `destroy` TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.

    temp2 = boolc( temp1 = 0 ).
    result = temp2.

  ENDMETHOD.

  METHOD view_xml.

    DATA ls_action LIKE LINE OF mo_action->ms_next-t_action_front.
    LOOP AT mo_action->ms_next-t_action_front INTO ls_action
         WHERE slot = z2ui5_if_client=>cs_view-main AND method = `display`.
      result = ls_action-xml.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
