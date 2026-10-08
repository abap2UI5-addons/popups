CLASS z2ui5_cl_pop_sample_02 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA ms_usr01 TYPE z2ui5_cl_popup_context=>ty_usr01.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event.
    METHODS render_main.
    METHODS call_search.

  PRIVATE SECTION.
    METHODS on_after_search.

ENDCLASS.


CLASS z2ui5_cl_pop_sample_02 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'BACK'.
        client->nav_app_leave( ).

      WHEN `CALL_POPUP_SEARCH`.
        call_search( ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD on_init.

    render_main( ).

  ENDMETHOD.

  METHOD render_main.

    DATA view TYPE REF TO z2ui5_cl_xml_view.
    DATA page TYPE REF TO z2ui5_cl_xml_view.
    DATA temp2 TYPE xsdboolean.
    DATA temp1 TYPE string_table.
    view = z2ui5_cl_xml_view=>factory( ).


    temp2 = boolc( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL ).
    page = view->shell( )->page(
                     title          = 'Search-Help'
                     navbuttonpress = client->_event( 'BACK' )
                     shownavbutton  = temp2
                     class          = 'sapUiContentPadding' ).


    CLEAR temp1.
    INSERT `SPLD` INTO TABLE temp1.
    INSERT `USR01` INTO TABLE temp1.
    page->simple_form( title    = 'Search-Help'
                       editable = abap_true
                    )->content( 'form'
                        )->text( `Table USR01 field SPLD has a Search-Help.`
                        )->label( `SPLD`
                        )->input( value            = client->_bind_edit( ms_usr01-spld )
                                  showvaluehelp    = abap_true
                                  valuehelprequest = client->_event( val   = 'CALL_POPUP_SEARCH'
                                                                     t_arg = temp1 ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      on_init( ).
    ENDIF.

    on_event( ).
    on_after_search( ).

  ENDMETHOD.

  METHOD call_search.

    DATA lt_arg TYPE string_table.
    DATA temp3 TYPE string.
    FIELD-SYMBOLS <temp1> LIKE LINE OF lt_arg.
    DATA temp2 LIKE sy-tabix.
    DATA search_field LIKE temp3.
    DATA temp4 TYPE string.
    FIELD-SYMBOLS <temp3> LIKE LINE OF lt_arg.
    DATA temp6 LIKE sy-tabix.
    DATA search_table LIKE temp4.
    DATA temp5 LIKE REF TO ms_usr01.
DATA temp7 TYPE string.
    lt_arg = client->get( )-t_event_arg.

    CLEAR temp3.


    temp2 = sy-tabix.
    READ TABLE lt_arg INDEX 1 ASSIGNING <temp1>.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    temp3 = <temp1>.

    search_field = temp3.

    CLEAR temp4.


    temp6 = sy-tabix.
    READ TABLE lt_arg INDEX 2 ASSIGNING <temp3>.
    sy-tabix = temp6.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    temp4 = <temp3>.

    search_table = temp4.


    GET REFERENCE OF ms_usr01 INTO temp5.

temp7 = ms_usr01-spld.
client->nav_app_call( z2ui5_cl_pop_search_help=>factory( i_table = search_table
                                                             i_fname = search_field
                                                             i_value = temp7
                                                             i_data  = temp5 ) ).

  ENDMETHOD.

  METHOD on_after_search.
        DATA temp6 TYPE REF TO z2ui5_cl_pop_search_help.
        DATA app LIKE temp6.

    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.

        temp6 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp6.

        IF app->mv_return_value IS NOT INITIAL.

          ms_usr01-spld = app->mv_return_value.

          client->view_model_update( ).

        ENDIF.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
