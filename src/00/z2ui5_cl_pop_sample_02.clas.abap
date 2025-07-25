CLASS z2ui5_cl_pop_sample_02 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA ms_usr01 TYPE z2ui5_cl_util_ext=>ty_usr01.


  PROTECTED SECTION.
    DATA client            TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.

    METHODS on_init.
    METHODS on_event.
    METHODS render_main.
    METHODS call_SEARCH.

  PRIVATE SECTION.
    METHODS on_after_SEARCH.

ENDCLASS.


CLASS z2ui5_cl_pop_sample_02 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'BACK'.
        client->nav_app_leave( ).

      WHEN `CALL_POPUP_SEARCH`.
        call_SEARCH( ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD on_init.

    render_main( ).

  ENDMETHOD.

  METHOD render_main.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    DATA(page) = view->shell( )->page(
                     title          = 'Layout'
                     navbuttonpress = client->_event( 'BACK' )
                     shownavbutton  = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
                     class          = 'sapUiContentPadding' ).

    page->simple_form( title    = 'Search-Help'
                       editable = abap_true
                    )->content( 'form'
                        )->text( `Table USR01 field SPLD has a Search-Help.`
                        )->label( `SPLD`
                        )->input( value            = client->_bind_edit( ms_USR01-spld )
                                  showvaluehelp    = abap_true
                                  valuehelprequest = client->_event( val   = 'CALL_POPUP_SEARCH'
                                                                     t_arg = VALUE #( ( `SPLD` ) ( `USR01` ) ) ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      on_init( ).
    ENDIF.

    on_event( ).
    on_after_SEARCH( ).

  ENDMETHOD.

  METHOD call_SEARCH.

    DATA(lt_arg) = client->get( )-t_event_arg.
    DATA(SEARCH_field) = VALUE string( lt_arg[ 1 ] ).
    DATA(SEARCH_table) = VALUE string( lt_arg[ 2 ] ).

    client->nav_app_call( z2ui5_cl_pop_search_help=>factory( i_table = SEARCH_table
                                                             i_fname = SEARCH_field
                                                             i_value = CONV #(  ms_usr01-spld )
                                                             i_data  = REF #( ms_usr01 )  ) ).

  ENDMETHOD.

  METHOD on_after_SEARCH.

    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        DATA(app) = CAST z2ui5_cl_pop_search_help( client->get_app( client->get( )-s_draft-id_prev_app ) ).

        IF app->mv_return_value IS NOT INITIAL.

          ms_usr01-spld = app->mv_return_value.

          client->view_model_update( ).

        ENDIF.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
