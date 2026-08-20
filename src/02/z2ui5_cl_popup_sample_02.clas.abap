CLASS z2ui5_cl_popup_sample_02 DEFINITION
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


CLASS z2ui5_cl_popup_sample_02 IMPLEMENTATION.

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

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory( 
                     )->ele( n = `View` ns = `mvc` 
                     )->a( n = `xmlns` v = `sap.m` 
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` 
                     )->a( n = `xmlns:core` v = `sap.ui.core` 
                     )->a( n = `xmlns:form` v = `sap.ui.layout.form` 
                     )->a( n = `displayBlock` v = `true` 
                     )->a( n = `height` v = `100%` ).
    DATA(page) = view->ele( `Shell` 
                     )->ele( `Page` 
                     )->a( n = `title` v = 'Search-Help' 
                     )->a( n = `navButtonPress` v = client->_event( 'BACK' ) 
                     )->a( n = `showNavButton` b = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL ) 
                     )->a( n = `class` v = 'sapUiContentPadding' ).

    page->ele( n = `SimpleForm` ns = `form` 
        )->a( n = `title` v = 'Search-Help' 
        )->a( n = `editable` b = abap_true 
        )->ele( n = `content` ns = `form` 
        )->tag( `Text` 
        )->a( n = `text` v = `Table USR01 field SPLD has a Search-Help.` 
        )->tag( `Label` 
        )->a( n = `text` v = `SPLD` 
        )->tag( `Input` 
        )->a( n = `value` v = client->_bind_edit( ms_usr01-spld ) 
        )->a( n = `showValueHelp` b = abap_true 
        )->a( n = `valueHelpRequest` v = client->_event( val   = 'CALL_POPUP_SEARCH'
                                                                     t_arg = VALUE #( ( `SPLD` ) ( `USR01` ) ) ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_init( ).
      on_init( ).
    ENDIF.

    on_event( ).
    on_after_search( ).

  ENDMETHOD.

  METHOD call_search.

    DATA(lt_arg) = client->get( )-t_event_arg.
    DATA(search_field) = VALUE string( lt_arg[ 1 ] ).
    DATA(search_table) = VALUE string( lt_arg[ 2 ] ).

    client->nav_app_call( z2ui5_cl_popup_search_help=>factory( i_table = search_table
                                                             i_fname = search_field
                                                             i_value = CONV #( ms_usr01-spld )
                                                             i_data  = REF #( ms_usr01 ) ) ).

  ENDMETHOD.

  METHOD on_after_search.

    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        DATA(app) = CAST z2ui5_cl_popup_search_help( client->get_app( client->get( )-s_draft-id_prev_app ) ).

        IF app->mv_return_value IS NOT INITIAL.

          ms_usr01-spld = app->mv_return_value.

          client->view_model_update( ).

        ENDIF.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
