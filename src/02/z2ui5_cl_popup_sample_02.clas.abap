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
                     )->a( n = `xmlns:form` v = `sap.ui.layout.form`
                     )->a( n = `displayBlock` v = `true`
                     )->a( n = `height` v = `100%` ).
    DATA(page) = view->ele( `Shell`
                     )->ele( `Page`
                     )->a( n = `title` v = `Search-Help`
                     )->a( n = `navButtonPress` v = client->_event( 'BACK' )
                     )->a( n = `showNavButton` b = client->check_app_prev_stack( )
                     )->a( n = `class` v = `sapUiContentPadding` ).

    page->ele( n = `SimpleForm` ns = `form`
        )->a( n = `title` v = `Search-Help`
        )->a( n = `editable` b = abap_true
        )->ele( n = `content` ns = `form`
        )->tag( `Text`
        )->a( n = `text` v = `Table USR01 field SPLD has a Search-Help.`
        )->tag( `Label`
        )->a( n = `text` v = `SPLD`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( ms_usr01-spld )
        )->a( n = `showValueHelp` b = abap_true
        )->a( n = `valueHelpRequest` v = client->_event( val   = 'CALL_POPUP_SEARCH'
                                                                     t_arg = VALUE #( ( `SPLD` ) ( `USR01` ) ) ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_init( ).
      on_init( ).

    ELSEIF client->check_on_nav_back( ).
      " back from the search help: read its result, then show the view again
      on_after_search( ).
      render_main( ).

    ELSEIF client->check_on_navigated( ).
      " a restored draft - the view is shown again
      render_main( ).

    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD call_search.

    DATA(lt_arg) = client->get( )-t_event_arg.
    DATA(search_field) = VALUE string( lt_arg[ 1 ] ).
    DATA(search_table) = VALUE string( lt_arg[ 2 ] ).

    client->nav_app_call( z2ui5_cl_popup_shlp=>factory( i_table = search_table
                                                             i_fname = search_field
                                                             i_value = CONV #( ms_usr01-spld )
                                                             i_data  = REF #( ms_usr01 ) ) ).

  ENDMETHOD.

  METHOD on_after_search.

    " every popup hands its result over through z2ui5_if_popup_result
    DATA(popup) = CAST z2ui5_if_popup_result( client->get_app_prev( ) ).
    IF popup->check_confirmed( ) = abap_true.
      ms_usr01-spld = popup->value_string( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
