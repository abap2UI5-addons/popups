CLASS z2ui5_cl_popup_sample_20 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_language TYPE string.
    DATA mv_user     TYPE string.
    DATA mv_prefill  TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event.
    METHODS render_main.

  PRIVATE SECTION.
    METHODS on_after_search.

ENDCLASS.


CLASS z2ui5_cl_popup_sample_20 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      on_init( ).

    ELSEIF client->check_on_nav_back( ) IS NOT INITIAL.
      " back from a search help: read its result, then show the view again
      on_after_search( ).
      render_main( ).

    ELSEIF client->check_on_navigated( ) IS NOT INITIAL.
      " a restored draft - the view is shown again
      render_main( ).

    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_init.

    mv_prefill = sy-uname.
    render_main( ).

  ENDMETHOD.

  METHOD on_event.
        DATA temp1 TYPE z2ui5_cl_popup_shlp=>ty_t_init.
        DATA temp2 LIKE LINE OF temp1.

    CASE client->get( )-event.

      WHEN `BACK`.
        client->nav_app_leave( ).

      WHEN `CALL_SHLP_BY_NAME`.
        " search help H_T002 (languages), returning its parameter SPRAS - the
        " tag names the field the value goes to, get_nav_tag( ) returns it
        client->nav_app_call( app = z2ui5_cl_popup_shlp=>factory_by_searchhelp( i_searchhelp = `H_T002`
                                                                                i_fname      = `SPRAS` )
                              tag = `LANGUAGE` ).

      WHEN `CALL_SHLP_PREFILLED`.
        " search help of field USR01-BNAME, its user field prefilled

        CLEAR temp1.

        temp2-name = `BNAME`.
        temp2-value = mv_prefill.
        INSERT temp2 INTO TABLE temp1.
        client->nav_app_call( app = z2ui5_cl_popup_shlp=>factory(
                                        i_table = `USR01`
                                        i_fname = `BNAME`
                                        i_value = mv_user
                                        it_init = temp1 )
                              tag = `USER` ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD render_main.

    DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA page TYPE REF TO z2ui5_cl_ui5_view_builder.
    view = z2ui5_cl_ui5_view_builder=>factory(
                     )->ele( n = `View` ns = `mvc`
                     )->a( n = `xmlns` v = `sap.m`
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                     )->a( n = `xmlns:form` v = `sap.ui.layout.form`
                     )->a( n = `displayBlock` v = `true`
                     )->a( n = `height` v = `100%` ).

    page = view->ele( `Shell`
                     )->ele( `Page`
                     )->a( n = `title` v = `Search-Help by Name and Prefill`
                     )->a( n = `navButtonPress` v = client->_event( `BACK` )
                     )->a( n = `showNavButton` b = client->check_app_prev_stack( )
                     )->a( n = `class` v = `sapUiContentPadding` ).

    page->ele( n = `SimpleForm` ns = `form`
        )->a( n = `title` v = `Search-Help by Name`
        )->a( n = `editable` b = abap_true
        )->ele( n = `content` ns = `form`
        )->tag( `Text`
        )->a( n = `text` v = `Opens the search help H_T002 by its name and returns its parameter SPRAS.`
        )->tag( `Label`
        )->a( n = `text` v = `Language`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_language )
        )->a( n = `showValueHelp` b = abap_true
        )->a( n = `valueHelpRequest` v = client->_event( `CALL_SHLP_BY_NAME` ) ).

    page->ele( n = `SimpleForm` ns = `form`
        )->a( n = `title` v = `Search-Help with prefilled Selection`
        )->a( n = `editable` b = abap_true
        )->ele( n = `content` ns = `form`
        )->tag( `Text`
        )->a( n = `text` v = `Opens the search help of field USR01-BNAME, its field BNAME prefilled.`
        )->tag( `Label`
        )->a( n = `text` v = `Prefill`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_prefill )
        )->tag( `Label`
        )->a( n = `text` v = `User`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_user )
        )->a( n = `showValueHelp` b = abap_true
        )->a( n = `valueHelpRequest` v = client->_event( `CALL_SHLP_PREFILLED` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD on_after_search.

    " every popup hands its result over through z2ui5_if_popup_result
    DATA temp3 TYPE REF TO z2ui5_if_popup_result.
    DATA popup LIKE temp3.
    temp3 ?= client->get_app_prev( ).

    popup = temp3.
    IF popup->check_confirmed( ) = abap_false.
      RETURN.
    ENDIF.

    CASE client->get_nav_tag( ).
      WHEN `LANGUAGE`.
        mv_language = popup->value_string( ).
      WHEN `USER`.
        mv_user = popup->value_string( ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
