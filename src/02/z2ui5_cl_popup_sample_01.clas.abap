CLASS z2ui5_cl_popup_sample_01 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_arbgb TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event.
    METHODS render_main.
    METHODS call_f4.

  PRIVATE SECTION.
    METHODS on_after_f4.

ENDCLASS.


CLASS z2ui5_cl_popup_sample_01 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `CALL_POPUP_F4`.
        call_f4( ).

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
                     )->a( n = `title` v = 'Value-Help' 
                     )->a( n = `navButtonPress` v = client->_event( 'BACK' ) 
                     )->a( n = `showNavButton` b = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL ) 
                     )->a( n = `class` v = 'sapUiContentPadding' ).

    page->ele( n = `SimpleForm` ns = `form` 
        )->a( n = `title` v = 'F4-Help' 
        )->a( n = `editable` b = abap_true 
        )->ele( n = `content` ns = `form` 
        )->tag( `Text` 
        )->a( n = `text` v = `Table t100 field ARBGB is linked to table t100a field ARBGB via a foreign key link.` 
        )->tag( `Label` 
        )->a( n = `text` v = `ARBGB` 
        )->tag( `Input` 
        )->a( n = `value` v = client->_bind_edit( mv_arbgb ) 
        )->a( n = `showValueHelp` b = abap_true 
        )->a( n = `valueHelpRequest` v = client->_event( val   = 'CALL_POPUP_F4'
                                                                     t_arg = VALUE #( ( `ARBGB` ) ( `T100` ) ) ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_init( ).
      on_init( ).
    ENDIF.

    on_event( ).
    on_after_f4( ).

  ENDMETHOD.

  METHOD call_f4.

    DATA(lt_arg) = client->get( )-t_event_arg.
    DATA(f4_field) = VALUE string( lt_arg[ 1 ] ).
    DATA(f4_table) = VALUE string( lt_arg[ 2 ] ).

    client->nav_app_call( z2ui5_cl_popup_value_help=>factory( i_table = f4_table
                                                            i_fname = f4_field
                                                            i_value = mv_arbgb ) ).

  ENDMETHOD.

  METHOD on_after_f4.

    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        DATA(app) = CAST z2ui5_cl_popup_value_help( client->get_app( client->get( )-s_draft-id_prev_app ) ).

        IF app->mv_return_value IS NOT INITIAL.

          CASE app->mv_field.
            WHEN `ARBGB`.
              mv_arbgb = app->mv_return_value.

            WHEN OTHERS.

          ENDCASE.

          client->view_model_update( ).

        ENDIF.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
