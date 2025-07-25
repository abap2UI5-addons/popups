CLASS z2ui5_cl_pop_search_help DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_table        TYPE string.
    DATA mv_fname        TYPE string.
    DATA mv_shlpfield    TYPE string.
    DATA mv_value        TYPE string.
    DATA mv_return_value TYPE string.
    DATA mv_rows         TYPE int1 VALUE '50'.
    DATA mt_data         TYPE REF TO data.
    DATA ms_data_row     TYPE REF TO data.
    DATA mo_layout       TYPE REF TO z2ui5_cl_layo_manager.
    DATA ms_shlp         TYPE z2ui5_cl_util_ext=>ty_shlp_descr.
    DATA mt_result_desc  TYPE z2ui5_cl_util_ext=>ty_t_dfies_2. "dfies.
    DATA mr_data         TYPE REF TO data.

    TYPES ty_t_dfies TYPE z2ui5_cl_util_ext=>ty_t_dfies_2.

    CLASS-METHODS factory
      IMPORTING
        i_table       TYPE string
        i_fname       TYPE string
        i_value       TYPE string
        i_data        TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_pop_search_help.

  PROTECTED SECTION.

    DATA client  TYPE REF TO z2ui5_if_client.
    METHODS on_init.
    METHODS render_view.
    METHODS on_event.
    METHODS on_after_layout.
    METHODS get_layout.
    METHODS set_selopt.
    METHODS set_init_selopt.

  PRIVATE SECTION.

ENDCLASS.


CLASS z2ui5_cl_pop_search_help IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      on_init( ).
      render_view( ).
    ELSE.
      on_event( ).
      on_after_layout( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_init.

    z2ui5_cl_util_ext=>bus_search_help_read(
         CHANGING
           ms_shlp        = ms_shlp
           mv_fname       = mv_fname
           mv_table       = mv_table
           mr_data        = mr_data
           mt_result_desc = mt_result_desc
           mv_shlpfield   = mv_shlpfield
           mt_data        = mt_data
           ms_data_row    = ms_data_row
       ).

    get_layout( ).

  ENDMETHOD.

  METHOD get_layout.

    DATA(class) = z2ui5_cl_util=>rtti_get_classname_by_ref( me ).
    DATA(app) = z2ui5_cl_util=>url_param_get( val = 'app'
                                              url = client->get( )-s_config-search ).

    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                          data     = mt_data
                                          handle01 = class
                                          handle02 = mv_shlpfield
                                          handle03 = app
                                          handle04 = ``  ).

  ENDMETHOD.

  METHOD render_view.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(simple_form) = popup->dialog( title        = z2ui5_cl_util=>rtti_get_data_element_texts( `SCRFMTCH`  )-medium
                                       contentwidth = '70%'
                                       afterclose   = client->_event( 'SHLP_CLOSE' )
          )->simple_form( layout   = 'ResponsiveGridLayout'
                          editable = abap_true
          )->content( ns = 'form' ).

    " Gehe über alle Comps
    LOOP AT mt_result_desc REFERENCE INTO DATA(dfies).

      " Fixed Value in Searchhelp
      IF VALUE #( ms_shlp-interface[ shlpfield = dfies->fieldname ]-value OPTIONAL ) IS INITIAL.
        DATA(enabled) = abap_true.
      ELSE.
        enabled = abap_false.
      ENDIF.

      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE ms_data_row->* TO FIELD-SYMBOL(<val>).

      simple_form->label( text = z2ui5_cl_util=>rtti_get_data_element_text_l( dfies->rollname ) ).

      simple_form->input( value         = client->_bind_edit( <val> )
                          showvaluehelp = abap_false
                          submit        = client->_event( 'SHLP_INPUT_DONE' )
                          enabled       = enabled ).

    ENDLOOP.

    DATA(table) = popup->get_child( )->table( growing    = 'true'
                                              width      = 'auto'
                                              items      = client->_bind( val = mt_data->* )
                                              headertext = z2ui5_cl_util_abap=>rtti_get_table_desrc( mv_table ) ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(headder) = table->header_toolbar(
                 )->overflow_toolbar(
                 )->title( text = z2ui5_cl_util_abap=>rtti_get_table_desrc( mv_table )
                 )->toolbar_spacer( ).

    headder = z2ui5_cl_layo_pop=>render_layout_function( xml    = headder
                                                                   client = client
                                                                   layout = mo_layout ).

    DATA(columns) = table->columns( ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO DATA(layout).
      DATA(lv_index) = sy-tabix.

      columns->column( visible         = client->_bind( val       = layout->visible
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
*                       halign          = client->_bind( val       = layout->halign
*                                                        tab       = mo_layout->ms_layout-t_layout
*                                                        tab_index = lv_index )
*                       importance      = client->_bind( val       = layout->importance
*                                                        tab       = mo_layout->ms_layout-t_layout
*                                                        tab_index = lv_index )
                       mergeduplicates = client->_bind( val       = layout->merge
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
                       minscreenwidth  = client->_bind( val       = layout->width
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
       )->text( z2ui5_cl_util=>rtti_get_data_element_text_l( layout->rollname ) ).

    ENDLOOP.

    DATA(cells) = columns->get_parent( )->items(
                                       )->column_list_item(
                                           valign = 'Middle'
                                           type   = 'Navigation'
                                           press  = client->_event( val   = 'SHLP_ROW_SELECT'
                                                                    t_arg = VALUE #( ( `${ROW_ID}`  ) ) )
                                       )->cells( ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      cells->object_identifier( text = |\{{ layout->fname }\}| ).

    ENDLOOP.

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    CASE client->get( )-event.

      WHEN `SHLP_CLOSE`.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `SHLP_ROW_SELECT`.

        DATA(lt_arg) = client->get( )-t_event_arg.

        ASSIGN mt_data->* TO <tab>.

        ASSIGN <tab>[ lt_arg[ 1 ] ] TO FIELD-SYMBOL(<row>).

        ASSIGN COMPONENT mv_shlpfield OF STRUCTURE <row> TO FIELD-SYMBOL(<value>).

        mv_return_value = <value>.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'SHLP_INPUT_DONE'.

        set_selopt( ).

        z2ui5_cl_util_ext=>bus_search_help_read(
          CHANGING
            ms_shlp        = ms_shlp
            mv_fname       = mv_fname
            mv_table       = mv_table
            mr_data        = mr_data
            mt_result_desc = mt_result_desc
            mv_shlpfield   = mv_shlpfield
            mt_data        = mt_data
            ms_data_row    = ms_data_row
        ).

        client->popup_model_update( ).

      WHEN OTHERS.

        z2ui5_cl_layo_pop=>on_event_layout( client = client
                                                      layout = mo_layout ).

    ENDCASE.

  ENDMETHOD.

  METHOD factory.

    result = NEW #( ).

    result->mv_table = i_table.
    result->mv_fname = i_fname.
    result->mv_value = i_value.

    IF i_data IS SUPPLIED.

      DATA(t_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_any( i_data ).
      DATA(struct_desc) = cl_abap_structdescr=>create( t_comp ).
      CREATE DATA result->mr_data TYPE HANDLE struct_desc.

      result->mr_data->* = i_data->*.

    ENDIF.

  ENDMETHOD.


  METHOD on_after_layout.

    IF client->check_on_navigated( ).
      RETURN.
    ENDIF.

    TRY.
        DATA(app) = CAST z2ui5_cl_layo_pop( client->get_app( client->get( )-s_draft-id_prev_app ) ).
        mo_layout = app->mo_layout.
        render_view( ).

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD set_selopt.

    CLEAR ms_shlp-selopt.

    LOOP AT mt_result_desc INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE ms_data_row->* TO FIELD-SYMBOL(<value>).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.

      ms_shlp-selopt = VALUE #( BASE ms_shlp-selopt
                                ( shlpfield = dfies-fieldname
                                  shlpname  = ''
                                  sign      = 'I'
                                  option    = 'CP'
                                  low       = |*{ <value> }*|  ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD set_init_selopt.

    CLEAR ms_shlp-selopt.

    LOOP AT mt_result_desc INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE mr_data->* TO FIELD-SYMBOL(<value>).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.

      ms_shlp-selopt = VALUE #( BASE ms_shlp-selopt
                                ( shlpfield = dfies-fieldname
                                  shlpname  = ''
                                  sign      = 'I'
                                  option    = 'CP'
                                  low       = |*{ <value> }*|  ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
