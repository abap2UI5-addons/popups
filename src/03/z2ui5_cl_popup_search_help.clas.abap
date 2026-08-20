CLASS z2ui5_cl_popup_search_help DEFINITION
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
    DATA ms_shlp         TYPE z2ui5_cl_popup_context=>ty_shlp_descr.
    DATA mt_result_desc  TYPE z2ui5_cl_popup_context=>ty_t_dfies_2.
    DATA mr_data         TYPE REF TO data.

    TYPES ty_t_dfies TYPE z2ui5_cl_popup_context=>ty_t_dfies_2.

    CLASS-METHODS factory
      IMPORTING
        i_table       TYPE string
        i_fname       TYPE string
        i_value       TYPE string
        i_data        TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_popup_search_help.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS render_view.
    METHODS on_event.
    METHODS on_after_layout.
    METHODS get_layout.
    METHODS set_selopt.

ENDCLASS.


CLASS z2ui5_cl_popup_search_help IMPLEMENTATION.

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

    z2ui5_cl_popup_context=>bus_search_help_read( CHANGING ms_shlp        = ms_shlp
                                                      mv_fname       = mv_fname
                                                      mv_table       = mv_table
                                                      mr_data        = mr_data
                                                      mt_result_desc = mt_result_desc
                                                      mv_shlpfield   = mv_shlpfield
                                                      mt_data        = mt_data
                                                      ms_data_row    = ms_data_row ).

    get_layout( ).

  ENDMETHOD.

  METHOD get_layout.

    DATA(class) = z2ui5_cl_popup_context=>rtti_get_classname_by_ref( me ).
    DATA(app) = z2ui5_cl_popup_context=>url_param_get( val = 'app'
                                              url = client->get( )-s_config-search ).

    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                                data     = mt_data
                                                handle01 = class
                                                handle02 = mv_shlpfield
                                                handle03 = app
                                                handle04 = ``  ).

  ENDMETHOD.

  METHOD render_view.

    DATA(popup) = z2ui5_cl_ui5_view_builder=>factory( 
                      )->ele( n = `FragmentDefinition` ns = `core` 
                      )->a( n = `xmlns` v = `sap.m` 
                      )->a( n = `xmlns:core` v = `sap.ui.core` 
                      )->a( n = `xmlns:form` v = `sap.ui.layout.form` ).

    DATA(dialog) = popup->ele( `Dialog` 
                          )->a( n = `title` v = z2ui5_cl_popup_context=>rtti_get_data_element_texts( `SCRFMTCH`  )-medium 
                          )->a( n = `contentWidth` v = '70%' 
                          )->a( n = `afterClose` v = client->_event( 'SHLP_CLOSE' ) ).

    DATA(simple_form) = dialog->ele( n = `SimpleForm` ns = `form` 
                                )->a( n = `layout` v = 'ResponsiveGridLayout'
                                )->a( n = `editable` b = abap_true
                                )->ele( n = `content` ns = `form` ).

    ASSIGN ms_data_row->* TO FIELD-SYMBOL(<data_row>).

    " loop over all components
    LOOP AT mt_result_desc REFERENCE INTO DATA(dfies).

      " fixed values of the search help are not editable
      IF VALUE #( ms_shlp-interface[ shlpfield = dfies->fieldname ]-value OPTIONAL ) IS INITIAL.
        DATA(enabled) = abap_true.
      ELSE.
        enabled = abap_false.
      ENDIF.

      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <data_row> TO FIELD-SYMBOL(<val>).

      simple_form->tag( `Label` 
          )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( dfies->rollname ) ).

      simple_form->tag( `Input` 
          )->a( n = `value` v = client->_bind_edit( <val> ) 
          )->a( n = `showValueHelp` b = abap_false 
          )->a( n = `submit` v = client->_event( 'SHLP_INPUT_DONE' ) 
          )->a( n = `enabled` b = enabled ).

    ENDLOOP.

    ASSIGN mt_data->* TO FIELD-SYMBOL(<mt_data>).

    DATA(table) = dialog->ele( `Table`
                      )->a( n = `growing`    v = 'true'
                      )->a( n = `width`      v = 'auto'
                      )->a( n = `items`      v = client->_bind( <mt_data> )
                      )->a( n = `headerText` v = z2ui5_cl_popup_context=>rtti_get_table_desrc( mv_table ) ).

    DATA(header) = table->ele( `headerToolbar` 
                       )->ele( `OverflowToolbar` 
                       )->tag( `Title` 
                       )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_table_desrc( mv_table ) 
                       )->tag( `ToolbarSpacer` ).

    header = z2ui5_cl_layo_pop=>render_layout_function( xml    = header
                                                        client = client
                                                        layout = mo_layout ).

    DATA(columns) = table->ele( `columns` ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO DATA(layout).
      DATA(lv_index) = sy-tabix.

      columns->ele( `Column` 
          )->a( n = `visible` v = client->_bind( val       = layout->visible
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
*                       halign          = client->_bind( val       = layout->halign
*                       tab             = mo_layout->ms_layout-t_layout
*                       tab_index       = lv_index )
*                       importance      = client->_bind( val       = layout->importance
*                       tab             = mo_layout->ms_layout-t_layout
*                       tab_index       = lv_index ) 
          )->a( n = `mergeDuplicates` v = client->_bind( val       = layout->merge
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index ) 
          )->a( n = `minScreenWidth` v = client->_bind( val       = layout->width
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index ) 
          )->tag( `Text` 
          )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( layout->rollname ) ).

    ENDLOOP.

    DATA(cells) = columns->end( 
                      )->ele( `items` 
                      )->ele( `ColumnListItem` 
                      )->a( n = `vAlign` v = 'Middle' 
                      )->a( n = `type` v = 'Navigation' 
                      )->a( n = `press` v = client->_event( val   = 'SHLP_ROW_SELECT'
                                                                    t_arg = VALUE #( ( `${ROW_ID}`  ) ) ) 
                      )->ele( `cells` ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      cells->ele( `ObjectIdentifier` 
          )->a( n = `text` v = |\{{ layout->fname }\}| ).

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
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        mv_return_value = <value>.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'SHLP_INPUT_DONE'.

        set_selopt( ).

        z2ui5_cl_popup_context=>bus_search_help_read( CHANGING ms_shlp        = ms_shlp
                                                          mv_fname       = mv_fname
                                                          mv_table       = mv_table
                                                          mr_data        = mr_data
                                                          mt_result_desc = mt_result_desc
                                                          mv_shlpfield   = mv_shlpfield
                                                          mt_data        = mt_data
                                                          ms_data_row    = ms_data_row ).

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

      DATA(t_comp) = z2ui5_cl_popup_context=>rtti_get_t_attri_by_any( i_data ).
      DATA(struct_desc) = cl_abap_structdescr=>create( t_comp ).
      CREATE DATA result->mr_data TYPE HANDLE struct_desc.

      ASSIGN i_data->* TO FIELD-SYMBOL(<i_data>).
      ASSIGN result->mr_data->* TO FIELD-SYMBOL(<mr_data>).

      <mr_data> = <i_data>.

    ENDIF.

  ENDMETHOD.

  METHOD on_after_layout.

    " only relevant when returning from another app
    IF client->check_on_navigated( ) = abap_false.
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

    ASSIGN ms_data_row->* TO FIELD-SYMBOL(<data_row>).

    LOOP AT mt_result_desc INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <data_row> TO FIELD-SYMBOL(<value>).

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
