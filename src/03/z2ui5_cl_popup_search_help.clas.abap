CLASS z2ui5_cl_popup_search_help DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- a popup's attributes are its interface to the app that called it, which reads them once the popup returns
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
    " abap2ui5lint-enable unbound-public-attribute

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

    IF client->check_on_init( ) IS NOT INITIAL.
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

    DATA class TYPE string.
    DATA app TYPE string.
    class = z2ui5_cl_popup_context=>rtti_get_classname_by_ref( me ).

    app = z2ui5_cl_popup_context=>url_param_get( val = 'app'
                                              url = client->get( )-s_config-search ).

    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                                data     = mt_data
                                                handle01 = class
                                                handle02 = mv_shlpfield
                                                handle03 = app
                                                handle04 = ``  ).

  ENDMETHOD.

  METHOD render_view.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA dialog TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA simple_form TYPE REF TO z2ui5_cl_ui5_view_builder.
    FIELD-SYMBOLS <data_row> TYPE data.
    DATA temp1 LIKE LINE OF mt_result_desc.
    DATA dfies LIKE REF TO temp1.
      DATA temp2 TYPE z2ui5_cl_popup_context=>ty_ddshiface-value.
      DATA temp3 TYPE z2ui5_cl_popup_context=>ty_ddshiface.
        DATA enabled LIKE abap_true.
      FIELD-SYMBOLS <val> TYPE any.
    FIELD-SYMBOLS <mt_data> TYPE data.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA header TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA columns TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp4 LIKE LINE OF mo_layout->ms_layout-t_layout.
    DATA layout LIKE REF TO temp4.
      DATA lv_index LIKE sy-tabix.
    DATA cells TYPE REF TO z2ui5_cl_ui5_view_builder.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core`
                      )->a( n = `xmlns:form` v = `sap.ui.layout.form` ).


    dialog = popup->ele( `Dialog`
                          )->a( n = `title` v = z2ui5_cl_popup_context=>rtti_get_data_element_texts( `SCRFMTCH`  )-medium
                          )->a( n = `contentWidth` v = '70%'
                          )->a( n = `afterClose` v = client->_event( 'SHLP_CLOSE' ) ).


    simple_form = dialog->ele( n = `SimpleForm` ns = `form`
                                )->a( n = `layout` v = 'ResponsiveGridLayout'
                                )->a( n = `editable` b = abap_true
                                )->ele( n = `content` ns = `form` ).


    ASSIGN ms_data_row->* TO <data_row>.

    " loop over all components


    LOOP AT mt_result_desc REFERENCE INTO dfies.

      " fixed values of the search help are not editable

      CLEAR temp2.

      READ TABLE ms_shlp-interface INTO temp3 WITH KEY shlpfield = dfies->fieldname.
      IF sy-subrc = 0.
        temp2 = temp3-value.
      ENDIF.
      IF temp2 IS INITIAL.

        enabled = abap_true.
      ELSE.
        enabled = abap_false.
      ENDIF.


      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <data_row> TO <val>.

      simple_form->tag( `Label`
          )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( dfies->rollname ) ).

      simple_form->tag( `Input`
          )->a( n = `value` v = client->_bind( <val> )
          )->a( n = `showValueHelp` b = abap_false
          )->a( n = `submit` v = client->_event( 'SHLP_INPUT_DONE' )
          )->a( n = `enabled` b = enabled ).

    ENDLOOP.


    ASSIGN mt_data->* TO <mt_data>.


    table = dialog->ele( `Table`
                      )->a( n = `growing`    v = 'true'
                      )->a( n = `width`      v = 'auto'
                      )->a( n = `items`      v = client->_bind( <mt_data> )
                      )->a( n = `headerText` v = z2ui5_cl_popup_context=>rtti_get_table_desrc( mv_table ) ).


    header = table->ele( `headerToolbar`
                       )->ele( `OverflowToolbar`
                       )->tag( `Title`
                       )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_table_desrc( mv_table )
                       )->tag( `ToolbarSpacer` ).

    header = z2ui5_cl_layo_pop=>render_layout_function( xml    = header
                                                        client = client
                                                        layout = mo_layout ).


    columns = table->ele( `columns` ).



    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      lv_index = sy-tabix.

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


    cells = columns->end(
                      )->ele( `items`
                      )->ele( `ColumnListItem`
                      )->a( n = `vAlign` v = 'Middle'
                      )->a( n = `type` v = 'Navigation'
                      )->a( n = `press` v = client->_event( val   = 'SHLP_ROW_SELECT'
                                                                    arg   = `${ROW_ID}` )
                      )->ele( `cells` ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      cells->ele( `ObjectIdentifier`
          )->a( n = `text` v = |\{{ layout->fname }\}| ).

    ENDLOOP.

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        DATA lt_arg TYPE string_table.
        FIELD-SYMBOLS <row> TYPE any.
        FIELD-SYMBOLS <temp1> LIKE LINE OF lt_arg.
        DATA temp2 LIKE sy-tabix.
        FIELD-SYMBOLS <value> TYPE any.

    CASE client->get( )-event.

      WHEN `SHLP_CLOSE`.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `SHLP_ROW_SELECT`.


        lt_arg = client->get( )-t_event_arg.

        ASSIGN mt_data->* TO <tab>.




        temp2 = sy-tabix.
        READ TABLE lt_arg INDEX 1 ASSIGNING <temp1>.
        sy-tabix = temp2.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        READ TABLE <tab> INDEX <temp1> ASSIGNING <row>.


        ASSIGN COMPONENT mv_shlpfield OF STRUCTURE <row> TO <value>.
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

      WHEN OTHERS.

        z2ui5_cl_layo_pop=>on_event_layout( client = client
                                            layout = mo_layout ).

    ENDCASE.

  ENDMETHOD.

  METHOD factory.
      DATA t_comp TYPE abap_component_tab.
      DATA struct_desc TYPE REF TO cl_abap_structdescr.
      FIELD-SYMBOLS <i_data> TYPE data.
      FIELD-SYMBOLS <mr_data> TYPE data.

    CREATE OBJECT result.

    result->mv_table = i_table.
    result->mv_fname = i_fname.
    result->mv_value = i_value.

    IF i_data IS SUPPLIED.


      t_comp = z2ui5_cl_popup_context=>rtti_get_t_attri_by_any( i_data ).

      struct_desc = cl_abap_structdescr=>create( t_comp ).
      CREATE DATA result->mr_data TYPE HANDLE struct_desc.


      ASSIGN i_data->* TO <i_data>.

      ASSIGN result->mr_data->* TO <mr_data>.

      <mr_data> = <i_data>.

    ENDIF.

  ENDMETHOD.

  METHOD on_after_layout.
          DATA temp5 TYPE REF TO z2ui5_cl_layo_pop.
          DATA app LIKE temp5.

    " only relevant when returning from another app
    IF client->check_on_navigated( ) IS NOT INITIAL.
      TRY.

          temp5 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

          app = temp5.
          mo_layout = app->mo_layout.
          render_view( ).

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD set_selopt.
    FIELD-SYMBOLS <data_row> TYPE data.
    DATA dfies LIKE LINE OF mt_result_desc.
      FIELD-SYMBOLS <value> TYPE any.
      DATA temp6 TYPE z2ui5_cl_popup_context=>ty_shlp_descr-selopt.
      DATA temp7 LIKE LINE OF temp6.

    CLEAR ms_shlp-selopt.


    ASSIGN ms_data_row->* TO <data_row>.


    LOOP AT mt_result_desc INTO dfies.


      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <data_row> TO <value>.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <value> IS INITIAL.
        CONTINUE.
      ENDIF.


      CLEAR temp6.
      temp6 = ms_shlp-selopt.

      temp7-shlpfield = dfies-fieldname.
      temp7-shlpname = ''.
      temp7-sign = 'I'.
      temp7-option = 'CP'.
      temp7-low = |*{ <value> }*|.
      INSERT temp7 INTO TABLE temp6.
      ms_shlp-selopt = temp6.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
