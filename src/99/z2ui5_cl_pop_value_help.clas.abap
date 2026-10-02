CLASS z2ui5_cl_pop_value_help DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " abap2ui5lint-disable unbound-public-attribute -- obsolete class, kept unchanged for compatibility
    INTERFACES z2ui5_if_app.

    DATA mt_data         TYPE REF TO data.
    DATA ms_data_row     TYPE REF TO data.
    DATA mo_layout       TYPE REF TO z2ui5_cl_layo_manager.

    DATA mv_table        TYPE string.
    DATA mv_field        TYPE string.
    DATA mv_value        TYPE string.
    DATA mv_return_value TYPE string.
    DATA mv_rows         TYPE int1 VALUE '50'.
    DATA mt_dfies        TYPE z2ui5_cl_popup_context=>ty_t_dfies.

    CLASS-METHODS factory
      IMPORTING
        i_table       TYPE string
        i_fname       TYPE string
        i_value       TYPE string
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_pop_value_help.
    " abap2ui5lint-enable unbound-public-attribute

  PROTECTED SECTION.
    DATA client             TYPE REF TO z2ui5_if_client.
    DATA mv_check_tab_field TYPE string.
    DATA mv_check_tab       TYPE string.

    METHODS get_dfies.

    METHODS on_init.

    METHODS render_view.

    METHODS on_event.

    METHODS set_row_id.

    METHODS get_data
      IMPORTING
        !where TYPE string.

    METHODS prefill_inputs.

    METHODS on_after_layout.

    METHODS get_layout.

    METHODS create_objects.
ENDCLASS.


CLASS z2ui5_cl_pop_value_help IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ) IS NOT INITIAL.
      on_init( ).

      IF mv_check_tab IS INITIAL.
        RETURN.
      ENDIF.
      render_view( ).

    ENDIF.

    on_event( ).
    on_after_layout( ).

  ENDMETHOD.

  METHOD on_init.
    DATA result TYPE string.

    get_dfies( ).

    IF mv_check_tab IS INITIAL.
      RETURN.
    ENDIF.

    create_objects( ).
    prefill_inputs( ).


    result = z2ui5_cl_popup_context=>tab_get_where_by_dfies( mv_check_tab_field = mv_check_tab_field
                                                              ms_data_row        = ms_data_row
                                                              it_dfies           = mt_dfies ).

    get_data( result ).
    get_layout( ).

  ENDMETHOD.

  METHOD create_objects.

    DATA index TYPE int4.
        DATA temp1 TYPE cl_abap_structdescr=>component_table.
        DATA temp2 LIKE LINE OF temp1.
        DATA temp3 TYPE REF TO cl_abap_datadescr.
        DATA comp LIKE temp1.
        DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA new_table_desc TYPE REF TO cl_abap_tabledescr.

    TRY.


        CLEAR temp1.

        temp2-name = 'ROW_ID'.

        temp3 ?= cl_abap_datadescr=>describe_by_data( index ).
        temp2-type = temp3.
        INSERT temp2 INTO TABLE temp1.

        comp = temp1.

        APPEND LINES OF z2ui5_cl_popup_context=>rtti_get_t_attri_by_table_name( mv_check_tab  ) TO comp.


        new_struct_desc = cl_abap_structdescr=>create( comp ).


        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mt_data     TYPE HANDLE new_table_desc.
        CREATE DATA ms_data_row TYPE HANDLE new_struct_desc.

      CATCH cx_root ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

  METHOD get_data.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    TRY.
        ASSIGN mt_data->* TO <table>.

        SELECT *
          FROM (mv_check_tab)
          INTO CORRESPONDING FIELDS OF TABLE <table>
          UP TO mv_rows ROWS
          WHERE (where).

        IF sy-subrc <> 0.
          client->message_toast_display( 'No Entries found.' ).
        ENDIF.

        set_row_id( ).

      CATCH cx_root.
        client->message_toast_display( 'Table not released.' ).
    ENDTRY.

  ENDMETHOD.

  METHOD render_view.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA dialog TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA simple_form TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp3 LIKE LINE OF mt_dfies.
    DATA dfies LIKE REF TO temp3.
      FIELD-SYMBOLS <row> TYPE data.
      FIELD-SYMBOLS <val> TYPE any.
    FIELD-SYMBOLS <table> TYPE data.
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
                          )->a( n = `title` v = z2ui5_cl_popup_context=>rtti_get_data_element_texts( `/IWFND/SU_GWC_RH_VH`  )-medium
                          )->a( n = `contentWidth` v = '90%'
                          )->a( n = `afterClose` v = client->_event( 'F4_CLOSE' ) ).


    simple_form = dialog->ele( n = `SimpleForm` ns = `form`
                                )->a( n = `layout` v = 'ResponsiveGridLayout'
                                )->a( n = `editable` b = abap_true
                                )->ele( n = `content` ns = `form` ).



    LOOP AT mt_dfies REFERENCE INTO dfies.

      IF dfies->fieldname = `MANDT`.
        CONTINUE.
      ENDIF.
      IF NOT ( dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field ).
        CONTINUE.
      ENDIF.


      ASSIGN ms_data_row->* TO <row>.


      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO <val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      simple_form->tag( `Label`
          )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( dfies->rollname ) ).

      simple_form->tag( `Input`
          )->a( n = `value` v = client->_bind_edit( <val> )
          )->a( n = `showValueHelp` b = abap_false
          )->a( n = `submit` v = client->_event( 'F4_INPUT_DONE' ) ).

    ENDLOOP.

    simple_form->tag( `Label`
        )->a( n = `text` v = z2ui5_cl_popup_context=>rtti_get_data_element_text_l( 'SYST_TABIX' ) ).

    simple_form->tag( `Input`
        )->a( n = `value` v = client->_bind_edit( mv_rows )
        )->a( n = `showValueHelp` b = abap_false
        )->a( n = `submit` v = client->_event( 'F4_INPUT_DONE' )
        )->a( n = `maxLength` v = '3' ).


    ASSIGN mt_data->* TO <table>.


    table = dialog->ele( `Table`
                      )->a( n = `growing`    v = 'true'
                      )->a( n = `width`      v = 'auto'
                      )->a( n = `items`      v = client->_bind( val = <table> )
                      )->a( n = `headerText` t = mv_check_tab ).


    header = table->ele( `headerToolbar`
                       )->ele( `OverflowToolbar`
                       )->tag( `Title`
                       )->a( n = `text` t = mv_check_tab
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
          )->a( n = `mergeDuplicates` v = client->_bind( val       = layout->merge
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->a( n = `minScreenWidth` v = client->_bind( val       = layout->width
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->tag( `Text`
          )->a( n = `text` v = layout->tlabel ).

    ENDLOOP.


    cells = columns->end(
                      )->ele( `items`
                      )->ele( `ColumnListItem`
                      )->a( n = `vAlign` v = 'Middle'
                      )->a( n = `type` v = 'Navigation'
                      )->a( n = `press` v = client->_event( val   = 'F4_ROW_SELECT'
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
        DATA result TYPE string.

    CASE client->get( )-event.

      WHEN `F4_CLOSE`.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `F4_ROW_SELECT`.


        lt_arg = client->get( )-t_event_arg.

        ASSIGN mt_data->* TO <tab>.




        temp2 = sy-tabix.
        READ TABLE lt_arg INDEX 1 ASSIGNING <temp1>.
        sy-tabix = temp2.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        READ TABLE <tab> INDEX <temp1> ASSIGNING <row>.


        ASSIGN COMPONENT mv_check_tab_field OF STRUCTURE <row> TO <value>.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        mv_return_value = <value>.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'F4_INPUT_DONE'.


        result = z2ui5_cl_popup_context=>tab_get_where_by_dfies( mv_check_tab_field = mv_check_tab_field
                                                                  ms_data_row        = ms_data_row
                                                                  it_dfies           = mt_dfies ).

        get_data( result ).

        client->popup_model_update( ).

      WHEN OTHERS.

        z2ui5_cl_layo_pop=>on_event_layout( client = client
                                            layout = mo_layout ).

    ENDCASE.

  ENDMETHOD.

  METHOD set_row_id.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
      DATA lv_tabix LIKE sy-tabix.
      FIELD-SYMBOLS <row> TYPE any.

    ASSIGN mt_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.


      lv_tabix = sy-tabix.


      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO <row>.
      IF sy-subrc = 0.
        <row> = lv_tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD factory.

    CREATE OBJECT result.

    result->mv_table = i_table.
    result->mv_field = i_fname.
    result->mv_value = i_value.

  ENDMETHOD.

  METHOD get_dfies.

    DATA t_dfies TYPE z2ui5_cl_popup_context=>ty_t_dfies.
    DATA dfies TYPE REF TO z2ui5_cl_popup_context=>ty_s_dfies.
    DATA temp5 TYPE string.
    DATA temp6 TYPE string.
    DATA temp7 TYPE z2ui5_cl_popup_context=>ty_s_dfies.
      DATA temp8 TYPE string.
      DATA temp9 TYPE z2ui5_cl_popup_context=>ty_s_dfies.
    t_dfies = z2ui5_cl_popup_context=>rtti_get_t_dfies_by_table_name( mv_table ).


    READ TABLE t_dfies REFERENCE INTO dfies WITH KEY fieldname = mv_field.
    IF sy-subrc <> 0.

      client->popup_destroy( ).
      client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
      RETURN.

    ENDIF.

    IF dfies->checktable IS INITIAL.
      RETURN.
    ENDIF.


    temp5 = dfies->checktable.
    mt_dfies = z2ui5_cl_popup_context=>rtti_get_t_dfies_by_table_name( temp5 ).

    " determine the field of the check table, first via the data element

    CLEAR temp6.

    READ TABLE mt_dfies INTO temp7 WITH KEY rollname = dfies->rollname.
    IF sy-subrc = 0.
      temp6 = temp7-fieldname.
    ENDIF.
    mv_check_tab_field = temp6.

    " as a fallback, try to find it via the domain
    IF mv_check_tab_field IS INITIAL.

      CLEAR temp8.

      READ TABLE mt_dfies INTO temp9 WITH KEY domname = dfies->domname.
      IF sy-subrc = 0.
        temp8 = temp9-fieldname.
      ENDIF.
      mv_check_tab_field = temp8.
    ENDIF.
    mv_check_tab = dfies->checktable.

  ENDMETHOD.

  METHOD prefill_inputs.

    DATA temp10 LIKE LINE OF mt_dfies.
    DATA dfies LIKE REF TO temp10.
      FIELD-SYMBOLS <row> TYPE data.
      FIELD-SYMBOLS <val> TYPE any.
    LOOP AT mt_dfies REFERENCE INTO dfies.

      IF NOT ( dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field ).
        CONTINUE.
      ENDIF.


      ASSIGN ms_data_row->* TO <row>.


      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO <val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF dfies->fieldname = mv_check_tab_field.

        <val> = mv_value.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_after_layout.
        DATA temp11 TYPE REF TO z2ui5_cl_layo_pop.
        DATA app LIKE temp11.

    " only relevant when returning from another app
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " check if the previous app was the layout popup

        temp11 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp11.

        mo_layout = app->mo_layout.

        render_view( ).

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD get_layout.

    DATA class TYPE string.
    class = z2ui5_cl_popup_context=>rtti_get_classname_by_ref( me ).

    mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                                data     = mt_data
                                                handle01 = class
                                                handle02 = mv_table
                                                handle03 = 'F4'  ).

  ENDMETHOD.

ENDCLASS.
